//! Conditional-compilation predicates (`#[cfg(...)]`).
//!
//! Mirrors Rust's cfg grammar: atoms (`name` or `name = "value"`), composed
//! via `any(...)`, `all(...)`, `not(...)`. Predicates are evaluated by each
//! backend at emission time against a [`CfgContext`] that supplies the
//! current backend name and (in the future) other axes like project
//! pointer size or feature flags.
//!
//! # Closed-world semantics
//!
//! Atoms that aren't known to the evaluator's context evaluate to **false**.
//! `not(unknown)` is therefore `true`. This means
//! `#[cfg(not(backend = "rust"))]` correctly emits in cpp/json/anything-else
//! and a future backend won't accidentally exclude code that was meant to
//! be "everything except X."

#[cfg(test)]
use crate::span::StripLocations;
use crate::span::{EqualsIgnoringLocations, HasLocation, ItemLocation};

/// A single cfg atom: either a bare identifier (`cfg(test)`) or a key/value
/// pair (`cfg(backend = "cpp")`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub enum CfgAtom {
    Ident {
        name: String,
        location: ItemLocation,
    },
    KeyValue {
        key: String,
        value: String,
        location: ItemLocation,
    },
}

impl CfgAtom {
    pub fn location(&self) -> &ItemLocation {
        match self {
            CfgAtom::Ident { location, .. } | CfgAtom::KeyValue { location, .. } => location,
        }
    }
}

impl EqualsIgnoringLocations for CfgAtom {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        match (self, other) {
            (CfgAtom::Ident { name: a, .. }, CfgAtom::Ident { name: b, .. }) => a == b,
            (
                CfgAtom::KeyValue {
                    key: ka, value: va, ..
                },
                CfgAtom::KeyValue {
                    key: kb, value: vb, ..
                },
            ) => ka == kb && va == vb,
            _ => false,
        }
    }
}

/// A cfg predicate AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub enum CfgPredicate {
    Atom {
        atom: CfgAtom,
        location: ItemLocation,
    },
    Any {
        predicates: Vec<CfgPredicate>,
        location: ItemLocation,
    },
    All {
        predicates: Vec<CfgPredicate>,
        location: ItemLocation,
    },
    Not {
        predicate: Box<CfgPredicate>,
        location: ItemLocation,
    },
}

impl EqualsIgnoringLocations for CfgPredicate {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        match (self, other) {
            (CfgPredicate::Atom { atom: a, .. }, CfgPredicate::Atom { atom: b, .. }) => {
                a.equals_ignoring_locations(b)
            }
            (CfgPredicate::Any { predicates: a, .. }, CfgPredicate::Any { predicates: b, .. })
            | (CfgPredicate::All { predicates: a, .. }, CfgPredicate::All { predicates: b, .. }) => {
                a.equals_ignoring_locations(b)
            }
            (CfgPredicate::Not { predicate: a, .. }, CfgPredicate::Not { predicate: b, .. }) => {
                a.equals_ignoring_locations(b)
            }
            _ => false,
        }
    }
}

/// Evaluation context. Currently only carries the active backend; in
/// the future it'll grow keys for `pointer_size`, `feature`, etc.
#[derive(Debug, Clone, Copy)]
pub struct CfgContext {
    pub backend: crate::Backend,
}

impl CfgContext {
    /// Resolve a single atom to true / false.
    fn resolve_atom(&self, atom: &CfgAtom) -> bool {
        match atom {
            // Bare identifier atoms have no values currently. None of pyxis's
            // built-in axes ("backend", "pointer_size") are bare flags, so
            // every bare ident is unknown → false (closed-world).
            CfgAtom::Ident { .. } => false,
            CfgAtom::KeyValue { key, value, .. } => match key.as_str() {
                "backend" => value == self.backend.name(),
                _ => false,
            },
        }
    }
}

impl CfgPredicate {
    pub fn evaluate(&self, ctx: &CfgContext) -> bool {
        match self {
            CfgPredicate::Atom { atom, .. } => ctx.resolve_atom(atom),
            CfgPredicate::Any { predicates, .. } => predicates.iter().any(|p| p.evaluate(ctx)),
            CfgPredicate::All { predicates, .. } => predicates.iter().all(|p| p.evaluate(ctx)),
            CfgPredicate::Not { predicate, .. } => !predicate.evaluate(ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn loc() -> ItemLocation {
        ItemLocation::test()
    }
    fn ident(name: &str) -> CfgPredicate {
        CfgPredicate::Atom {
            atom: CfgAtom::Ident {
                name: name.into(),
                location: loc(),
            },
            location: loc(),
        }
    }
    fn kv(key: &str, value: &str) -> CfgPredicate {
        CfgPredicate::Atom {
            atom: CfgAtom::KeyValue {
                key: key.into(),
                value: value.into(),
                location: loc(),
            },
            location: loc(),
        }
    }
    fn any(ps: Vec<CfgPredicate>) -> CfgPredicate {
        CfgPredicate::Any {
            predicates: ps,
            location: loc(),
        }
    }
    fn all(ps: Vec<CfgPredicate>) -> CfgPredicate {
        CfgPredicate::All {
            predicates: ps,
            location: loc(),
        }
    }
    fn not(p: CfgPredicate) -> CfgPredicate {
        CfgPredicate::Not {
            predicate: Box::new(p),
            location: loc(),
        }
    }

    fn ctx(backend: crate::Backend) -> CfgContext {
        CfgContext { backend }
    }

    #[cfg(feature = "cpp")]
    fn cpp() -> CfgContext {
        ctx(crate::Backend::Cpp)
    }
    fn rust() -> CfgContext {
        ctx(crate::Backend::Rust)
    }
    #[cfg(feature = "json")]
    fn json() -> CfgContext {
        ctx(crate::Backend::Json)
    }

    #[cfg(feature = "cpp")]
    #[test]
    fn atom_backend_match() {
        let p = kv("backend", "cpp");
        assert!(p.evaluate(&cpp()));
        assert!(!p.evaluate(&rust()));
    }

    #[test]
    fn unknown_atom_is_false() {
        // Closed-world: unknown atoms always false.
        assert!(!ident("test").evaluate(&rust()));
        assert!(!kv("unknown_key", "x").evaluate(&rust()));
    }

    #[cfg(all(feature = "cpp", feature = "json"))]
    #[test]
    fn any_short_circuits_on_first_true() {
        let p = any(vec![kv("backend", "rust"), kv("backend", "cpp")]);
        assert!(p.evaluate(&cpp()));
        assert!(p.evaluate(&rust()));
        assert!(!p.evaluate(&json()));
    }

    #[cfg(feature = "cpp")]
    #[test]
    fn all_requires_every_clause() {
        let p = all(vec![kv("backend", "cpp"), ident("test")]);
        // Even when backend matches, the unknown `test` atom is false.
        assert!(!p.evaluate(&cpp()));
    }

    #[cfg(all(feature = "cpp", feature = "json"))]
    #[test]
    fn not_inverts() {
        // `not(backend = "rust")` should be true on every non-rust backend,
        // including hypothetical future ones (closed-world keeps `not`
        // sane: an unknown backend is just "not rust").
        let p = not(kv("backend", "rust"));
        assert!(p.evaluate(&cpp()));
        assert!(p.evaluate(&json()));
        assert!(!p.evaluate(&rust()));
    }

    #[cfg(all(feature = "cpp", feature = "json"))]
    #[test]
    fn nested_combinations() {
        // any(all(backend = "cpp", not(test)), backend = "rust")
        let p = any(vec![
            all(vec![kv("backend", "cpp"), not(ident("test"))]),
            kv("backend", "rust"),
        ]);
        assert!(p.evaluate(&cpp())); // first arm: cpp + not(test=false)=true
        assert!(p.evaluate(&rust())); // second arm
        assert!(!p.evaluate(&json())); // neither arm
    }

    #[test]
    fn empty_any_and_all() {
        // Mathematical conventions: empty any = false, empty all = true.
        assert!(!any(vec![]).evaluate(&rust()));
        assert!(all(vec![]).evaluate(&rust()));
    }
}
