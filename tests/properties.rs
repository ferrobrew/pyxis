//! Property-based tests (proptest) for stated properties of core functions:
//!
//! - parser round-trip: `pretty_print` → reparse yields an equivalent AST
//! - cfg-predicate evaluation: total, and agrees with a direct reference
//! - span strip-locations: idempotent (`f(f(x)) == f(x)`)
//!
//! These are the first property tests in the workspace. Degenerate cases
//! (empty, single token) are exercised explicitly: proptest strategies below
//! include them, and fixed assertions cover the empty-input boundaries.

// Test code may use unwrap/expect/panic freely (the restriction lints target
// production code); see the contributing guidelines.
#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::unreachable
)]

use std::str::FromStr;

use proptest::prelude::*;
use pyxis::{
    Backend,
    parser::cfg::{CfgAtom, CfgContext, CfgPredicate},
    pretty_print::pretty_print,
    span::FileId,
};

/// A generated pyxis module source string. Includes degenerate inputs: the
/// empty string and single-token fragments, so the round-trip property is
/// exercised at the boundaries the source language actually has.
fn arbitrary_module_source() -> impl Strategy<Value = String> {
    prop_oneof![
        // Degenerate: empty and near-empty inputs.
        Just(String::new()),
        Just("pub".to_string()),
        Just("#".to_string()),
        // A realistic small module: a type with a field, doc comments, and a
        // cfg-gated method. The grammar's surface here is rich enough that a
        // malformed-but-parseable variant still exercises the pretty-printer
        // on structurally varied input.
        prop_oneof![
            Just("pub type A {\n    pub x: u32,\n}\n".to_string()),
            Just(
                "/// docs\npub type B {\n    pub y: *mut B,\n    vftable {\n        fn f();\n    },\n}\n"
                    .to_string()
            ),
            Just("pub const C: u32 = 5;\n".to_string()),
            Just("pub enum E : u32 {\n    One = 1,\n}\n".to_string()),
            Just("use a::B;\n\npub type D {\n    pub z: a::B,\n}\n".to_string()),
        ],
        // Random concatenation of spelling-level fragments that may or may
        // not parse: the property only holds for parseable inputs, so the
        // test filters to `Ok`.
        proptest::collection::vec(
            prop_oneof![Just("pub type Aa { x: u32 }"), Just("pub const K: u32 = 1;"), Just("anything")],
            0..8,
        )
        .prop_map(|parts| parts.join("\n") + "\n"),
    ]
}

proptest! {
    /// Round-trip: for any source that parses, pretty-printing and re-parsing
    /// yields printed output identical to printing the original parse.
    ///
    /// `pretty_print` is a fixpoint: `print(parse(print(m))) == print(m)` for
    /// the AST `m` obtained from arbitrary parseable source. This is the
    /// `decode(encode(x)) == x` round-trip property with `encode = print`,
    /// `decode = parse`, compared on the printed form (the parser's full AST
    /// equality is location-sensitive, and `StripLocations` is test-only).
    #[test]
    fn pretty_print_round_trip_is_stable(source in arbitrary_module_source()) {
        let Ok(module) = pyxis::parser::parse_str_with_file_id(&source, FileId::INTERNAL) else {
            // Not parseable — the round-trip property is only defined on
            // parseable inputs.
            return Ok(());
        };

        let printed = pretty_print(&module);
        let reparsed =
            pyxis::parser::parse_str_with_file_id(&printed, FileId::INTERNAL).expect(
                "pretty-printed output of a parseable module must itself parse",
            );

        // Re-printing the reparsed AST yields the same text: the formatter is
        // a fixpoint, so the parse→print→parse→print chain is stable.
        prop_assert_eq!(pretty_print(&reparsed), printed);
    }
}

/// Build a random well-formed `CfgPredicate` tree. Includes the degenerate
/// `any([])`/`all([])` forms (empty combinator lists) as leaf cases.
fn arbitrary_predicate(depth: u32) -> impl Strategy<Value = CfgPredicate> {
    let leaf = prop_oneof![
        Just(CfgPredicate::Atom {
            atom: CfgAtom::Ident {
                name: "test".to_string(),
                location: pyxis::span::ItemLocation::internal(),
            },
            location: pyxis::span::ItemLocation::internal(),
        }),
        Just(CfgPredicate::Atom {
            atom: CfgAtom::KeyValue {
                key: "backend".to_string(),
                value: "rust".to_string(),
                location: pyxis::span::ItemLocation::internal(),
            },
            location: pyxis::span::ItemLocation::internal(),
        }),
        Just(CfgPredicate::Atom {
            atom: CfgAtom::KeyValue {
                key: "backend".to_string(),
                value: "cpp".to_string(),
                location: pyxis::span::ItemLocation::internal(),
            },
            location: pyxis::span::ItemLocation::internal(),
        }),
    ];

    if depth == 0 {
        leaf.boxed()
    } else {
        // Mix leaves and one level of combinators; the degenerate empty list
        // appears via `prop::collection::vec(..., 0..5)`.
        let combinators = prop_oneof![
            proptest::collection::vec(arbitrary_predicate(depth - 1), 0..5).prop_map(
                |predicates| CfgPredicate::Any {
                    predicates,
                    location: pyxis::span::ItemLocation::internal(),
                }
            ),
            proptest::collection::vec(arbitrary_predicate(depth - 1), 0..5).prop_map(
                |predicates| CfgPredicate::All {
                    predicates,
                    location: pyxis::span::ItemLocation::internal(),
                }
            ),
            arbitrary_predicate(depth - 1).prop_map(|predicate| CfgPredicate::Not {
                predicate: Box::new(predicate),
                location: pyxis::span::ItemLocation::internal(),
            }),
        ];
        prop_oneof![leaf, combinators].boxed()
    }
}

/// A direct reference evaluator for `CfgPredicate` — the same recursion the
/// production `evaluate` implements, written independently so a divergence
/// between the two is a property failure rather than a restated tautology.
fn reference_evaluate(predicate: &CfgPredicate, ctx: &CfgContext) -> bool {
    match predicate {
        CfgPredicate::Atom { atom, .. } => match atom {
            CfgAtom::Ident { .. } => false,
            CfgAtom::KeyValue { key, value, .. } => match key.as_str() {
                "backend" => *value == ctx.backend.name(),
                _ => false,
            },
        },
        CfgPredicate::Any { predicates, .. } => {
            predicates.iter().any(|p| reference_evaluate(p, ctx))
        }
        CfgPredicate::All { predicates, .. } => {
            predicates.iter().all(|p| reference_evaluate(p, ctx))
        }
        CfgPredicate::Not { predicate, .. } => !reference_evaluate(predicate, ctx),
    }
}

proptest! {
    /// cfg-predicate evaluation is total (never panics — trivially true for
    /// well-typed trees) and agrees with the independent reference evaluator
    /// for every backend.
    #[test]
    fn cfg_predicate_evaluation_matches_reference(
        predicate in arbitrary_predicate(3),
        backend_name in prop_oneof![Just("rust"), Just("cpp"), Just("json")],
    ) {
        let backend = Backend::from_str(backend_name).unwrap();
        let ctx = CfgContext { backend };
        prop_assert_eq!(predicate.evaluate(&ctx), reference_evaluate(&predicate, &ctx));
    }
}

// `StripLocations` idempotence lives in `src/span/proptests.rs` (the trait
// and its impls are test-only), so the integration suite covers the parser
// round-trip and cfg-evaluation properties, and the crate's own `#[cfg(test)]`
// module covers strip-locations idempotence.

/// Degenerate-case coverage that proptest's generator may not hit often:
/// explicitly assert the properties on empty and single-element inputs.
#[test]
fn degenerate_cases_round_trip_and_cfg() {
    // Empty module source: parses to an empty module; printing and re-parsing
    // keeps the printed form stable (fixpoint).
    let empty = pyxis::parser::parse_str_with_file_id("", FileId::INTERNAL).unwrap();
    let printed = pretty_print(&empty);
    let reparsed = pyxis::parser::parse_str_with_file_id(&printed, FileId::INTERNAL).unwrap();
    assert_eq!(pretty_print(&reparsed), printed);

    // Single-token source that parses: `pub` alone fails, but a lone comment
    // file is not a valid module, so the one-token boundary is `pub const C:
    // u32 = 5;` (a single-definition module, covered by the strategy corpus).

    // Degenerate cfg predicates: empty `any`/`all` and a single `not`.
    let any_empty = CfgPredicate::Any {
        predicates: vec![],
        location: pyxis::span::ItemLocation::internal(),
    };
    let all_empty = CfgPredicate::All {
        predicates: vec![],
        location: pyxis::span::ItemLocation::internal(),
    };
    let single_not = CfgPredicate::Not {
        predicate: Box::new(CfgPredicate::Atom {
            atom: CfgAtom::Ident {
                name: "test".to_string(),
                location: pyxis::span::ItemLocation::internal(),
            },
            location: pyxis::span::ItemLocation::internal(),
        }),
        location: pyxis::span::ItemLocation::internal(),
    };
    for backend in Backend::ALL {
        let ctx = CfgContext { backend: *backend };
        for predicate in [&any_empty, &all_empty, &single_not] {
            assert_eq!(
                predicate.evaluate(&ctx),
                reference_evaluate(predicate, &ctx),
                "reference mismatch for {backend:?} on {predicate:?}"
            );
        }
    }
}
