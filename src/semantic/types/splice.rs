use crate::{
    grammar::ItemPath,
    span::{HasLocation, ItemLocation},
};

#[cfg(test)]
use crate::span::StripLocations;

/// A resolved `prologue`/`epilogue` splice: raw backend code spliced into
/// a module's generated output.
///
/// `cfg` carries the optional gate: `None` means "every backend"; a
/// predicate means "only the backends it selects" (evaluated via
/// [`Splice::active_for`]). `definition` routes the splice into the cpp
/// `.cpp` source file rather than the `.hpp` header — only valid when the
/// cfg resolves cpp-only (enforced at validation time).
///
/// `for_type`, when set, holds the **resolved absolute item path** of the
/// type this splice is attributed to (from `epilogue for <Type> ...`),
/// resolved and validated (same-module) during semantic analysis. `None`
/// means "module-level".
#[derive(PartialEq, Eq, Debug, Clone, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct Splice {
    pub kind: crate::grammar::SpliceKind,
    pub cfg: Option<crate::parser::cfg::CfgPredicate>,
    pub definition: bool,
    pub for_type: Option<ItemPath>,
    pub text: String,
    pub location: ItemLocation,
}
impl Splice {
    /// Whether this splice is emitted for `backend` (ungated → always).
    pub fn active_for(&self, backend: crate::Backend) -> bool {
        let ctx = crate::parser::cfg::CfgContext { backend };
        self.cfg.as_ref().is_none_or(|p| p.evaluate(&ctx))
    }
}
#[cfg(test)]
impl Splice {
    fn new(kind: crate::grammar::SpliceKind, text: impl Into<String>) -> Self {
        Splice {
            kind,
            cfg: None,
            definition: false,
            for_type: None,
            text: text.into(),
            location: ItemLocation::test(),
        }
    }
    pub fn prologue(text: impl Into<String>) -> Self {
        Self::new(crate::grammar::SpliceKind::Prologue, text)
    }
    pub fn epilogue(text: impl Into<String>) -> Self {
        Self::new(crate::grammar::SpliceKind::Epilogue, text)
    }
    pub fn definition(mut self) -> Self {
        self.definition = true;
        self
    }
    pub fn for_type(mut self, for_type: impl Into<ItemPath>) -> Self {
        self.for_type = Some(for_type.into());
        self
    }
    /// Gate this splice with `#[cfg(backend = "<name>")]`.
    pub fn cfg_backend(mut self, name: &str) -> Self {
        use crate::parser::cfg::{CfgAtom, CfgPredicate};
        self.cfg = Some(CfgPredicate::Atom {
            atom: CfgAtom::KeyValue {
                key: "backend".into(),
                value: name.into(),
                location: ItemLocation::test(),
            },
            location: ItemLocation::test(),
        });
        self
    }
}
