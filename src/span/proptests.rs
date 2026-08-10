//! Property test: `StripLocations` is idempotent (`f(f(x)) == f(x)`).
//!
//! `StripLocations` and its `Module` impl are `#[cfg(test)]`-only (they are
//! test infrastructure, not public API), so this lives inside the crate. The
//! generator covers arbitrary parseable pyxis modules — including degenerate
//! cases: the empty document and single-item documents.

use proptest::prelude::*;

use crate::{
    parser::parse_str_with_file_id,
    span::{FileId, StripLocations},
};

/// Generate an arbitrary parseable pyxis module (degenerate inputs included).
fn arbitrary_module() -> impl Strategy<Value = crate::parser::module::Module> {
    prop_oneof![
        // Degenerate: empty and single-document inputs.
        Just(String::new()),
        Just("pub type A {\n    pub x: u32,\n}\n".to_string()),
        Just("pub const K: u32 = 1;\n".to_string()),
        // A mixed module hitting several item kinds.
        Just(
            "/// docs\npub type B {\n    pub y: *mut B,\n    vftable {\n        fn f();\n    },\n}\n\
             pub enum E : u32 {\n    One = 1,\n}\nuse a::B;\n"
                .to_string()
        ),
        // Random concatenation of spelling-level fragments: filter to
        // parseable inputs (the property is defined on the module AST, which
        // only exists for parseable text).
        proptest::collection::vec(
            prop_oneof![Just("pub type Aa { x: u32 }"), Just("pub const K: u32 = 1;"), Just("anything")],
            0..8,
        )
        .prop_map(|parts| parts.join("\n") + "\n"),
    ]
    .prop_filter_map("parseable module", |source| {
        parse_str_with_file_id(&source, FileId::INTERNAL).ok()
    })
}

proptest! {
    /// Stripping locations is idempotent: applying it to an already-stripped
    /// value is the identity. A stripped value has no locations left to
    /// remove, so re-stripping must return an equal value.
    #[test]
    fn strip_locations_is_idempotent(module in arbitrary_module()) {
        let once = module.strip_locations();
        let twice = once.strip_locations();
        prop_assert_eq!(once, twice);
    }
}
