//! Tests for resolving qualified references to nested items in type position.
//!
//! A nested item is always spelled with its parent (`Outer::Inner`); a bare
//! `Inner` inside `Outer` does not resolve. Both halves of that contract are
//! exercised here.

use crate::{
    grammar::test_aliases::*,
    semantic::{
        builder::SemanticBuilder,
        error::{SemanticError, UnresolvedTypeContext, UnresolvedTypeReference},
        types::test_aliases::*,
    },
    span::{ItemLocation, StripLocations},
};

use super::util::*;

/// A nested item referenced by its qualified name inside the parent's own body
/// resolves: `Outer::Header` -> `test::Outer::Header`.
#[test]
fn nested_qualified_field_resolves() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Outer"),
            TD::new([
                TS::item(ID::new(
                    (V::Public, "Header"),
                    TD::new([TS::field((V::Public, "magic"), T::ident("u32"))]),
                )),
                TS::field((V::Public, "header"), T::ident("Outer::Header")),
            ]),
        )]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Outer"),
                SISR::new(
                    (4, 4),
                    STD::new()
                        .with_regions([SR::field(
                            (SV::Public, "header"),
                            ST::raw("test::Outer::Header"),
                        )])
                        .with_nested_item_paths([IP::from("test::Outer::Header")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Outer::Header"),
                SISR::new(
                    (4, 4),
                    STD::new().with_regions([SR::field((SV::Public, "magic"), ST::raw("u32"))]),
                ),
            ),
        ],
    );
}

/// A nested `enum` referenced by its qualified name resolves.
#[test]
fn nested_qualified_enum_resolves() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Outer"),
            TD::new([
                TS::item(ID::new(
                    (V::Public, "InnerEnum"),
                    ED::new(T::ident("u8"), [ES::field("A"), ES::field("B")], []),
                )),
                TS::field((V::Public, "tag"), T::ident("Outer::InnerEnum")),
            ]),
        )]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Outer"),
                SISR::new(
                    (1, 1),
                    STD::new()
                        .with_regions([SR::field(
                            (SV::Public, "tag"),
                            ST::raw("test::Outer::InnerEnum"),
                        )])
                        .with_nested_item_paths([IP::from("test::Outer::InnerEnum")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Outer::InnerEnum"),
                SISR::new(
                    (1, 1),
                    SED::new(ST::raw("u8")).with_variants([("A", 0), ("B", 1)]),
                ),
            ),
        ],
    );
}

/// A nested `bitflags` referenced by its qualified name resolves.
#[test]
fn nested_qualified_bitflags_resolves() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Outer"),
            TD::new([
                TS::item(ID::new(
                    (V::Public, "Flags"),
                    BFD::new(T::ident("u32"), [BFS::field("A", int_literal(1))], []),
                )),
                TS::field((V::Public, "flags"), T::ident("Outer::Flags")),
            ]),
        )]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Outer"),
                SISR::new(
                    (4, 4),
                    STD::new()
                        .with_regions([SR::field(
                            (SV::Public, "flags"),
                            ST::raw("test::Outer::Flags"),
                        )])
                        .with_nested_item_paths([IP::from("test::Outer::Flags")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Outer::Flags"),
                SISR::new((4, 4), SBFD::new(ST::raw("u32")).with_flags([("A", 1)])),
            ),
        ],
    );
}

/// A nested type alias referenced by its qualified name resolves (and expands).
#[test]
fn nested_qualified_type_alias_resolves() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Outer"),
            TD::new([
                TS::item(ID::new(
                    (V::Public, "InnerAlias"),
                    TAD::new(T::ident("u16")),
                )),
                TS::field((V::Public, "alias"), T::ident("Outer::InnerAlias")),
            ]),
        )]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Outer"),
                SISR::new(
                    (2, 2),
                    STD::new()
                        .with_regions([SR::field((SV::Public, "alias"), ST::raw("u16"))])
                        .with_nested_item_paths([IP::from("test::Outer::InnerAlias")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Outer::InnerAlias"),
                SISR::new((0, 1), STAD::new(ST::raw("u16"), vec![])),
            ),
        ],
    );
}

/// A nested item referenced by its qualified name inside a `union` body resolves.
#[test]
fn nested_qualified_union_member_resolves() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Outer"),
            UD::new([
                TS::item(ID::new(
                    (V::Public, "Header"),
                    TD::new([TS::field((V::Public, "magic"), T::ident("u32"))]),
                )),
                TS::field((V::Public, "header"), T::ident("Outer::Header")),
                TS::field((V::Public, "raw"), T::ident("u32")),
            ]),
        )]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Outer"),
                SISR::new(
                    (4, 4),
                    SUD::new()
                        .with_regions([
                            SR::field((SV::Public, "header"), ST::raw("test::Outer::Header")),
                            SR::field((SV::Public, "raw"), ST::raw("u32")),
                        ])
                        .with_nested_item_paths([IP::from("test::Outer::Header")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Outer::Header"),
                SISR::new(
                    (4, 4),
                    STD::new().with_regions([SR::field((SV::Public, "magic"), ST::raw("u32"))]),
                ),
            ),
        ],
    );
}

/// A missing multi-segment path reports the full written path, not just the leaf.
#[test]
fn missing_multisegment_path_reports_full_path() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "Outer"),
            TD::new([
                TS::item(ID::new(
                    (V::Public, "Header"),
                    TD::new([TS::field((V::Public, "magic"), T::ident("u32"))]),
                )),
                TS::field((V::Public, "missing"), T::ident("Outer::Missing")),
            ]),
        )]),
        SemanticError::TypeResolutionStalled {
            unresolved_types: vec!["test::Outer".to_string()],
            resolved_types: vec![],
            unresolved_references: vec![UnresolvedTypeReference {
                type_name: "Outer::Missing".to_string(),
                location: ItemLocation::test(),
                context: UnresolvedTypeContext::StructField {
                    field_name: "missing".to_string(),
                    type_path: IP::from("test::Outer"),
                },
            }],
        },
    );
}

/// A module-relative multi-segment path resolves inline: in a `main` module,
/// `sub::inner::Header` (where `sub::inner` defines `Header`) resolves without
/// stalling.
#[test]
fn module_relative_multisegment_type_resolves() {
    let main = M::new().with_definitions([ID::new(
        (V::Public, "User"),
        TD::new([TS::field((V::Public, "h"), T::ident("sub::inner::Header"))]),
    )]);
    let inner = M::new().with_definitions([ID::new(
        (V::Public, "Header"),
        TD::new([TS::field((V::Public, "magic"), T::ident("u32"))]),
    )]);

    let mut builder = SemanticBuilder::new(pointer_size());
    builder.add_module(&main, &IP::from("main")).unwrap();
    builder.add_module(&inner, &IP::from("sub::inner")).unwrap();
    let resolved = builder.build().unwrap();

    let resolved_type = resolved
        .type_registry()
        .get(&IP::from("main::User"), &ItemLocation::test())
        .cloned()
        .expect("failed to get type");
    let state = resolved_type.resolved().expect("should resolve");
    assert_eq!(
        state.strip_locations(),
        SISR::new(
            (4, 4),
            STD::new().with_regions([SR::field((SV::Public, "h"), ST::raw("sub::inner::Header"))]),
        )
        .strip_locations()
    );
}

/// A bare nested reference (unqualified) still fails: a nested item is always
/// spelled with its parent.
#[test]
fn bare_nested_reference_still_errors() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "Outer"),
            TD::new([
                TS::item(ID::new(
                    (V::Public, "Header"),
                    TD::new([TS::field((V::Public, "magic"), T::ident("u32"))]),
                )),
                TS::field((V::Public, "header"), T::ident("Header")),
            ]),
        )]),
        SemanticError::TypeResolutionStalled {
            unresolved_types: vec!["test::Outer".to_string()],
            resolved_types: vec![],
            unresolved_references: vec![UnresolvedTypeReference {
                type_name: "Header".to_string(),
                location: ItemLocation::test(),
                context: UnresolvedTypeContext::StructField {
                    field_name: "header".to_string(),
                    type_path: IP::from("test::Outer"),
                },
            }],
        },
    );
}
