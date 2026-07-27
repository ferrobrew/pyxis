//! Tests for function-pointer types in type position (ferrobrew/pyxis#121).
//!
//! The happy path — every nesting, both backends' output — is covered by the
//! `function_pointers` codegen corpus entry. These tests pin down resolution
//! details and the attribute failure modes that the corpus can't express.

use crate::{
    grammar::test_aliases::*,
    semantic::{error::SemanticError, types::test_aliases::*},
    span::{ItemLocation, StripLocations},
};

use super::util::*;
use pretty_assertions::assert_eq;

/// A function-pointer field is pointer-sized and resolves its parameter and
/// return types like any other type reference.
#[test]
fn function_pointer_field_resolves_to_a_pointer_sized_region() {
    let pointer_size = pointer_size();
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Engine"),
                TD::new([TS::field((V::Public, "frame"), T::ident("u32"))]),
            ),
            ID::new(
                (V::Public, "Callbacks"),
                TD::new([TS::field(
                    (V::Public, "on_event"),
                    T::function(
                        [
                            GFA::new(Some("engine"), T::ident("Engine").mut_pointer()),
                            GFA::new(None, T::ident("u32")),
                        ],
                        Some(T::ident("bool")),
                    ),
                )]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Callbacks"),
                SISR::new(
                    (pointer_size, pointer_size),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "on_event"),
                        ST::function(
                            SCC::System,
                            [
                                SFA::named("engine", ST::raw("test::Engine").mut_pointer()),
                                SFA::unnamed(ST::raw("u32")),
                            ],
                            ST::raw("bool"),
                        ),
                    )]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Engine"),
                SISR::new(
                    (4, 4),
                    STD::new().with_regions([SR::field((SV::Public, "frame"), ST::raw("u32"))]),
                ),
            ),
        ],
    );
}

/// `#[calling_convention(...)]` in type position selects the ABI; without it
/// the type defaults the same way a freestanding function does.
#[test]
fn calling_convention_attribute_applies_to_the_function_pointer_type() {
    let pointer_size = pointer_size();
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Callbacks"),
            TD::new([TS::field(
                (V::Public, "on_tick"),
                T::function([], None).with_attributes([A::calling_convention("cdecl")]),
            )]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::Callbacks"),
            SISR::new(
                (pointer_size, pointer_size),
                STD::new().with_regions([SR::field(
                    (SV::Public, "on_tick"),
                    ST::function(SCC::Cdecl, [] as [SFA; 0], None),
                )]),
            ),
        )],
    );
}

#[test]
fn unknown_calling_convention_on_a_function_pointer_is_rejected() {
    let ast = crate::parser::parse_str_for_tests(
        "pub type Callbacks { pub on_tick: #[calling_convention(bogus)] fn(), }",
    )
    .unwrap();
    let err = build_state(&ast, &IP::from("test")).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        SemanticError::InvalidTypeCallingConvention {
            convention: "bogus".to_string(),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

/// Attributes parse ahead of any type, but only a function-pointer type has
/// anything to do with one — the rest are rejected rather than ignored.
#[test]
fn attributes_on_a_type_that_cannot_consume_them_are_rejected() {
    for (source, type_description) in [
        (
            "pub type Callbacks { pub a: #[calling_convention(cdecl)] u32, }",
            "a named type",
        ),
        (
            "pub type Callbacks { pub a: #[calling_convention(cdecl)] *mut u32, }",
            "a pointer type",
        ),
        (
            "pub type Callbacks { pub a: #[calling_convention(cdecl)] [u32; 2], }",
            "an array type",
        ),
    ] {
        let ast = crate::parser::parse_str_for_tests(source).unwrap();
        let err = build_state(&ast, &IP::from("test")).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            SemanticError::UnsupportedTypeAttribute {
                attribute_name: "calling_convention".to_string(),
                type_description: type_description.to_string(),
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }
}

/// An attribute a function-pointer type doesn't recognise is an error too —
/// only `calling_convention` is meaningful here.
#[test]
fn unrecognised_attribute_on_a_function_pointer_is_rejected() {
    let ast = crate::parser::parse_str_for_tests("pub type Callbacks { pub a: #[packed] fn(), }")
        .unwrap();
    let err = build_state(&ast, &IP::from("test")).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        SemanticError::UnsupportedTypeAttribute {
            attribute_name: "packed".to_string(),
            type_description: "a function pointer type".to_string(),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}
