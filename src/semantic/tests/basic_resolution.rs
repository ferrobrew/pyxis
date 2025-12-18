//! Tests for basic type resolution functionality.

use crate::{
    grammar::test_aliases::*,
    semantic::{
        error::{SemanticError, UnresolvedTypeContext, UnresolvedTypeReference},
        types::test_aliases::*,
    },
    span::ItemLocation,
};

use super::util::*;

#[test]
fn can_resolve_basic_struct() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([
                TS::field((V::Public, "field_1"), T::ident("i32")),
                TS::field((V::Private, "_"), T::unknown(4)),
                TS::field((V::Public, "field_2"), T::ident("u64")),
            ])
            .with_attributes([A::align(8)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (16, 8),
                STD::new().with_regions([
                    SR::field((SV::Public, "field_1"), ST::raw("i32")),
                    SR::field((SV::Private, "_field_4"), unknown(4)),
                    SR::field((SV::Public, "field_2"), ST::raw("u64")),
                ]),
            ),
        )],
    );
}

#[test]
fn can_resolve_pointer_to_another_struct() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "TestType1"),
                TD::new([TS::field((V::Public, "field_1"), T::ident("u64"))]),
            ),
            ID::new(
                (V::Public, "TestType2"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("i32")),
                    TS::field((V::Public, "field_2"), T::ident("TestType1"))
                        .with_attributes([A::address(8)]),
                    TS::field(
                        (V::Public, "field_3"),
                        T::ident("TestType1").const_pointer(),
                    ),
                    TS::field((V::Public, "field_4"), T::ident("TestType1").mut_pointer()),
                ])
                .with_attributes([A::align(8)]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::TestType1"),
                SISR::new(
                    (8, 8),
                    STD::new().with_regions([SR::field((SV::Public, "field_1"), ST::raw("u64"))]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::TestType2"),
                SISR::new(
                    (16 + 2 * pointer_size(), 8),
                    STD::new().with_regions([
                        SR::field((SV::Public, "field_1"), ST::raw("i32")),
                        SR::field((SV::Private, "_field_4"), unknown(4)),
                        SR::field((SV::Public, "field_2"), ST::raw("test::TestType1")),
                        SR::field(
                            (SV::Public, "field_3"),
                            ST::raw("test::TestType1").const_pointer(),
                        ),
                        SR::field(
                            (SV::Public, "field_4"),
                            ST::raw("test::TestType1").mut_pointer(),
                        ),
                    ]),
                ),
            ),
        ],
    );
}

#[test]
fn can_resolve_complex_type() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_definitions([
                ID::new(
                    (V::Public, "TestType"),
                    TD::new([
                        TS::field((V::Public, "field_1"), T::ident("i32")),
                        TS::field((V::Private, "_"), T::unknown(4)),
                    ]),
                ),
                ID::new(
                    (V::Public, "Singleton"),
                    TD::new([
                        TS::field((V::Public, "max_num_1"), T::ident("u16"))
                            .with_attributes([A::address(0x78)]),
                        TS::field((V::Public, "max_num_2"), T::ident("u16")),
                        TS::field((V::Public, "test_type"), T::ident("TestType"))
                            .with_attributes([A::address(0xA00)]),
                        TS::field((V::Public, "settings"), T::unknown(804)),
                    ])
                    .with_attributes([A::size(0x1750), A::singleton(0x1_200_000)]),
                ),
            ])
            .with_impls([FB::new(
                "Singleton",
                [F::new(
                    (V::Public, "test_function"),
                    [
                        Ar::mut_self(),
                        Ar::named("arg1", T::ident("TestType").mut_pointer()),
                        Ar::named("arg2", T::ident("i32")),
                        Ar::named("arg3", T::ident("u32").const_pointer()),
                    ],
                )
                .with_attributes([A::address(0x800_000)])
                .with_return_type(T::ident("TestType").mut_pointer())],
            )]),
        [
            SID::defined_resolved(
                (SV::Public, "test::TestType"),
                SISR::new(
                    (8, pointer_size()),
                    STD::new().with_regions([
                        SR::field((SV::Public, "field_1"), ST::raw("i32")),
                        SR::field((SV::Private, "_field_4"), unknown(4)),
                    ]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Singleton"),
                SISR::new(
                    (0x1750, pointer_size()),
                    STD::new()
                        .with_regions([
                            SR::field((SV::Private, "_field_0"), unknown(0x78)),
                            SR::field((SV::Public, "max_num_1"), ST::raw("u16")),
                            SR::field((SV::Public, "max_num_2"), ST::raw("u16")),
                            SR::field((SV::Private, "_field_7c"), unknown(0x984)),
                            SR::field((SV::Public, "test_type"), ST::raw("test::TestType")),
                            SR::field((SV::Public, "settings"), unknown(804)),
                            SR::field((SV::Private, "_field_d2c"), unknown(0xA24)),
                        ])
                        .with_associated_functions([SF::new(
                            (SV::Public, "test_function"),
                            SFB::address(0x800_000),
                            SCC::for_member_function(pointer_size()),
                        )
                        .with_arguments([
                            SAr::mut_self(),
                            SAr::field("arg1".to_string(), ST::raw("test::TestType").mut_pointer()),
                            SAr::field("arg2", ST::raw("i32")),
                            SAr::field("arg3", ST::raw("u32").const_pointer()),
                        ])
                        .with_return_type(ST::raw("test::TestType").mut_pointer())])
                        .with_singleton(0x1_200_000),
                ),
            ),
        ],
    );
}

#[test]
fn will_eventually_terminate_with_an_unknown_type() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType2"),
            TD::new([TS::field((V::Private, "field_2"), T::ident("TestType1"))]),
        )]),
        SemanticError::TypeResolutionStalled {
            unresolved_types: vec!["test::TestType2".to_string()],
            resolved_types: vec![],
            unresolved_references: vec![UnresolvedTypeReference {
                type_name: "TestType1".to_string(),
                location: ItemLocation::test(),
                context: UnresolvedTypeContext::StructField {
                    field_name: "field_2".to_string(),
                    type_path: IP::from("test::TestType2"),
                },
            }],
        },
    );
}

#[test]
fn will_reject_types_that_are_larger_than_their_specified_size() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Matrix4"),
                TD::new([TS::field((V::Public, "data"), T::ident("f32").array(16))]),
            ),
            ID::new(
                (V::Public, "TestType"),
                TD::new([TS::field(
                    (V::Public, "matrices"),
                    T::ident("Matrix4").array(8),
                )])
                .with_attributes([A::size(0x100)]),
            ),
        ]),
        SemanticError::SizeMismatch {
            expected: 256,
            actual: 512,
            item_path: IP::from("test::TestType"),
            location: ItemLocation::test(),
        },
    );
}
