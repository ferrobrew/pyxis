//! Tests for defaultable trait resolution.

use crate::{
    grammar::test_aliases::*,
    semantic::{error::SemanticError, types::test_aliases::*},
    span::ItemLocation,
};

use super::util::*;

#[test]
fn can_handle_defaultable_on_primitive_types() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([
                TS::field((V::Private, "field_1"), T::ident("u64")),
                TS::field((V::Private, "field_2"), T::ident("f32").array(16)),
            ])
            .with_attributes([A::defaultable(), A::align(8)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (72, 8),
                STD::new()
                    .with_regions([
                        SR::field((SV::Private, "field_1"), ST::raw("u64")),
                        SR::field((SV::Private, "field_2"), ST::raw("f32").array(16)),
                    ])
                    .with_defaultable(true),
            ),
        )],
    );
}

#[test]
fn will_reject_defaultable_on_pointer() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field(
                (V::Private, "field_1"),
                T::ident("i32").mut_pointer(),
            )])
            .with_attributes([A::defaultable()]),
        )]),
        SemanticError::DefaultableError {
            field_name: "field_1".to_string(),
            item_path: IP::from("test::TestType"),
            message: "is not a defaultable type (pointer or function?)".to_string(),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn will_reject_defaultable_on_enum_field() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            ID::new(
                (V::Public, "TestType"),
                TD::new([TS::field((V::Private, "field_1"), T::ident("TestEnum"))])
                    .with_attributes([A::defaultable()]),
            ),
            ID::new(
                (V::Public, "TestEnum"),
                ED::new(T::ident("u32"), [ES::field("Item1")], []),
            ),
        ]),
        SemanticError::DefaultableError {
            field_name: "field_1".to_string(),
            item_path: IP::from("test::TestType"),
            message: "is not a defaultable type".to_string(),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn can_handle_defaultable_on_enum_with_default_field() {
    // Enum marked as defaultable but has no default variant
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            ED::new(
                T::ident("u32"),
                [ES::field("Item1"), ES::field("Item2")],
                [],
            )
            .with_attributes([A::defaultable()]),
        )]),
        SemanticError::EnumDefaultableMissingDefault {
            item_path: IP::from("test::TestType"),
            location: ItemLocation::test(),
        },
    );

    // Enum has a default variant but is not marked as defaultable
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            ED::new(
                T::ident("u32"),
                [
                    ES::field("Item1"),
                    ES::field("Item2").with_attributes([A::default()]),
                ],
                [],
            )
            .with_attributes([]),
        )]),
        SemanticError::EnumDefaultWithoutDefaultable {
            item_path: IP::from("test::TestType"),
            location: ItemLocation::test(),
        },
    );

    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            ED::new(
                T::ident("u32"),
                [
                    ES::field("Item1"),
                    ES::field("Item2").with_attributes([A::default()]),
                ],
                [],
            )
            .with_attributes([A::defaultable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SED::new(ST::raw("u32"))
                    .with_variants([("Item1", 0), ("Item2", 1)])
                    .with_default(1),
            ),
        )],
    );
}

#[test]
fn will_reject_defaultable_on_non_defaultable_type() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            ID::new(
                (V::Public, "TestType"),
                TD::new([TS::field(
                    (V::Private, "field_1"),
                    T::ident("TestNonDefaultable"),
                )])
                .with_attributes([A::defaultable()]),
            ),
            ID::new((V::Public, "TestNonDefaultable"), TD::new([])),
        ]),
        SemanticError::DefaultableError {
            field_name: "field_1".to_string(),
            item_path: IP::from("test::TestType"),
            message: "is not a defaultable type".to_string(),
            location: ItemLocation::test(),
        },
    );
}
