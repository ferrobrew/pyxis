//! Tests for copyable and cloneable trait resolution.

use crate::{
    grammar::test_aliases::*,
    semantic::{error::SemanticError, types::test_aliases::*},
    span::ItemLocation,
};

use super::util::*;

#[test]
fn can_extract_copyable_and_cloneable_correctly() {
    // Check cloneable
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Private, "field_1"), T::ident("i32"))])
                .with_attributes([A::cloneable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                STD::new()
                    .with_regions([SR::field((SV::Private, "field_1"), ST::raw("i32"))])
                    .with_cloneable(true),
            ),
        )],
    );

    // Check copyable -> copyable + cloneable
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Private, "field_1"), T::ident("i32"))])
                .with_attributes([A::copyable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                STD::new()
                    .with_regions([SR::field((SV::Private, "field_1"), ST::raw("i32"))])
                    .with_copyable(true)
                    .with_cloneable(true),
            ),
        )],
    );
}

#[test]
fn can_extract_copyable_and_cloneable_for_enum_correctly() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            ED::new(
                T::ident("u32"),
                [ES::field("Item1"), ES::field("Item2")],
                [],
            )
            .with_attributes([A::cloneable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SED::new(ST::raw("u32"))
                    .with_variants([("Item1", 0), ("Item2", 1)])
                    .with_cloneable(true),
            ),
        )],
    );

    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            ED::new(
                T::ident("u32"),
                [ES::field("Item1"), ES::field("Item2")],
                [],
            )
            .with_attributes([A::copyable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SED::new(ST::raw("u32"))
                    .with_variants([("Item1", 0), ("Item2", 1)])
                    .with_copyable(true)
                    .with_cloneable(true),
            ),
        )],
    );
}

#[test]
fn will_reject_copyable_on_non_copyable_field() {
    // First, create a non-copyable struct
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Inner"),
                TD::new([TS::field((V::Private, "field_1"), T::ident("i32"))]),
                // Note: Inner is NOT marked as copyable
            ),
            ID::new(
                (V::Public, "Outer"),
                TD::new([TS::field((V::Private, "inner"), T::ident("Inner"))])
                    .with_attributes([A::copyable()]),
            ),
        ]),
        SemanticError::CopyableError {
            field_name: "inner".to_string(),
            item_path: IP::from("test::Outer"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn will_reject_cloneable_on_non_cloneable_field() {
    // First, create a non-cloneable struct
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Inner"),
                TD::new([TS::field((V::Private, "field_1"), T::ident("i32"))]),
                // Note: Inner is NOT marked as cloneable
            ),
            ID::new(
                (V::Public, "Outer"),
                TD::new([TS::field((V::Private, "inner"), T::ident("Inner"))])
                    .with_attributes([A::cloneable()]),
            ),
        ]),
        SemanticError::CloneableError {
            field_name: "inner".to_string(),
            item_path: IP::from("test::Outer"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn can_handle_copyable_with_pointer_field() {
    // Pointers are copyable (like raw pointers in Rust)
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field(
                (V::Private, "field_1"),
                T::ident("i32").mut_pointer(),
            )])
            .with_attributes([A::copyable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (pointer_size(), pointer_size()),
                STD::new()
                    .with_regions([SR::field(
                        (SV::Private, "field_1"),
                        ST::raw("i32").mut_pointer(),
                    )])
                    .with_copyable(true)
                    .with_cloneable(true),
            ),
        )],
    );
}

#[test]
fn can_handle_cloneable_with_pointer_field() {
    // Pointers are cloneable (like raw pointers in Rust)
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field(
                (V::Private, "field_1"),
                T::ident("i32").const_pointer(),
            )])
            .with_attributes([A::cloneable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (pointer_size(), pointer_size()),
                STD::new()
                    .with_regions([SR::field(
                        (SV::Private, "field_1"),
                        ST::raw("i32").const_pointer(),
                    )])
                    .with_cloneable(true),
            ),
        )],
    );
}

#[test]
fn can_handle_copyable_with_array_of_copyable() {
    // Arrays of copyable types are copyable
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Private, "field_1"), T::ident("i32").array(4))])
                .with_attributes([A::copyable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (16, 4),
                STD::new()
                    .with_regions([SR::field((SV::Private, "field_1"), ST::raw("i32").array(4))])
                    .with_copyable(true)
                    .with_cloneable(true),
            ),
        )],
    );
}

#[test]
fn will_reject_copyable_with_array_of_non_copyable() {
    // Arrays of non-copyable types are not copyable
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Inner"),
                TD::new([TS::field((V::Private, "field_1"), T::ident("i32"))]),
                // Note: Inner is NOT marked as copyable
            ),
            ID::new(
                (V::Public, "Outer"),
                TD::new([TS::field((V::Private, "items"), T::ident("Inner").array(4))])
                    .with_attributes([A::copyable()]),
            ),
        ]),
        SemanticError::CopyableError {
            field_name: "items".to_string(),
            item_path: IP::from("test::Outer"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn transitive_copyable_verification() {
    // A copyable struct containing another copyable struct should work
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Inner"),
                TD::new([TS::field((V::Private, "value"), T::ident("i32"))])
                    .with_attributes([A::copyable()]),
            ),
            ID::new(
                (V::Public, "Outer"),
                TD::new([TS::field((V::Private, "inner"), T::ident("Inner"))])
                    .with_attributes([A::copyable()]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Inner"),
                SISR::new(
                    (4, 4),
                    STD::new()
                        .with_regions([SR::field((SV::Private, "value"), ST::raw("i32"))])
                        .with_copyable(true)
                        .with_cloneable(true),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Outer"),
                SISR::new(
                    (4, 4),
                    STD::new()
                        .with_regions([SR::field((SV::Private, "inner"), ST::raw("test::Inner"))])
                        .with_copyable(true)
                        .with_cloneable(true),
                ),
            ),
        ],
    );
}

#[test]
fn transitive_cloneable_verification() {
    // A cloneable struct containing another cloneable struct should work
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Inner"),
                TD::new([TS::field((V::Private, "value"), T::ident("i32"))])
                    .with_attributes([A::cloneable()]),
            ),
            ID::new(
                (V::Public, "Outer"),
                TD::new([TS::field((V::Private, "inner"), T::ident("Inner"))])
                    .with_attributes([A::cloneable()]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Inner"),
                SISR::new(
                    (4, 4),
                    STD::new()
                        .with_regions([SR::field((SV::Private, "value"), ST::raw("i32"))])
                        .with_cloneable(true),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Outer"),
                SISR::new(
                    (4, 4),
                    STD::new()
                        .with_regions([SR::field((SV::Private, "inner"), ST::raw("test::Inner"))])
                        .with_cloneable(true),
                ),
            ),
        ],
    );
}

#[test]
fn copyable_requires_cloneable_on_fields() {
    // Since copyable implies cloneable, if a struct is copyable,
    // its fields must be both copyable and cloneable. But since copyable
    // already implies cloneable, we just need to check copyable.
    // This test verifies that a type marked only as cloneable cannot be
    // used in a copyable struct.
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Inner"),
                TD::new([TS::field((V::Private, "value"), T::ident("i32"))])
                    .with_attributes([A::cloneable()]), // Only cloneable, not copyable
            ),
            ID::new(
                (V::Public, "Outer"),
                TD::new([TS::field((V::Private, "inner"), T::ident("Inner"))])
                    .with_attributes([A::copyable()]),
            ),
        ]),
        SemanticError::CopyableError {
            field_name: "inner".to_string(),
            item_path: IP::from("test::Outer"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn will_reject_copyable_generic_with_non_copyable_type_argument() {
    // A copyable struct containing Generic<NonCopyable> should fail
    // Use pointer to T since generic types with direct T can't compute size
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            // A generic wrapper type marked as copyable (with pointer to T)
            ID::generic(
                (V::Public, "Wrapper"),
                [TP::new("T")],
                TD::new([TS::field((V::Private, "ptr"), T::ident("T").mut_pointer())])
                    .with_attributes([A::copyable(), A::size(pointer_size())]),
            ),
            // A non-copyable type
            ID::new(
                (V::Public, "NonCopyable"),
                TD::new([TS::field((V::Private, "x"), T::ident("i32"))]),
            ),
            // A copyable type that contains Wrapper<NonCopyable>
            ID::new(
                (V::Public, "Container"),
                TD::new([TS::field(
                    (V::Private, "wrapped"),
                    T::generic("Wrapper", [T::ident("NonCopyable")]),
                )])
                .with_attributes([A::copyable(), A::size(pointer_size())]),
            ),
        ]),
        SemanticError::CopyableError {
            field_name: "wrapped".to_string(),
            item_path: IP::from("test::Container"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn will_reject_copyable_with_non_copyable_generic_base() {
    // A copyable struct containing NonCopyableGeneric<Copyable> should fail
    // Use pointer to T since generic types with direct T can't compute size
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            // A generic wrapper type NOT marked as copyable (with pointer to T)
            ID::generic(
                (V::Public, "NonCopyableWrapper"),
                [TP::new("T")],
                TD::new([TS::field((V::Private, "ptr"), T::ident("T").mut_pointer())])
                    .with_attributes([A::size(pointer_size())]),
            ),
            // A copyable type
            ID::new(
                (V::Public, "CopyableInner"),
                TD::new([TS::field((V::Private, "x"), T::ident("i32"))])
                    .with_attributes([A::copyable()]),
            ),
            // A copyable type that contains NonCopyableWrapper<CopyableInner>
            ID::new(
                (V::Public, "Container"),
                TD::new([TS::field(
                    (V::Private, "wrapped"),
                    T::generic("NonCopyableWrapper", [T::ident("CopyableInner")]),
                )])
                .with_attributes([A::copyable(), A::size(pointer_size())]),
            ),
        ]),
        SemanticError::CopyableError {
            field_name: "wrapped".to_string(),
            item_path: IP::from("test::Container"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn can_handle_copyable_generic_with_copyable_type_argument() {
    // A copyable struct containing CopyableGeneric<Copyable> should succeed
    // Use pointer to T since generic types with direct T can't compute size
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            // A generic wrapper type marked as copyable (with pointer to T)
            ID::generic(
                (V::Public, "Wrapper"),
                [TP::new("T")],
                TD::new([TS::field((V::Private, "ptr"), T::ident("T").mut_pointer())])
                    .with_attributes([A::copyable(), A::size(pointer_size())]),
            ),
            // A copyable type
            ID::new(
                (V::Public, "CopyableInner"),
                TD::new([TS::field((V::Private, "x"), T::ident("i32"))])
                    .with_attributes([A::copyable()]),
            ),
            // A copyable type that contains Wrapper<CopyableInner>
            ID::new(
                (V::Public, "Container"),
                TD::new([TS::field(
                    (V::Private, "wrapped"),
                    T::generic("Wrapper", [T::ident("CopyableInner")]),
                )])
                .with_attributes([A::copyable(), A::size(pointer_size())]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Container"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new()
                        .with_regions([SR::field(
                            (SV::Private, "wrapped"),
                            ST::generic("test::Wrapper", [ST::raw("test::CopyableInner")]),
                        )])
                        .with_copyable(true)
                        .with_cloneable(true),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::CopyableInner"),
                SISR::new(
                    (4, 4),
                    STD::new()
                        .with_regions([SR::field((SV::Private, "x"), ST::raw("i32"))])
                        .with_copyable(true)
                        .with_cloneable(true),
                ),
            ),
            SID::generic_defined_resolved(
                (SV::Public, "test::Wrapper"),
                ["T"],
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new()
                        .with_regions([SR::field(
                            (SV::Private, "ptr"),
                            ST::type_parameter("T").mut_pointer(),
                        )])
                        .with_copyable(true)
                        .with_cloneable(true),
                ),
            ),
        ],
    );
}
