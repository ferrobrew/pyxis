//! Tests for extern types with generic-like names.

use crate::{grammar::test_aliases::*, semantic::types::test_aliases::*};

use super::super::util::*;

// ========== Extern types with generic-like names ==========

/// Tests that extern types with generic-like names (e.g., "SharedPtr<u32>") are resolved
/// as exact-match extern types rather than attempting generic type resolution.
#[test]
fn can_resolve_extern_type_with_generic_like_name() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_extern_types([
                // An extern type with a generic-like name - the entire string is the type name
                (
                    "SharedPtr<u32>".into(),
                    As::from_iter([A::size(pointer_size()), A::align(pointer_size())]),
                ),
            ])
            .with_definitions([ID::new(
                (V::Public, "TestType"),
                TD::new([
                    // Use the extern type with generic-like name
                    TS::field(
                        (V::Public, "shared"),
                        T::generic("SharedPtr", [T::ident("u32")]),
                    ),
                    // Also test pointer to it
                    TS::field(
                        (V::Public, "shared_ptr"),
                        T::generic("SharedPtr", [T::ident("u32")]).mut_pointer(),
                    ),
                ])
                .with_attributes([A::align(pointer_size())]),
            )]),
        [
            SID::defined_resolved(
                (SV::Public, "test::TestType"),
                SISR::new(
                    (2 * pointer_size(), pointer_size()),
                    STD::new().with_regions([
                        SR::field((SV::Public, "shared"), ST::raw("test::SharedPtr<u32>")),
                        SR::field(
                            (SV::Public, "shared_ptr"),
                            ST::raw("test::SharedPtr<u32>").mut_pointer(),
                        ),
                    ]),
                ),
            ),
            SID::category_resolved(
                (SV::Public, "test::SharedPtr<u32>"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([]),
                ),
                SIC::Extern,
            ),
        ],
    );
}

/// Tests that extern types with nested generic-like names work correctly.
#[test]
fn can_resolve_extern_type_with_nested_generic_like_name() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_extern_types([
                // An extern type with nested generics in the name
                (
                    "ManuallyDrop<SharedPtr<u32>>".into(),
                    As::from_iter([A::size(pointer_size()), A::align(pointer_size())]),
                ),
            ])
            .with_definitions([ID::new(
                (V::Public, "TestType"),
                TD::new([TS::field(
                    (V::Public, "texture"),
                    T::generic("ManuallyDrop", [T::generic("SharedPtr", [T::ident("u32")])])
                        .mut_pointer(),
                )])
                .with_attributes([A::size(pointer_size())]),
            )]),
        [
            SID::defined_resolved(
                (SV::Public, "test::TestType"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "texture"),
                        ST::raw("test::ManuallyDrop<SharedPtr<u32>>").mut_pointer(),
                    )]),
                ),
            ),
            SID::category_resolved(
                (SV::Public, "test::ManuallyDrop<SharedPtr<u32>>"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([]),
                ),
                SIC::Extern,
            ),
        ],
    );
}
