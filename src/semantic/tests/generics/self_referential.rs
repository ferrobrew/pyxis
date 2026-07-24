//! Tests for self-referential generic types.

use crate::{grammar::test_aliases::*, semantic::types::test_aliases::*};

use super::super::util::*;

// ========== Self-referential generic types ==========

/// Tests that generic types with pointer fields to themselves can be resolved.
/// This is a common pattern for self-referential types like tree nodes.
#[test]
fn can_resolve_self_referential_generic_type_with_pointer() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            // Generic type that contains a pointer to itself when instantiated
            ID::generic(
                (V::Public, "SharedPtr"),
                [TP::new("T")],
                TD::new([TS::field((V::Public, "px"), T::ident("T").mut_pointer())])
                    .with_attributes([A::size(pointer_size())]),
            ),
            // Type that uses the generic with a self-reference
            // Note: pointer field comes first for proper alignment
            ID::new(
                (V::Public, "GameObject"),
                TD::new([
                    TS::field(
                        (V::Public, "parent"),
                        T::generic("SharedPtr", [T::ident("GameObject")]),
                    ),
                    TS::field((V::Public, "id"), T::ident("u32")),
                ])
                .with_attributes([A::size(pointer_size() * 2)]),
            ),
        ]),
        [
            // Alphabetical order: GameObject < SharedPtr
            SID::defined_resolved(
                (SV::Public, "test::GameObject"),
                SISR::new(
                    (pointer_size() * 2, pointer_size()),
                    STD::new().with_regions(filter_out_empty_regions([
                        SR::field(
                            (SV::Public, "parent"),
                            ST::generic("test::SharedPtr", [ST::raw("test::GameObject")]),
                        ),
                        SR::field((SV::Public, "id"), ST::raw("u32")),
                        // Padding to align struct to pointer_size (only when pointer_size > 4)
                        SR::field(
                            (SV::Private, "_field_c"),
                            ST::array(ST::raw("u8"), pointer_size() - 4),
                        ),
                    ])),
                ),
            ),
            SID::generic_defined_resolved(
                (SV::Public, "test::SharedPtr"),
                ["T"],
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "px"),
                        ST::type_parameter("T").mut_pointer(),
                    )]),
                ),
            ),
        ],
    );
}

/// Tests that generic types with multiple pointer fields to the same type can be resolved.
#[test]
fn can_resolve_generic_type_with_multiple_self_referential_pointers() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::generic(
                (V::Public, "SharedPtr"),
                [TP::new("T")],
                TD::new([TS::field((V::Public, "px"), T::ident("T").mut_pointer())])
                    .with_attributes([A::size(pointer_size())]),
            ),
            ID::generic(
                (V::Public, "WeakPtr"),
                [TP::new("T")],
                TD::new([TS::field((V::Public, "px"), T::ident("T").const_pointer())])
                    .with_attributes([A::size(pointer_size())]),
            ),
            // Note: pointer fields come first for proper alignment
            ID::new(
                (V::Public, "Node"),
                TD::new([
                    TS::field(
                        (V::Public, "parent"),
                        T::generic("SharedPtr", [T::ident("Node")]),
                    ),
                    TS::field(
                        (V::Public, "weak_self"),
                        T::generic("WeakPtr", [T::ident("Node")]),
                    ),
                    TS::field((V::Public, "data"), T::ident("u32")),
                ])
                .with_attributes([A::size(pointer_size() * 3)]),
            ),
        ]),
        [
            // Alphabetical order: Node < SharedPtr < WeakPtr
            SID::defined_resolved(
                (SV::Public, "test::Node"),
                SISR::new(
                    (pointer_size() * 3, pointer_size()),
                    STD::new().with_regions(filter_out_empty_regions([
                        SR::field(
                            (SV::Public, "parent"),
                            ST::generic("test::SharedPtr", [ST::raw("test::Node")]),
                        ),
                        SR::field(
                            (SV::Public, "weak_self"),
                            ST::generic("test::WeakPtr", [ST::raw("test::Node")]),
                        ),
                        SR::field((SV::Public, "data"), ST::raw("u32")),
                        // Padding to align struct to pointer_size (only when pointer_size > 4)
                        SR::field(
                            (SV::Private, "_field_14"),
                            ST::array(ST::raw("u8"), pointer_size() - 4),
                        ),
                    ])),
                ),
            ),
            SID::generic_defined_resolved(
                (SV::Public, "test::SharedPtr"),
                ["T"],
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "px"),
                        ST::type_parameter("T").mut_pointer(),
                    )]),
                ),
            ),
            SID::generic_defined_resolved(
                (SV::Public, "test::WeakPtr"),
                ["T"],
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "px"),
                        ST::type_parameter("T").const_pointer(),
                    )]),
                ),
            ),
        ],
    );
}

/// Tests that nested generic types with self-references can be resolved.
#[test]
fn can_resolve_nested_generic_self_reference() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::generic(
                (V::Public, "Ptr"),
                [TP::new("T")],
                TD::new([TS::field((V::Public, "p"), T::ident("T").mut_pointer())])
                    .with_attributes([A::size(pointer_size())]),
            ),
            ID::new(
                (V::Public, "Container"),
                TD::new([TS::field(
                    (V::Public, "item"),
                    T::generic("Ptr", [T::generic("Ptr", [T::ident("Container")])]),
                )])
                .with_attributes([A::size(pointer_size())]),
            ),
        ]),
        [
            SID::generic_defined_resolved(
                (SV::Public, "test::Ptr"),
                ["T"],
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "p"),
                        ST::type_parameter("T").mut_pointer(),
                    )]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Container"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "item"),
                        ST::generic(
                            "test::Ptr",
                            [ST::generic("test::Ptr", [ST::raw("test::Container")])],
                        ),
                    )]),
                ),
            ),
        ],
    );
}
