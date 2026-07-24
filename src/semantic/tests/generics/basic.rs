//! Basic generic type resolution tests.

use crate::{grammar::test_aliases::*, semantic::types::test_aliases::*};

use super::super::util::*;

// ========== Basic generics tests ==========

#[test]
fn can_resolve_generic_type_with_single_parameter() {
    // Generic type: type Shared<T> { ptr: *mut T }
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::generic(
            (V::Public, "Shared"),
            [TP::new("T")],
            TD::new([TS::field((V::Public, "ptr"), T::ident("T").mut_pointer())])
                .with_attributes([A::size(pointer_size())]),
        )]),
        [SID::generic_defined_resolved(
            (SV::Public, "test::Shared"),
            ["T"],
            SISR::new(
                (pointer_size(), pointer_size()),
                STD::new().with_regions([SR::field(
                    (SV::Public, "ptr"),
                    ST::type_parameter("T").mut_pointer(),
                )]),
            ),
        )],
    );
}

#[test]
fn can_resolve_generic_type_with_multiple_parameters() {
    // Generic type: type Map<K, V> { key: *mut K, value: *mut V }
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::generic(
            (V::Public, "Map"),
            [TP::new("K"), TP::new("V")],
            TD::new([
                TS::field((V::Public, "key"), T::ident("K").mut_pointer()),
                TS::field((V::Public, "value"), T::ident("V").mut_pointer()),
            ])
            .with_attributes([A::size(2 * pointer_size())]),
        )]),
        [SID::generic_defined_resolved(
            (SV::Public, "test::Map"),
            ["K", "V"],
            SISR::new(
                (2 * pointer_size(), pointer_size()),
                STD::new().with_regions([
                    SR::field((SV::Public, "key"), ST::type_parameter("K").mut_pointer()),
                    SR::field((SV::Public, "value"), ST::type_parameter("V").mut_pointer()),
                ]),
            ),
        )],
    );
}

#[test]
fn can_resolve_field_with_generic_type_instantiation() {
    // Container with Shared<Entity> field
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::generic(
                (V::Public, "Shared"),
                [TP::new("T")],
                TD::new([TS::field((V::Public, "ptr"), T::ident("T").mut_pointer())])
                    .with_attributes([A::size(pointer_size())]),
            ),
            ID::new(
                (V::Public, "Entity"),
                TD::new([TS::field((V::Public, "id"), T::ident("u32"))]),
            ),
            ID::new(
                (V::Public, "Container"),
                TD::new([TS::field(
                    (V::Public, "entity"),
                    T::generic("Shared", [T::ident("Entity")]),
                )])
                .with_attributes([A::size(pointer_size())]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Container"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "entity"),
                        ST::generic("test::Shared", [ST::raw("test::Entity")]),
                    )]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Entity"),
                SISR::new(
                    (4, 4),
                    STD::new().with_regions([SR::field((SV::Public, "id"), ST::raw("u32"))]),
                ),
            ),
            SID::generic_defined_resolved(
                (SV::Public, "test::Shared"),
                ["T"],
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "ptr"),
                        ST::type_parameter("T").mut_pointer(),
                    )]),
                ),
            ),
        ],
    );
}

#[test]
fn can_resolve_pointer_to_generic_type() {
    // Field with *mut Shared<Entity>
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::generic(
                (V::Public, "Shared"),
                [TP::new("T")],
                TD::new([TS::field((V::Public, "ptr"), T::ident("T").mut_pointer())])
                    .with_attributes([A::size(pointer_size())]),
            ),
            ID::new(
                (V::Public, "Entity"),
                TD::new([TS::field((V::Public, "id"), T::ident("u32"))]),
            ),
            ID::new(
                (V::Public, "Container"),
                TD::new([TS::field(
                    (V::Public, "entity_ptr"),
                    T::generic("Shared", [T::ident("Entity")]).mut_pointer(),
                )])
                .with_attributes([A::size(pointer_size())]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Container"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "entity_ptr"),
                        ST::generic("test::Shared", [ST::raw("test::Entity")]).mut_pointer(),
                    )]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Entity"),
                SISR::new(
                    (4, 4),
                    STD::new().with_regions([SR::field((SV::Public, "id"), ST::raw("u32"))]),
                ),
            ),
            SID::generic_defined_resolved(
                (SV::Public, "test::Shared"),
                ["T"],
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "ptr"),
                        ST::type_parameter("T").mut_pointer(),
                    )]),
                ),
            ),
        ],
    );
}

#[test]
fn can_resolve_nested_generic_types() {
    // Field with Shared<Map<u32, Entity>>
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::generic(
                (V::Public, "Shared"),
                [TP::new("T")],
                TD::new([TS::field((V::Public, "ptr"), T::ident("T").mut_pointer())])
                    .with_attributes([A::size(pointer_size())]),
            ),
            ID::generic(
                (V::Public, "Map"),
                [TP::new("K"), TP::new("V")],
                TD::new([
                    TS::field((V::Public, "key"), T::ident("K").mut_pointer()),
                    TS::field((V::Public, "value"), T::ident("V").mut_pointer()),
                ])
                .with_attributes([A::size(2 * pointer_size())]),
            ),
            ID::new(
                (V::Public, "Entity"),
                TD::new([TS::field((V::Public, "id"), T::ident("u32"))]),
            ),
            ID::new(
                (V::Public, "Container"),
                TD::new([TS::field(
                    (V::Public, "shared_map"),
                    T::generic(
                        "Shared",
                        [T::generic("Map", [T::ident("u32"), T::ident("Entity")])],
                    ),
                )])
                .with_attributes([A::size(pointer_size())]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Container"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "shared_map"),
                        ST::generic(
                            "test::Shared",
                            [ST::generic(
                                "test::Map",
                                [ST::raw("u32"), ST::raw("test::Entity")],
                            )],
                        ),
                    )]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Entity"),
                SISR::new(
                    (4, 4),
                    STD::new().with_regions([SR::field((SV::Public, "id"), ST::raw("u32"))]),
                ),
            ),
            SID::generic_defined_resolved(
                (SV::Public, "test::Map"),
                ["K", "V"],
                SISR::new(
                    (2 * pointer_size(), pointer_size()),
                    STD::new().with_regions([
                        SR::field((SV::Public, "key"), ST::type_parameter("K").mut_pointer()),
                        SR::field((SV::Public, "value"), ST::type_parameter("V").mut_pointer()),
                    ]),
                ),
            ),
            SID::generic_defined_resolved(
                (SV::Public, "test::Shared"),
                ["T"],
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "ptr"),
                        ST::type_parameter("T").mut_pointer(),
                    )]),
                ),
            ),
        ],
    );
}
