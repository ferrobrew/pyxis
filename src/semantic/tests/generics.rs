//! Tests for generic type resolution.

use crate::{grammar::test_aliases::*, semantic::types::test_aliases::*};

use super::util::*;

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

// ========== Generic type alias tests ==========

#[test]
fn can_resolve_generic_type_alias_single_param() {
    // Generic type alias: type SharedPtr<T> = *mut T;
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::generic(
            (V::Public, "SharedPtr"),
            [TP::new("T")],
            TAD::new(T::ident("T").mut_pointer()),
        )]),
        [SID::generic_defined_resolved(
            (SV::Public, "test::SharedPtr"),
            ["T"],
            SISR::new(
                (0, 1),
                STAD::new(ST::type_parameter("T").mut_pointer(), vec![]),
            ),
        )],
    );
}

#[test]
fn can_resolve_generic_type_alias_multiple_params() {
    // Generic type alias: type Pair<K, V> = Map<K, V>;
    // where Map<K, V> is a generic type
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::generic(
                (V::Public, "Map"),
                [TP::new("K"), TP::new("V")],
                TD::new([
                    TS::field((V::Public, "key"), T::ident("K").mut_pointer()),
                    TS::field((V::Public, "value"), T::ident("V").mut_pointer()),
                ])
                .with_attributes([A::size(2 * pointer_size())]),
            ),
            ID::generic(
                (V::Public, "Pair"),
                [TP::new("A"), TP::new("B")],
                TAD::new(T::generic("Map", [T::ident("A"), T::ident("B")])),
            ),
        ]),
        [
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
                (SV::Public, "test::Pair"),
                ["A", "B"],
                SISR::new(
                    (0, 1),
                    STAD::new(
                        ST::generic(
                            "test::Map",
                            [ST::type_parameter("A"), ST::type_parameter("B")],
                        ),
                        vec![],
                    ),
                ),
            ),
        ],
    );
}

#[test]
fn can_resolve_generic_type_alias_wrapping_generic_type() {
    // Generic type alias: type EntityPtr<T> = Shared<T>;
    // where Shared<T> is a generic type
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::generic(
                (V::Public, "Shared"),
                [TP::new("T")],
                TD::new([TS::field((V::Public, "ptr"), T::ident("T").mut_pointer())])
                    .with_attributes([A::size(pointer_size())]),
            ),
            ID::generic(
                (V::Public, "EntityPtr"),
                [TP::new("T")],
                TAD::new(T::generic("Shared", [T::ident("T")])),
            ),
        ]),
        [
            SID::generic_defined_resolved(
                (SV::Public, "test::EntityPtr"),
                ["T"],
                SISR::new(
                    (0, 1),
                    STAD::new(
                        ST::generic("test::Shared", [ST::type_parameter("T")]),
                        vec![],
                    ),
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
fn can_resolve_generic_type_alias_pointer_to_generic() {
    // Generic type alias: type WeakRef<T> = *const Weak<T>;
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::generic(
                (V::Public, "Weak"),
                [TP::new("T")],
                TD::new([TS::field((V::Public, "ptr"), T::ident("T").const_pointer())])
                    .with_attributes([A::size(pointer_size())]),
            ),
            ID::generic(
                (V::Public, "WeakRef"),
                [TP::new("T")],
                TAD::new(T::generic("Weak", [T::ident("T")]).const_pointer()),
            ),
        ]),
        [
            SID::generic_defined_resolved(
                (SV::Public, "test::Weak"),
                ["T"],
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "ptr"),
                        ST::type_parameter("T").const_pointer(),
                    )]),
                ),
            ),
            SID::generic_defined_resolved(
                (SV::Public, "test::WeakRef"),
                ["T"],
                SISR::new(
                    (0, 1),
                    STAD::new(
                        ST::generic("test::Weak", [ST::type_parameter("T")]).const_pointer(),
                        vec![],
                    ),
                ),
            ),
        ],
    );
}

// ========== Tests for varied type parameter names ==========
// These tests verify that generic type resolution doesn't assume specific parameter names
// like "T", "U", "V", "W" and instead correctly looks up parameter names dynamically.

#[test]
fn can_resolve_generic_type_with_nonstandard_parameter_name() {
    // Generic type with non-standard parameter name: type Container<Element> { ptr: *mut Element }
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::generic(
            (V::Public, "Container"),
            [TP::new("Element")],
            TD::new([TS::field(
                (V::Public, "ptr"),
                T::ident("Element").mut_pointer(),
            )])
            .with_attributes([A::size(pointer_size())]),
        )]),
        [SID::generic_defined_resolved(
            (SV::Public, "test::Container"),
            ["Element"],
            SISR::new(
                (pointer_size(), pointer_size()),
                STD::new().with_regions([SR::field(
                    (SV::Public, "ptr"),
                    ST::type_parameter("Element").mut_pointer(),
                )]),
            ),
        )],
    );
}

#[test]
fn can_resolve_generic_type_with_multiple_nonstandard_parameter_names() {
    // Generic type: type Dictionary<Key, Value> { key: *mut Key, value: *mut Value }
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::generic(
            (V::Public, "Dictionary"),
            [TP::new("Key"), TP::new("Value")],
            TD::new([
                TS::field((V::Public, "key"), T::ident("Key").mut_pointer()),
                TS::field((V::Public, "value"), T::ident("Value").mut_pointer()),
            ])
            .with_attributes([A::size(2 * pointer_size())]),
        )]),
        [SID::generic_defined_resolved(
            (SV::Public, "test::Dictionary"),
            ["Key", "Value"],
            SISR::new(
                (2 * pointer_size(), pointer_size()),
                STD::new().with_regions([
                    SR::field((SV::Public, "key"), ST::type_parameter("Key").mut_pointer()),
                    SR::field(
                        (SV::Public, "value"),
                        ST::type_parameter("Value").mut_pointer(),
                    ),
                ]),
            ),
        )],
    );
}

#[test]
fn can_resolve_generic_type_alias_with_nonstandard_parameter_name() {
    // Generic type alias with non-standard parameter: type Ptr<Item> = *mut Item;
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::generic(
            (V::Public, "Ptr"),
            [TP::new("Item")],
            TAD::new(T::ident("Item").mut_pointer()),
        )]),
        [SID::generic_defined_resolved(
            (SV::Public, "test::Ptr"),
            ["Item"],
            SISR::new(
                (0, 1),
                STAD::new(ST::type_parameter("Item").mut_pointer(), vec![]),
            ),
        )],
    );
}

#[test]
fn can_resolve_field_using_generic_with_nonstandard_parameter_instantiated() {
    // Test that using a generic type with non-standard parameter names works in fields
    // type Container<Element> { ptr: *mut Element }
    // type Holder { data: Container<u32> }
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::generic(
                (V::Public, "Container"),
                [TP::new("Element")],
                TD::new([TS::field(
                    (V::Public, "ptr"),
                    T::ident("Element").mut_pointer(),
                )])
                .with_attributes([A::size(pointer_size())]),
            ),
            ID::new(
                (V::Public, "Holder"),
                TD::new([TS::field(
                    (V::Public, "data"),
                    T::generic("Container", [T::ident("u32")]),
                )])
                .with_attributes([A::size(pointer_size())]),
            ),
        ]),
        [
            SID::generic_defined_resolved(
                (SV::Public, "test::Container"),
                ["Element"],
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "ptr"),
                        ST::type_parameter("Element").mut_pointer(),
                    )]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Holder"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "data"),
                        ST::generic("test::Container", [ST::raw("u32")]),
                    )]),
                ),
            ),
        ],
    );
}

#[test]
fn can_resolve_generic_type_alias_wrapping_generic_with_different_param_names() {
    // Test aliasing a generic with different parameter names:
    // type Box<Content> { ptr: *mut Content }
    // type Wrapper<Data> = Box<Data>;
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::generic(
                (V::Public, "Box"),
                [TP::new("Content")],
                TD::new([TS::field(
                    (V::Public, "ptr"),
                    T::ident("Content").mut_pointer(),
                )])
                .with_attributes([A::size(pointer_size())]),
            ),
            ID::generic(
                (V::Public, "Wrapper"),
                [TP::new("Data")],
                TAD::new(T::generic("Box", [T::ident("Data")])),
            ),
        ]),
        [
            SID::generic_defined_resolved(
                (SV::Public, "test::Box"),
                ["Content"],
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "ptr"),
                        ST::type_parameter("Content").mut_pointer(),
                    )]),
                ),
            ),
            SID::generic_defined_resolved(
                (SV::Public, "test::Wrapper"),
                ["Data"],
                SISR::new(
                    (0, 1),
                    STAD::new(
                        ST::generic("test::Box", [ST::type_parameter("Data")]),
                        vec![],
                    ),
                ),
            ),
        ],
    );
}

#[test]
fn can_resolve_nested_generics_with_varied_parameter_names() {
    // Test nested generics with varied parameter names:
    // type Outer<X> { ptr: *mut X }
    // type Inner<Y, Z> { first: *mut Y, second: *mut Z }
    // type Combined { data: Outer<Inner<u32, i64>> }
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::generic(
                (V::Public, "Outer"),
                [TP::new("X")],
                TD::new([TS::field((V::Public, "ptr"), T::ident("X").mut_pointer())])
                    .with_attributes([A::size(pointer_size())]),
            ),
            ID::generic(
                (V::Public, "Inner"),
                [TP::new("Y"), TP::new("Z")],
                TD::new([
                    TS::field((V::Public, "first"), T::ident("Y").mut_pointer()),
                    TS::field((V::Public, "second"), T::ident("Z").mut_pointer()),
                ])
                .with_attributes([A::size(2 * pointer_size())]),
            ),
            ID::new(
                (V::Public, "Combined"),
                TD::new([TS::field(
                    (V::Public, "data"),
                    T::generic(
                        "Outer",
                        [T::generic("Inner", [T::ident("u32"), T::ident("i64")])],
                    ),
                )])
                .with_attributes([A::size(pointer_size())]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Combined"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "data"),
                        ST::generic(
                            "test::Outer",
                            [ST::generic("test::Inner", [ST::raw("u32"), ST::raw("i64")])],
                        ),
                    )]),
                ),
            ),
            SID::generic_defined_resolved(
                (SV::Public, "test::Inner"),
                ["Y", "Z"],
                SISR::new(
                    (2 * pointer_size(), pointer_size()),
                    STD::new().with_regions([
                        SR::field((SV::Public, "first"), ST::type_parameter("Y").mut_pointer()),
                        SR::field(
                            (SV::Public, "second"),
                            ST::type_parameter("Z").mut_pointer(),
                        ),
                    ]),
                ),
            ),
            SID::generic_defined_resolved(
                (SV::Public, "test::Outer"),
                ["X"],
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "ptr"),
                        ST::type_parameter("X").mut_pointer(),
                    )]),
                ),
            ),
        ],
    );
}

#[test]
fn can_resolve_generic_type_alias_partial_application_with_varied_names() {
    // Test partial application with varied parameter names:
    // type Map<K, V> { key: *mut K, value: *mut V }
    // type StringMap<ValueType> = Map<u32, ValueType>;  // "Key" is fixed to u32
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::generic(
                (V::Public, "Map"),
                [TP::new("KeyType"), TP::new("ValueType")],
                TD::new([
                    TS::field((V::Public, "key"), T::ident("KeyType").mut_pointer()),
                    TS::field((V::Public, "value"), T::ident("ValueType").mut_pointer()),
                ])
                .with_attributes([A::size(2 * pointer_size())]),
            ),
            ID::generic(
                (V::Public, "IntKeyMap"),
                [TP::new("Val")],
                TAD::new(T::generic("Map", [T::ident("u32"), T::ident("Val")])),
            ),
        ]),
        [
            SID::generic_defined_resolved(
                (SV::Public, "test::IntKeyMap"),
                ["Val"],
                SISR::new(
                    (0, 1),
                    STAD::new(
                        ST::generic("test::Map", [ST::raw("u32"), ST::type_parameter("Val")]),
                        vec![],
                    ),
                ),
            ),
            SID::generic_defined_resolved(
                (SV::Public, "test::Map"),
                ["KeyType", "ValueType"],
                SISR::new(
                    (2 * pointer_size(), pointer_size()),
                    STD::new().with_regions([
                        SR::field(
                            (SV::Public, "key"),
                            ST::type_parameter("KeyType").mut_pointer(),
                        ),
                        SR::field(
                            (SV::Public, "value"),
                            ST::type_parameter("ValueType").mut_pointer(),
                        ),
                    ]),
                ),
            ),
        ],
    );
}

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
