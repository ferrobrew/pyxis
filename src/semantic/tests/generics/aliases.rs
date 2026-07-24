//! Generic type alias resolution tests, including varied type parameter names.

use crate::{grammar::test_aliases::*, semantic::types::test_aliases::*};

use super::super::util::*;

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
