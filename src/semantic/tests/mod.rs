use crate::{
    grammar::test_aliases::{int_literal, *},
    semantic::{error::SemanticError, semantic_state::SemanticState, types::test_aliases::*},
    span::ItemLocation,
};

use pretty_assertions::assert_eq;

mod alignment;
mod inheritance;
mod min_size;
mod util;
mod visibility;
use util::*;

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
    assert_ast_produces_error(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType2"),
            TD::new([TS::field((V::Private, "field_2"), T::ident("TestType1"))]),
        )]),
        |err| match err {
            SemanticError::TypeResolutionStalled {
                unresolved_types,
                resolved_types,
                unresolved_references,
            } => {
                unresolved_types == &["test::TestType2".to_string()]
                    && resolved_types.is_empty()
                    && unresolved_references.len() == 1
                    && unresolved_references[0].type_name == "TestType1"
                    && unresolved_references[0].context
                        == "field `field_2` of type `test::TestType2`"
            }
            _ => false,
        },
    );
}

#[test]
fn unresolved_type_includes_type_name_in_error() {
    // Test that when a type can't be resolved, the error includes the specific missing type name
    assert_ast_produces_error(
        M::new().with_definitions([ID::new(
            (V::Public, "MyType"),
            TD::new([TS::field(
                (V::Private, "my_field"),
                T::ident("NonexistentType"),
            )]),
        )]),
        |err| {
            match err {
                SemanticError::TypeResolutionStalled {
                    unresolved_references,
                    ..
                } => {
                    // Should have exactly one unresolved reference
                    unresolved_references.len() == 1
                        && unresolved_references[0].type_name == "NonexistentType"
                        && unresolved_references[0].context
                            == "field `my_field` of type `test::MyType`"
                }
                _ => false,
            }
        },
    );
}

#[test]
fn unresolved_type_in_enum_includes_context() {
    // Test that enum base type errors include proper context
    assert_ast_produces_error(
        M::new().with_definitions([ID::new(
            (V::Public, "MyEnum"),
            ED::new(
                T::ident("NonexistentBase"),
                [ES::field("A"), ES::field("B")],
                [],
            ),
        )]),
        |err| match err {
            SemanticError::TypeResolutionStalled {
                unresolved_references,
                ..
            } => {
                unresolved_references.len() == 1
                    && unresolved_references[0].type_name == "NonexistentBase"
                    && unresolved_references[0].context == "base type of enum `test::MyEnum`"
            }
            _ => false,
        },
    );
}

#[test]
fn multiple_unresolved_types_are_all_reported() {
    // Test that multiple missing types are all reported
    assert_ast_produces_error(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Type1"),
                TD::new([TS::field((V::Private, "field_a"), T::ident("Missing1"))]),
            ),
            ID::new(
                (V::Public, "Type2"),
                TD::new([TS::field((V::Private, "field_b"), T::ident("Missing2"))]),
            ),
        ]),
        |err| {
            match err {
                SemanticError::TypeResolutionStalled {
                    unresolved_references,
                    ..
                } => {
                    // Should have two unresolved references (order may vary)
                    if unresolved_references.len() != 2 {
                        return false;
                    }
                    let ref1 = unresolved_references
                        .iter()
                        .find(|r| r.type_name == "Missing1");
                    let ref2 = unresolved_references
                        .iter()
                        .find(|r| r.type_name == "Missing2");
                    match (ref1, ref2) {
                        (Some(r1), Some(r2)) => {
                            r1.context == "field `field_a` of type `test::Type1`"
                                && r2.context == "field `field_b` of type `test::Type2`"
                        }
                        _ => false,
                    }
                }
                _ => false,
            }
        },
    );
}

#[test]
fn can_use_type_from_another_module() {
    let module1 = M::new()
        .with_uses([IP::from("module2::TestType2")])
        .with_definitions([ID::new(
            (V::Public, "TestType1"),
            TD::new([TS::field((V::Private, "field"), T::ident("TestType2"))]),
        )]);
    let module2 = M::new().with_definitions([ID::new(
        (V::Public, "TestType2"),
        TD::new([TS::field((V::Private, "field"), T::ident("u32"))]),
    )]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();
    semantic_state
        .add_module(&module2, &IP::from("module2"))
        .unwrap();
    let semantic_state = semantic_state.build().unwrap();

    let path = IP::from("module1::TestType1");
    let resolved_type = semantic_state
        .type_registry()
        .get(&path, &ItemLocation::test())
        .cloned()
        .expect("failed to get type");
    assert_eq!(
        resolved_type,
        SID::defined_resolved(
            (SV::Public, path.clone(),),
            SISR::new(
                (4, 4,),
                STD::new().with_regions([SR::field(
                    (SV::Private, "field"),
                    ST::raw("module2::TestType2")
                )])
            )
        )
    );
}

#[test]
fn can_use_braced_imports_from_another_module() {
    // Test that braced imports like `use math::{Matrix4, Vector3}` work correctly.
    // The UseTree::Group is flattened by the semantic layer's scope() function.
    let module1 = M::new()
        .with_use_trees([UT::group(
            "math",
            [UT::path("Matrix4"), UT::path("Vector3")],
        )])
        .with_definitions([ID::new(
            (V::Public, "Transform"),
            TD::new([
                TS::field((V::Private, "matrix"), T::ident("Matrix4")),
                TS::field((V::Private, "position"), T::ident("Vector3")),
            ]),
        )]);
    let module_math = M::new().with_definitions([
        ID::new(
            (V::Public, "Matrix4"),
            TD::new([TS::field((V::Public, "data"), T::ident("f32").array(16))]),
        ),
        ID::new(
            (V::Public, "Vector3"),
            TD::new([
                TS::field((V::Public, "x"), T::ident("f32")),
                TS::field((V::Public, "y"), T::ident("f32")),
                TS::field((V::Public, "z"), T::ident("f32")),
            ]),
        ),
    ]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();
    semantic_state
        .add_module(&module_math, &IP::from("math"))
        .unwrap();
    let semantic_state = semantic_state.build().unwrap();

    // Verify the Transform type was resolved correctly with both imported types
    let path = IP::from("module1::Transform");
    let resolved_type = semantic_state
        .type_registry()
        .get(&path, &ItemLocation::test())
        .cloned()
        .expect("failed to get Transform type");
    assert_eq!(
        resolved_type,
        SID::defined_resolved(
            (SV::Public, path.clone(),),
            SISR::new(
                (76, 4,), // 64 bytes for Matrix4 + 12 bytes for Vector3
                STD::new().with_regions([
                    SR::field((SV::Private, "matrix"), ST::raw("math::Matrix4")),
                    SR::field((SV::Private, "position"), ST::raw("math::Vector3")),
                ])
            )
        )
    );
}

#[test]
fn will_fail_on_use_of_nonexistent_type() {
    // Test that using a type that doesn't exist produces an error
    let module1 = M::new()
        .with_uses([IP::from("math::NonExistent")])
        .with_definitions([ID::new(
            (V::Public, "Test"),
            TD::new([TS::field((V::Private, "value"), T::ident("u32"))]),
        )]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();

    let err = semantic_state.build().unwrap_err();
    assert!(
        matches!(err, SemanticError::UseItemNotFound { ref path, .. } if *path == IP::from("math::NonExistent")),
        "Expected UseItemNotFound error for math::NonExistent, got: {err:?}"
    );
}

#[test]
fn will_fail_on_braced_import_with_nonexistent_type() {
    // Test that braced imports with a nonexistent type produce an error
    let module1 = M::new()
        .with_use_trees([UT::group(
            "math",
            [UT::path("Matrix4"), UT::path("NonExistent")],
        )])
        .with_definitions([ID::new(
            (V::Public, "Test"),
            TD::new([TS::field((V::Private, "value"), T::ident("u32"))]),
        )]);
    let module_math = M::new().with_definitions([ID::new(
        (V::Public, "Matrix4"),
        TD::new([TS::field((V::Public, "data"), T::ident("f32").array(16))]),
    )]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();
    semantic_state
        .add_module(&module_math, &IP::from("math"))
        .unwrap();

    let err = semantic_state.build().unwrap_err();
    assert!(
        matches!(err, SemanticError::UseItemNotFound { ref path, .. } if *path == IP::from("math::NonExistent")),
        "Expected UseItemNotFound error for math::NonExistent, got: {err:?}"
    );
}

#[test]
fn will_fail_on_use_of_nonexistent_module() {
    // Test that using a module that doesn't exist produces an error
    let module1 = M::new()
        .with_uses([IP::from("nonexistent::SomeType")])
        .with_definitions([ID::new(
            (V::Public, "Test"),
            TD::new([TS::field((V::Private, "value"), T::ident("u32"))]),
        )]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();

    let err = semantic_state.build().unwrap_err();
    assert!(
        matches!(err, SemanticError::UseItemNotFound { ref path, .. } if *path == IP::from("nonexistent::SomeType")),
        "Expected UseItemNotFound error for nonexistent::SomeType, got: {err:?}"
    );
}

#[test]
fn can_use_module_in_use_statement() {
    // Test that using a module path (not just a type) is allowed
    // This is useful for `use math` to bring the module into scope
    let module1 = M::new()
        .with_uses([IP::from("math")])
        .with_definitions([ID::new(
            (V::Public, "Test"),
            TD::new([TS::field((V::Private, "value"), T::ident("u32"))]),
        )]);
    let module_math = M::new().with_definitions([ID::new(
        (V::Public, "Vector3"),
        TD::new([TS::field((V::Public, "x"), T::ident("f32"))]),
    )]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();
    semantic_state
        .add_module(&module_math, &IP::from("math"))
        .unwrap();

    // Should succeed because "math" is a valid module
    semantic_state.build().unwrap();
}

#[test]
fn will_fail_on_an_extern_without_size() {
    let err = build_state(
        &M::new().with_extern_types([("TestType".into(), As::default())]),
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(
        err.to_string().contains(
            "failed to find `size` attribute for extern type `TestType` in module `test`"
        )
    );
}

#[test]
fn can_resolve_embed_of_an_extern() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_extern_types([(
                "TestType1".into(),
                As::from_iter([A::size(16), A::align(4)]),
            )])
            .with_definitions([ID::new(
                (V::Public, "TestType2"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("u64")),
                    TS::field((V::Public, "field_2"), T::ident("TestType1")),
                    TS::field(
                        (V::Public, "field_3"),
                        T::ident("TestType1").const_pointer(),
                    ),
                    TS::field((V::Public, "field_4"), T::ident("TestType1").mut_pointer()),
                ])
                .with_attributes([A::align(8)]),
            )]),
        [
            SID::defined_resolved(
                (SV::Public, "test::TestType2"),
                SISR::new(
                    (24 + 2 * pointer_size(), 8),
                    STD::new().with_regions([
                        SR::field((SV::Public, "field_1"), ST::raw("u64")),
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
            SID::category_resolved(
                (SV::Public, "test::TestType1"),
                SISR::new((16, 4), STD::new().with_regions([])),
                SIC::Extern,
            ),
        ],
    );
}

#[test]
fn can_generate_vftable() {
    let vftable_type = ST::raw("test::TestTypeVftable").const_pointer();
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::vftable([
                F::new(
                    (V::Public, "test_function0"),
                    [
                        Ar::mut_self(),
                        Ar::named("arg0", T::ident("u32")),
                        Ar::named("arg1", T::ident("f32")),
                    ],
                )
                .with_return_type("i32"),
                F::new(
                    (V::Public, "test_function1"),
                    [
                        Ar::mut_self(),
                        Ar::named("arg0", T::ident("u32")),
                        Ar::named("arg1", T::ident("f32")),
                    ],
                ),
            ])
            .with_attributes([A::size(4)])]),
        )]),
        [
            // TestType
            SID::defined_resolved(
                (SV::Public, "test::TestType"),
                SISR::new((pointer_size(), pointer_size()), {
                    STD::new()
                        .with_regions([SR::field((SV::Private, "vftable"), vftable_type.clone())])
                        .with_vftable(STV::new(
                            [
                                SF::new(
                                    (SV::Public, "test_function0"),
                                    SFB::vftable("test_function0"),
                                    SCC::for_member_function(pointer_size()),
                                )
                                .with_arguments([
                                    SAr::mut_self(),
                                    SAr::field("arg0", ST::raw("u32")),
                                    SAr::field("arg1", ST::raw("f32")),
                                ])
                                .with_return_type(ST::raw("i32")),
                                SF::new(
                                    (SV::Public, "test_function1"),
                                    SFB::vftable("test_function1"),
                                    SCC::for_member_function(pointer_size()),
                                )
                                .with_arguments([
                                    SAr::mut_self(),
                                    SAr::field("arg0", ST::raw("u32")),
                                    SAr::field("arg1", ST::raw("f32")),
                                ]),
                                make_vfunc(2),
                                make_vfunc(3),
                            ],
                            None,
                            vftable_type,
                        ))
                }),
            ),
            // TestTypeVftable
            SID::defined_resolved(
                (SV::Public, "test::TestTypeVftable"),
                SISR::new(
                    (4 * pointer_size(), pointer_size()),
                    STD::new().with_regions([
                        SR::field(
                            (SV::Public, "test_function0"),
                            ST::function(
                                SCC::for_member_function(pointer_size()),
                                [
                                    ("this", ST::raw("test::TestType").mut_pointer()),
                                    ("arg0", ST::raw("u32")),
                                    ("arg1", ST::raw("f32")),
                                ],
                                ST::raw("i32"),
                            ),
                        ),
                        SR::field(
                            (SV::Public, "test_function1"),
                            ST::function(
                                SCC::for_member_function(pointer_size()),
                                [
                                    ("this", ST::raw("test::TestType").mut_pointer()),
                                    ("arg0", ST::raw("u32")),
                                    ("arg1", ST::raw("f32")),
                                ],
                                None,
                            ),
                        ),
                        make_vfunc_region(2),
                        make_vfunc_region(3),
                    ]),
                ),
            ),
        ],
    );
}

#[test]
fn can_generate_vftable_with_indices() {
    let vftable_type = ST::raw("test::TestTypeVftable").const_pointer();
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::vftable([
                F::new(
                    (V::Public, "test_function0"),
                    [
                        Ar::mut_self(),
                        Ar::named("arg0", T::ident("u32")),
                        Ar::named("arg1", T::ident("f32")),
                    ],
                )
                .with_attributes([A::index(2)])
                .with_return_type("i32"),
                F::new(
                    (V::Public, "test_function1"),
                    [
                        Ar::mut_self(),
                        Ar::named("arg0", T::ident("u32")),
                        Ar::named("arg1", T::ident("f32")),
                    ],
                )
                .with_attributes([A::index(5)]),
            ])
            .with_attributes([A::size(8)])]),
        )]),
        [
            // TestType
            SID::defined_resolved(
                (SV::Public, "test::TestType"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new()
                        .with_regions([SR::field((SV::Private, "vftable"), vftable_type.clone())])
                        .with_vftable(STV::new(
                            [
                                make_vfunc(0),
                                make_vfunc(1),
                                SF::new(
                                    (SV::Public, "test_function0"),
                                    SFB::vftable("test_function0"),
                                    SCC::for_member_function(pointer_size()),
                                )
                                .with_arguments([
                                    SAr::mut_self(),
                                    SAr::field("arg0", ST::raw("u32")),
                                    SAr::field("arg1", ST::raw("f32")),
                                ])
                                .with_return_type(ST::raw("i32")),
                                make_vfunc(3),
                                make_vfunc(4),
                                SF::new(
                                    (SV::Public, "test_function1"),
                                    SFB::vftable("test_function1"),
                                    SCC::for_member_function(pointer_size()),
                                )
                                .with_arguments([
                                    SAr::mut_self(),
                                    SAr::field("arg0", ST::raw("u32")),
                                    SAr::field("arg1", ST::raw("f32")),
                                ]),
                                make_vfunc(6),
                                make_vfunc(7),
                            ],
                            None,
                            vftable_type,
                        )),
                ),
            ),
            // TestTypeVftable
            SID::defined_resolved(
                (SV::Public, "test::TestTypeVftable"),
                SISR::new(
                    (8 * pointer_size(), pointer_size()),
                    STD::new().with_regions([
                        make_vfunc_region(0),
                        make_vfunc_region(1),
                        SR::field(
                            (SV::Public, "test_function0"),
                            ST::function(
                                SCC::for_member_function(pointer_size()),
                                [
                                    ("this", ST::raw("test::TestType").mut_pointer()),
                                    ("arg0", ST::raw("u32")),
                                    ("arg1", ST::raw("f32")),
                                ],
                                ST::raw("i32"),
                            ),
                        ),
                        make_vfunc_region(3),
                        make_vfunc_region(4),
                        SR::field(
                            (SV::Public, "test_function1"),
                            ST::function(
                                SCC::for_member_function(pointer_size()),
                                [
                                    ("this", ST::raw("test::TestType").mut_pointer()),
                                    ("arg0", ST::raw("u32")),
                                    ("arg1", ST::raw("f32")),
                                ],
                                None,
                            ),
                        ),
                        make_vfunc_region(6),
                        make_vfunc_region(7),
                    ]),
                ),
            ),
        ],
    );
}

#[test]
fn will_propagate_calling_convention_for_impl_and_vftable() {
    let vftable_type = ST::raw("test::TestTypeVftable").const_pointer();
    assert_ast_produces_type_definitions(
        M::new()
            .with_definitions([ID::new(
                (V::Public, "TestType"),
                TD::new([TS::vftable([F::new(
                    (V::Public, "test_function0"),
                    [
                        Ar::mut_self(),
                        Ar::named("arg0", T::ident("u32")),
                        Ar::named("arg1", T::ident("f32")),
                    ],
                )
                .with_return_type("i32")
                .with_attributes([A::calling_convention("cdecl")])])]),
            )])
            .with_impls([FB::new(
                "TestType",
                [F::new(
                    (V::Public, "test_function"),
                    [Ar::named("arg1", T::ident("i32"))],
                )
                .with_attributes([A::address(0x800_000), A::calling_convention("cdecl")])
                .with_return_type(T::ident("i32"))],
            )]),
        [
            // TestType
            SID::defined_resolved(
                (SV::Public, "test::TestType"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    {
                        STD::new()
                            .with_regions([SR::field(
                                (SV::Private, "vftable"),
                                vftable_type.clone(),
                            )])
                            .with_vftable(STV::new(
                                [SF::new(
                                    (SV::Public, "test_function0"),
                                    SFB::vftable("test_function0"),
                                    SCC::for_member_function(pointer_size()),
                                )
                                .with_arguments([
                                    SAr::mut_self(),
                                    SAr::field("arg0", ST::raw("u32")),
                                    SAr::field("arg1", ST::raw("f32")),
                                ])
                                .with_calling_convention(SCC::Cdecl)
                                .with_return_type(ST::raw("i32"))],
                                None,
                                vftable_type,
                            ))
                    }
                    .with_associated_functions([SF::new(
                        (SV::Public, "test_function"),
                        SFB::address(0x800_000),
                        SCC::for_member_function(pointer_size()),
                    )
                    .with_calling_convention(SCC::Cdecl)
                    .with_arguments([SAr::field("arg1", ST::raw("i32"))])
                    .with_return_type(ST::raw("i32"))]),
                ),
            ),
            // TestTypeVftable
            SID::defined_resolved(
                (SV::Public, "test::TestTypeVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "test_function0"),
                        ST::function(
                            SCC::Cdecl,
                            [
                                ("this", ST::raw("test::TestType").mut_pointer()),
                                ("arg0", ST::raw("u32")),
                                ("arg1", ST::raw("f32")),
                            ],
                            ST::raw("i32"),
                        ),
                    )]),
                ),
            ),
        ],
    );
}

fn make_vfunc(index: usize) -> SF {
    let name = format!("_vfunc_{index}");
    SF::new(
        (SV::Private, name.clone()),
        SFB::vftable(name),
        SCC::for_member_function(pointer_size()),
    )
    .with_arguments([SAr::mut_self()])
}

fn make_vfunc_region(index: usize) -> SR {
    SR::field(
        (SV::Private, format!("_vfunc_{index}")),
        ST::function(
            SCC::for_member_function(pointer_size()),
            [("this", ST::raw("test::TestType").mut_pointer())],
            None,
        ),
    )
}

#[test]
fn can_define_extern_value() {
    let module1 = M::new().with_extern_values([EV::new(
        V::Public,
        "test",
        T::ident("u32").mut_pointer(),
        [A::address(0x1337)],
    )]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();
    let semantic_state = semantic_state.build().unwrap();

    let extern_value = semantic_state
        .modules()
        .get(&IP::from("module1"))
        .unwrap()
        .extern_values
        .first()
        .unwrap();

    assert_eq!(
        extern_value,
        &SEV {
            visibility: SV::Public,
            name: "test".into(),
            type_: ST::raw("u32").mut_pointer(),
            address: 0x1337,
            location: ItemLocation::test(),
        }
    );
}

#[test]
fn can_resolve_enum() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            ED::new(
                T::ident("u32"),
                [
                    ES::field_with_expr("Item0", int_literal(-2)),
                    ES::field("Item1"),
                    ES::field("Item2"),
                    ES::field_with_expr("Item3", int_literal(10)),
                    ES::field("Item4"),
                ],
                [A::singleton(0x1234)],
            ),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SED::new(ST::raw("u32"))
                    .with_variants([
                        ("Item0", -2),
                        ("Item1", -1),
                        ("Item2", 0),
                        ("Item3", 10),
                        ("Item4", 11),
                    ])
                    .with_singleton(0x1234),
            ),
        )],
    );
}

#[test]
fn can_resolve_enum_with_associated_functions() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_definitions([ID::new(
                (V::Public, "TestEnum"),
                ED::new(
                    T::ident("u32"),
                    [ES::field("None"), ES::field("Some"), ES::field("Value")],
                    [],
                ),
            )])
            .with_impls([FB::new(
                "TestEnum",
                [
                    F::new((V::Public, "test"), []).with_attributes([A::address(0x123)]),
                    F::new((V::Public, "another_test"), [Ar::const_self()])
                        .with_attributes([A::address(0x456)])
                        .with_return_type(T::ident("i32")),
                ],
            )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestEnum"),
            SISR::new(
                (4, 4),
                SED::new(ST::raw("u32"))
                    .with_variants([("None", 0), ("Some", 1), ("Value", 2)])
                    .with_associated_functions([
                        SF::new((SV::Public, "test"), SFB::address(0x123), SCC::System),
                        SF::new(
                            (SV::Public, "another_test"),
                            SFB::address(0x456),
                            SCC::for_member_function(pointer_size()),
                        )
                        .with_arguments([SAr::const_self()])
                        .with_return_type(ST::raw("i32")),
                    ]),
            ),
        )],
    );
}

#[test]
fn can_carry_backend_across() {
    let prologue = r#"
        use std::ffi::CString;
        use std::os::raw::c_char;
    "#
    .trim();

    let epilogue = r#"
        fn main() {
            println!("Hello, world!");
        }
    "#
    .trim();

    // Intentionally double-include the epilogue to test if it's correctly carried across
    let ast = M::new().with_backends([
        B::new("rust")
            .with_prologue(prologue)
            .with_epilogue(epilogue),
        B::new("rust").with_epilogue(epilogue),
    ]);
    let test_path = IP::from("test");

    let state = build_state(&ast, &test_path).unwrap();
    let module = state.modules().get(&test_path).unwrap();

    assert_eq!(
        module.backends.get("rust").unwrap(),
        &[
            SB::new(prologue.to_string(), epilogue.to_string()),
            SB::new(None, epilogue.to_string()),
        ]
    );
}

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
    let err = build_state(
        &M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field(
                (V::Private, "field_1"),
                T::ident("i32").mut_pointer(),
            )])
            .with_attributes([A::defaultable()]),
        )]),
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(err.to_string().contains(
        "field `field_1` of type `test::TestType` is not a defaultable type (pointer or function?)"
    ));
}

#[test]
fn will_reject_defaultable_on_enum_field() {
    let err = build_state(
        &M::new().with_definitions([
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
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(
        err.to_string()
            .contains("field `field_1` of type `test::TestType` is not a defaultable type")
    );
}

#[test]
fn can_handle_defaultable_on_enum_with_default_field() {
    let err1 = build_state(
        &M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            ED::new(
                T::ident("u32"),
                [ES::field("Item1"), ES::field("Item2")],
                [],
            )
            .with_attributes([A::defaultable()]),
        )]),
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(
        err1.to_string().contains(
            "enum `test::TestType` is marked as defaultable but has no default variant set"
        )
    );

    let err2 = build_state(
        &M::new().with_definitions([ID::new(
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
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(err2.to_string().contains(
        "enum `test::TestType` has a default variant set but is not marked as defaultable"
    ));

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
    let err = build_state(
        &M::new().with_definitions([
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
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(
        err.to_string()
            .contains("field `field_1` of type `test::TestType` is not a defaultable type")
    );
}

#[test]
fn will_reject_types_that_are_larger_than_their_specified_size() {
    let err = build_state(
        &M::new().with_definitions([
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
        &IP::from("test"),
    )
    .unwrap_err();
    let err_str = err.to_string();
    assert!(err_str.contains("while processing `test::TestType`"));
    assert!(
        err_str.contains(
            "calculated size 512 for type `test::TestType` does not match target size 256"
        )
    );
    assert!(err_str.contains("is your target size correct?"));
}

#[test]
fn can_propagate_doc_comments() {
    let created_module = assert_ast_produces_type_definitions(
        M::new()
            .with_definitions([ID::new(
                (V::Private, "TestType"),
                TD::new([
                    TS::vftable([F::new((V::Private, "test_vfunc"), [Ar::const_self()])
                        .with_doc_comments(vec![" My test vfunc!".to_string()])]),
                    TS::field((V::Private, "field_1"), T::ident("u64"))
                        .with_doc_comments(vec![" This is a field doc comment".to_string()])
                        .with_attributes([A::address(8)]),
                ])
                .with_attributes([A::align(8)]),
            )
            .with_doc_comments(vec![" This is a doc comment".to_string()])])
            .with_impls([FB::new(
                "TestType",
                [F::new((V::Private, "test_func"), [Ar::const_self()])
                    .with_doc_comments(vec![" My test func!".to_string()])
                    .with_attributes([A::address(0x123)])],
            )])
            .with_doc_comments(vec![
                " This is a module doc comment".to_string(),
                " The best of its kind".to_string(),
            ]),
        [
            SID::defined_resolved(
                (SV::Private, "test::TestType"),
                SISR::new(
                    (16, 8),
                    STD::new()
                        .with_regions(filter_out_empty_regions([
                            SR::field(
                                (SV::Private, "vftable"),
                                ST::raw("test::TestTypeVftable").const_pointer(),
                            ),
                            pad_up_to_8_region(),
                            SR::field((SV::Private, "field_1"), ST::raw("u64"))
                                .with_doc([" This is a field doc comment"]),
                        ]))
                        .with_doc([" This is a doc comment"])
                        .with_vftable(STV::new(
                            [SF::new(
                                (SV::Private, "test_vfunc"),
                                SFB::vftable("test_vfunc"),
                                SCC::for_member_function(pointer_size()),
                            )
                            .with_arguments([SAr::const_self()])
                            .with_doc([" My test vfunc!"])],
                            None,
                            ST::raw("test::TestTypeVftable").const_pointer(),
                        ))
                        .with_associated_functions([SF::new(
                            (SV::Private, "test_func"),
                            SFB::address(0x123),
                            SCC::for_member_function(pointer_size()),
                        )
                        .with_arguments([SAr::const_self()])
                        .with_doc([" My test func!"])]),
                ),
            ),
            SID::defined_resolved(
                (SV::Private, "test::TestTypeVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Private, "test_vfunc"),
                        ST::function(
                            SCC::for_member_function(pointer_size()),
                            [("this", ST::raw("test::TestType").const_pointer())],
                            None,
                        ),
                    )
                    .with_doc([" My test vfunc!"])]),
                ),
            ),
        ],
    );

    assert_eq!(
        created_module.doc,
        vec![
            " This is a module doc comment".to_string(),
            " The best of its kind".to_string()
        ]
    );
}

#[test]
fn can_resolve_bitflags() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)),
                    BFS::field("Item3", int_literal(0b0100)),
                    BFS::field("Item4", int_literal(0b1000)),
                ],
                [A::singleton(0x1234)],
            ),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SBFD::new(ST::raw("u32"))
                    .with_flags([
                        ("Item1", 0b0001),
                        ("Item2", 0b0010),
                        ("Item3", 0b0100),
                        ("Item4", 0b1000),
                    ])
                    .with_singleton(0x1234),
            ),
        )],
    );
}

#[test]
fn bitflags_handle_defaultable_correctly() {
    let err1 = build_state(
        &M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)),
                ],
                [],
            )
            .with_attributes([A::defaultable()]),
        )]),
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(err1.to_string().contains(
        "bitflags `test::TestType` is marked as defaultable but has no default value set"
    ));

    let err2 = build_state(
        &M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)).with_attributes([A::default()]),
                ],
                [],
            )
            .with_attributes([]),
        )]),
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(err2.to_string().contains(
        "bitflags `test::TestType` has a default value set but is not marked as defaultable"
    ));

    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)).with_attributes([A::default()]),
                ],
                [],
            )
            .with_attributes([A::defaultable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SBFD::new(ST::raw("u32"))
                    .with_flags([("Item1", 0b0001), ("Item2", 0b0010)])
                    .with_default(1),
            ),
        )],
    );
}

#[test]
fn bitflags_with_invalid_underlying_type_are_rejected() {
    for invalid_type in ["i8", "i16", "i32", "i64", "i128", "Lol"] {
        let err = build_state(
            &M::new().with_definitions([
                ID::new((V::Public, "Lol"), TD::new([])),
                ID::new(
                    (V::Public, "TestType"),
                    BFD::new(
                        T::ident(invalid_type),
                        [
                            BFS::field("Item1", int_literal(0b0001)),
                            BFS::field("Item2", int_literal(0b0010)),
                        ],
                        [],
                    ),
                ),
            ]),
            &IP::from("test"),
        )
        .unwrap_err();
        let expected = if invalid_type == "Lol" {
            "bitflags definition `test::TestType` has a type that is not a predefined type: test::Lol".to_string()
        } else {
            format!(
                "bitflags definition `test::TestType` has a type that is not an unsigned integer: {invalid_type}"
            )
        };
        assert!(err.to_string().contains(&expected));
    }

    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)),
                ],
                [],
            ),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SBFD::new(ST::raw("u32")).with_flags([("Item1", 0b0001), ("Item2", 0b0010)]),
            ),
        )],
    );
}

#[test]
fn can_resolve_freestanding_functions() {
    let ast = M::new()
        .with_definitions([ID::new(
            (V::Public, "TestEnum"),
            ED::new(T::ident("u32"), [ES::field("None")], []),
        )])
        .with_impls([FB::new(
            "TestEnum",
            [F::new((V::Public, "test"), []).with_attributes([A::address(0x123)])],
        )])
        .with_functions([
            F::new((V::Public, "freestanding"), []).with_attributes([A::address(0x456)]),
            F::new(
                (V::Private, "another_freestanding"),
                [Ar::named("arg1", T::ident("i32"))],
            )
            .with_attributes([A::address(0x789)])
            .with_return_type(T::ident("i32")),
        ]);

    let test_path = IP::from("test");
    let state = build_state(&ast, &test_path).unwrap();
    let module = state.modules().get(&test_path).unwrap();

    assert_eq!(module.functions().len(), 2);

    // Check first freestanding function
    let func1 = &module.functions()[0];
    assert_eq!(func1.name, "freestanding");
    assert_eq!(func1.visibility, SV::Public);
    assert_eq!(func1.arguments.len(), 0);
    assert_eq!(func1.return_type, None);
    assert_eq!(func1.calling_convention, SCC::System);
    assert!(matches!(func1.body, SFB::Address { address: 0x456 }));

    // Check second freestanding function
    let func2 = &module.functions()[1];
    assert_eq!(func2.name, "another_freestanding");
    assert_eq!(func2.visibility, SV::Private);
    assert_eq!(func2.arguments.len(), 1);
    assert!(matches!(&func2.arguments[0], SAr::Field { name, .. } if name == "arg1"));
    assert_eq!(func2.return_type, Some(ST::raw("i32")));
    assert_eq!(func2.calling_convention, SCC::System);
    assert!(matches!(func2.body, SFB::Address { address: 0x789 }));
}

#[test]
fn can_resolve_type_alias() {
    // Test basic type alias resolution: type IntPtr = *const i32;
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "IntPtr"),
            TAD::new(T::ident("i32").const_pointer()),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::IntPtr"),
            SISR::new((0, 1), STAD::new(ST::raw("i32").const_pointer(), vec![])),
        )],
    );
}

#[test]
fn can_resolve_type_alias_to_user_type() {
    // Test type alias to a user-defined type
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Texture"),
                TD::new([TS::field((V::Public, "data"), T::ident("u32"))]),
            ),
            ID::new(
                (V::Public, "TexturePtr"),
                TAD::new(T::ident("Texture").const_pointer()),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Texture"),
                SISR::new(
                    (4, 4),
                    STD::new().with_regions([SR::field((SV::Public, "data"), ST::raw("u32"))]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::TexturePtr"),
                SISR::new(
                    (0, 1),
                    STAD::new(ST::raw("test::Texture").const_pointer(), vec![]),
                ),
            ),
        ],
    );
}

#[test]
fn can_use_type_alias_in_struct_field() {
    // Test using a type alias as a field type
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "IntPtr"),
                TAD::new(T::ident("i32").const_pointer()),
            ),
            ID::new(
                (V::Public, "Container"),
                TD::new([TS::field((V::Public, "ptr"), T::ident("IntPtr"))]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Container"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "ptr"),
                        ST::raw("i32").const_pointer(),
                    )]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::IntPtr"),
                SISR::new((0, 1), STAD::new(ST::raw("i32").const_pointer(), vec![])),
            ),
        ],
    );
}

#[test]
fn can_use_type_alias_as_reexport() {
    // Test cross-module type alias re-export semantics:
    // - module_a: defines Texture
    // - module_b: imports Texture from A, defines TexturePtr = *const Texture
    // - module_c: imports TexturePtr from B (NOT Texture from A), uses it in a struct
    // This tests that visibility is checked at definition time, not usage time.
    let module_a = M::new().with_definitions([ID::new(
        (V::Public, "Texture"),
        TD::new([TS::field((V::Public, "data"), T::ident("u32"))]),
    )]);

    let module_b = M::new()
        .with_uses([IP::from("module_a::Texture")])
        .with_definitions([ID::new(
            (V::Public, "TexturePtr"),
            TAD::new(T::ident("Texture").const_pointer()),
        )]);

    // module_c imports only TexturePtr, not Texture - demonstrating re-export semantics
    let module_c = M::new()
        .with_uses([IP::from("module_b::TexturePtr")])
        .with_definitions([ID::new(
            (V::Public, "Container"),
            TD::new([TS::field((V::Public, "ptr"), T::ident("TexturePtr"))]),
        )]);

    let mut semantic_state = SemanticState::new(pointer_size());
    semantic_state
        .add_module(&module_a, &IP::from("module_a"))
        .unwrap();
    semantic_state
        .add_module(&module_b, &IP::from("module_b"))
        .unwrap();
    semantic_state
        .add_module(&module_c, &IP::from("module_c"))
        .unwrap();
    let semantic_state = semantic_state.build().unwrap();

    // Verify Container was created correctly with the aliased type
    let container_path = IP::from("module_c::Container");
    let container = semantic_state
        .type_registry()
        .get(&container_path, &ItemLocation::test())
        .expect("Container should exist");

    let resolved = container.resolved().expect("Container should be resolved");
    let type_def = resolved
        .inner
        .as_type()
        .expect("Container should be a type");

    // The field should be *const module_a::Texture (the fully resolved type)
    assert_eq!(type_def.regions.len(), 1);
    assert_eq!(
        type_def.regions[0].type_ref,
        ST::raw("module_a::Texture").const_pointer()
    );

    // Also verify the type alias itself was resolved correctly
    let alias_path = IP::from("module_b::TexturePtr");
    let alias = semantic_state
        .type_registry()
        .get(&alias_path, &ItemLocation::test())
        .expect("TexturePtr alias should exist");

    let alias_resolved = alias.resolved().expect("TexturePtr should be resolved");
    let alias_def = alias_resolved
        .inner
        .as_type_alias()
        .expect("TexturePtr should be a type alias");

    assert_eq!(
        alias_def.target,
        ST::raw("module_a::Texture").const_pointer()
    );
}

// ========== Generics tests ==========

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
