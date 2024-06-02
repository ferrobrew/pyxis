use std::collections::HashSet;

use crate::{
    grammar::test_aliases::*,
    semantic_analysis::{
        semantic_state::{ResolvedSemanticState, SemanticState},
        types::test_aliases::*,
    },
};

use anyhow::Context;

fn build_state(module: &M, module_path: &IP) -> anyhow::Result<ResolvedSemanticState> {
    let mut semantic_state = SemanticState::new(4);
    semantic_state.add_module(module, module_path)?;
    semantic_state.build()
}

fn build_type(module: &M, type_path: &IP) -> anyhow::Result<SID> {
    build_state(
        module,
        &type_path.parent().context("failed to get path parent")?,
    )?
    .type_registry()
    .get(type_path)
    .cloned()
    .context("failed to get type")
}

fn unknown(size: usize) -> ST {
    ST::raw("u8").array(size)
}

#[test]
fn can_resolve_basic_struct() {
    let module = M::new().with_definitions([ID::new(
        "TestType",
        TD::new([
            TS::field("field_1", T::ident("i32")),
            TS::field("_", T::unknown(4)),
            TS::field("field_2", T::ident("u64")),
        ]),
    )]);

    let path = IP::from("test::TestType");
    let type_definition = SID {
        path: path.clone(),
        state: SIS::Resolved(SISR {
            size: 16,
            inner: STD::new()
                .with_regions([
                    SR::field("field_1", ST::raw("i32")),
                    SR::field("_field_4", unknown(4)),
                    SR::field("field_2", ST::raw("u64")),
                ])
                .into(),
        }),
        category: SIC::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
}

#[test]
fn can_resolve_pointer_to_another_struct() {
    let module = M::new().with_definitions([
        ID::new(
            "TestType1",
            TD::new([TS::field("field_1", T::ident("u64"))]),
        ),
        ID::new(
            "TestType2",
            TD::new([
                TS::field("field_1", T::ident("i32")),
                TS::field("field_2", T::ident("TestType1")),
                TS::field("field_3", T::ident("TestType1").const_pointer()),
                TS::field("field_4", T::ident("TestType1").mut_pointer()),
            ]),
        ),
    ]);

    let path = IP::from("test::TestType2");
    let type_definition = SID {
        path: path.clone(),
        state: SIS::Resolved(SISR {
            size: 20,
            inner: STD::new()
                .with_regions([
                    SR::field("field_1", ST::raw("i32")),
                    SR::field("field_2", ST::raw("test::TestType1")),
                    SR::field("field_3", ST::raw("test::TestType1").const_pointer()),
                    SR::field("field_4", ST::raw("test::TestType1").mut_pointer()),
                ])
                .into(),
        }),
        category: SIC::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
}

#[test]
fn can_resolve_complex_type() {
    let module = M::new()
        .with_definitions([
            ID::new(
                "TestType",
                TD::new([
                    TS::field("field_1", T::ident("i32")),
                    TS::field("_", T::unknown(4)),
                ]),
            ),
            ID::new(
                "Singleton",
                TD::new([
                    TS::field("max_num_1", T::ident("u16")).with_attributes([A::address(0x78)]),
                    TS::field("max_num_2", T::ident("u16")),
                    TS::field("test_type", T::ident("TestType"))
                        .with_attributes([A::address(0xA00)]),
                    TS::field("settings", T::unknown(804)),
                ])
                .with_attributes([A::size(0x1750), A::singleton(0x1_200_000)]),
            ),
        ])
        .with_impls([FB::new(
            "Singleton",
            [F::new(
                "test_function",
                [
                    Ar::MutSelf,
                    Ar::field("arg1", T::ident("TestType").mut_pointer()),
                    Ar::field("arg2", T::ident("i32")),
                    Ar::field("arg3", T::ident("u32").const_pointer()),
                ],
            )
            .with_attributes([A::address(0x800_000)])
            .with_return_type(T::ident("TestType").mut_pointer())],
        )]);

    let path = IP::from("test::Singleton");
    let type_definition = SID {
        path: path.clone(),
        state: SIS::Resolved(SISR {
            size: 0x1750,
            inner: STD::new()
                .with_regions([
                    SR::field("_field_0", unknown(0x78)),
                    SR::field("max_num_1", ST::raw("u16")),
                    SR::field("max_num_2", ST::raw("u16")),
                    SR::field("_field_7c", unknown(0x984)),
                    SR::field("test_type", ST::raw("test::TestType")),
                    SR::field("settings", unknown(804)),
                    SR::field("_field_d2c", unknown(0xA24)),
                ])
                .with_free_functions([SF::new("test_function")
                    .with_address(0x800_000)
                    .with_arguments([
                        SAr::MutSelf,
                        SAr::field("arg1".to_string(), ST::raw("test::TestType").mut_pointer()),
                        SAr::field("arg2", ST::raw("i32")),
                        SAr::field("arg3", ST::raw("u32").const_pointer()),
                    ])
                    .with_return_type(ST::raw("test::TestType").mut_pointer())])
                .with_singleton(0x1_200_000)
                .into(),
        }),
        category: SIC::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
}

#[test]
fn will_eventually_terminate_with_an_unknown_type() {
    let module = M::new().with_definitions([ID::new(
        "TestType2",
        TD::new([TS::field("field_2", T::ident("TestType1"))]),
    )]);

    let path = IP::from("test::TestType2");
    assert_eq!(
        build_type(&module, &path).err().unwrap().to_string(),
        r#"type resolution will not terminate, failed on types: ["test::TestType2"] (resolved types: [])"#
    );
}

#[test]
fn can_use_type_from_another_module() {
    let module1 = M::new()
        .with_uses([IP::from("module2::TestType2")])
        .with_definitions([ID::new(
            "TestType1",
            TD::new([TS::field("field", T::ident("TestType2"))]),
        )]);
    let module2 = M::new().with_definitions([ID::new(
        "TestType2",
        TD::new([TS::field("field", T::ident("u32"))]),
    )]);

    let path = IP::from("module1::TestType1");
    let target_resolved_type = SID {
        path: path.clone(),
        state: SIS::Resolved(SISR {
            size: 4,
            inner: STD::new()
                .with_regions([SR::field("field", ST::raw("module2::TestType2"))])
                .into(),
        }),
        category: SIC::Defined,
    };

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();
    semantic_state
        .add_module(&module2, &IP::from("module2"))
        .unwrap();
    let semantic_state = semantic_state.build().unwrap();

    let resolved_type = semantic_state
        .type_registry()
        .get(&path)
        .cloned()
        .context("failed to get type")
        .unwrap();
    assert_eq!(resolved_type, target_resolved_type);
}

#[test]
fn will_fail_on_an_extern_without_size() {
    let module = M::new().with_extern_types([("TestType".into(), vec![])]);

    assert_eq!(
        build_type(&module, &IP::from("module"))
            .err()
            .unwrap()
            .to_string(),
        "failed to find size attribute for extern type"
    );
}

#[test]
fn can_resolve_embed_of_an_extern() {
    let module = M::new()
        .with_extern_types([("TestType1".into(), vec![A::size(16)])])
        .with_definitions([ID::new(
            "TestType2",
            TD::new([
                TS::field("field_1", T::ident("i32")),
                TS::field("field_2", T::ident("TestType1")),
                TS::field("field_3", T::ident("TestType1").const_pointer()),
                TS::field("field_4", T::ident("TestType1").mut_pointer()),
            ]),
        )]);

    let path = IP::from("test::TestType2");
    let type_definition = SID {
        path: path.clone(),
        state: SIS::Resolved(SISR {
            size: 28,
            inner: STD::new()
                .with_regions([
                    SR::field("field_1", ST::raw("i32")),
                    SR::field("field_2", ST::raw("test::TestType1")),
                    SR::field("field_3", ST::raw("test::TestType1").const_pointer()),
                    SR::field("field_4", ST::raw("test::TestType1").mut_pointer()),
                ])
                .into(),
        }),
        category: SIC::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
}

#[test]
fn will_fail_on_type_with_vfuncs_but_no_vftable() {
    let module = M::new()
        .with_definitions([ID::new("TestType", TD::new([]))])
        .with_vftable([FB::new(
            "TestType",
            [F::new(
                "test_function0",
                [
                    Ar::MutSelf,
                    Ar::Field(TF::new("arg0", T::ident("u32"))),
                    Ar::Field(TF::new("arg1", T::ident("f32"))),
                ],
            )
            .with_return_type("i32")],
        )]);

    let test_module_path = IP::from("test");
    assert_eq!(
        build_state(&module, &test_module_path)
            .unwrap_err()
            .to_string(),
        "type test::TestType has vftable functions but no vftable field"
    );
}

#[test]
fn can_generate_vftable() {
    let module = M::new()
        .with_definitions([ID::new("TestType", TD::new([TF::vftable().into()]))])
        .with_vftable([FB::new(
            "TestType",
            [
                F::new(
                    "test_function0",
                    [
                        Ar::MutSelf,
                        Ar::Field(TF::new("arg0", T::ident("u32"))),
                        Ar::Field(TF::new("arg1", T::ident("f32"))),
                    ],
                )
                .with_return_type("i32"),
                F::new(
                    "test_function1",
                    [
                        Ar::MutSelf,
                        Ar::Field(TF::new("arg0", T::ident("u32"))),
                        Ar::Field(TF::new("arg1", T::ident("f32"))),
                    ],
                ),
            ],
        )
        .with_attributes([A::size(4)])]);

    let type_definition = SID {
        path: IP::from("test::TestType"),
        state: SIS::Resolved(SISR {
            size: 4,
            inner: STD::new()
                .with_regions([SR::field(
                    "vftable",
                    ST::raw("test::TestTypeVftable").const_pointer(),
                )])
                .with_vftable_functions([
                    SF::new("test_function0")
                        .with_arguments([
                            SAr::MutSelf,
                            SAr::field("arg0", ST::raw("u32")),
                            SAr::field("arg1", ST::raw("f32")),
                        ])
                        .with_return_type(ST::raw("i32")),
                    SF::new("test_function1").with_arguments([
                        SAr::MutSelf,
                        SAr::field("arg0", ST::raw("u32")),
                        SAr::field("arg1", ST::raw("f32")),
                    ]),
                    make_vfunc(2),
                    make_vfunc(3),
                ])
                .into(),
        }),
        category: SIC::Defined,
    };
    let vftable_type_definition = SID {
        path: IP::from("test::TestTypeVftable"),
        state: SIS::Resolved(SISR {
            size: 0,
            inner: STD::new()
                .with_regions([
                    SR::field(
                        "test_function0",
                        ST::function(
                            [
                                ("this", ST::raw("test::TestType").mut_pointer()),
                                ("arg0", ST::raw("u32")),
                                ("arg1", ST::raw("f32")),
                            ],
                            ST::raw("i32"),
                        ),
                    ),
                    SR::field(
                        "test_function1",
                        ST::function(
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
                ])
                .into(),
        }),
        category: SIC::Defined,
    };

    let test_module_path = IP::from("test");
    let test_type_path = IP::from("test::TestType");
    let test_type_vftable_path = IP::from("test::TestTypeVftable");

    let state = build_state(&module, &test_module_path).unwrap();
    let type_registry = state.type_registry();
    let test_module = state.modules().get(&test_module_path).unwrap();

    assert_eq!(
        test_module.definition_paths(),
        &HashSet::from_iter([test_type_path.clone(), test_type_vftable_path.clone()])
    );

    assert_eq!(
        test_module
            .definitions(type_registry)
            .find(|td| td.path == test_type_path)
            .unwrap(),
        &type_definition
    );
    assert_eq!(
        test_module
            .definitions(type_registry)
            .find(|td| td.path == test_type_vftable_path)
            .unwrap(),
        &vftable_type_definition
    );
}

#[test]
fn can_generate_vftable_with_indices() {
    let module = M::new()
        .with_definitions([ID::new("TestType", TD::new([TF::vftable().into()]))])
        .with_vftable([FB::new(
            "TestType",
            [
                F::new(
                    "test_function0",
                    [
                        Ar::MutSelf,
                        Ar::Field(TF::new("arg0", T::ident("u32"))),
                        Ar::Field(TF::new("arg1", T::ident("f32"))),
                    ],
                )
                .with_attributes([A::index(2)])
                .with_return_type("i32"),
                F::new(
                    "test_function1",
                    [
                        Ar::MutSelf,
                        Ar::Field(TF::new("arg0", T::ident("u32"))),
                        Ar::Field(TF::new("arg1", T::ident("f32"))),
                    ],
                )
                .with_attributes([A::index(5)]),
            ],
        )
        .with_attributes([A::size(8)])]);

    let type_definition = SID {
        path: IP::from("test::TestType"),
        state: SIS::Resolved(SISR {
            size: 4,
            inner: STD::new()
                .with_regions([SR::field(
                    "vftable",
                    ST::raw("test::TestTypeVftable").const_pointer(),
                )])
                .with_vftable_functions([
                    make_vfunc(0),
                    make_vfunc(1),
                    SF::new("test_function0")
                        .with_arguments([
                            SAr::MutSelf,
                            SAr::field("arg0", ST::raw("u32")),
                            SAr::field("arg1", ST::raw("f32")),
                        ])
                        .with_return_type(ST::raw("i32")),
                    make_vfunc(3),
                    make_vfunc(4),
                    SF::new("test_function1").with_arguments([
                        SAr::MutSelf,
                        SAr::field("arg0", ST::raw("u32")),
                        SAr::field("arg1", ST::raw("f32")),
                    ]),
                    make_vfunc(6),
                    make_vfunc(7),
                ])
                .into(),
        }),
        category: SIC::Defined,
    };

    let vftable_type_definition = SID {
        path: IP::from("test::TestTypeVftable"),
        state: SIS::Resolved(SISR {
            size: 0,
            inner: STD::new()
                .with_regions([
                    make_vfunc_region(0),
                    make_vfunc_region(1),
                    SR::field(
                        "test_function0",
                        ST::function(
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
                        "test_function1",
                        ST::function(
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
                ])
                .into(),
        }),
        category: SIC::Defined,
    };

    let test_module_path = IP::from("test");
    let test_type_path = IP::from("test::TestType");
    let test_type_vftable_path = IP::from("test::TestTypeVftable");

    let state = build_state(&module, &test_module_path).unwrap();
    let type_registry = state.type_registry();
    let test_module = state.modules().get(&test_module_path).unwrap();

    assert_eq!(
        test_module.definition_paths(),
        &HashSet::from_iter([test_type_path.clone(), test_type_vftable_path.clone()])
    );

    assert_eq!(
        test_module
            .definitions(type_registry)
            .find(|td| td.path == test_type_path)
            .unwrap(),
        &type_definition
    );
    assert_eq!(
        test_module
            .definitions(type_registry)
            .find(|td| td.path == test_type_vftable_path)
            .unwrap(),
        &vftable_type_definition
    );
}

fn make_vfunc(index: usize) -> SF {
    SF::new(format!("_vfunc_{}", index)).with_arguments([SAr::MutSelf])
}

fn make_vfunc_region(index: usize) -> SR {
    SR::field(
        format!("_vfunc_{}", index),
        ST::function([("this", ST::raw("test::TestType").mut_pointer())], None),
    )
}

#[test]
fn can_define_extern_value() {
    let module1 = M::new().with_extern_values([(
        "test".into(),
        T::ident("u32").mut_pointer(),
        vec![A::address(0x1337)],
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
        &("test".into(), ST::raw("u32").mut_pointer(), 0x1337)
    );
}

#[test]
fn can_resolve_enum() {
    let module = M::new().with_definitions([ID::new(
        "TestType",
        ED::new(
            T::ident("u32"),
            [
                ES::field("Item1"),
                ES::field("Item2"),
                ES::field_with_expr("Item3", E::IntLiteral(10)),
                ES::field("Item4"),
            ],
            [A::singleton(0x1234)],
        ),
    )]);

    let path = IP::from("test::TestType");
    let type_definition = SID {
        path: path.clone(),
        state: SIS::Resolved(SISR {
            size: 4,
            inner: SED::new(ST::raw("u32"))
                .with_fields([("Item1", 0), ("Item2", 1), ("Item3", 10), ("Item4", 11)])
                .with_singleton(0x1234)
                .into(),
        }),
        category: SIC::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
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

    let ast = M::new().with_backends([B::new("rust")
        .with_prologue(prologue)
        .with_epilogue(epilogue)]);
    let test_path = IP::from("test");

    let state = build_state(&ast, &test_path).unwrap();
    let module = state.modules().get(&test_path).unwrap();

    assert_eq!(
        module.backends.get("rust").unwrap(),
        &SB {
            prologue: Some(prologue.to_string()),
            epilogue: Some(epilogue.to_string()),
        }
    );
}

#[test]
fn can_extract_copyable_and_cloneable_correctly() {
    let module = M::new().with_definitions([ID::new(
        "TestType",
        TD::new([TS::field("field_1", T::ident("i32"))]).with_attributes([A::cloneable()]),
    )]);

    let path = IP::from("test::TestType");
    let type_definition = SID {
        path: path.clone(),
        state: SIS::Resolved(SISR {
            size: 4,
            inner: STD::new()
                .with_regions([SR::field("field_1", ST::raw("i32"))])
                .with_cloneable(true)
                .into(),
        }),
        category: SIC::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);

    let module = M::new().with_definitions([ID::new(
        "TestType",
        TD::new([TS::field("field_1", T::ident("i32"))]).with_attributes([A::copyable()]),
    )]);

    let path = IP::from("test::TestType");
    let type_definition = SID {
        path: path.clone(),
        state: SIS::Resolved(SISR {
            size: 4,
            inner: STD::new()
                .with_regions([SR::field("field_1", ST::raw("i32"))])
                .with_copyable(true)
                .with_cloneable(true)
                .into(),
        }),
        category: SIC::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
}

#[test]
fn can_handle_defaultable_on_primitive_types() {
    let module = M::new().with_definitions([ID::new(
        "TestType",
        TD::new([
            TS::field("field_1", T::ident("i32")),
            TS::field("field_2", T::ident("f32").array(16)),
        ])
        .with_attributes([A::defaultable()]),
    )]);

    let path = IP::from("test::TestType");
    let type_definition = SID {
        path: path.clone(),
        state: SIS::Resolved(SISR {
            size: 68,
            inner: STD::new()
                .with_regions([
                    SR::field("field_1", ST::raw("i32")),
                    SR::field("field_2", ST::raw("f32").array(16)),
                ])
                .with_defaultable(true)
                .into(),
        }),
        category: SIC::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
}

#[test]
fn will_reject_defaultable_on_pointer() {
    let module = M::new().with_definitions([ID::new(
        "TestType",
        TD::new([TS::field("field_1", T::ident("i32").mut_pointer())])
            .with_attributes([A::defaultable()]),
    )]);

    let path = IP::from("test::TestType");
    assert_eq!(
        build_type(&module, &path).unwrap_err().to_string(),
        "field field_1 of type test::TestType is not a defaultable type (pointer or function?)"
    );
}

#[test]
fn will_reject_defaultable_on_enum_field() {
    let module = M::new().with_definitions([
        ID::new(
            "TestType",
            TD::new([TS::field("field_1", T::ident("TestEnum"))])
                .with_attributes([A::defaultable()]),
        ),
        ID::new(
            "TestEnum",
            ED::new(T::ident("u32"), [ES::field("Item1")], []),
        ),
    ]);

    let path = IP::from("test::TestType");
    assert_eq!(
        build_type(&module, &path).unwrap_err().to_string(),
        "field field_1 of type test::TestType is not a defaultable type (non-type?)"
    );
}

#[test]
fn will_reject_defaultable_on_non_defaultable_type() {
    let module = M::new().with_definitions([
        ID::new(
            "TestType",
            TD::new([TS::field("field_1", T::ident("TestNonDefaultable"))])
                .with_attributes([A::defaultable()]),
        ),
        ID::new("TestNonDefaultable", TD::new([])),
    ]);

    let path = IP::from("test::TestType");
    assert_eq!(
        build_type(&module, &path).unwrap_err().to_string(),
        "field field_1 of type test::TestType is not marked as defaultable"
    );
}
