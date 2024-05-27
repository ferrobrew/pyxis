use std::collections::HashSet;

use crate::{
    grammar::test_aliases::*,
    semantic_analysis::{
        semantic_state::SemanticState,
        types::{ItemDefinition, *},
        *,
    },
};

use anyhow::Context;

fn build_state(module: &M, path: &IP) -> anyhow::Result<semantic_state::ResolvedSemanticState> {
    let mut semantic_state = SemanticState::new(4);
    semantic_state.add_module(module, &path.parent().context("failed to get path parent")?)?;
    semantic_state.build()
}

fn build_type(module: &M, path: &IP) -> anyhow::Result<ItemDefinition> {
    build_state(module, path)?
        .type_registry()
        .get(path)
        .cloned()
        .context("failed to get type")
}

fn unknown(size: usize) -> Type {
    Type::Array(
        Box::new(Type::Raw(IP::from_colon_delimited_str("u8"))),
        size,
    )
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

    let path = IP::from_colon_delimited_str("test::TestType");
    let type_definition = ItemDefinition {
        path: path.clone(),
        state: ItemState::Resolved(ItemStateResolved {
            size: 16,
            inner: TypeDefinition {
                regions: vec![
                    Region::field("field_1", Type::Raw(IP::from_colon_delimited_str("i32"))),
                    Region::field("_field_4", unknown(4)),
                    Region::field("field_2", Type::Raw(IP::from_colon_delimited_str("u64"))),
                ],
                free_functions: vec![],
                vftable_functions: None,
                singleton: None,
            }
            .into(),
        }),
        category: ItemCategory::Defined,
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

    let path = IP::from_colon_delimited_str("test::TestType2");
    let type_definition = ItemDefinition {
        path: path.clone(),
        state: ItemState::Resolved(ItemStateResolved {
            size: 20,
            inner: TypeDefinition {
                regions: vec![
                    Region::field("field_1", Type::Raw(IP::from_colon_delimited_str("i32"))),
                    Region::field(
                        "field_2",
                        Type::Raw(IP::from_colon_delimited_str("test::TestType1")),
                    ),
                    Region::field(
                        "field_3",
                        Type::ConstPointer(Box::new(Type::Raw(IP::from_colon_delimited_str(
                            "test::TestType1",
                        )))),
                    ),
                    Region::field(
                        "field_4",
                        Type::MutPointer(Box::new(Type::Raw(IP::from_colon_delimited_str(
                            "test::TestType1",
                        )))),
                    ),
                ],
                free_functions: vec![],
                vftable_functions: None,
                singleton: None,
            }
            .into(),
        }),
        category: ItemCategory::Defined,
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

    let path = IP::from_colon_delimited_str("test::Singleton");
    let type_definition = types::ItemDefinition {
        path: path.clone(),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 0x1750,
            inner: TypeDefinition {
                regions: vec![
                    Region::field("_field_0", unknown(0x78)),
                    Region::field("max_num_1", Type::Raw(IP::from_colon_delimited_str("u16"))),
                    Region::field("max_num_2", Type::Raw(IP::from_colon_delimited_str("u16"))),
                    Region::field("_field_7c", unknown(0x984)),
                    Region::field(
                        "test_type",
                        Type::Raw(IP::from_colon_delimited_str("test::TestType")),
                    ),
                    Region::field("settings", unknown(804)),
                    Region::field("_field_d2c", unknown(0xA24)),
                ],
                free_functions: vec![Function {
                    name: "test_function".to_string(),
                    address: Some(0x800_000),
                    arguments: vec![
                        Argument::MutSelf,
                        Argument::Field(
                            "arg1".to_string(),
                            Type::MutPointer(Box::new(Type::Raw(IP::from_colon_delimited_str(
                                "test::TestType",
                            )))),
                        ),
                        Argument::Field(
                            "arg2".to_string(),
                            Type::Raw(IP::from_colon_delimited_str("i32")),
                        ),
                        Argument::Field(
                            "arg3".to_string(),
                            Type::ConstPointer(Box::new(Type::Raw(IP::from_colon_delimited_str(
                                "u32",
                            )))),
                        ),
                    ],
                    return_type: Some(Type::MutPointer(Box::new(Type::Raw(
                        IP::from_colon_delimited_str("test::TestType"),
                    )))),
                }],
                vftable_functions: None,
                singleton: Some(0x1_200_000),
            }
            .into(),
        }),
        category: types::ItemCategory::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
}

#[test]
fn will_eventually_terminate_with_an_unknown_type() {
    let module = M::new().with_definitions([ID::new(
        "TestType2",
        TD::new([TS::field("field_2", T::ident("TestType1"))]),
    )]);

    let path = IP::from_colon_delimited_str("test::TestType2");
    assert_eq!(
        build_type(&module, &path).err().unwrap().to_string(),
        r#"type resolution will not terminate, failed on types: ["test::TestType2"] (resolved types: [])"#
    );
}

#[test]
fn can_use_type_from_another_module() {
    let module1 = M::new()
        .with_uses([IP::from_colon_delimited_str("module2::TestType2")])
        .with_definitions([ID::new(
            "TestType1",
            TD::new([TS::field("field", T::ident("TestType2"))]),
        )]);
    let module2 = M::new().with_definitions([ID::new(
        "TestType2",
        TD::new([TS::field("field", T::ident("u32"))]),
    )]);

    let path = IP::from_colon_delimited_str("module1::TestType1");
    let target_resolved_type = types::ItemDefinition {
        path: path.clone(),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 4,
            inner: TypeDefinition {
                free_functions: vec![],
                vftable_functions: None,
                regions: vec![Region::field(
                    "field",
                    Type::Raw(IP::from_colon_delimited_str("module2::TestType2")),
                )],
                singleton: None,
            }
            .into(),
        }),
        category: types::ItemCategory::Defined,
    };

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from_colon_delimited_str("module1"))
        .unwrap();
    semantic_state
        .add_module(&module2, &IP::from_colon_delimited_str("module2"))
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
        build_type(&module, &IP::from_colon_delimited_str("module"))
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

    let path = IP::from_colon_delimited_str("test::TestType2");
    let type_definition = types::ItemDefinition {
        path: path.clone(),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 28,
            inner: TypeDefinition {
                regions: vec![
                    Region::field("field_1", Type::Raw(IP::from_colon_delimited_str("i32"))),
                    Region::field(
                        "field_2",
                        Type::Raw(IP::from_colon_delimited_str("test::TestType1")),
                    ),
                    Region::field(
                        "field_3",
                        Type::ConstPointer(Box::new(Type::Raw(IP::from_colon_delimited_str(
                            "test::TestType1",
                        )))),
                    ),
                    Region::field(
                        "field_4",
                        Type::MutPointer(Box::new(Type::Raw(IP::from_colon_delimited_str(
                            "test::TestType1",
                        )))),
                    ),
                ],
                free_functions: vec![],
                vftable_functions: None,
                singleton: None,
            }
            .into(),
        }),
        category: types::ItemCategory::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
}

#[test]
fn can_generate_vftable() {
    let module = M::new()
        .with_definitions([ID::new("TestType", TD::new([]))])
        .with_vftables([FB::new(
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

    let type_definition = types::ItemDefinition {
        path: IP::from_colon_delimited_str("test::TestType"),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 4,
            inner: TypeDefinition {
                regions: vec![Region::field(
                    "vftable",
                    Type::ConstPointer(Box::new(Type::Raw(IP::from_colon_delimited_str(
                        "test::TestTypeVftable",
                    )))),
                )],
                vftable_functions: Some(vec![
                    Function {
                        name: "test_function0".to_string(),
                        address: None,
                        arguments: vec![
                            Argument::MutSelf,
                            Argument::Field(
                                "arg0".to_string(),
                                Type::Raw(IP::from_colon_delimited_str("u32")),
                            ),
                            Argument::Field(
                                "arg1".to_string(),
                                Type::Raw(IP::from_colon_delimited_str("f32")),
                            ),
                        ],
                        return_type: Some(Type::Raw(IP::from_colon_delimited_str("i32"))),
                    },
                    Function {
                        name: "test_function1".to_string(),
                        address: None,
                        arguments: vec![
                            Argument::MutSelf,
                            Argument::Field(
                                "arg0".to_string(),
                                Type::Raw(IP::from_colon_delimited_str("u32")),
                            ),
                            Argument::Field(
                                "arg1".to_string(),
                                Type::Raw(IP::from_colon_delimited_str("f32")),
                            ),
                        ],
                        return_type: None,
                    },
                    make_vfunc(2),
                    make_vfunc(3),
                ]),
                free_functions: vec![],
                singleton: None,
            }
            .into(),
        }),
        category: types::ItemCategory::Defined,
    };
    let vftable_type_definition = types::ItemDefinition {
        path: IP::from_colon_delimited_str("test::TestTypeVftable"),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 0,
            inner: TypeDefinition {
                regions: vec![
                    Region::field(
                        "test_function0".to_string(),
                        Type::Function(
                            vec![
                                (
                                    "this".to_string(),
                                    Box::new(Type::MutPointer(Box::new(Type::Raw(
                                        IP::from_colon_delimited_str("test::TestType"),
                                    )))),
                                ),
                                (
                                    "arg0".to_string(),
                                    Box::new(Type::Raw(IP::from_colon_delimited_str("u32"))),
                                ),
                                (
                                    "arg1".to_string(),
                                    Box::new(Type::Raw(IP::from_colon_delimited_str("f32"))),
                                ),
                            ],
                            Some(Box::new(Type::Raw(IP::from_colon_delimited_str("i32")))),
                        ),
                    ),
                    Region::field(
                        "test_function1".to_string(),
                        Type::Function(
                            vec![
                                (
                                    "this".to_string(),
                                    Box::new(Type::MutPointer(Box::new(Type::Raw(
                                        IP::from_colon_delimited_str("test::TestType"),
                                    )))),
                                ),
                                (
                                    "arg0".to_string(),
                                    Box::new(Type::Raw(IP::from_colon_delimited_str("u32"))),
                                ),
                                (
                                    "arg1".to_string(),
                                    Box::new(Type::Raw(IP::from_colon_delimited_str("f32"))),
                                ),
                            ],
                            None,
                        ),
                    ),
                    make_vfunc_region(2),
                    make_vfunc_region(3),
                ],
                free_functions: vec![],
                vftable_functions: None,
                singleton: None,
            }
            .into(),
        }),
        category: types::ItemCategory::Defined,
    };

    let test_type_path = IP::from_colon_delimited_str("test::TestType");
    let test_type_vftable_path = IP::from_colon_delimited_str("test::TestTypeVftable");

    let build_state = build_state(&module, &test_type_path).unwrap();
    let type_registry = build_state.type_registry();

    let test_module = build_state
        .modules()
        .get(&IP::from_colon_delimited_str("test"))
        .unwrap();

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
        .with_definitions([ID::new("TestType", TD::new([]))])
        .with_vftables([FB::new(
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

    let type_definition = types::ItemDefinition {
        path: IP::from_colon_delimited_str("test::TestType"),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 4,
            inner: TypeDefinition {
                regions: vec![Region::field(
                    "vftable",
                    Type::ConstPointer(Box::new(Type::Raw(IP::from_colon_delimited_str(
                        "test::TestTypeVftable",
                    )))),
                )],
                vftable_functions: Some(vec![
                    make_vfunc(0),
                    make_vfunc(1),
                    Function {
                        name: "test_function0".to_string(),
                        address: None,
                        arguments: vec![
                            Argument::MutSelf,
                            Argument::Field(
                                "arg0".to_string(),
                                Type::Raw(IP::from_colon_delimited_str("u32")),
                            ),
                            Argument::Field(
                                "arg1".to_string(),
                                Type::Raw(IP::from_colon_delimited_str("f32")),
                            ),
                        ],
                        return_type: Some(Type::Raw(IP::from_colon_delimited_str("i32"))),
                    },
                    make_vfunc(3),
                    make_vfunc(4),
                    Function {
                        name: "test_function1".to_string(),
                        address: None,
                        arguments: vec![
                            Argument::MutSelf,
                            Argument::Field(
                                "arg0".to_string(),
                                Type::Raw(IP::from_colon_delimited_str("u32")),
                            ),
                            Argument::Field(
                                "arg1".to_string(),
                                Type::Raw(IP::from_colon_delimited_str("f32")),
                            ),
                        ],
                        return_type: None,
                    },
                    make_vfunc(6),
                    make_vfunc(7),
                ]),
                free_functions: vec![],
                singleton: None,
            }
            .into(),
        }),
        category: types::ItemCategory::Defined,
    };

    let vftable_type_definition = types::ItemDefinition {
        path: IP::from_colon_delimited_str("test::TestTypeVftable"),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 0,
            inner: TypeDefinition {
                regions: vec![
                    make_vfunc_region(0),
                    make_vfunc_region(1),
                    Region::field(
                        "test_function0".to_string(),
                        Type::Function(
                            vec![
                                (
                                    "this".to_string(),
                                    Box::new(Type::MutPointer(Box::new(Type::Raw(
                                        IP::from_colon_delimited_str("test::TestType"),
                                    )))),
                                ),
                                (
                                    "arg0".to_string(),
                                    Box::new(Type::Raw(IP::from_colon_delimited_str("u32"))),
                                ),
                                (
                                    "arg1".to_string(),
                                    Box::new(Type::Raw(IP::from_colon_delimited_str("f32"))),
                                ),
                            ],
                            Some(Box::new(Type::Raw(IP::from_colon_delimited_str("i32")))),
                        ),
                    ),
                    make_vfunc_region(3),
                    make_vfunc_region(4),
                    Region::field(
                        "test_function1".to_string(),
                        Type::Function(
                            vec![
                                (
                                    "this".to_string(),
                                    Box::new(Type::MutPointer(Box::new(Type::Raw(
                                        IP::from_colon_delimited_str("test::TestType"),
                                    )))),
                                ),
                                (
                                    "arg0".to_string(),
                                    Box::new(Type::Raw(IP::from_colon_delimited_str("u32"))),
                                ),
                                (
                                    "arg1".to_string(),
                                    Box::new(Type::Raw(IP::from_colon_delimited_str("f32"))),
                                ),
                            ],
                            None,
                        ),
                    ),
                    make_vfunc_region(6),
                    make_vfunc_region(7),
                ],
                free_functions: vec![],
                vftable_functions: None,
                singleton: None,
            }
            .into(),
        }),
        category: types::ItemCategory::Defined,
    };

    let test_type_path = IP::from_colon_delimited_str("test::TestType");
    let test_type_vftable_path = IP::from_colon_delimited_str("test::TestTypeVftable");

    let build_state = build_state(&module, &test_type_path).unwrap();
    let type_registry = build_state.type_registry();

    let test_module = build_state
        .modules()
        .get(&IP::from_colon_delimited_str("test"))
        .unwrap();

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

fn make_vfunc(index: usize) -> Function {
    Function {
        name: format!("_vfunc_{}", index),
        address: None,
        arguments: vec![Argument::MutSelf],
        return_type: None,
    }
}

fn make_vfunc_region(index: usize) -> Region {
    Region::field(
        format!("_vfunc_{}", index),
        Type::Function(
            vec![(
                "this".to_string(),
                Box::new(Type::MutPointer(Box::new(Type::Raw(
                    IP::from_colon_delimited_str("test::TestType"),
                )))),
            )],
            None,
        ),
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
        .add_module(&module1, &IP::from_colon_delimited_str("module1"))
        .unwrap();
    let semantic_state = semantic_state.build().unwrap();

    let extern_value = semantic_state
        .modules()
        .get(&IP::from_colon_delimited_str("module1"))
        .unwrap()
        .extern_values
        .first()
        .unwrap();

    assert_eq!(
        extern_value,
        &(
            "test".into(),
            Type::MutPointer(Box::new(Type::Raw(IP::from_colon_delimited_str("u32")))),
            0x1337
        )
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

    let path = IP::from_colon_delimited_str("test::TestType");
    let type_definition = ItemDefinition {
        path: path.clone(),
        state: ItemState::Resolved(ItemStateResolved {
            size: 4,
            inner: EnumDefinition {
                type_: Type::Raw(IP::from_colon_delimited_str("u32")),
                fields: vec![
                    ("Item1".to_string(), 0),
                    ("Item2".to_string(), 1),
                    ("Item3".to_string(), 10),
                    ("Item4".to_string(), 11),
                ],
                singleton: Some(0x1234),
            }
            .into(),
        }),
        category: ItemCategory::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
}
