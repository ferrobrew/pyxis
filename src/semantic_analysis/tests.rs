use std::collections::{HashMap, HashSet};

use crate::{
    grammar::{self, ItemPath},
    semantic_analysis::{
        semantic_state::SemanticState,
        types::{ItemDefinition, *},
        *,
    },
};

use anyhow::Context;

fn build_state(
    module: &grammar::Module,
    path: &ItemPath,
) -> anyhow::Result<semantic_state::ResolvedSemanticState> {
    let mut semantic_state = semantic_state::SemanticState::new(4);
    semantic_state.add_module(module, &path.parent().context("failed to get path parent")?)?;
    semantic_state.build()
}

fn build_type(module: &grammar::Module, path: &ItemPath) -> anyhow::Result<ItemDefinition> {
    build_state(module, path)?
        .type_registry()
        .get(path)
        .cloned()
        .context("failed to get type")
}

#[test]
fn can_resolve_basic_struct() {
    let module = {
        use grammar::*;

        type TS = TypeStatement;
        type TR = TypeRef;

        Module::new(
            &[],
            &[],
            &[],
            &[ItemDefinition::new(
                "TestType",
                TypeDefinition::new(&[
                    TS::field("field_1", TR::ident_type("i32")),
                    TS::macro_("padding", &[Expr::IntLiteral(4)]),
                    TS::field("field_2", TR::ident_type("u64")),
                ]),
            )],
        )
    };

    let path = ItemPath::from_colon_delimited_str("test::TestType");
    let type_definition = ItemDefinition {
        path: path.clone(),
        state: ItemState::Resolved(ItemStateResolved {
            size: 16,
            inner: TypeDefinition {
                regions: vec![
                    Region::Field(
                        "field_1".into(),
                        Type::Raw(ItemPath::from_colon_delimited_str("i32")),
                    ),
                    Region::Padding(4),
                    Region::Field(
                        "field_2".into(),
                        Type::Raw(ItemPath::from_colon_delimited_str("u64")),
                    ),
                ],
                functions: HashMap::new(),
                metadata: HashMap::new(),
            }
            .into(),
        }),
        category: ItemCategory::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
}

#[test]
fn will_fail_on_unsupported_macro() {
    let module = {
        use grammar::*;

        Module::new(
            &[],
            &[],
            &[],
            &[ItemDefinition::new(
                "TestType",
                TypeDefinition::new(&[TypeStatement::macro_("foobar", &[Expr::IntLiteral(4)])]),
            )],
        )
    };

    let path = ItemPath::from_colon_delimited_str("test::TestType");
    assert_eq!(
        build_type(&module, &path).err().unwrap().to_string(),
        "unsupported macro: foobar"
    );
}

#[test]
fn can_resolve_pointer_to_another_struct() {
    let module = {
        use grammar::*;

        type TS = TypeStatement;
        type TR = TypeRef;

        Module::new(
            &[],
            &[],
            &[],
            &[
                ItemDefinition::new(
                    "TestType1",
                    TypeDefinition::new(&[TS::field("field_1", TR::ident_type("u64"))]),
                ),
                ItemDefinition::new(
                    "TestType2",
                    TypeDefinition::new(&[
                        TS::field("field_1", TR::ident_type("i32")),
                        TS::field("field_2", TR::ident_type("TestType1")),
                        TS::field(
                            "field_3",
                            TR::Type(Type::ident("TestType1").const_pointer()),
                        ),
                        TS::field("field_4", TR::Type(Type::ident("TestType1").mut_pointer())),
                    ]),
                ),
            ],
        )
    };

    let path = ItemPath::from_colon_delimited_str("test::TestType2");
    let type_definition = ItemDefinition {
        path: path.clone(),
        state: ItemState::Resolved(ItemStateResolved {
            size: 20,
            inner: TypeDefinition {
                regions: vec![
                    Region::Field(
                        "field_1".into(),
                        Type::Raw(ItemPath::from_colon_delimited_str("i32")),
                    ),
                    Region::Field(
                        "field_2".into(),
                        Type::Raw(ItemPath::from_colon_delimited_str("test::TestType1")),
                    ),
                    Region::Field(
                        "field_3".into(),
                        Type::ConstPointer(Box::new(Type::Raw(
                            ItemPath::from_colon_delimited_str("test::TestType1"),
                        ))),
                    ),
                    Region::Field(
                        "field_4".into(),
                        Type::MutPointer(Box::new(Type::Raw(ItemPath::from_colon_delimited_str(
                            "test::TestType1",
                        )))),
                    ),
                ],
                functions: HashMap::new(),
                metadata: HashMap::new(),
            }
            .into(),
        }),
        category: ItemCategory::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
}

#[test]
fn can_resolve_complex_type() {
    let module = {
        use grammar::*;

        type T = Type;
        type TS = TypeStatement;
        type TR = TypeRef;
        type A = Argument;

        Module::new(
            &[],
            &[],
            &[],
            &[
                ItemDefinition::new(
                    "TestType",
                    TypeDefinition::new(&[
                        TS::field("field_1", TR::ident_type("i32")),
                        TS::macro_("padding", &[Expr::IntLiteral(4)]),
                    ]),
                ),
                ItemDefinition::new(
                    "Singleton",
                    TypeDefinition::new(&[
                        TS::meta(&[
                            ("size", Expr::IntLiteral(0x1750)),
                            ("singleton", Expr::IntLiteral(0x1_200_000)),
                        ]),
                        TS::address(
                            0x78,
                            &[
                                ("max_num_1", TR::ident_type("u16")),
                                ("max_num_2", TR::ident_type("u16")),
                            ],
                        ),
                        TS::address(
                            0xA00,
                            &[
                                ("test_type", TR::ident_type("TestType")),
                                ("settings", MacroCall::unk(804).into()),
                            ],
                        ),
                        TS::functions(&[(
                            "free",
                            &[Function::new(
                                "test_function",
                                &[Attribute::address(0x800_000)],
                                &[
                                    A::MutSelf,
                                    A::field("arg1", T::ident("TestType").mut_pointer().into()),
                                    A::field("arg2", TR::ident_type("i32")),
                                    A::field("arg3", T::ident("u32").const_pointer().into()),
                                ],
                                Some(Type::ident("TestType").mut_pointer()),
                            )],
                        )]),
                    ]),
                ),
            ],
        )
    };

    let path = ItemPath::from_colon_delimited_str("test::Singleton");
    let type_definition = types::ItemDefinition {
        path: path.clone(),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 0x1750,
            inner: TypeDefinition {
                regions: vec![
                    Region::Padding(0x78),
                    Region::Field(
                        "max_num_1".into(),
                        Type::Raw(ItemPath::from_colon_delimited_str("u16")),
                    ),
                    Region::Field(
                        "max_num_2".into(),
                        Type::Raw(ItemPath::from_colon_delimited_str("u16")),
                    ),
                    Region::Padding(0x984),
                    Region::Field(
                        "test_type".into(),
                        Type::Raw(ItemPath::from_colon_delimited_str("test::TestType")),
                    ),
                    Region::Field(
                        "settings".into(),
                        Type::Array(
                            Box::new(Type::Raw(ItemPath::from_colon_delimited_str("u8"))),
                            804,
                        ),
                    ),
                    Region::Padding(0xA24),
                ],
                functions: [(
                    "free".to_string(),
                    vec![Function {
                        name: "test_function".to_string(),
                        attributes: vec![Attribute::Address(0x800_000)],
                        arguments: vec![
                            Argument::MutSelf,
                            Argument::Field(
                                "arg1".to_string(),
                                Type::MutPointer(Box::new(Type::Raw(
                                    ItemPath::from_colon_delimited_str("test::TestType"),
                                ))),
                            ),
                            Argument::Field(
                                "arg2".to_string(),
                                Type::Raw(ItemPath::from_colon_delimited_str("i32")),
                            ),
                            Argument::Field(
                                "arg3".to_string(),
                                Type::ConstPointer(Box::new(Type::Raw(
                                    ItemPath::from_colon_delimited_str("u32"),
                                ))),
                            ),
                        ],
                        return_type: Some(Type::MutPointer(Box::new(Type::Raw(
                            ItemPath::from_colon_delimited_str("test::TestType"),
                        )))),
                    }],
                )]
                .into_iter()
                .collect(),
                metadata: HashMap::from([(
                    "singleton".to_string(),
                    types::MetadataValue::Integer(0x1_200_000),
                )]),
            }
            .into(),
        }),
        category: types::ItemCategory::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
}

#[test]
fn will_eventually_terminate_with_an_unknown_type() {
    let module = {
        use grammar::*;

        type TS = TypeStatement;
        type TR = TypeRef;

        Module::new(
            &[],
            &[],
            &[],
            &[ItemDefinition::new(
                "TestType2",
                TypeDefinition::new(&[TS::field("field_2", TR::ident_type("TestType1"))]),
            )],
        )
    };

    let path = ItemPath::from_colon_delimited_str("test::TestType2");
    assert_eq!(
        build_type(&module, &path).err().unwrap().to_string(),
        r#"type resolution will not terminate, failed on types: ["test::TestType2"] (resolved types: [])"#
    );
}

#[test]
fn can_use_type_from_another_module() {
    let module1 = {
        use grammar::*;

        type TS = TypeStatement;
        type TR = TypeRef;

        Module::new(
            &[ItemPath::from_colon_delimited_str("module2::TestType2")],
            &[],
            &[],
            &[ItemDefinition::new(
                "TestType1",
                TypeDefinition::new(&[TS::field("field", TR::ident_type("TestType2"))]),
            )],
        )
    };
    let module2 = {
        use grammar::*;

        type TS = TypeStatement;
        type TR = TypeRef;

        Module::new(
            &[],
            &[],
            &[],
            &[ItemDefinition::new(
                "TestType2",
                TypeDefinition::new(&[TS::field("field", TR::ident_type("u32"))]),
            )],
        )
    };

    let path = ItemPath::from_colon_delimited_str("module1::TestType1");
    let target_resolved_type = types::ItemDefinition {
        path: path.clone(),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 4,
            inner: TypeDefinition {
                functions: HashMap::new(),
                regions: vec![Region::Field(
                    "field".into(),
                    Type::Raw(ItemPath::from_colon_delimited_str("module2::TestType2")),
                )],
                metadata: HashMap::new(),
            }
            .into(),
        }),
        category: types::ItemCategory::Defined,
    };

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &ItemPath::from_colon_delimited_str("module1"))
        .unwrap();
    semantic_state
        .add_module(&module2, &ItemPath::from_colon_delimited_str("module2"))
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
    let module = {
        use grammar::*;

        Module::new(&[], &[("TestType".into(), vec![])], &[], &[])
    };

    assert_eq!(
        build_type(&module, &ItemPath::from_colon_delimited_str("module"))
            .err()
            .unwrap()
            .to_string(),
        "failed to find size field in extern type for module"
    );
}

#[test]
fn can_resolve_embed_of_an_extern() {
    let module = {
        use grammar::*;

        type TS = TypeStatement;
        type TR = TypeRef;

        Module::new(
            &[],
            &[(
                "TestType1".into(),
                vec![ExprField("size".into(), Expr::IntLiteral(16))],
            )],
            &[],
            &[ItemDefinition::new(
                "TestType2",
                TypeDefinition::new(&[
                    TS::field("field_1", TR::ident_type("i32")),
                    TS::field("field_2", TR::ident_type("TestType1")),
                    TS::field(
                        "field_3",
                        TR::Type(Type::ident("TestType1").const_pointer()),
                    ),
                    TS::field("field_4", TR::Type(Type::ident("TestType1").mut_pointer())),
                ]),
            )],
        )
    };

    let path = ItemPath::from_colon_delimited_str("test::TestType2");
    let type_definition = types::ItemDefinition {
        path: path.clone(),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 28,
            inner: TypeDefinition {
                regions: vec![
                    Region::Field(
                        "field_1".into(),
                        Type::Raw(ItemPath::from_colon_delimited_str("i32")),
                    ),
                    Region::Field(
                        "field_2".into(),
                        Type::Raw(ItemPath::from_colon_delimited_str("test::TestType1")),
                    ),
                    Region::Field(
                        "field_3".into(),
                        Type::ConstPointer(Box::new(Type::Raw(
                            ItemPath::from_colon_delimited_str("test::TestType1"),
                        ))),
                    ),
                    Region::Field(
                        "field_4".into(),
                        Type::MutPointer(Box::new(Type::Raw(ItemPath::from_colon_delimited_str(
                            "test::TestType1",
                        )))),
                    ),
                ],
                functions: HashMap::new(),
                metadata: HashMap::new(),
            }
            .into(),
        }),
        category: types::ItemCategory::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
}

#[test]
fn can_generate_vftable() {
    let module = {
        use grammar::*;

        type TS = TypeStatement;
        type TR = TypeRef;

        Module::new(
            &[],
            &[],
            &[],
            &[ItemDefinition::new(
                "TestType",
                TypeDefinition::new(&[TS::Functions(vec![(
                    "vftable".into(),
                    vec![
                        Function {
                            name: "test_function0".into(),
                            attributes: vec![],
                            arguments: vec![
                                Argument::MutSelf,
                                Argument::Field(TypeField("arg0".into(), TR::ident_type("u32"))),
                                Argument::Field(TypeField("arg1".into(), TR::ident_type("f32"))),
                            ],
                            return_type: Some("i32".into()),
                        },
                        Function {
                            name: "test_function1".into(),
                            attributes: vec![],
                            arguments: vec![
                                Argument::MutSelf,
                                Argument::Field(TypeField("arg0".into(), TR::ident_type("u32"))),
                                Argument::Field(TypeField("arg1".into(), TR::ident_type("f32"))),
                            ],
                            return_type: None,
                        },
                    ],
                )])]),
            )],
        )
    };

    let type_definition = types::ItemDefinition {
        path: ItemPath::from_colon_delimited_str("test::TestType"),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 4,
            inner: TypeDefinition {
                regions: vec![Region::Field(
                    "vftable".to_string(),
                    Type::ConstPointer(Box::new(Type::Raw(ItemPath::from_colon_delimited_str(
                        "test::TestTypeVftable",
                    )))),
                )],
                functions: HashMap::from([(
                    "vftable".into(),
                    vec![
                        Function {
                            name: "test_function0".to_string(),
                            attributes: vec![],
                            arguments: vec![
                                Argument::MutSelf,
                                Argument::Field(
                                    "arg0".to_string(),
                                    Type::Raw(ItemPath::from_colon_delimited_str("u32")),
                                ),
                                Argument::Field(
                                    "arg1".to_string(),
                                    Type::Raw(ItemPath::from_colon_delimited_str("f32")),
                                ),
                            ],
                            return_type: Some(Type::Raw(ItemPath::from_colon_delimited_str("i32"))),
                        },
                        Function {
                            name: "test_function1".to_string(),
                            attributes: vec![],
                            arguments: vec![
                                Argument::MutSelf,
                                Argument::Field(
                                    "arg0".to_string(),
                                    Type::Raw(ItemPath::from_colon_delimited_str("u32")),
                                ),
                                Argument::Field(
                                    "arg1".to_string(),
                                    Type::Raw(ItemPath::from_colon_delimited_str("f32")),
                                ),
                            ],
                            return_type: None,
                        },
                    ],
                )]),
                metadata: HashMap::new(),
            }
            .into(),
        }),
        category: types::ItemCategory::Defined,
    };
    let vftable_type_definition = types::ItemDefinition {
        path: ItemPath::from_colon_delimited_str("test::TestTypeVftable"),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 0,
            inner: TypeDefinition {
                regions: vec![
                    Region::Field(
                        "test_function0".to_string(),
                        Type::Function(
                            vec![
                                (
                                    "this".to_string(),
                                    Box::new(Type::MutPointer(Box::new(Type::Raw(
                                        ItemPath::from_colon_delimited_str("test::TestType"),
                                    )))),
                                ),
                                (
                                    "arg0".to_string(),
                                    Box::new(Type::Raw(ItemPath::from_colon_delimited_str("u32"))),
                                ),
                                (
                                    "arg1".to_string(),
                                    Box::new(Type::Raw(ItemPath::from_colon_delimited_str("f32"))),
                                ),
                            ],
                            Some(Box::new(Type::Raw(ItemPath::from_colon_delimited_str(
                                "i32",
                            )))),
                        ),
                    ),
                    Region::Field(
                        "test_function1".to_string(),
                        Type::Function(
                            vec![
                                (
                                    "this".to_string(),
                                    Box::new(Type::MutPointer(Box::new(Type::Raw(
                                        ItemPath::from_colon_delimited_str("test::TestType"),
                                    )))),
                                ),
                                (
                                    "arg0".to_string(),
                                    Box::new(Type::Raw(ItemPath::from_colon_delimited_str("u32"))),
                                ),
                                (
                                    "arg1".to_string(),
                                    Box::new(Type::Raw(ItemPath::from_colon_delimited_str("f32"))),
                                ),
                            ],
                            None,
                        ),
                    ),
                ],
                functions: HashMap::new(),
                metadata: HashMap::new(),
            }
            .into(),
        }),
        category: types::ItemCategory::Defined,
    };

    let test_type_path = ItemPath::from_colon_delimited_str("test::TestType");
    let test_type_vftable_path = ItemPath::from_colon_delimited_str("test::TestTypeVftable");

    let build_state = build_state(&module, &test_type_path).unwrap();
    let type_registry = build_state.type_registry();

    let test_module = build_state
        .modules()
        .get(&ItemPath::from_colon_delimited_str("test"))
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
fn can_define_extern_value() {
    let module1 = {
        use grammar::*;

        Module::new(
            &[],
            &[],
            &[(
                "test".into(),
                Type::Ident("u32".into()).mut_pointer(),
                0x1337,
            )],
            &[],
        )
    };

    let mut semantic_state = semantic_state::SemanticState::new(4);
    semantic_state
        .add_module(&module1, &ItemPath::from_colon_delimited_str("module1"))
        .unwrap();
    let semantic_state = semantic_state.build().unwrap();

    let extern_value = semantic_state
        .modules()
        .get(&ItemPath::from_colon_delimited_str("module1"))
        .unwrap()
        .extern_values
        .first()
        .unwrap();

    assert_eq!(
        extern_value,
        &(
            "test".into(),
            Type::MutPointer(Box::new(Type::Raw(ItemPath::from_colon_delimited_str(
                "u32"
            )))),
            0x1337
        )
    );
}

#[test]
fn can_resolve_enum() {
    let module = {
        use grammar::*;

        type ES = EnumStatement;
        type TR = TypeRef;
        use Expr::IntLiteral;

        Module::new(
            &[],
            &[],
            &[],
            &[ItemDefinition::new(
                "TestType",
                EnumDefinition::new(
                    TR::ident_type("u32"),
                    &[
                        ES::meta(&[("singleton", IntLiteral(0x1234))]),
                        ES::field("Item1"),
                        ES::field("Item2"),
                        ES::field_with_expr("Item3", IntLiteral(10)),
                        ES::field("Item4"),
                    ],
                ),
            )],
        )
    };

    let path = ItemPath::from_colon_delimited_str("test::TestType");
    let type_definition = ItemDefinition {
        path: path.clone(),
        state: ItemState::Resolved(ItemStateResolved {
            size: 4,
            inner: EnumDefinition {
                ty: Type::Raw(ItemPath::from_colon_delimited_str("u32")),
                fields: vec![
                    ("Item1".to_string(), 0),
                    ("Item2".to_string(), 1),
                    ("Item3".to_string(), 10),
                    ("Item4".to_string(), 11),
                ],
                metadata: HashMap::from([(
                    "singleton".to_string(),
                    types::MetadataValue::Integer(0x1234),
                )]),
            }
            .into(),
        }),
        category: ItemCategory::Defined,
    };

    assert_eq!(build_type(&module, &path).unwrap(), type_definition);
}
