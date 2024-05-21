use std::collections::HashSet;

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

fn unknown(size: usize) -> Type {
    Type::Array(
        Box::new(Type::Raw(ItemPath::from_colon_delimited_str("u8"))),
        size,
    )
}

#[test]
fn can_resolve_basic_struct() {
    let module = {
        use grammar::*;

        type TS = TypeStatement;
        type T = Type;

        Module::new().with_definitions([ItemDefinition::new(
            "TestType",
            TypeDefinition::new(
                [
                    TS::field("field_1", T::ident("i32"), []),
                    TS::field("_", Type::unknown(4), []),
                    TS::field("field_2", T::ident("u64"), []),
                ],
                [],
            ),
        )])
    };

    let path = ItemPath::from_colon_delimited_str("test::TestType");
    let type_definition = ItemDefinition {
        path: path.clone(),
        state: ItemState::Resolved(ItemStateResolved {
            size: 16,
            inner: TypeDefinition {
                regions: vec![
                    Region::field(
                        "field_1",
                        Type::Raw(ItemPath::from_colon_delimited_str("i32")),
                    ),
                    Region::field("_field_4", unknown(4)),
                    Region::field(
                        "field_2",
                        Type::Raw(ItemPath::from_colon_delimited_str("u64")),
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
fn can_resolve_pointer_to_another_struct() {
    let module = {
        use grammar::*;

        type TS = TypeStatement;
        type T = Type;

        Module::new().with_definitions([
            ItemDefinition::new(
                "TestType1",
                TypeDefinition::new([TS::field("field_1", T::ident("u64"), [])], []),
            ),
            ItemDefinition::new(
                "TestType2",
                TypeDefinition::new(
                    [
                        TS::field("field_1", T::ident("i32"), []),
                        TS::field("field_2", T::ident("TestType1"), []),
                        TS::field("field_3", Type::ident("TestType1").const_pointer(), []),
                        TS::field("field_4", Type::ident("TestType1").mut_pointer(), []),
                    ],
                    [],
                ),
            ),
        ])
    };

    let path = ItemPath::from_colon_delimited_str("test::TestType2");
    let type_definition = ItemDefinition {
        path: path.clone(),
        state: ItemState::Resolved(ItemStateResolved {
            size: 20,
            inner: TypeDefinition {
                regions: vec![
                    Region::field(
                        "field_1",
                        Type::Raw(ItemPath::from_colon_delimited_str("i32")),
                    ),
                    Region::field(
                        "field_2",
                        Type::Raw(ItemPath::from_colon_delimited_str("test::TestType1")),
                    ),
                    Region::field(
                        "field_3",
                        Type::ConstPointer(Box::new(Type::Raw(
                            ItemPath::from_colon_delimited_str("test::TestType1"),
                        ))),
                    ),
                    Region::field(
                        "field_4",
                        Type::MutPointer(Box::new(Type::Raw(ItemPath::from_colon_delimited_str(
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
    let module = {
        use grammar::*;

        type T = Type;
        type TS = TypeStatement;
        type A = Argument;

        Module::new()
            .with_definitions([
                ItemDefinition::new(
                    "TestType",
                    TypeDefinition::new(
                        [
                            TS::field("field_1", T::ident("i32"), []),
                            TS::field("_", T::unknown(4), []),
                        ],
                        [],
                    ),
                ),
                ItemDefinition::new(
                    "Singleton",
                    TypeDefinition::new(
                        [
                            TS::field("max_num_1", T::ident("u16"), [Attribute::address(0x78)]),
                            TS::field("max_num_2", T::ident("u16"), []),
                            TS::field(
                                "test_type",
                                T::ident("TestType"),
                                [Attribute::address(0xA00)],
                            ),
                            TS::field("settings", T::unknown(804), []),
                        ],
                        [Attribute::size(0x1750), Attribute::singleton(0x1_200_000)],
                    ),
                ),
            ])
            .with_impls([FunctionBlock::new(
                Ident::from("Singleton"),
                [Function::new(
                    "test_function",
                    [Attribute::address(0x800_000)],
                    [
                        A::MutSelf,
                        A::field("arg1", T::ident("TestType").mut_pointer()),
                        A::field("arg2", T::ident("i32")),
                        A::field("arg3", T::ident("u32").const_pointer()),
                    ],
                    Some(T::ident("TestType").mut_pointer()),
                )],
                [],
            )])
    };

    let path = ItemPath::from_colon_delimited_str("test::Singleton");
    let type_definition = types::ItemDefinition {
        path: path.clone(),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 0x1750,
            inner: TypeDefinition {
                regions: vec![
                    Region::field("_field_0", unknown(0x78)),
                    Region::field(
                        "max_num_1",
                        Type::Raw(ItemPath::from_colon_delimited_str("u16")),
                    ),
                    Region::field(
                        "max_num_2",
                        Type::Raw(ItemPath::from_colon_delimited_str("u16")),
                    ),
                    Region::field("_field_7c", unknown(0x984)),
                    Region::field(
                        "test_type",
                        Type::Raw(ItemPath::from_colon_delimited_str("test::TestType")),
                    ),
                    Region::field("settings", unknown(804)),
                    Region::field("_field_d2c", unknown(0xA24)),
                ],
                free_functions: vec![Function {
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
    let module = {
        use grammar::*;

        type TS = TypeStatement;
        type T = Type;

        Module::new().with_definitions([ItemDefinition::new(
            "TestType2",
            TypeDefinition::new([TS::field("field_2", T::ident("TestType1"), [])], []),
        )])
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
        type T = Type;

        Module::new()
            .with_uses([ItemPath::from_colon_delimited_str("module2::TestType2")])
            .with_definitions([ItemDefinition::new(
                "TestType1",
                TypeDefinition::new([TS::field("field", T::ident("TestType2"), [])], []),
            )])
    };
    let module2 = {
        use grammar::*;

        type TS = TypeStatement;
        type T = Type;

        Module::new().with_definitions([ItemDefinition::new(
            "TestType2",
            TypeDefinition::new([TS::field("field", T::ident("u32"), [])], []),
        )])
    };

    let path = ItemPath::from_colon_delimited_str("module1::TestType1");
    let target_resolved_type = types::ItemDefinition {
        path: path.clone(),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 4,
            inner: TypeDefinition {
                free_functions: vec![],
                vftable_functions: None,
                regions: vec![Region::field(
                    "field",
                    Type::Raw(ItemPath::from_colon_delimited_str("module2::TestType2")),
                )],
                singleton: None,
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
    let module = grammar::Module::new().with_extern_types([("TestType".into(), vec![])]);

    assert_eq!(
        build_type(&module, &ItemPath::from_colon_delimited_str("module"))
            .err()
            .unwrap()
            .to_string(),
        "failed to find size attribute for extern type"
    );
}

#[test]
fn can_resolve_embed_of_an_extern() {
    let module = {
        use grammar::*;

        type TS = TypeStatement;
        type T = Type;

        Module::new()
            .with_extern_types([("TestType1".into(), vec![Attribute::size(16)])])
            .with_definitions([ItemDefinition::new(
                "TestType2",
                TypeDefinition::new(
                    [
                        TS::field("field_1", T::ident("i32"), []),
                        TS::field("field_2", T::ident("TestType1"), []),
                        TS::field("field_3", Type::ident("TestType1").const_pointer(), []),
                        TS::field("field_4", Type::ident("TestType1").mut_pointer(), []),
                    ],
                    [],
                ),
            )])
    };

    let path = ItemPath::from_colon_delimited_str("test::TestType2");
    let type_definition = types::ItemDefinition {
        path: path.clone(),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 28,
            inner: TypeDefinition {
                regions: vec![
                    Region::field(
                        "field_1",
                        Type::Raw(ItemPath::from_colon_delimited_str("i32")),
                    ),
                    Region::field(
                        "field_2",
                        Type::Raw(ItemPath::from_colon_delimited_str("test::TestType1")),
                    ),
                    Region::field(
                        "field_3",
                        Type::ConstPointer(Box::new(Type::Raw(
                            ItemPath::from_colon_delimited_str("test::TestType1"),
                        ))),
                    ),
                    Region::field(
                        "field_4",
                        Type::MutPointer(Box::new(Type::Raw(ItemPath::from_colon_delimited_str(
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
    let module = {
        use grammar::*;

        type T = Type;

        Module::new()
            .with_definitions([ItemDefinition::new("TestType", TypeDefinition::new([], []))])
            .with_vftables([FunctionBlock::new(
                Ident::from("TestType"),
                [
                    Function {
                        name: "test_function0".into(),
                        attributes: vec![],
                        arguments: vec![
                            Argument::MutSelf,
                            Argument::Field(TypeField("arg0".into(), T::ident("u32"))),
                            Argument::Field(TypeField("arg1".into(), T::ident("f32"))),
                        ],
                        return_type: Some("i32".into()),
                    },
                    Function {
                        name: "test_function1".into(),
                        attributes: vec![],
                        arguments: vec![
                            Argument::MutSelf,
                            Argument::Field(TypeField("arg0".into(), T::ident("u32"))),
                            Argument::Field(TypeField("arg1".into(), T::ident("f32"))),
                        ],
                        return_type: None,
                    },
                ],
                [Attribute::size(4)],
            )])
    };

    let type_definition = types::ItemDefinition {
        path: ItemPath::from_colon_delimited_str("test::TestType"),
        state: types::ItemState::Resolved(types::ItemStateResolved {
            size: 4,
            inner: TypeDefinition {
                regions: vec![Region::field(
                    "vftable",
                    Type::ConstPointer(Box::new(Type::Raw(ItemPath::from_colon_delimited_str(
                        "test::TestTypeVftable",
                    )))),
                )],
                vftable_functions: Some(vec![
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
                ]),
                free_functions: vec![],
                singleton: None,
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
                    Region::field(
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
                    Region::field(
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
                free_functions: vec![],
                vftable_functions: None,
                singleton: None,
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

        Module::new().with_extern_values([(
            "test".into(),
            Type::Ident("u32".into()).mut_pointer(),
            vec![grammar::Attribute::address(0x1337)],
        )])
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
        type T = Type;
        use Expr::IntLiteral;

        Module::new().with_definitions([ItemDefinition::new(
            "TestType",
            EnumDefinition::new(
                T::ident("u32"),
                [
                    ES::field("Item1"),
                    ES::field("Item2"),
                    ES::field_with_expr("Item3", IntLiteral(10)),
                    ES::field("Item4"),
                ],
                [Attribute::singleton(0x1234)],
            ),
        )])
    };

    let path = ItemPath::from_colon_delimited_str("test::TestType");
    let type_definition = ItemDefinition {
        path: path.clone(),
        state: ItemState::Resolved(ItemStateResolved {
            size: 4,
            inner: EnumDefinition {
                type_: Type::Raw(ItemPath::from_colon_delimited_str("u32")),
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
