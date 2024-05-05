use super::*;

#[test]
fn can_parse_basic_struct() {
    let text = r#"
        type TestType {
            field_1: i32,
            field_2: i32,
        }
        "#;

    let ast = {
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
                    TS::field("field_2", TR::ident_type("i32")),
                ]),
            )],
        )
    };

    assert_eq!(parse_str(text).ok(), Some(ast));
}

#[test]
fn can_parse_vehicle_types() {
    let text = r#"
        type VehicleTypes {
            hash_edacd65b_likely_max_models: i32,
            hash_2ff58884: i32,

            maximum_gpu_cost: i32,
            maximum_cpu_cost: i32,

            field_10: i32,

            accumulated_gpu_cost: i32,
            accumulated_cpu_cost: i32,

            field_1c: i32,
            loaded_models: *const LoadedModel,
            padding!(0x10),
        }
        "#;

    let ast = {
        type T = Type;
        type TS = TypeStatement;
        type TR = TypeRef;

        Module::new(
            &[],
            &[],
            &[],
            &[ItemDefinition::new(
                "VehicleTypes",
                TypeDefinition::new(&[
                    TS::field("hash_edacd65b_likely_max_models", TR::ident_type("i32")),
                    TS::field("hash_2ff58884", TR::ident_type("i32")),
                    TS::field("maximum_gpu_cost", TR::ident_type("i32")),
                    TS::field("maximum_cpu_cost", TR::ident_type("i32")),
                    TS::field("field_10", TR::ident_type("i32")),
                    TS::field("accumulated_gpu_cost", TR::ident_type("i32")),
                    TS::field("accumulated_cpu_cost", TR::ident_type("i32")),
                    TS::field("field_1c", TR::ident_type("i32")),
                    TS::field(
                        "loaded_models",
                        T::ident("LoadedModel").const_pointer().into(),
                    ),
                    TS::macro_("padding", &[Expr::IntLiteral(0x10)]),
                ]),
            )],
        )
    };

    assert_eq!(parse_str(text).ok(), Some(ast));
}

#[test]
fn can_parse_spawn_manager() {
    let text = r#"
        type SpawnManager {
            meta {
                size: 0x1754,
                singleton: 0x1_191_918,
            },

            address(0x78) {
                max_num_characters: u16,
                max_num_vehicles: u16,
            },

            address(0xA00) {
                world_sim: WorldSim,
                enemy_type_spawn_settings: unk!(804),
                character_types: unk!(0x74),
                vehicle_types: VehicleTypes,
            },

            functions {
                free {
                    #[address(0x84C_4C0)]
                    fn engine_spawn_vehicle(
                        &mut self,
                        vehicle: *mut SharedPtr<Vehicle>,
                        context: i32,
                        unk1: *mut StdString,
                        model_id: *const u32,
                        faction: u32,
                        unk2: *mut StdString
                    ) -> *mut SharedPtr<Vehicle>,

                    #[address(0x73F_DB0)]
                    fn request_vehicle_model(
                        &mut self,
                        model_id: *const u32,
                        category: i32
                    )
                }
            }
        }
        "#;

    let ast = {
        use Expr::*;

        type T = Type;
        type TS = TypeStatement;
        type TR = TypeRef;
        type A = Argument;

        Module::new(
            &[],
            &[],
            &[],
            &[ItemDefinition::new(
                "SpawnManager",
                TypeDefinition::new(&[
                    TS::meta(&[
                        ("size", IntLiteral(0x1754)),
                        ("singleton", IntLiteral(0x1_191_918)),
                    ]),
                    TS::address(
                        0x78,
                        &[
                            ("max_num_characters", TR::ident_type("u16")),
                            ("max_num_vehicles", TR::ident_type("u16")),
                        ],
                    ),
                    TS::address(
                        0xA00,
                        &[
                            ("world_sim", TR::ident_type("WorldSim")),
                            ("enemy_type_spawn_settings", MacroCall::unk(804).into()),
                            ("character_types", MacroCall::unk(0x74).into()),
                            ("vehicle_types", TR::ident_type("VehicleTypes")),
                        ],
                    ),
                    TS::functions(&[(
                        "free",
                        &[
                            Function::new(
                                "engine_spawn_vehicle",
                                &[Attribute::address(0x84C_4C0)],
                                &[
                                    A::MutSelf,
                                    A::field(
                                        "vehicle",
                                        T::ident("SharedPtr<Vehicle>").mut_pointer().into(),
                                    ),
                                    A::field("context", TR::ident_type("i32")),
                                    A::field("unk1", T::ident("StdString").mut_pointer().into()),
                                    A::field("model_id", T::ident("u32").const_pointer().into()),
                                    A::field("faction", TR::ident_type("u32")),
                                    A::field("unk2", T::ident("StdString").mut_pointer().into()),
                                ],
                                Some(Type::ident("SharedPtr<Vehicle>").mut_pointer()),
                            ),
                            Function::new(
                                "request_vehicle_model",
                                &[Attribute::address(0x73F_DB0)],
                                &[
                                    A::MutSelf,
                                    A::field("model_id", T::ident("u32").const_pointer().into()),
                                    A::field("category", TR::ident_type("i32")),
                                ],
                                None,
                            ),
                        ],
                    )]),
                ]),
            )],
        )
    };

    assert_eq!(parse_str(text).ok(), Some(ast));
}

#[test]
fn can_parse_address_field() {
    let text = r#"
        type Test {
            address(0x78) max_num_characters: u16,
        }
        "#;

    let ast = {
        Module::new(
            &[],
            &[],
            &[],
            &[ItemDefinition::new(
                "Test",
                TypeDefinition::new(&[TypeStatement::address(
                    0x78,
                    &[("max_num_characters", TypeRef::ident_type("u16"))],
                )]),
            )],
        )
    };

    assert_eq!(parse_str(text).ok(), Some(ast));
}

#[test]
fn can_parse_use() {
    let text = r#"
        use hello::TestType<Hey>;
        type Test {
            test: TestType<Hey>,
        }
        "#;

    let ast = {
        Module::new(
            &[ItemPath::from_colon_delimited_str("hello::TestType<Hey>")],
            &[],
            &[],
            &[ItemDefinition::new(
                "Test",
                TypeDefinition::new(&[TypeStatement::field(
                    "test",
                    TypeRef::ident_type("TestType<Hey>"),
                )]),
            )],
        )
    };

    assert_eq!(parse_str(text).ok(), Some(ast));
}

#[test]
fn will_die_on_super_for_now() {
    let text = r#"
        use super::TestType<Hey>;
        "#;

    assert_eq!(
        parse_str(text).err().unwrap().to_string(),
        "super not supported"
    );
}

#[test]
fn can_parse_extern() {
    let text = r#"
        extern type TestType<Hey> { size: 12 }
        type Test {
            test: TestType<Hey>,
        }
        "#;

    let ast = {
        Module::new(
            &[],
            &[(
                "TestType<Hey>".into(),
                vec![ExprField("size".into(), Expr::IntLiteral(12))],
            )],
            &[],
            &[ItemDefinition::new(
                "Test",
                TypeDefinition::new(&[TypeStatement::field(
                    "test",
                    TypeRef::ident_type("TestType<Hey>"),
                )]),
            )],
        )
    };

    assert_eq!(parse_str(text).ok(), Some(ast));
}

#[test]
fn can_parse_an_empty_type() {
    let text = r#"
        type Test;
        "#;

    let ast = Module::new(
        &[],
        &[],
        &[],
        &[ItemDefinition::new("Test", TypeDefinition::new(&[]))],
    );
    assert_eq!(parse_str(text).ok(), Some(ast));
}

#[test]
fn can_parse_extern_value() {
    let text = r#"
        extern type SomeType { size: 4 }
        extern some_value: *mut SomeType @ 0x1337;
        "#;

    let ast = {
        Module::new(
            &[],
            &[(
                "SomeType".into(),
                vec![ExprField("size".into(), Expr::IntLiteral(4))],
            )],
            &[(
                "some_value".into(),
                Type::ident("SomeType").mut_pointer(),
                0x1337,
            )],
            &[],
        )
    };

    assert_eq!(parse_str(text).ok(), Some(ast));
}

#[test]
fn can_parse_enum() {
    let text = r#"
        enum TestType: u32 {
            meta {
                singleton: 0x1234,
            },

            Item1,
            Item2,
            Item3: 10,
            Item4
        }
        "#;

    let ast = {
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

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_array_field() {
    let text = r#"
        type TestType {
            field_1: [i32; 4],
        }
        "#;

    let ast = {
        type TS = TypeStatement;

        Module::new(
            &[],
            &[],
            &[],
            &[ItemDefinition::new(
                "TestType",
                TypeDefinition::new(&[TS::field("field_1", Type::ident("i32").array(4).into())]),
            )],
        )
    };

    assert_eq!(parse_str(text).unwrap(), ast);
}
