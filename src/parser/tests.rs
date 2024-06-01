use crate::{grammar::test_aliases::*, parser::parse_str};

#[test]
fn can_parse_basic_struct() {
    let text = r#"
        type TestType {
            field_1: i32,
            field_2: i32,
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        "TestType",
        TD::new([
            TS::field("field_1", T::ident("i32")),
            TS::field("field_2", T::ident("i32")),
        ]),
    )]);

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_vftable() {
    let text = r#"
        type TestType;
        impl virtual TestType {
            fn test(&mut self, test2: i32);
        }
        "#;

    let ast = M::new()
        .with_definitions([ID::new("TestType", TD::new([]))])
        .with_vftables([FB::new(
            "TestType",
            [F::new(
                "test",
                [Ar::MutSelf, Ar::field("test2", T::ident("i32"))],
            )],
        )]);

    assert_eq!(parse_str(text).unwrap(), ast);
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
            _: unknown<0x10>,
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        "VehicleTypes",
        TD::new([
            TS::field("hash_edacd65b_likely_max_models", T::ident("i32")),
            TS::field("hash_2ff58884", T::ident("i32")),
            TS::field("maximum_gpu_cost", T::ident("i32")),
            TS::field("maximum_cpu_cost", T::ident("i32")),
            TS::field("field_10", T::ident("i32")),
            TS::field("accumulated_gpu_cost", T::ident("i32")),
            TS::field("accumulated_cpu_cost", T::ident("i32")),
            TS::field("field_1c", T::ident("i32")),
            TS::field("loaded_models", T::ident("LoadedModel").const_pointer()),
            TS::field("_", T::unknown(0x10)),
        ]),
    )]);

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_spawn_manager() {
    let text = r#"
        #[size(0x1754), singleton(0x1_191_918)]
        type SpawnManager {
            #[address(0x78)]
            max_num_characters: u16,
            max_num_vehicles: u16,

            #[address(0xA00)]
            world_sim: WorldSim,
            enemy_type_spawn_settings: unknown<804>,
            character_types: unknown<0x74>,
            vehicle_types: VehicleTypes,
        }
        impl SpawnManager {
            #[address(0x84C_4C0)]
            fn engine_spawn_vehicle(
                &mut self,
                vehicle: *mut SharedPtr<Vehicle>,
                context: i32,
                unk1: *mut StdString,
                model_id: *const u32,
                faction: u32,
                unk2: *mut StdString
            ) -> *mut SharedPtr<Vehicle>;

            #[address(0x73F_DB0)]
            fn request_vehicle_model(
                &mut self,
                model_id: *const u32,
                category: i32
            );
        }
        "#;

    let ast = M::new()
        .with_definitions([ID::new(
            "SpawnManager",
            TD::new([
                TS::field("max_num_characters", T::ident("u16"))
                    .with_attributes([A::address(0x78)]),
                TS::field("max_num_vehicles", T::ident("u16")),
                TS::field("world_sim", T::ident("WorldSim")).with_attributes([A::address(0xA00)]),
                TS::field("enemy_type_spawn_settings", T::unknown(804)),
                TS::field("character_types", T::unknown(0x74)),
                TS::field("vehicle_types", T::ident("VehicleTypes")),
            ])
            .with_attributes([A::size(0x1754), A::singleton(0x1_191_918)]),
        )])
        .with_impls([FB::new(
            "SpawnManager",
            [
                F::new(
                    "engine_spawn_vehicle",
                    [
                        Ar::MutSelf,
                        Ar::field("vehicle", T::ident("SharedPtr<Vehicle>").mut_pointer()),
                        Ar::field("context", T::ident("i32")),
                        Ar::field("unk1", T::ident("StdString").mut_pointer()),
                        Ar::field("model_id", T::ident("u32").const_pointer()),
                        Ar::field("faction", T::ident("u32")),
                        Ar::field("unk2", T::ident("StdString").mut_pointer()),
                    ],
                )
                .with_attributes([A::address(0x84C_4C0)])
                .with_return_type(T::ident("SharedPtr<Vehicle>").mut_pointer()),
                F::new(
                    "request_vehicle_model",
                    [
                        Ar::MutSelf,
                        Ar::field("model_id", T::ident("u32").const_pointer()),
                        Ar::field("category", T::ident("i32")),
                    ],
                )
                .with_attributes([A::address(0x73F_DB0)]),
            ],
        )]);

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_address_field() {
    let text = r#"
        type Test {
            #[address(0x78)] max_num_characters: u16,
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        "Test",
        TD::new([
            TS::field("max_num_characters", T::ident("u16")).with_attributes([A::address(0x78)])
        ]),
    )]);

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_use() {
    let text = r#"
        use hello::TestType<Hey>;
        type Test {
            test: TestType<Hey>,
        }
        "#;

    let ast = M::new()
        .with_uses([IP::from_colon_delimited_str("hello::TestType<Hey>")])
        .with_definitions([ID::new(
            "Test",
            TD::new([TS::field("test", T::ident("TestType<Hey>"))]),
        )]);

    assert_eq!(parse_str(text).unwrap(), ast);
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
        #[size(12)]
        extern type TestType<Hey>;
        type Test {
            test: TestType<Hey>,
        }
        "#;

    let ast = M::new()
        .with_extern_types([("TestType<Hey>".into(), vec![A::size(12)])])
        .with_definitions([ID::new(
            "Test",
            TD::new([TS::field("test", T::ident("TestType<Hey>"))]),
        )]);

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_an_empty_type() {
    let text = r#"
        type Test;
        "#;

    let ast = M::new().with_definitions([ID::new("Test", TD::new([]))]);
    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_extern_value() {
    let text = r#"
        #[size(4)]
        extern type SomeType;
        #[address(0x1337)]
        extern some_value: *mut SomeType;
        "#;

    let ast = M::new()
        .with_extern_types([("SomeType".into(), vec![A::size(4)])])
        .with_extern_values([(
            "some_value".into(),
            T::ident("SomeType").mut_pointer(),
            vec![A::address(0x1337)],
        )]);

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_enum() {
    let text = r#"
        #[singleton(0x1234)]
        enum TestType: u32 {
            Item1,
            Item2,
            Item3: 10,
            Item4
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
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

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_array_field() {
    let text = r#"
        type TestType {
            field_1: [i32; 4],
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        "TestType",
        TD::new([TS::field("field_1", T::ident("i32").array(4))]),
    )]);

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_backends() {
    let text = r##"
backend rust {
    prelude r#"
        use std::ffi::CString;
        use std::os::raw::c_char;
    "#;

    postlude r#"
        fn main() {
            println!("Hello, world!");
        }
    "#;
}
"##;

    let ast = M::new().with_backends([B::new("rust")
        .with_prelude(
            r#"
        use std::ffi::CString;
        use std::os::raw::c_char;
    "#
            .trim(),
        )
        .with_postlude(
            r#"
        fn main() {
            println!("Hello, world!");
        }
    "#
            .trim(),
        )]);

    assert_eq!(parse_str(text).unwrap(), ast);
}
