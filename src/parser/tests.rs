use crate::{grammar::test_aliases::*, parser::parse_str};

use pretty_assertions::assert_eq;

#[test]
fn can_parse_basic_struct() {
    let text = r#"
        pub type TestType {
            field_1: i32,
            field_2: i32,
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Public, "TestType"),
        TD::new([
            TS::field((V::Private, "field_1"), T::ident("i32")),
            TS::field((V::Private, "field_2"), T::ident("i32")),
        ]),
    )]);

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_vftable() {
    let text = r#"
        type TestType {
            #[size(4)]
            vftable {
                pub fn test(&mut self, test2: i32);
            }
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Private, "TestType"),
        TD::new([TS::vftable([F::new(
            (V::Public, "test"),
            [Ar::MutSelf, Ar::named("test2", T::ident("i32"))],
        )])
        .with_attributes([A::size(4)])]),
    )]);

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_vehicle_types() {
    let text = r#"
        type VehicleTypes {
            hash_edacd65b_likely_max_models: i32,
            hash_2ff58884: i32,

            pub maximum_gpu_cost: i32,
            pub maximum_cpu_cost: i32,

            field_10: i32,

            pub accumulated_gpu_cost: i32,
            pub accumulated_cpu_cost: i32,

            field_1c: i32,
            loaded_models: *const LoadedModel,
            _: unknown<0x10>,
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Private, "VehicleTypes"),
        TD::new([
            TS::field(
                (V::Private, "hash_edacd65b_likely_max_models"),
                T::ident("i32"),
            ),
            TS::field((V::Private, "hash_2ff58884"), T::ident("i32")),
            TS::field((V::Public, "maximum_gpu_cost"), T::ident("i32")),
            TS::field((V::Public, "maximum_cpu_cost"), T::ident("i32")),
            TS::field((V::Private, "field_10"), T::ident("i32")),
            TS::field((V::Public, "accumulated_gpu_cost"), T::ident("i32")),
            TS::field((V::Public, "accumulated_cpu_cost"), T::ident("i32")),
            TS::field((V::Private, "field_1c"), T::ident("i32")),
            TS::field(
                (V::Private, "loaded_models"),
                T::ident("LoadedModel").const_pointer(),
            ),
            TS::field((V::Private, "_"), T::unknown(0x10)),
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
            pub max_num_characters: u16,
            pub max_num_vehicles: u16,

            #[address(0xA00)]
            pub world_sim: WorldSim,
            pub enemy_type_spawn_settings: unknown<804>,
            pub character_types: unknown<0x74>,
            pub vehicle_types: VehicleTypes,
        }
        impl SpawnManager {
            #[address(0x84C_4C0)]
            pub fn engine_spawn_vehicle(
                &mut self,
                vehicle: *mut SharedPtr<Vehicle>,
                context: i32,
                unk1: *mut StdString,
                model_id: *const u32,
                faction: u32,
                unk2: *mut StdString
            ) -> *mut SharedPtr<Vehicle>;

            #[address(0x73F_DB0)]
            pub fn request_vehicle_model(
                &mut self,
                model_id: *const u32,
                category: i32
            );
        }
        "#;

    let ast = M::new()
        .with_definitions([ID::new(
            (V::Private, "SpawnManager"),
            TD::new([
                TS::field((V::Public, "max_num_characters"), T::ident("u16"))
                    .with_attributes([A::address(0x78)]),
                TS::field((V::Public, "max_num_vehicles"), T::ident("u16")),
                TS::field((V::Public, "world_sim"), T::ident("WorldSim"))
                    .with_attributes([A::address(0xA00)]),
                TS::field((V::Public, "enemy_type_spawn_settings"), T::unknown(804)),
                TS::field((V::Public, "character_types"), T::unknown(0x74)),
                TS::field((V::Public, "vehicle_types"), T::ident("VehicleTypes")),
            ])
            .with_attributes([A::size(0x1754), A::singleton(0x1_191_918)]),
        )])
        .with_impls([FB::new(
            "SpawnManager",
            [
                F::new(
                    (V::Public, "engine_spawn_vehicle"),
                    [
                        Ar::MutSelf,
                        Ar::named("vehicle", T::ident("SharedPtr<Vehicle>").mut_pointer()),
                        Ar::named("context", T::ident("i32")),
                        Ar::named("unk1", T::ident("StdString").mut_pointer()),
                        Ar::named("model_id", T::ident("u32").const_pointer()),
                        Ar::named("faction", T::ident("u32")),
                        Ar::named("unk2", T::ident("StdString").mut_pointer()),
                    ],
                )
                .with_attributes([A::address(0x84C_4C0)])
                .with_return_type(T::ident("SharedPtr<Vehicle>").mut_pointer()),
                F::new(
                    (V::Public, "request_vehicle_model"),
                    [
                        Ar::MutSelf,
                        Ar::named("model_id", T::ident("u32").const_pointer()),
                        Ar::named("category", T::ident("i32")),
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
            #[address(0x78)]
            pub max_num_characters: u16,
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Private, "Test"),
        TD::new([
            TS::field((V::Public, "max_num_characters"), T::ident("u16"))
                .with_attributes([A::address(0x78)]),
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
        .with_uses([IP::from("hello::TestType<Hey>")])
        .with_definitions([ID::new(
            (V::Private, "Test"),
            TD::new([TS::field((V::Private, "test"), T::ident("TestType<Hey>"))]),
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
        .with_extern_types([("TestType<Hey>".into(), As::from_iter([A::size(12)]))])
        .with_definitions([ID::new(
            (V::Private, "Test"),
            TD::new([TS::field((V::Private, "test"), T::ident("TestType<Hey>"))]),
        )]);

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_an_empty_type() {
    let text = r#"
        type Test;
        "#;

    let ast = M::new().with_definitions([ID::new((V::Private, "Test"), TD::new([]))]);
    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_extern_value() {
    let text = r#"
        #[size(4)]
        extern type SomeType;
        #[address(0x1337)]
        pub extern some_value: *mut SomeType;
        #[address(0x1338)]
        extern some_private_value: *mut SomeType;
        "#;

    let ast = M::new()
        .with_extern_types([("SomeType".into(), As::from_iter([A::size(4)]))])
        .with_extern_values([
            EV::new(
                V::Public,
                "some_value",
                T::ident("SomeType").mut_pointer(),
                [A::address(0x1337)],
            ),
            EV::new(
                V::Private,
                "some_private_value",
                T::ident("SomeType").mut_pointer(),
                [A::address(0x1338)],
            ),
        ]);

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_enum() {
    let text = r#"
        #[singleton(0x1234)]
        pub enum TestType: u32 {
            Item0 = -5,
            #[default]
            Item1,
            Item2,
            Item3 = 10,
            Item4
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Public, "TestType"),
        ED::new(
            T::ident("u32"),
            [
                ES::field_with_expr("Item0", E::IntLiteral(-5)),
                ES::field("Item1").with_attributes([A::default()]),
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
        pub type TestType {
            field_1: [i32; 4],
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Public, "TestType"),
        TD::new([TS::field((V::Private, "field_1"), T::ident("i32").array(4))]),
    )]);

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_backends() {
    let text = r##"
backend rust {
    prologue r#"
        use std::ffi::CString;
        use std::os::raw::c_char;
    "#;

    epilogue r#"
        fn main() {
            println!("Hello, world!");
        }
    "#;
}


backend rust prologue r#"
    use std::ffi::CString;
    use std::os::raw::c_char;
"#;

backend rust epilogue r#"
    fn main() {
        println!("Hello, world!");
    }
"#;
"##;

    let ast = M::new().with_backends([
        B::new("rust")
            .with_prologue(
                r#"
        use std::ffi::CString;
        use std::os::raw::c_char;
    "#
                .trim(),
            )
            .with_epilogue(
                r#"
        fn main() {
            println!("Hello, world!");
        }
    "#
                .trim(),
            ),
        B::new("rust").with_prologue(
            r#"
    use std::ffi::CString;
    use std::os::raw::c_char;
"#
            .trim(),
        ),
        B::new("rust").with_epilogue(
            r#"
    fn main() {
        println!("Hello, world!");
    }
"#
            .trim(),
        ),
    ]);

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_ident_attributes() {
    let text = r#"
        #[copyable, cloneable]
        type TestType {
            field_1: i32,
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Private, "TestType"),
        TD::new([TS::field((V::Private, "field_1"), T::ident("i32"))])
            .with_attributes([A::copyable(), A::cloneable()]),
    )]);

    assert_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_doc_comments() {
    let text = r#"
        //! This is a module doc comment
        //! The best of its kind

        /// This is a doc comment
        type TestType {
            vftable {
                /// My test vfunc!
                fn test_vfunc(&self);
            },
            /// This is a field doc comment
            field_1: i32,
        }
        impl TestType {
            /// My test func!
            #[address(0x123)]
            fn test_func(&self);
        }
        "#;

    let ast = M::new()
        .with_definitions([ID::new(
            (V::Private, "TestType"),
            TD::new([
                TS::vftable([F::new((V::Private, "test_vfunc"), [Ar::ConstSelf])
                    .with_attributes([A::doc(" My test vfunc!")])]),
                TS::field((V::Private, "field_1"), T::ident("i32"))
                    .with_attributes([A::doc(" This is a field doc comment")]),
            ])
            .with_attributes([A::doc(" This is a doc comment")]),
        )])
        .with_impls([FB::new(
            "TestType",
            [F::new((V::Private, "test_func"), [Ar::ConstSelf])
                .with_attributes([A::doc(" My test func!"), A::address(0x123)])],
        )])
        .with_attributes([
            A::doc(" This is a module doc comment"),
            A::doc(" The best of its kind"),
        ]);

    assert_eq!(parse_str(text).unwrap(), ast);
}
