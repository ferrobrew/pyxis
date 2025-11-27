use crate::{
    grammar::{
        AttributeItem, IntFormat, ItemDefinitionInner, ModuleItem, StringFormat, TypeDefItem,
        Visibility,
        test_aliases::{int_literal, int_literal_with_format, *},
    },
    parser::{ParseError, parse_str_for_tests},
    span::StripLocations,
    tokenizer::TokenKind,
};

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

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
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
            [Ar::mut_self(), Ar::named("test2", T::ident("i32"))],
        )])
        .with_attributes([A::size(4)])]),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
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

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
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
            .with_attributes([
                A::Function(
                    "size".into(),
                    vec![AttributeItem::Expr(int_literal_with_format(
                        0x1754,
                        IntFormat::Hex,
                    ))],
                ),
                A::singleton(0x1_191_918),
            ]),
        )])
        .with_impls([FB::new(
            "SpawnManager",
            [
                F::new(
                    (V::Public, "engine_spawn_vehicle"),
                    [
                        Ar::mut_self(),
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
                        Ar::mut_self(),
                        Ar::named("model_id", T::ident("u32").const_pointer()),
                        Ar::named("category", T::ident("i32")),
                    ],
                )
                .with_attributes([A::address(0x73F_DB0)]),
            ],
        )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
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

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
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

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn will_die_on_super_for_now() {
    let text = r#"
        use super::TestType<Hey>;
        "#;

    let error = parse_str_for_tests(text).err().unwrap();
    assert!(
        matches!(error, ParseError::SuperNotSupported { .. }),
        "Expected SuperNotSupported error, got: {error:?}"
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

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_extern_with_multiline_doc_comment() {
    let text = r#"
#[size(8), align(4)]
/// `ManuallyDrop<SharedPtr<u32>>` is used instead of `SharedPtr<u32>` to avoid
/// the `Drop` implementation of `SharedPtr<u32>` being called when the `RenderBlock`
/// is dropped. The destructor, which we call in `drop`, will decrement the refcount
/// for us.
extern type ManuallyDrop<SharedPtr<u32>>;
    "#;

    let module = parse_str_for_tests(text).unwrap().strip_locations();

    // Verify we have one extern type item
    assert_eq!(module.items.len(), 1);

    // Verify it's an ExternType with the correct attributes and doc comments
    match &module.items[0] {
        ModuleItem::ExternType(name, attrs, doc_comments, _location) => {
            assert_eq!(name.0, "ManuallyDrop<SharedPtr<u32>>");
            assert_eq!(attrs.0.len(), 2);
            assert_eq!(doc_comments.len(), 4); // 4 lines of doc comment
            assert!(doc_comments[0].contains("ManuallyDrop<SharedPtr<u32>>"));
            assert!(doc_comments[1].contains("Drop` implementation"));
            assert!(doc_comments[2].contains("dropped"));
            assert!(doc_comments[3].contains("for us"));
        }
        _ => panic!("Expected ExternType"),
    }
}

#[test]
fn can_parse_an_empty_type() {
    let text = r#"
        type Test;
        "#;

    let ast = M::new().with_definitions([ID::new((V::Private, "Test"), TD::new([]))]);
    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
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

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
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
                ES::field_with_expr("Item0", int_literal(-5)),
                ES::field("Item1").with_attributes([A::default()]),
                ES::field("Item2"),
                ES::field_with_expr("Item3", int_literal(10)),
                ES::field("Item4"),
            ],
            [A::singleton(0x1234)],
        ),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_bitflags() {
    let text = r#"
        #[singleton(0x1234)]
        pub bitflags TestType: u32 {
            #[default]
            Item1 = 0b0001,
            Item2 = 0b0010,
            Item3 = 0b0100,
            Item4 = 0b1000,
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Public, "TestType"),
        BFD::new(
            T::ident("u32"),
            [
                BFS::field("Item1", int_literal_with_format(1, IntFormat::Binary))
                    .with_attributes([A::default()]),
                BFS::field("Item2", int_literal_with_format(2, IntFormat::Binary)),
                BFS::field("Item3", int_literal_with_format(4, IntFormat::Binary)),
                BFS::field("Item4", int_literal_with_format(8, IntFormat::Binary)),
            ],
            [A::singleton(0x1234)],
        ),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
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

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
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
            .with_prologue_format(
                r#"
        use std::ffi::CString;
        use std::os::raw::c_char;
    "#,
                StringFormat::Raw,
            )
            .with_epilogue_format(
                r#"
        fn main() {
            println!("Hello, world!");
        }
    "#,
                StringFormat::Raw,
            ),
        B::new("rust").with_prologue_format(
            r#"
    use std::ffi::CString;
    use std::os::raw::c_char;
"#,
            StringFormat::Raw,
        ),
        B::new("rust").with_epilogue_format(
            r#"
    fn main() {
        println!("Hello, world!");
    }
"#,
            StringFormat::Raw,
        ),
    ]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_backend_with_multiline_prologue() {
    let text = r#"
backend rust prologue "
    use crate::shared_ptr::*;
    use std::mem::ManuallyDrop;
";
    "#;

    let ast = M::new().with_backends([B::new("rust")
        .with_prologue("\n    use crate::shared_ptr::*;\n    use std::mem::ManuallyDrop;\n")]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
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

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
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
                TS::vftable([F::new((V::Private, "test_vfunc"), [Ar::const_self()])
                    .with_doc_comments(vec![" My test vfunc!".to_string()])]),
                TS::field((V::Private, "field_1"), T::ident("i32"))
                    .with_doc_comments(vec![" This is a field doc comment".to_string()]),
            ]),
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
        ]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_freestanding_functions() {
    let text = r#"
        enum A: i32 {}
        impl A {
            #[address(0x123)]
            fn test();
        }

        #[address(0x456)]
        pub fn freestanding();

        #[address(0x789)]
        fn another_freestanding(arg1: i32) -> i32;
        "#;

    let ast = M::new()
        .with_definitions([ID::new((V::Private, "A"), ED::new(T::ident("i32"), [], []))])
        .with_impls([FB::new(
            "A",
            [F::new((V::Private, "test"), []).with_attributes([A::address(0x123)])],
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

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_multiple_attributes_with_underscored_literals() {
    let text = r#"
        #[singleton(0x1_18F_B64), size(0x40), align(16)] // 0x3C
        pub type InputDeviceManager {
            #[address(0x18)]
            pub enabled: bool,

            #[address(0x38)]
            pub in_focus: bool,
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Public, "InputDeviceManager"),
        TD::new([
            TS::field((V::Public, "enabled"), T::ident("bool")).with_attributes([A::address(0x18)]),
            TS::field((V::Public, "in_focus"), T::ident("bool"))
                .with_attributes([A::address(0x38)]),
        ])
        .with_attributes([
            A::singleton(0x118FB64),
            A::Function(
                "size".into(),
                vec![AttributeItem::Expr(int_literal_with_format(
                    0x40,
                    IntFormat::Hex,
                ))],
            ),
            A::align(16),
        ]),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_pfx_instance_with_vftable_and_impl() {
    let text = r#"
        #[size(0x10)]
        /// `CPfxInstance` in original game
        pub type PfxInstance {
            vftable {},
            pub instance: SharedPtr<PfxInstanceInterface>,
        }
        impl PfxInstance {
            #[address(0x6B7C40)]
            pub fn set_game_object(&mut self, game_object: *mut PfxGameObject);
        }
        "#;

    let ast = M::new()
        .with_definitions([ID::new(
            (V::Public, "PfxInstance"),
            TD::new([
                TS::vftable([]),
                TS::field(
                    (V::Public, "instance"),
                    T::ident("SharedPtr<PfxInstanceInterface>"),
                ),
            ])
            .with_attributes([A::Function(
                "size".into(),
                vec![AttributeItem::Expr(int_literal_with_format(
                    0x10,
                    IntFormat::Hex,
                ))],
            )]),
        )
        .with_doc_comments(vec![" `CPfxInstance` in original game".to_string()])])
        .with_impls([FB::new(
            "PfxInstance",
            [F::new(
                (V::Public, "set_game_object"),
                [
                    Ar::mut_self(),
                    Ar::named("game_object", T::ident("PfxGameObject").mut_pointer()),
                ],
            )
            .with_attributes([A::address(0x6B7C40)])],
        )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_raycast_result_with_pointers_and_arrays() {
    let text = r#"
        #[size(0x2C)]
        pub type RayCastResult {
            game_object: *mut u32,
            pub normal: Vector3,
            pub distance: f32,
            rigid_body: *mut u32,
            shape: *mut u32,
            unknown: [u32; 4],
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Public, "RayCastResult"),
        TD::new([
            TS::field((V::Private, "game_object"), T::ident("u32").mut_pointer()),
            TS::field((V::Public, "normal"), T::ident("Vector3")),
            TS::field((V::Public, "distance"), T::ident("f32")),
            TS::field((V::Private, "rigid_body"), T::ident("u32").mut_pointer()),
            TS::field((V::Private, "shape"), T::ident("u32").mut_pointer()),
            TS::field((V::Private, "unknown"), T::array(T::ident("u32"), 4)),
        ])
        .with_attributes([A::Function(
            "size".into(),
            vec![AttributeItem::Expr(int_literal_with_format(
                0x2C,
                IntFormat::Hex,
            ))],
        )]),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_type_with_comment_in_attribute() {
    let text = r#"
#[singleton(0x1_18F_C20), size(0x620 /* actually 0x61C */), align(16)]
pub type AnarkGui {
    vftable {},

    #[address(0x1A0)]
    pub next_state: AnarkState,
    pub active_state: AnarkState,
}
    "#;

    let module = parse_str_for_tests(text).unwrap().strip_locations();

    // Verify we have one type definition
    assert_eq!(module.items.len(), 1);

    // Verify it's the correct type with attributes and fields
    match &module.items[0] {
        ModuleItem::Definition(def) => {
            assert_eq!(def.name.0, "AnarkGui");
            assert_eq!(def.visibility, Visibility::Public);

            // Check the type has attributes
            if let ItemDefinitionInner::Type(td) = &def.inner {
                assert_eq!(td.attributes.0.len(), 3); // singleton, size, align

                // Verify we have vftable and two fields
                // vftable + 2 fields = 3 statements
                let statement_count = td
                    .items
                    .iter()
                    .filter(|item| matches!(item, TypeDefItem::Statement(_)))
                    .count();
                assert_eq!(statement_count, 3);
            } else {
                panic!("Expected Type definition");
            }
        }
        _ => panic!("Expected Definition"),
    }
}

#[test]
fn can_parse_doc_comments_after_attributes() {
    let text = r#"
#[size(8), align(4)]
extern type SharedPtr<PfxInstanceInterface>;

/// `IPfxInstance` in original game
pub type PfxInstanceInterface {
    vftable {}
}

#[size(0x10)]
/// `CPfxInstance` in original game
pub type PfxInstance {
    vftable {},
    pub instance: SharedPtr<PfxInstanceInterface>,
}
impl PfxInstance {
    #[address(0x6B7C40)]
    pub fn set_game_object(&mut self, game_object: *mut PfxGameObject);
}
    "#;

    // Don't use strip_locations() - it empties doc_comments and converts them to attributes
    let module = parse_str_for_tests(text).unwrap();

    // Check extern type has no doc comments (doc comes after, not before)
    if let ModuleItem::ExternType(_name, _attrs, doc_comments, _location) = &module.items[0] {
        assert_eq!(
            doc_comments.len(),
            0,
            "Extern type should have no doc comments"
        );
    } else {
        panic!("Expected ExternType");
    }

    // Check first type definition has doc comments
    if let ModuleItem::Definition(def) = &module.items[1] {
        assert_eq!(def.name.0, "PfxInstanceInterface");
        assert_eq!(
            def.doc_comments.len(),
            1,
            "PfxInstanceInterface should have 1 doc comment, got: {:?}",
            def.doc_comments
        );
        assert!(def.doc_comments[0].contains("IPfxInstance"));
    } else {
        panic!("Expected Definition for PfxInstanceInterface");
    }

    // Check second type definition has doc comments (after attributes)
    if let ModuleItem::Definition(def) = &module.items[2] {
        assert_eq!(def.name.0, "PfxInstance");
        assert_eq!(
            def.doc_comments.len(),
            1,
            "PfxInstance should have 1 doc comment, got: {:?}",
            def.doc_comments
        );
        assert!(def.doc_comments[0].contains("CPfxInstance"));
    } else {
        panic!("Expected Definition for PfxInstance");
    }
}

// ============================================================================
// Negative tests - parser should reject invalid syntax
// ============================================================================

#[test]
fn should_fail_on_missing_closing_brace() {
    let text = r#"
        pub type TestType {
            field1: i32
        "#;

    let err = parse_str_for_tests(text).unwrap_err();
    assert!(
        matches!(
            err,
            ParseError::ExpectedIdentifier {
                found: TokenKind::Eof,
                ..
            }
        ),
        "Expected ExpectedIdentifier with Eof, got: {err:?}"
    );
}

#[test]
fn should_fail_on_missing_field_type() {
    let text = r#"
        pub type TestType {
            field1:,
        }
        "#;

    let err = parse_str_for_tests(text).unwrap_err();
    assert!(
        matches!(
            err,
            ParseError::ExpectedType {
                found: TokenKind::Comma,
                ..
            }
        ),
        "Expected ExpectedType with Comma, got: {err:?}"
    );
}

#[test]
fn should_fail_on_missing_enum_type_annotation() {
    let text = r#"
        pub enum State {
            Idle = 0,
        }
        "#;

    let err = parse_str_for_tests(text).unwrap_err();
    assert!(
        matches!(
            err,
            ParseError::ExpectedToken {
                expected: TokenKind::Colon,
                found: TokenKind::LBrace,
                ..
            }
        ),
        "Expected ExpectedToken with Colon/LBrace, got: {err:?}"
    );
}

#[test]
fn should_fail_on_missing_equals_in_bitflags() {
    let text = r#"
        pub bitflags Flags: u32 {
            READ 0x1,
        }
        "#;

    let err = parse_str_for_tests(text).unwrap_err();
    assert!(
        matches!(
            err,
            ParseError::ExpectedToken {
                expected: TokenKind::Eq,
                found: TokenKind::IntLiteral(_),
                ..
            }
        ),
        "Expected ExpectedToken with Eq/IntLiteral, got: {err:?}"
    );
}

#[test]
fn should_fail_on_malformed_pointer_type() {
    let text = r#"
        pub type TestType {
            field: *i32,
        }
        "#;

    let err = parse_str_for_tests(text).unwrap_err();
    assert!(
        matches!(err, ParseError::MissingPointerQualifier { .. }),
        "Expected MissingPointerQualifier error, got: {err:?}"
    );
}

#[test]
fn should_fail_on_missing_semicolon_after_extern() {
    let text = r#"
        extern type TestType
        "#;

    let err = parse_str_for_tests(text).unwrap_err();
    assert!(
        matches!(
            err,
            ParseError::ExpectedToken {
                expected: TokenKind::Semi,
                found: TokenKind::Eof,
                ..
            }
        ),
        "Expected ExpectedToken with Semi/Eof, got: {err:?}"
    );
}

#[test]
fn should_fail_on_incomplete_function() {
    let text = r#"
        pub fn test(
        "#;

    let err = parse_str_for_tests(text).unwrap_err();
    assert!(
        matches!(
            err,
            ParseError::ExpectedItemDefinition {
                found: TokenKind::Fn,
                ..
            }
        ),
        "Expected ExpectedItemDefinition with Fn, got: {err:?}"
    );
}
