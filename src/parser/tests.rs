use crate::{assert_ast_eq, grammar::test_aliases::*, parser::parse_str};

/// Helper function to format parse errors with "test.pyxis" as the source name
fn format_test_errors(text: &str, errors: &[crate::parser::Rich<'_, char>]) -> String {
    crate::parser::format_parse_errors("test.pyxis", text, errors)
}

/// Helper macro to parse and compare AST with pretty error formatting on failure
macro_rules! assert_parse_eq {
    ($text:expr, $expected:expr) => {
        match parse_str($text) {
            Ok(parsed) => assert_ast_eq!(parsed, $expected),
            Err(errors) => {
                eprintln!("\n{}", format_test_errors($text, &errors));
                panic!("Parse failed - see formatted error above");
            }
        }
    };
}

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

    assert_ast_eq!(parse_str(text).unwrap(), ast);
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

    assert_parse_eq!(text, ast);
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

    assert_ast_eq!(parse_str(text).unwrap(), ast);
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

    assert_parse_eq!(text, ast);
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

    assert_ast_eq!(parse_str(text).unwrap(), ast);
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

    assert_ast_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn will_die_on_super_for_now() {
    let text = r#"
        use super::TestType<Hey>;
        "#;

    let errors = parse_str(text).err().unwrap();
    let error_str = format_test_errors(text, &errors);
    assert!(
        error_str.contains("super not supported"),
        "Expected 'super not supported' error, got:\n{}",
        error_str
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

    assert_ast_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_an_empty_type() {
    let text = r#"
        type Test;
        "#;

    let ast = M::new().with_definitions([ID::new((V::Private, "Test"), TD::new([]))]);
    assert_ast_eq!(parse_str(text).unwrap(), ast);
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

    assert_ast_eq!(parse_str(text).unwrap(), ast);
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

    assert_ast_eq!(parse_str(text).unwrap(), ast);
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
                BFS::field("Item1", E::IntLiteral(0b0001)).with_attributes([A::default()]),
                BFS::field("Item2", E::IntLiteral(0b0010)),
                BFS::field("Item3", E::IntLiteral(0b0100)),
                BFS::field("Item4", E::IntLiteral(0b1000)),
            ],
            [A::singleton(0x1234)],
        ),
    )]);

    assert_parse_eq!(text, ast);
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

    assert_ast_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn can_parse_backends() {
    let text = r#####"
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

backend cpp epilogue r####"
    // This has "# and "## and "### inside
    int main() {
        return 0;
    }
"####;
"#####;

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
        B::new("cpp").with_epilogue(
            r####"
    // This has "# and "## and "### inside
    int main() {
        return 0;
    }
"####
                .trim(),
        ),
    ]);

    assert_ast_eq!(parse_str(text).unwrap(), ast);
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

    assert_ast_eq!(parse_str(text).unwrap(), ast);
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
        .with_module_doc_comments([" This is a module doc comment", " The best of its kind"])
        .with_definitions([ID::new(
            (V::Private, "TestType"),
            TD::new([
                TS::vftable([F::new((V::Private, "test_vfunc"), [Ar::ConstSelf])
                    .with_doc_comments([" My test vfunc!"])]),
                TS::field((V::Private, "field_1"), T::ident("i32"))
                    .with_doc_comments([" This is a field doc comment"]),
            ]),
        )
        .with_doc_comments([" This is a doc comment"])])
        .with_impls([FB::new(
            "TestType",
            [F::new((V::Private, "test_func"), [Ar::ConstSelf])
                .with_doc_comments([" My test func!"])
                .with_attributes([A::address(0x123)])],
        )]);

    assert_ast_eq!(parse_str(text).unwrap(), ast);
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

    assert_ast_eq!(parse_str(text).unwrap(), ast);
}

#[test]
fn test_invalid_hex_literal() {
    // Hexadecimal literal that's too large for isize
    let text = r#"
        enum Test: i32 {
            Item = 0xFFFFFFFFFFFFFFFFFFFFFFFF,
        }
    "#;

    let errors = parse_str(text).err().unwrap();
    let error_str = format_test_errors(text, &errors);
    assert!(
        error_str.contains("Invalid hexadecimal literal"),
        "Expected hexadecimal overflow error, got:\n{}",
        error_str
    );
}

#[test]
fn test_invalid_binary_literal() {
    // Binary literal that's too large for isize
    let text = r#"
        enum Test: i32 {
            Item = 0b11111111111111111111111111111111111111111111111111111111111111111111111111111111,
        }
    "#;

    let errors = parse_str(text).err().unwrap();
    let error_str = format_test_errors(text, &errors);
    assert!(
        error_str.contains("Invalid binary literal"),
        "Expected binary overflow error, got:\n{}",
        error_str
    );
}

#[test]
fn test_invalid_decimal_literal() {
    // Decimal literal that's too large for isize
    let text = r#"
        enum Test: i32 {
            Item = 999999999999999999999999999999,
        }
    "#;

    let errors = parse_str(text).err().unwrap();
    let error_str = format_test_errors(text, &errors);
    assert!(
        error_str.contains("Invalid decimal literal"),
        "Expected decimal overflow error, got:\n{}",
        error_str
    );
}

#[test]
fn test_hex_literal_with_invalid_chars() {
    // Hexadecimal literal with invalid hex characters
    let text = r#"
        enum Test: i32 {
            Item = 0x12G4,
        }
    "#;

    let errors = parse_str(text).err().unwrap();
    let error_str = format_test_errors(text, &errors);
    // Should fail to parse because 'G' is not a valid hex digit
    assert!(
        error_str.contains("found 'G'") || error_str.contains("expected"),
        "Expected parse error for invalid hex character, got:\n{}",
        error_str
    );
}

#[test]
fn test_binary_literal_with_invalid_chars() {
    // Binary literal with invalid binary characters
    let text = r#"
        enum Test: i32 {
            Item = 0b1012,
        }
    "#;

    let errors = parse_str(text).err().unwrap();
    let error_str = format_test_errors(text, &errors);
    // Should fail to parse because '2' is not a valid binary digit
    assert!(
        error_str.contains("found '2'") || error_str.contains("expected"),
        "Expected parse error for invalid binary character, got:\n{}",
        error_str
    );
}

#[test]
fn test_hex_literal_empty() {
    // Hexadecimal prefix with no digits
    let text = r#"
        enum Test: i32 {
            Item = 0x,
        }
    "#;

    let errors = parse_str(text).err().unwrap();
    let error_str = format_test_errors(text, &errors);
    // Should fail because there are no hex digits after 0x
    assert!(
        error_str.contains("found ','") || error_str.contains("expected"),
        "Expected parse error for empty hex literal, got:\n{}",
        error_str
    );
}

#[test]
fn test_binary_literal_empty() {
    // Binary prefix with no digits
    let text = r#"
        enum Test: i32 {
            Item = 0b,
        }
    "#;

    let errors = parse_str(text).err().unwrap();
    let error_str = format_test_errors(text, &errors);
    // Should fail because there are no binary digits after 0b
    assert!(
        error_str.contains("found ','") || error_str.contains("expected"),
        "Expected parse error for empty binary literal, got:\n{}",
        error_str
    );
}

#[test]
fn can_parse_single_line_comments() {
    let text = r#"
        // This is a regular comment
        pub type TestType {
            // Comment before field
            field_1: i32, // Comment after field
            // Another comment
            field_2: i32,
        }
        "#;

    let ast = M::new()
        .with_comments([C::Line(" This is a regular comment".into())])
        .with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::with_children([
                TC::line_comment(" Comment before field"),
                TC::statement(TS::field((V::Private, "field_1"), T::ident("i32"))),
                TC::line_comment(" Comment after field"),
                TC::line_comment(" Another comment"),
                TC::statement(TS::field((V::Private, "field_2"), T::ident("i32"))),
            ]),
        )]);

    assert_parse_eq!(text, ast);
}

#[test]
fn can_parse_multi_line_comments() {
    let text = r#"
        /* This is a multi-line comment
           that spans multiple lines */
        pub type TestType {
            /* Comment before field */
            field_1: i32, /* Comment after field */
            /* Another
               multi-line
               comment */
            field_2: i32,
        }
        "#;

    let ast = M::new()
        .with_comments([C::Block(" This is a multi-line comment\n           that spans multiple lines ".into())])
        .with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::with_children([
                TC::block_comment(" Comment before field "),
                TC::statement(TS::field((V::Private, "field_1"), T::ident("i32"))),
                TC::block_comment(" Comment after field "),
                TC::block_comment(" Another\n               multi-line\n               comment "),
                TC::statement(TS::field((V::Private, "field_2"), T::ident("i32"))),
            ]),
        )]);

    assert_parse_eq!(text, ast);
}

#[test]
fn can_parse_mixed_comments() {
    let text = r#"
        // Single-line comment
        /* Multi-line comment */
        pub type TestType {
            // Single-line comment
            field_1: i32, /* inline multi-line */
            /* Multi-line
               comment */
            field_2: i32, // inline single-line
        }
        "#;

    let ast = M::new()
        .with_comments([
            C::Line(" Single-line comment".into()),
            C::Block(" Multi-line comment ".into()),
        ])
        .with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::with_children([
                TC::line_comment(" Single-line comment"),
                TC::statement(TS::field((V::Private, "field_1"), T::ident("i32"))),
                TC::block_comment(" inline multi-line "),
                TC::block_comment(" Multi-line\n               comment "),
                TC::statement(TS::field((V::Private, "field_2"), T::ident("i32"))),
                TC::line_comment(" inline single-line"),
            ]),
        )]);

    assert_parse_eq!(text, ast);
}

#[test]
fn comments_do_not_interfere_with_doc_comments() {
    let text = r#"
        // Regular comment
        /// Doc comment for type
        pub type TestType {
            // Regular comment
            /// Doc comment for field
            field_1: i32,
        }
        "#;

    let ast = M::new()
        .with_comments([C::Line(" Regular comment".into())])
        .with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::with_children([
                TC::line_comment(" Regular comment"),
                TC::statement(TS::field((V::Private, "field_1"), T::ident("i32"))
                    .with_doc_comments([" Doc comment for field"])),
            ]),
        )
        .with_doc_comments([" Doc comment for type"])]);

    assert_parse_eq!(text, ast);
}

#[test]
fn can_parse_comments_in_functions() {
    let text = r#"
        type TestType {
            vftable {
                // Comment before function
                fn test_func(
                    // Comment in args
                    arg1: i32, /* multi-line in args */
                    arg2: i32
                ) -> i32; /* comment after function signature */
            }
        }
        "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Private, "TestType"),
        TD::new([TS::vftable_with_children(VD::with_children([
            VC::line_comment(" Comment before function"),
            VC::function(
                F::new_with_children(
                    (V::Private, "test_func"),
                    [
                        AC::line_comment(" Comment in args"),
                        AC::argument(Ar::named("arg1", T::ident("i32"))),
                        AC::block_comment(" multi-line in args "),
                        AC::argument(Ar::named("arg2", T::ident("i32"))),
                    ],
                )
                .with_return_type(T::ident("i32")),
            ),
            VC::block_comment(" comment after function signature "),
        ]))]),
    )]);

    assert_parse_eq!(text, ast);
}

#[test]
fn can_parse_comments_with_special_chars() {
    let text = r#"
        // Comment with special chars: !@#$%^&*()
        /* Comment with slashes: // and more /* stuff */
        pub type TestType {
            field_1: i32,
        }
        "#;

    let ast = M::new()
        .with_comments([
            C::Line(" Comment with special chars: !@#$%^&*()".into()),
            C::Block(" Comment with slashes: // and more /* stuff ".into()),
        ])
        .with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Private, "field_1"), T::ident("i32"))]),
        )]);

    assert_parse_eq!(text, ast);
}
