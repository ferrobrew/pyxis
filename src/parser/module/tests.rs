use crate::{
    grammar::{ModuleItem, test_aliases::*},
    parser::parse_str_for_tests,
    span::StripLocations,
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
        .with_attributes([A::size_decimal(4)])]),
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
            .with_attributes([A::size(0x1754), A::singleton(0x1_191_918)]),
        )])
        .with_impls([FB::new(
            "SpawnManager",
            [
                F::new(
                    (V::Public, "engine_spawn_vehicle"),
                    [
                        Ar::mut_self(),
                        Ar::named(
                            "vehicle",
                            T::generic("SharedPtr", [T::ident("Vehicle")]).mut_pointer(),
                        ),
                        Ar::named("context", T::ident("i32")),
                        Ar::named("unk1", T::ident("StdString").mut_pointer()),
                        Ar::named("model_id", T::ident("u32").const_pointer()),
                        Ar::named("faction", T::ident("u32")),
                        Ar::named("unk2", T::ident("StdString").mut_pointer()),
                    ],
                )
                .with_attributes([A::address(0x84C_4C0)])
                .with_return_type(T::generic("SharedPtr", [T::ident("Vehicle")]).mut_pointer()),
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
fn can_parse_an_opaque_type() {
    let text = r#"
        type Test;
        "#;

    let ast = M::new().with_definitions([ID::new((V::Private, "Test"), TD::opaque())]);
    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_an_empty_braced_type() {
    // A braced empty body is distinct from an opaque type: it is
    // self-terminating and round-trips to `type Test {}`, not `type Test;`.
    let text = r#"
        type Test {
        }
        "#;

    let ast = M::new().with_definitions([ID::new((V::Private, "Test"), TD::new([]))]);
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
        .with_attributes([A::size(0x2C)]),
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
fn function_location_starts_at_declaration_not_attributes() {
    // The location used for documentation source links should point at the
    // `fn` declaration, not its leading doc comment / attributes.
    let text = "\n/// a doc comment\n#[address(0x10)]\nfn f();\n";
    let module = parse_str_for_tests(text).unwrap();
    let ModuleItem::Function { function } = &module.items[0] else {
        panic!("expected Function, got {:?}", module.items[0]);
    };
    // Line 2 is the doc comment, 3 the attribute, 4 the `fn f();` itself.
    assert_eq!(function.location.span.start.line, 4);
}

#[test]
fn can_parse_doc_comment_before_extern_value() {
    // Regression: a doc comment before a `pub extern` value used to route
    // the parser into `parse_item_definition`, which rejects `extern`.
    let text = r#"
/// This is a global value.
#[address(0x1000)]
pub extern g_thing: u32;

/// A private one too.
extern g_other: *mut u8;
        "#;

    // Don't strip locations - it empties doc_comments.
    let module = parse_str_for_tests(text).unwrap();

    let ModuleItem::Definition { definition } = &module.items[0] else {
        panic!("expected Definition, got {:?}", module.items[0]);
    };
    assert!(matches!(
        definition.inner,
        crate::grammar::ItemDefinitionInner::ExternValue(_)
    ));
    assert_eq!(definition.name.0, "g_thing");
    assert_eq!(definition.visibility, V::Public);
    assert_eq!(definition.doc_comments, vec![" This is a global value."]);

    let ModuleItem::Definition { definition } = &module.items[1] else {
        panic!("expected Definition, got {:?}", module.items[1]);
    };
    assert!(matches!(
        definition.inner,
        crate::grammar::ItemDefinitionInner::ExternValue(_)
    ));
    assert_eq!(definition.name.0, "g_other");
    assert_eq!(definition.visibility, V::Private);
    assert_eq!(definition.doc_comments, vec![" A private one too."]);
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
    if let ModuleItem::ExternType { doc_comments, .. } = &module.items[0] {
        assert_eq!(
            doc_comments.len(),
            0,
            "Extern type should have no doc comments"
        );
    } else {
        panic!("Expected ExternType");
    }

    // Check first type definition has doc comments
    if let ModuleItem::Definition { definition } = &module.items[1] {
        assert_eq!(definition.name.0, "PfxInstanceInterface");
        assert_eq!(
            definition.doc_comments,
            vec![" `IPfxInstance` in original game"]
        );
    } else {
        panic!("Expected Definition for PfxInstanceInterface");
    }

    // Check second type definition has doc comments (after attributes)
    if let ModuleItem::Definition { definition } = &module.items[2] {
        assert_eq!(definition.name.0, "PfxInstance");
        assert_eq!(
            definition.doc_comments,
            vec![" `CPfxInstance` in original game"]
        );
    } else {
        panic!("Expected Definition for PfxInstance");
    }
}

#[test]
fn can_parse_module_inner_attributes() {
    // Inner attributes (`#![...]`) parse generically; this exercises the
    // parsing/round-tripping machinery independent of any specific attribute.
    let text = r#"
        #![rust(example_flag)]

        pub type Foo {
            field: i32,
        }
        "#;

    let module = parse_str_for_tests(text).unwrap();
    let attrs: Vec<_> = module.inner_attributes().collect();
    assert_eq!(attrs.len(), 1);
    let (name, items) = attrs[0]
        .function()
        .expect("expected a function-form attribute");
    assert_eq!(name.as_str(), "rust");
    assert!(items.exprs().any(|e| matches!(
        e,
        crate::grammar::Expr::Ident { ident, .. } if ident.as_str() == "example_flag"
    )));
}

// ========================================================================
// Module-level error tests
// ========================================================================

use super::ParseError;
use crate::{span::ItemLocation, tokenizer::TokenKind};

#[test]
fn unexpected_token_at_module_level_errors() {
    let text = r#"
        123
        "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::UnexpectedModuleToken {
            found: TokenKind::IntLiteral("123".to_string()),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn unexpected_keyword_at_module_level_errors() {
    // `self` is a keyword that is not valid at module level.
    let text = r#"
        self
        "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::UnexpectedModuleToken {
            found: TokenKind::SelfValue,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn random_punctuation_at_module_level_errors() {
    let text = r#"
        ;
        "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::UnexpectedModuleToken {
            found: TokenKind::Semi,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}
