use crate::{
    grammar::{ModuleItem, StringFormat, test_aliases::*},
    parser::{ParseError, parse_str_for_tests},
    span::StripLocations,
};
use pretty_assertions::assert_eq;

#[test]
fn can_parse_use() {
    let text = r#"
        use hello::TestType<Hey>;
        type Test {
            test: TestType<Hey>,
        }
        "#;

    // Use paths preserve generic syntax in the path (as "TestType<Hey>")
    // Type references parse generics properly
    let ast = M::new()
        .with_uses([IP::from("hello::TestType<Hey>")])
        .with_definitions([ID::new(
            (V::Private, "Test"),
            TD::new([TS::field(
                (V::Private, "test"),
                T::generic("TestType", [T::ident("Hey")]),
            )]),
        )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_pub_use_reexport() {
    // `pub use` is an explicit re-export; plain `use` is private.
    let text = r#"
        pub use math::Vector3;
        use math::Matrix4;
        "#;

    let module = parse_str_for_tests(text).unwrap();
    let uses: Vec<_> = module
        .items
        .iter()
        .filter_map(|i| match i {
            ModuleItem::Use {
                tree, visibility, ..
            } => Some((tree.flatten(), *visibility)),
            _ => None,
        })
        .collect();
    assert_eq!(uses.len(), 2);
    assert_eq!(uses[0].0, vec![IP::from("math::Vector3")]);
    assert_eq!(uses[0].1, V::Public);
    assert_eq!(uses[1].0, vec![IP::from("math::Matrix4")]);
    assert_eq!(uses[1].1, V::Private);
}

#[test]
fn can_parse_pub_use_braced() {
    let text = r#"
        pub use math::{Matrix4, Vector3};
        "#;

    let ast = M::new().with_pub_use_trees([UT::group(
        "math",
        [UT::path("Matrix4"), UT::path("Vector3")],
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_braced_imports() {
    let text = r#"
        use math::{Matrix4, Vector3};
        type Test {
            matrix: Matrix4,
            vector: Vector3,
        }
        "#;

    let ast = M::new()
        .with_use_trees([UT::group(
            "math",
            [UT::path("Matrix4"), UT::path("Vector3")],
        )])
        .with_definitions([ID::new(
            (V::Private, "Test"),
            TD::new([
                TS::field((V::Private, "matrix"), T::ident("Matrix4")),
                TS::field((V::Private, "vector"), T::ident("Vector3")),
            ]),
        )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_braced_imports_with_generics() {
    let text = r#"
        use types::{SharedPtr<T>, Vec<u32>};
        "#;

    let ast = M::new().with_use_trees([UT::group(
        "types",
        [UT::path("SharedPtr<T>"), UT::path("Vec<u32>")],
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_braced_imports_single_item() {
    let text = r#"
        use math::{Matrix4};
        "#;

    let ast = M::new().with_use_trees([UT::group("math", [UT::path("Matrix4")])]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_braced_imports_with_trailing_comma() {
    let text = r#"
        use math::{Matrix4, Vector3,};
        "#;

    let ast = M::new().with_use_trees([UT::group(
        "math",
        [UT::path("Matrix4"), UT::path("Vector3")],
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_braced_imports_nested_path() {
    let text = r#"
        use graphics::math::{Matrix4, Vector3};
        "#;

    let ast = M::new().with_use_trees([UT::group(
        "graphics::math",
        [UT::path("Matrix4"), UT::path("Vector3")],
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_empty_braced_imports() {
    let text = r#"
        use math::{};
        "#;

    let ast = M::new().with_use_trees([UT::group("math", [])]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_nested_braced_imports() {
    let text = r#"
        use types::{math::{Vector3, Matrix4}, Game};
        "#;

    let ast = M::new().with_use_trees([UT::group(
        "types",
        [
            UT::group("math", [UT::path("Vector3"), UT::path("Matrix4")]),
            UT::path("Game"),
        ],
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_flatten_nested_braced_imports() {
    // Test that UseTree::flatten() works correctly for nested imports
    let tree = UT::group(
        "types",
        [
            UT::group("math", [UT::path("Vector3"), UT::path("Matrix4")]),
            UT::path("Game"),
        ],
    );

    let flattened = tree.flatten();
    assert_eq!(flattened.len(), 3);
    assert_eq!(flattened[0], IP::from("types::math::Vector3"));
    assert_eq!(flattened[1], IP::from("types::math::Matrix4"));
    assert_eq!(flattened[2], IP::from("types::Game"));
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

    // Extern type names preserve the generic syntax literally in the name
    // But type references now parse generics properly
    let ast = M::new()
        .with_extern_types([("TestType<Hey>".into(), As::from_iter([A::size_decimal(12)]))])
        .with_definitions([ID::new(
            (V::Private, "Test"),
            TD::new([TS::field(
                (V::Private, "test"),
                T::generic("TestType", [T::ident("Hey")]),
            )]),
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
        ModuleItem::ExternType {
            name,
            attributes,
            doc_comments,
            ..
        } => {
            assert_eq!(name.0, "ManuallyDrop<SharedPtr<u32>>");
            assert_eq!(attributes.0.len(), 2);
            assert_eq!(
                doc_comments,
                &[
                    " `ManuallyDrop<SharedPtr<u32>>` is used instead of `SharedPtr<u32>` to avoid",
                    " the `Drop` implementation of `SharedPtr<u32>` being called when the `RenderBlock`",
                    " is dropped. The destructor, which we call in `drop`, will decrement the refcount",
                    " for us.",
                ]
            );
        }
        _ => panic!("Expected ExternType"),
    }
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
        .with_extern_types([("SomeType".into(), As::from_iter([A::size_decimal(4)]))])
        .with_definitions([
            ID::new(
                (V::Public, "some_value"),
                EVD::new(T::ident("SomeType").mut_pointer()).with_attributes([A::address(0x1337)]),
            ),
            ID::new(
                (V::Private, "some_private_value"),
                EVD::new(T::ident("SomeType").mut_pointer()).with_attributes([A::address(0x1338)]),
            ),
        ]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_splices() {
    // Standalone splice statements: an ungated one (every backend) and
    // cfg-gated ones. Each is its own module item, in source order.
    let text = r##"
prologue r#"// licence header"#;

#[cfg(backend = "rust")]
prologue r#"
    use std::ffi::CString;
    use std::os::raw::c_char;
"#;

#[cfg(backend = "rust")]
epilogue r#"
    fn main() {
        println!("Hello, world!");
    }
"#;
"##;

    let ast = M::new().with_splices([
        SP::prologue("// licence header"),
        SP::prologue("\n    use std::ffi::CString;\n    use std::os::raw::c_char;\n")
            .cfg_backend("rust"),
        SP::epilogue("\n    fn main() {\n        println!(\"Hello, world!\");\n    }\n")
            .cfg_backend("rust"),
    ]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_cfg_gated_use() {
    // A leading `#[cfg(...)]` on a `use` parses into the use's attributes.
    let text = r#"
        #[cfg(backend = "cpp")]
        use types::math::Matrix4;
        "#;

    let module = parse_str_for_tests(text).unwrap();
    let uses: Vec<_> = module
        .items
        .iter()
        .filter_map(|i| match i {
            ModuleItem::Use {
                tree, attributes, ..
            } => Some((tree.flatten(), attributes.cfg())),
            _ => None,
        })
        .collect();
    assert_eq!(uses.len(), 1);
    assert_eq!(uses[0].0, vec![IP::from("types::math::Matrix4")]);
    assert!(uses[0].1.is_some(), "cfg-gated use should carry a cfg");
}

#[test]
fn can_parse_splice_with_definition_modifier() {
    // The `definition` modifier lands the splice in the cpp `.cpp` source
    // file rather than the `.hpp` header.
    let text = r##"
#[cfg(backend = "cpp")]
epilogue definition r#"
    bool Probe::read() const { return value; }
"#;
"##;

    let ast =
        M::new().with_splices([
            SP::epilogue("\n    bool Probe::read() const { return value; }\n")
                .definition()
                .cfg_backend("cpp"),
        ]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_splice_with_for_type() {
    // `for <Type>` tags a splice as belonging to that type's page rather
    // than the module page.
    let text = r##"
#[cfg(backend = "rust")]
epilogue for Widget r#"
    impl Widget { pub fn new() -> Widget { Widget { id: 0 } } }
"#;
"##;

    let ast = M::new().with_splices([SP::epilogue(
        "\n    impl Widget { pub fn new() -> Widget { Widget { id: 0 } } }\n",
    )
    .for_type("Widget")
    .cfg_backend("rust")]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_splice_for_type_with_definition_any_order() {
    // `definition` and `for` are independent modifiers — either order works.
    let text = r##"
#[cfg(backend = "cpp")]
epilogue definition for Probe r#"
    bool Probe::read() const { return value; }
"#;
#[cfg(backend = "cpp")]
epilogue for Probe definition r#"
    bool Probe::init() { value = 0; }
"#;
#[cfg(backend = "cpp")]
epilogue for Probe r#"
    bool Probe::is_ready() const { return value != 0; }
"#;
"##;

    let ast = M::new().with_splices([
        SP::epilogue("\n    bool Probe::read() const { return value; }\n")
            .definition()
            .for_type("Probe")
            .cfg_backend("cpp"),
        SP::epilogue("\n    bool Probe::init() { value = 0; }\n")
            .definition()
            .for_type("Probe")
            .cfg_backend("cpp"),
        SP::epilogue("\n    bool Probe::is_ready() const { return value != 0; }\n")
            .for_type("Probe")
            .cfg_backend("cpp"),
    ]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_ungated_splice_with_plain_string() {
    // A plain (non-raw) string literal splice, emitted for every backend.
    let text = r#"
prologue "
    use crate::shared_ptr::*;
    use std::mem::ManuallyDrop;
";
    "#;

    let ast = M::new().with_splices([SP::prologue(
        "\n    use crate::shared_ptr::*;\n    use std::mem::ManuallyDrop;\n",
    )
    .with_format(StringFormat::Regular)]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

// ========================================================================
// Use statement error tests
// ========================================================================

use crate::{span::ItemLocation, tokenizer::TokenKind};

#[test]
fn use_empty_path_parses_ok() {
    // Parser accepts `use;` - semantic layer catches it
    let text = r#"
        use;
        "#;
    // Empty use path parses OK, semantic validation would catch
    assert!(parse_str_for_tests(text).is_ok());
}

#[test]
fn use_missing_semicolon() {
    let text = r#"
        use foo::bar
        type Test {}
        "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::Semi],
            found: TokenKind::Type,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn use_braced_missing_closing_brace() {
    let text = r#"
        use foo::{bar, baz;
        "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::RBrace],
            found: TokenKind::Semi,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn use_braced_empty_parses_ok() {
    // Parser accepts `use foo::{};` - empty brace group
    let text = r#"
        use foo::{};
        "#;
    // Empty braced import parses OK
    assert!(parse_str_for_tests(text).is_ok());
}

// ========================================================================
// Extern type/value error tests
// ========================================================================

#[test]
fn extern_type_missing_name() {
    let text = r#"
        extern type;
        "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedIdentifier {
            found: TokenKind::Semi,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn extern_type_missing_semicolon() {
    let text = r#"
        extern type Foo
        type Bar {}
        "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::Semi],
            found: TokenKind::Type,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}
