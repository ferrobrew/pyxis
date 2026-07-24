use crate::{
    grammar::test_aliases::*,
    parser::{error::ParseError, parse_str_for_tests},
    span::{ItemLocation, StripLocations},
    tokenizer::TokenKind,
};
use pretty_assertions::assert_eq;

#[test]
fn can_parse_generic_type_definition_single_param() {
    // Generic type with single type parameter
    let text = r#"
    #[size(0x8)]
    pub type Shared<T> {
        pub ptr: *mut T,
    }
    "#;

    let ast = M::new().with_definitions([ID::generic(
        (V::Public, "Shared"),
        [TP::new("T")],
        TD::new([TS::field((V::Public, "ptr"), T::ident("T").mut_pointer())])
            .with_attributes([A::size(8)]),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_generic_type_definition_multiple_params() {
    // Generic type with multiple type parameters
    let text = r#"
    #[size(0x10)]
    pub type Map<K, V> {
        pub key: *mut K,
        pub value: *mut V,
    }
    "#;

    let ast = M::new().with_definitions([ID::generic(
        (V::Public, "Map"),
        [TP::new("K"), TP::new("V")],
        TD::new([
            TS::field((V::Public, "key"), T::ident("K").mut_pointer()),
            TS::field((V::Public, "value"), T::ident("V").mut_pointer()),
        ])
        .with_attributes([A::size(16)]),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_field_with_generic_type_reference() {
    // Type with a field that uses a generic type
    let text = r#"
    #[size(0x8)]
    pub type Container {
        pub shared: Shared<GameObject>,
    }
    "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Public, "Container"),
        TD::new([TS::field(
            (V::Public, "shared"),
            T::generic("Shared", [T::ident("GameObject")]),
        )])
        .with_attributes([A::size(8)]),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_field_with_nested_generic_type() {
    // Type with a nested generic type (e.g., Shared<Map<K, V>>)
    let text = r#"
    #[size(0x8)]
    pub type Container {
        pub shared_map: Shared<Map<u32, Entity>>,
    }
    "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Public, "Container"),
        TD::new([TS::field(
            (V::Public, "shared_map"),
            T::generic(
                "Shared",
                [T::generic("Map", [T::ident("u32"), T::ident("Entity")])],
            ),
        )])
        .with_attributes([A::size(8)]),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_pointer_to_generic_type() {
    // Pointer to a generic type
    let text = r#"
    #[size(0x8)]
    pub type Container {
        pub ptr: *mut Shared<GameObject>,
    }
    "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Public, "Container"),
        TD::new([TS::field(
            (V::Public, "ptr"),
            T::generic("Shared", [T::ident("GameObject")]).mut_pointer(),
        )])
        .with_attributes([A::size(8)]),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_array_of_generic_type() {
    // Array of generic type
    let text = r#"
    #[size(0x20)]
    pub type Container {
        pub items: [Shared<Entity>; 4],
    }
    "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Public, "Container"),
        TD::new([TS::field(
            (V::Public, "items"),
            T::generic("Shared", [T::ident("Entity")]).array(4),
        )])
        .with_attributes([A::size(32)]),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

// ========================================================================
// Type definition error tests
// ========================================================================

#[test]
fn type_missing_closing_brace() {
    let text = r#"
    pub type TestType {
        field1: i32
    "#;

    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedIdentifier {
            found: TokenKind::Eof,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn type_field_missing_type() {
    let text = r#"
    pub type TestType {
        field1:,
    }
    "#;

    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedType {
            found: TokenKind::Comma,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn type_missing_name() {
    let text = r#"
    pub type {
        field: i32,
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedIdentifier {
            found: TokenKind::LBrace,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn type_field_missing_colon() {
    let text = r#"
    type TestType {
        field i32,
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::Colon],
            found: TokenKind::Ident("i32".to_string()),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn type_multiple_vftables_parses_ok() {
    // Valid syntax but semantically wrong - parser should accept it
    let text = r#"
    type TestType {
        vftable {},
        vftable {},
    }
    "#;
    // Parses fine - semantic check would catch it
    assert!(parse_str_for_tests(text).is_ok());
}

#[test]
fn empty_type_body_is_valid() {
    let text = r#"
    type Test {}
    "#;
    assert!(parse_str_for_tests(text).is_ok());
}

#[test]
fn type_with_only_unknown_field() {
    let text = r#"
    type Test {
        _: unknown<16>,
    }
    "#;
    assert!(parse_str_for_tests(text).is_ok());
}

#[test]
fn generic_type_def_missing_closing_angle() {
    let text = r#"
    type Shared<T {
        field: *mut T,
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::Gt],
            found: TokenKind::LBrace,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn generic_type_def_empty_params_parses_ok() {
    // Empty type params parse OK - `Shared<>` is just non-generic
    let text = r#"
    type Shared<> {
        field: i32,
    }
    "#;
    assert!(parse_str_for_tests(text).is_ok());
}

// ========================================================================
// Vftable error tests
// ========================================================================

#[test]
fn vftable_missing_opening_brace() {
    let text = r#"
    type TestType {
        vftable
            pub fn test(&self);
        }
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::LBrace],
            found: TokenKind::Pub,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn vftable_missing_closing_brace() {
    let text = r#"
    type TestType {
        vftable {
            pub fn test(&self);
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    // Runs into EOF trying to parse after the unclosed vftable
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedIdentifier {
            found: TokenKind::Eof,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn vftable_function_missing_semicolon() {
    let text = r#"
    type TestType {
        vftable {
            pub fn test(&self)
        }
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::Semi],
            found: TokenKind::RBrace,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn vftable_functions_using_comma_instead_of_semicolon() {
    let text = r#"
    type TestType {
        vftable {
            pub fn test1(&self),
            pub fn test2(&self);
        }
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::Semi],
            found: TokenKind::Comma,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn vftable_functions_missing_separator_entirely() {
    let text = r#"
    type TestType {
        vftable {
            pub fn test1(&self)
            pub fn test2(&self);
        }
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::Semi],
            found: TokenKind::Pub,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn vftable_functions_missing_separator_private() {
    let text = r#"
    type TestType {
        vftable {
            fn test1(&self)
            fn test2(&self);
        }
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::Semi],
            found: TokenKind::Fn,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn vftable_function_missing_fn_keyword() {
    let text = r#"
    type TestType {
        vftable {
            pub test(&self);
        }
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::Fn],
            found: TokenKind::Ident("test".to_string()),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn vftable_function_missing_parentheses() {
    let text = r#"
    type TestType {
        vftable {
            pub fn test;
        }
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::LParen],
            found: TokenKind::Semi,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn vftable_function_missing_closing_paren() {
    let text = r#"
    type TestType {
        vftable {
            pub fn test(&self;
        }
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::RParen],
            found: TokenKind::Semi,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn vftable_function_missing_return_type_after_arrow() {
    let text = r#"
    type TestType {
        vftable {
            pub fn test(&self) ->;
        }
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedType {
            found: TokenKind::Semi,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn vftable_function_invalid_self_parameter() {
    let text = r#"
    type TestType {
        vftable {
            pub fn test(self);
        }
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedIdentifier {
            found: TokenKind::SelfValue,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn empty_vftable_is_valid() {
    let text = r#"
    type Test {
        vftable {},
    }
    "#;
    assert!(parse_str_for_tests(text).is_ok());
}

#[test]
fn module_level_opaque_type_requires_semicolon() {
    assert!(parse_str_for_tests("pub type Opaque").is_err());
    assert!(parse_str_for_tests("pub type Opaque,").is_err());
    assert!(parse_str_for_tests("pub type Opaque;").is_ok());
}
