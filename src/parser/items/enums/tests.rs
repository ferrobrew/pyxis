use crate::{
    grammar::test_aliases::{int_literal, *},
    parser::{error::ParseError, parse_str_for_tests},
    span::{ItemLocation, StripLocations},
    tokenizer::TokenKind,
};
use pretty_assertions::assert_eq;

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

// ========================================================================
// Enum error tests
// ========================================================================

#[test]
fn enum_missing_type_annotation() {
    let text = r#"
    pub enum State {
        Idle = 0,
    }
    "#;

    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::Colon],
            found: TokenKind::LBrace,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn enum_missing_name() {
    let text = r#"
    pub enum : u32 {
        Item = 0,
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedIdentifier {
            found: TokenKind::Colon,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn enum_missing_type() {
    let text = r#"
    pub enum State: {
        Item = 0,
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedType {
            found: TokenKind::LBrace,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn enum_missing_opening_brace() {
    let text = r#"
    pub enum State: u32
        Item = 0,
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::LBrace],
            found: TokenKind::Ident("Item".to_string()),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn enum_missing_closing_brace() {
    let text = r#"
    pub enum State: u32 {
        Item = 0,
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
fn enum_variant_invalid_expression() {
    let text = r#"
    pub enum State: u32 {
        Item = ,
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedExpression {
            found: TokenKind::Comma,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn empty_enum_is_valid() {
    let text = r#"
    enum Test: u32 {}
    "#;
    assert!(parse_str_for_tests(text).is_ok());
}
