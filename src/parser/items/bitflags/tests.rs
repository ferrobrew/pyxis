use crate::{
    grammar::{
        IntFormat,
        test_aliases::{int_literal_with_format, *},
    },
    parser::{error::ParseError, parse_str_for_tests},
    span::{ItemLocation, StripLocations},
    tokenizer::TokenKind,
};
use pretty_assertions::assert_eq;

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

// ========================================================================
// Bitflags error tests
// ========================================================================

#[test]
fn bitflags_missing_equals() {
    let text = r#"
    pub bitflags Flags: u32 {
        READ 0x1,
    }
    "#;

    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::Eq],
            found: TokenKind::IntLiteral("0x1".to_string()),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn bitflags_missing_name() {
    let text = r#"
    pub bitflags : u32 {
        FLAG = 1,
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
fn bitflags_missing_type() {
    let text = r#"
    pub bitflags Flags: {
        FLAG = 1,
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
fn bitflags_missing_value() {
    let text = r#"
    pub bitflags Flags: u32 {
        FLAG =,
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
fn bitflags_missing_opening_brace() {
    let text = r#"
    pub bitflags Flags: u32
        FLAG = 1,
    }
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::LBrace],
            found: TokenKind::Ident("FLAG".to_string()),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn empty_bitflags_is_valid() {
    let text = r#"
    bitflags Test: u32 {}
    "#;
    assert!(parse_str_for_tests(text).is_ok());
}

#[test]
fn can_parse_nested_bitflags() {
    let text = r#"
    pub type Outer {
        pub field: u32,
        pub bitflags InnerFlags: u32 {
            FLAG_A = 1,
            FLAG_B = 2,
        }
    }
    "#;
    assert!(parse_str_for_tests(text).is_ok());
}
