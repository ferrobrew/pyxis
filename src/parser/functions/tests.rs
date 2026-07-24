use crate::{
    grammar::test_aliases::*,
    parser::{error::ParseError, parse_str_for_tests},
    span::{ItemLocation, StripLocations},
    tokenizer::TokenKind,
};
use pretty_assertions::assert_eq;

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
                    T::generic("SharedPtr", [T::ident("PfxInstanceInterface")]),
                ),
            ])
            .with_attributes([A::size(0x10)]),
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

// ========================================================================
// Impl block error tests
// ========================================================================

#[test]
fn impl_missing_name() {
    let text = r#"
        impl {
            fn test(&self);
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
fn impl_missing_opening_brace() {
    let text = r#"
        impl TestType
            fn test(&self);
        }
        "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::LBrace],
            found: TokenKind::Fn,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn impl_missing_closing_brace() {
    let text = r#"
        impl TestType {
            fn test(&self);
        "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::Fn],
            found: TokenKind::Eof,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn impl_functions_using_comma_instead_of_semicolon() {
    let text = r#"
        impl TestType {
            fn test1(&self),
            fn test2(&self);
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
fn impl_functions_missing_separator_entirely() {
    let text = r#"
        impl TestType {
            fn test1(&self)
            fn test2(&self);
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
fn impl_functions_missing_separator_with_attributes() {
    let text = r#"
        impl TestType {
            fn test1(&self)
            #[address(0x123)]
            fn test2(&self);
        }
        "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::Semi],
            found: TokenKind::Hash,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn empty_impl_is_valid() {
    let text = r#"
        impl Test {}
        "#;
    assert!(parse_str_for_tests(text).is_ok());
}

// ========================================================================
// Function signature error tests
// ========================================================================

#[test]
fn incomplete_function() {
    let text = r#"
        pub fn test(
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
fn function_missing_name() {
    let text = r#"
        impl TestType {
            fn (&self);
        }
        "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedIdentifier {
            found: TokenKind::LParen,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn function_missing_opening_paren() {
    let text = r#"
        impl TestType {
            fn test;
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
fn function_missing_closing_paren() {
    let text = r#"
        impl TestType {
            fn test(&self;
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
fn function_arg_missing_type() {
    let text = r#"
        impl TestType {
            fn test(arg:);
        }
        "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedType {
            found: TokenKind::RParen,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn function_arg_missing_colon() {
    let text = r#"
        impl TestType {
            fn test(arg i32);
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
fn function_with_only_comma_in_params() {
    let text = r#"
        impl Test {
            fn foo(,);
        }
        "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedIdentifier {
            found: TokenKind::Comma,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn function_with_trailing_comma_is_valid() {
    let text = r#"
        impl Test {
            fn foo(&self,);
        }
        "#;
    assert!(parse_str_for_tests(text).is_ok());
}

// ========================================================================
// Freestanding function tests
// ========================================================================

#[test]
fn freestanding_function_with_address_parses_ok() {
    let text = r#"
        #[address(0x123)]
        pub fn test();
        "#;
    assert!(parse_str_for_tests(text).is_ok());
}

#[test]
fn freestanding_private_function_parses_ok() {
    let text = r#"
        #[address(0x123)]
        fn test();
        "#;
    assert!(parse_str_for_tests(text).is_ok());
}

#[test]
fn pub_fn_without_attributes_parses_ok() {
    // `pub fn` at module level parses - semantic layer catches missing address
    let text = r#"
        pub fn test();
        "#;
    assert!(parse_str_for_tests(text).is_ok());
}

#[test]
fn private_fn_without_attributes_parses_ok() {
    // Private `fn` at module level parses - semantic layer catches missing address
    let text = r#"
        fn test();
        "#;
    assert!(parse_str_for_tests(text).is_ok());
}

#[test]
fn freestanding_function_supports_doc_comments() {
    let text = r#"
        /// This is a freestanding function
        #[address(0x123)]
        pub fn test();
        "#;

    let ast = M::new().with_functions([F::new((V::Public, "test"), [])
        .with_attributes([A::address(0x123)])
        .with_doc_comments(vec![" This is a freestanding function".to_string()])]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}
