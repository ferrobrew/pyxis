use crate::{
    source_store::FileStore,
    span::ItemLocation,
    tokenizer::{LexError, TokenKind},
};
use ariadne::{Color, Label, Report, ReportKind, Source};

#[cfg(test)]
use crate::span::StripLocations;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(StripLocations))]
pub enum ParseError {
    ExpectedToken {
        expected: Vec<TokenKind>,
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedIdentifier {
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedType {
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedExpression {
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedIntLiteral {
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedStringLiteral {
        found: TokenKind,
        location: ItemLocation,
    },
    InvalidIntLiteral {
        kind: String,
        value: String,
        location: ItemLocation,
    },
    MissingPointerQualifier {
        location: ItemLocation,
    },
    SuperNotSupported {
        location: ItemLocation,
    },
    UnexpectedModuleToken {
        found: TokenKind,
        location: ItemLocation,
    },
    UnexpectedTokenAfterAttributes {
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedItemDefinition {
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedBackendContent {
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedPrologueOrEpilogue {
        found: TokenKind,
        location: ItemLocation,
    },
    Tokenizer(LexError),
}
impl ParseError {
    /// Format a list of expected tokens for display
    fn format_expected_tokens(expected: &[TokenKind]) -> String {
        match expected.len() {
            0 => "nothing".to_string(),
            1 => format!("{:?}", expected[0]),
            2 => format!("{:?} or {:?}", expected[0], expected[1]),
            _ => {
                let all_but_last: Vec<_> = expected[..expected.len() - 1]
                    .iter()
                    .map(|t| format!("{t:?}"))
                    .collect();
                format!(
                    "{}, or {:?}",
                    all_but_last.join(", "),
                    expected.last().unwrap()
                )
            }
        }
    }

    /// Helper to build ariadne report with source
    fn format_ariadne_with_source(
        location: &ItemLocation,
        file_store: &FileStore,
        message: &str,
        label_message: &str,
    ) -> String {
        let filename = file_store.filename(location.file_id);
        let (offset, length, source) = if let Some(source) = file_store.source(location.file_id) {
            (
                crate::span::span_to_offset(&source, &location.span),
                crate::span::span_length(&source, &location.span).max(1),
                source,
            )
        } else {
            (0, 1, String::new())
        };

        let report = Report::build(ReportKind::Error, (filename, offset..offset + length))
            .with_message(message)
            .with_label(
                Label::new((filename, offset..offset + length))
                    .with_message(label_message)
                    .with_color(Color::Red),
            )
            .finish();

        let mut buffer = Vec::new();
        report
            .write((filename, Source::from(source)), &mut buffer)
            .expect("writing to Vec should not fail");
        String::from_utf8_lossy(&buffer).to_string()
    }

    /// Format error with ariadne using a file store. Always produces ariadne-formatted output.
    pub fn format_with_ariadne(&self, file_store: &FileStore) -> String {
        match self {
            ParseError::ExpectedToken {
                expected,
                found,
                location,
            } => {
                let expected_str = Self::format_expected_tokens(expected);
                Self::format_ariadne_with_source(
                    location,
                    file_store,
                    &format!("Expected {expected_str}, found {found:?}"),
                    &format!("expected {expected_str} here"),
                )
            }
            ParseError::ExpectedIdentifier { found, location } => Self::format_ariadne_with_source(
                location,
                file_store,
                &format!("Expected identifier, found {found:?}"),
                "expected identifier here",
            ),
            ParseError::ExpectedType { found, location } => Self::format_ariadne_with_source(
                location,
                file_store,
                &format!("Expected type, found {found:?}"),
                "expected type here",
            ),
            ParseError::ExpectedExpression { found, location } => Self::format_ariadne_with_source(
                location,
                file_store,
                &format!("Expected expression, found {found:?}"),
                "expected expression here",
            ),
            ParseError::ExpectedIntLiteral { found, location } => Self::format_ariadne_with_source(
                location,
                file_store,
                &format!("Expected integer literal, found {found:?}"),
                "expected integer literal here",
            ),
            ParseError::ExpectedStringLiteral { found, location } => {
                Self::format_ariadne_with_source(
                    location,
                    file_store,
                    &format!("Expected string literal, found {found:?}"),
                    "expected string literal here",
                )
            }
            ParseError::InvalidIntLiteral {
                kind,
                value,
                location,
            } => Self::format_ariadne_with_source(
                location,
                file_store,
                &format!("Invalid {kind} literal: {value}"),
                "invalid literal",
            ),
            ParseError::MissingPointerQualifier { location } => Self::format_ariadne_with_source(
                location,
                file_store,
                "Expected const or mut after *",
                "expected 'const' or 'mut' here",
            ),
            ParseError::SuperNotSupported { location } => Self::format_ariadne_with_source(
                location,
                file_store,
                "super not supported",
                "super keyword not supported",
            ),
            ParseError::UnexpectedModuleToken { found, location } => {
                Self::format_ariadne_with_source(
                    location,
                    file_store,
                    &format!("Unexpected token at module level: {found:?}"),
                    "unexpected token",
                )
            }
            ParseError::UnexpectedTokenAfterAttributes { found, location } => {
                Self::format_ariadne_with_source(
                    location,
                    file_store,
                    &format!("Unexpected token after attributes: {found:?}"),
                    "unexpected token after attributes",
                )
            }
            ParseError::ExpectedItemDefinition { found, location } => {
                Self::format_ariadne_with_source(
                    location,
                    file_store,
                    &format!("Expected type, enum, or bitflags, found {found:?}"),
                    "expected type, enum, or bitflags here",
                )
            }
            ParseError::ExpectedBackendContent { found, location } => {
                Self::format_ariadne_with_source(
                    location,
                    file_store,
                    &format!("Expected LBrace, Prologue, or Epilogue, found {found:?}"),
                    "expected backend content here",
                )
            }
            ParseError::ExpectedPrologueOrEpilogue { found, location } => {
                Self::format_ariadne_with_source(
                    location,
                    file_store,
                    &format!("Expected prologue or epilogue, found {found:?}"),
                    "expected prologue or epilogue here",
                )
            }
            ParseError::Tokenizer(err) => err.format_with_ariadne(file_store),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::ExpectedToken {
                expected, found, ..
            } => {
                let expected_str = Self::format_expected_tokens(expected);
                write!(f, "Expected {expected_str}, found {found:?}")
            }
            ParseError::ExpectedIdentifier { found, .. } => {
                write!(f, "Expected identifier, found {found:?}")
            }
            ParseError::ExpectedType { found, .. } => {
                write!(f, "Expected type, found {found:?}")
            }
            ParseError::ExpectedExpression { found, .. } => {
                write!(f, "Expected expression, found {found:?}")
            }
            ParseError::ExpectedIntLiteral { found, .. } => {
                write!(f, "Expected integer literal, found {found:?}")
            }
            ParseError::ExpectedStringLiteral { found, .. } => {
                write!(f, "Expected string literal, found {found:?}")
            }
            ParseError::InvalidIntLiteral { kind, value, .. } => {
                write!(f, "Invalid {kind} literal: {value}")
            }
            ParseError::MissingPointerQualifier { .. } => {
                write!(f, "Expected const or mut after *")
            }
            ParseError::SuperNotSupported { .. } => write!(f, "super not supported"),
            ParseError::UnexpectedModuleToken { found, .. } => {
                write!(f, "Unexpected token at module level: {found:?}")
            }
            ParseError::UnexpectedTokenAfterAttributes { found, .. } => {
                write!(f, "Unexpected token after attributes: {found:?}")
            }
            ParseError::ExpectedItemDefinition { found, .. } => {
                write!(f, "Expected type, enum, or bitflags, found {found:?}")
            }
            ParseError::ExpectedBackendContent { found, .. } => {
                write!(f, "Expected LBrace, Prologue, or Epilogue, found {found:?}")
            }
            ParseError::ExpectedPrologueOrEpilogue { found, .. } => {
                write!(f, "Expected prologue or epilogue, found {found:?}")
            }
            ParseError::Tokenizer(err) => write!(f, "{err}"),
        }
    }
}
impl std::error::Error for ParseError {}
impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        ParseError::Tokenizer(err)
    }
}

// ============================================================================
// Negative tests - parser should reject invalid syntax
// ============================================================================

#[cfg(test)]
mod tests {
    use crate::{
        parser::parse_str_for_tests,
        span::{ItemLocation, StripLocations},
        tokenizer::TokenKind,
    };

    use super::ParseError;

    #[test]
    fn should_fail_on_missing_closing_brace() {
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
    fn should_fail_on_missing_field_type() {
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
    fn should_fail_on_missing_enum_type_annotation() {
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
    fn should_fail_on_missing_equals_in_bitflags() {
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
    fn should_fail_on_malformed_pointer_type() {
        let text = r#"
        pub type TestType {
            field: *i32,
        }
        "#;

        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::MissingPointerQualifier {
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn should_fail_on_missing_semicolon_after_extern() {
        let text = r#"
        extern type TestType
        "#;

        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Semi],
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn should_fail_on_incomplete_function() {
        let text = r#"
        pub fn test(
        "#;

        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedItemDefinition {
                found: TokenKind::Fn,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    // ========================================================================
    // Parser bounds checking tests - ensure no panics on truncated input
    // ========================================================================

    #[test]
    fn should_not_panic_on_attribute_at_eof() {
        // Just a # with nothing after - should error about missing [
        let text = "#";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::LBracket],
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn should_not_panic_on_incomplete_attribute_bracket() {
        // #[ with nothing after - should error about missing attribute name
        let text = "#[";
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
    fn should_not_panic_on_unclosed_attribute() {
        // #[foo without closing ] - should error about missing ] or ,
        let text = "#[foo";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RBracket, TokenKind::Comma],
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn should_not_panic_on_attribute_without_item() {
        // Complete attribute but nothing after - should error about missing item
        let text = "#[size(4)]";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedItemDefinition {
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn should_not_panic_on_multiple_attributes_at_eof() {
        // Multiple attributes but nothing after - should error about missing item
        let text = "#[size(4)] #[align(4)]";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedItemDefinition {
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn should_not_panic_on_doc_comment_then_incomplete_attribute() {
        // Doc comment followed by incomplete attribute - should error about missing attribute name
        let text = "/// My doc\n#[";
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
    fn should_not_panic_on_doc_comment_then_attribute_at_eof() {
        // Doc comment followed by complete attribute but nothing else - should error about missing item
        let text = "/// My doc\n#[size(4)]";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedItemDefinition {
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn should_not_panic_on_unclosed_attribute_with_parens() {
        // #[size(4) without closing ] - should error about missing ] or ,
        // This is the case from the issue: #[size(0x3540) followed by more code
        let text = "#[size(4)";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RBracket, TokenKind::Comma],
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn should_not_panic_on_multiple_attrs_missing_bracket() {
        // #[size(0x3540), align(4) without closing ] - should error about missing ] or ,
        let text = "#[size(0x3540), align(4)";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RBracket, TokenKind::Comma],
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn should_not_panic_on_multiple_attrs_missing_bracket_with_item() {
        // #[size(0x3540), align(4) followed by item - should error about missing ] or ,
        let text = "#[size(0x3540), align(4)\npub type Foo {}";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RBracket, TokenKind::Comma],
                found: TokenKind::Pub,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
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
        // Comma is not valid after a function - expects semicolon
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
        // After first function, expects semicolon but finds 'pub' keyword
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
        // Same test but with private functions (no pub keyword)
        let text = r#"
        type TestType {
            vftable {
                fn test1(&self)
                fn test2(&self);
            }
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        // After first function, expects semicolon but finds 'fn' keyword
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
        // Self without & is treated as a keyword, not a valid identifier for argument name
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIdentifier {
                found: TokenKind::SelfValue,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    // ========================================================================
    // Type definition error tests
    // ========================================================================

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
    fn type_field_missing_type() {
        let text = r#"
        type TestType {
            field:
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedType {
                found: TokenKind::RBrace,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn type_alias_missing_target() {
        let text = r#"
        type IntPtr =;
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
    fn type_alias_missing_semicolon() {
        let text = r#"
        type IntPtr = i32
        type Another {}
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
    fn type_multiple_vftables() {
        // Valid syntax but semantically wrong - parser should accept it
        // This test verifies the parser doesn't crash on unusual structures
        let text = r#"
        type TestType {
            vftable {},
            vftable {},
        }
        "#;
        // This actually parses fine - semantic check would catch it
        assert!(parse_str_for_tests(text).is_ok());
    }

    // ========================================================================
    // Enum error tests
    // ========================================================================

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

    // ========================================================================
    // Bitflags error tests
    // ========================================================================

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

    // ========================================================================
    // Function and impl block error tests
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
        // Parser continues looking for more functions after the valid one, expecting 'fn'
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
        // Comma is not valid after a function - expects semicolon
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
        // After first function, expects semicolon but finds 'fn' keyword
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
        // Test with attributes between functions - missing semicolon
        let text = r#"
        impl TestType {
            fn test1(&self)
            #[address(0x123)]
            fn test2(&self);
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        // After first function, expects semicolon but finds '#' for attribute
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
    fn freestanding_function_parses_without_address() {
        // Freestanding function without #[address(...)] should parse at syntax level
        // Semantic layer should reject it (missing implementation)
        let text = r#"
        #[address(0x123)]
        pub fn test();
        "#;
        // Note: needs address attribute to parse at module level as a function
        assert!(parse_str_for_tests(text).is_ok());
    }

    // ========================================================================
    // Generic type error tests
    // ========================================================================

    #[test]
    fn generic_missing_closing_angle() {
        let text = r#"
        type Container {
            field: Shared<i32,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        // After the comma, parser expects another type parameter
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedType {
                found: TokenKind::RBrace,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn generic_empty_type_params_in_field() {
        // Empty generic params in a field type reference parse OK - semantic layer catches
        let text = r#"
        type Container {
            field: Shared<>,
        }
        "#;
        // This actually parses fine, semantic layer would catch the invalid usage
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
    fn generic_type_def_empty_params() {
        // Empty type params in type definition parse OK - `Shared<>` is just non-generic
        let text = r#"
        type Shared<> {
            field: i32,
        }
        "#;
        // Parser accepts this as a non-generic type (empty angle brackets)
        assert!(parse_str_for_tests(text).is_ok());
    }

    // ========================================================================
    // Pointer and array type error tests
    // ========================================================================

    #[test]
    fn pointer_missing_qualifier() {
        let text = r#"
        type Test {
            field: *i32,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::MissingPointerQualifier {
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn pointer_missing_target_type() {
        let text = r#"
        type Test {
            field: *const,
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
    fn array_missing_size() {
        let text = r#"
        type Test {
            field: [i32;],
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIntLiteral {
                found: TokenKind::RBracket,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn array_missing_semicolon_between_type_and_size() {
        let text = r#"
        type Test {
            field: [i32 4],
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Semi],
                found: TokenKind::IntLiteral("4".to_string()),
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn array_missing_closing_bracket() {
        let text = r#"
        type Test {
            field: [i32; 4,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RBracket],
                found: TokenKind::Comma,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn array_missing_type() {
        let text = r#"
        type Test {
            field: [; 4],
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

    // ========================================================================
    // Use statement error tests
    // ========================================================================

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

    // ========================================================================
    // Attribute error tests
    // ========================================================================

    #[test]
    fn attribute_empty_parens_parses_ok() {
        // #[size()] is parsed as a function-like attribute with no arguments
        // Semantic validation catches that size requires an argument
        let text = r#"
        #[size()]
        type Test {}
        "#;
        // Parser accepts this, semantic layer validates argument count
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn attribute_missing_closing_paren() {
        let text = r#"
        #[size(4]
        type Test {}
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RParen],
                found: TokenKind::RBracket,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn attribute_unknown_character() {
        let text = r#"
        #[size(4) @]
        type Test {}
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        // Tokenizer error for unknown character
        assert_eq!(
            err.strip_locations(),
            ParseError::Tokenizer(crate::tokenizer::LexError::UnexpectedCharacter {
                character: '@',
                location: ItemLocation::test(),
            })
            .strip_locations()
        );
    }

    // ========================================================================
    // Module-level error tests
    // ========================================================================

    #[test]
    fn unexpected_token_at_module_level() {
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
    fn unexpected_keyword_at_module_level() {
        let text = r#"
        const
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::UnexpectedModuleToken {
                found: TokenKind::Const,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn random_punctuation_at_module_level() {
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

    // ========================================================================
    // Complex/nested error tests
    // ========================================================================

    #[test]
    fn nested_generic_missing_outer_closing() {
        let text = r#"
        type Test {
            field: Outer<Inner<i32>,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        // After comma, parser expects another type but finds closing brace
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedType {
                found: TokenKind::RBrace,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn pointer_to_invalid_array() {
        let text = r#"
        type Test {
            field: *const [i32],
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Semi],
                found: TokenKind::RBracket,
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
        // Trailing comma after &self is allowed by the parser
        assert!(parse_str_for_tests(text).is_ok());
    }

    // ========================================================================
    // Invalid integer literal tests
    // ========================================================================

    #[test]
    fn array_size_with_invalid_hex() {
        let text = r#"
        type Test {
            field: [i32; 0xZZZ],
        }
        "#;
        // Parser catches the invalid hex literal (0x with no valid digits)
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::InvalidIntLiteral {
                kind: "hex".to_string(),
                value: "0x".to_string(),
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    // ========================================================================
    // Edge cases and recovery tests
    // ========================================================================

    #[test]
    fn empty_type_body_is_valid() {
        let text = r#"
        type Test {}
        "#;
        assert!(parse_str_for_tests(text).is_ok());
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
    fn empty_impl_is_valid() {
        let text = r#"
        impl Test {}
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn empty_enum_is_valid() {
        let text = r#"
        enum Test: u32 {}
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn empty_bitflags_is_valid() {
        let text = r#"
        bitflags Test: u32 {}
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
    fn deeply_nested_pointer() {
        let text = r#"
        type Test {
            field: *const *mut *const i32,
        }
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn array_of_pointers() {
        let text = r#"
        type Test {
            field: [*mut i32; 4],
        }
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn pointer_to_array() {
        let text = r#"
        type Test {
            field: *const [i32; 4],
        }
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }
}
