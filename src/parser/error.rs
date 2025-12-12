use crate::{
    source_store::FileStore,
    span::ItemLocation,
    tokenizer::{LexError, TokenKind},
};
use ariadne::{Color, Label, Report, ReportKind, Source};

#[derive(Debug, Clone, PartialEq)]
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
        let (offset, source) = if let Some(source) = file_store.source(location.file_id) {
            (crate::span::span_to_offset(&source, &location.span), source)
        } else {
            (0, String::new())
        };

        let report = Report::build(ReportKind::Error, filename, offset)
            .with_message(message)
            .with_label(
                Label::new((filename, offset..offset + 1))
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
    use crate::{parser::parse_str_for_tests, tokenizer::TokenKind};

    use super::ParseError;

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
                &err,
                ParseError::ExpectedToken {
                    expected,
                    found: TokenKind::LBrace,
                    ..
                } if expected == &[TokenKind::Colon]
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
                &err,
                ParseError::ExpectedToken {
                    expected,
                    found: TokenKind::IntLiteral(_),
                    ..
                } if expected == &[TokenKind::Eq]
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
                &err,
                ParseError::ExpectedToken {
                    expected,
                    found: TokenKind::Eof,
                    ..
                } if expected == &[TokenKind::Semi]
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

    // ========================================================================
    // Parser bounds checking tests - ensure no panics on truncated input
    // ========================================================================

    #[test]
    fn should_not_panic_on_attribute_at_eof() {
        // Just a # with nothing after - should error about missing [
        let text = "#";
        let err = parse_str_for_tests(text).unwrap_err();
        assert!(
            matches!(
                &err,
                ParseError::ExpectedToken {
                    expected,
                    found: TokenKind::Eof,
                    ..
                } if expected == &[TokenKind::LBracket]
            ),
            "Expected ExpectedToken with LBracket/Eof, got: {err:?}"
        );
    }

    #[test]
    fn should_not_panic_on_incomplete_attribute_bracket() {
        // #[ with nothing after - should error about missing attribute name
        let text = "#[";
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
    fn should_not_panic_on_unclosed_attribute() {
        // #[foo without closing ] - should error about missing ] or ,
        let text = "#[foo";
        let err = parse_str_for_tests(text).unwrap_err();
        assert!(
            matches!(
                err,
                ParseError::ExpectedToken {
                    found: TokenKind::Eof,
                    ..
                }
            ),
            "Expected ExpectedToken with Eof, got: {err:?}"
        );
    }

    #[test]
    fn should_not_panic_on_attribute_without_item() {
        // Complete attribute but nothing after - should error about missing item
        let text = "#[size(4)]";
        let err = parse_str_for_tests(text).unwrap_err();
        assert!(
            matches!(
                err,
                ParseError::ExpectedItemDefinition {
                    found: TokenKind::Eof,
                    ..
                }
            ),
            "Expected ExpectedItemDefinition with Eof, got: {err:?}"
        );
    }

    #[test]
    fn should_not_panic_on_multiple_attributes_at_eof() {
        // Multiple attributes but nothing after - should error about missing item
        let text = "#[size(4)] #[align(4)]";
        let err = parse_str_for_tests(text).unwrap_err();
        assert!(
            matches!(
                err,
                ParseError::ExpectedItemDefinition {
                    found: TokenKind::Eof,
                    ..
                }
            ),
            "Expected ExpectedItemDefinition with Eof, got: {err:?}"
        );
    }

    #[test]
    fn should_not_panic_on_doc_comment_then_incomplete_attribute() {
        // Doc comment followed by incomplete attribute - should error about missing attribute name
        let text = "/// My doc\n#[";
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
    fn should_not_panic_on_doc_comment_then_attribute_at_eof() {
        // Doc comment followed by complete attribute but nothing else - should error about missing item
        let text = "/// My doc\n#[size(4)]";
        let err = parse_str_for_tests(text).unwrap_err();
        assert!(
            matches!(
                err,
                ParseError::ExpectedItemDefinition {
                    found: TokenKind::Eof,
                    ..
                }
            ),
            "Expected ExpectedItemDefinition with Eof, got: {err:?}"
        );
    }

    #[test]
    fn should_not_panic_on_unclosed_attribute_with_parens() {
        // #[size(4) without closing ] - should error about missing ] or ,
        // This is the case from the issue: #[size(0x3540) followed by more code
        let text = "#[size(4)";
        let err = parse_str_for_tests(text).unwrap_err();
        assert!(
            matches!(
                err,
                ParseError::ExpectedToken {
                    found: TokenKind::Eof,
                    ..
                }
            ),
            "Expected ExpectedToken with Eof, got: {err:?}"
        );
    }

    #[test]
    fn should_not_panic_on_multiple_attrs_missing_bracket() {
        // #[size(0x3540), align(4) without closing ] - should error about missing ] or ,
        let text = "#[size(0x3540), align(4)";
        let err = parse_str_for_tests(text).unwrap_err();
        assert!(
            matches!(
                err,
                ParseError::ExpectedToken {
                    found: TokenKind::Eof,
                    ..
                }
            ),
            "Expected ExpectedToken with Eof, got: {err:?}"
        );
    }

    #[test]
    fn should_not_panic_on_multiple_attrs_missing_bracket_with_item() {
        // #[size(0x3540), align(4) followed by item - should error about missing ] or ,
        let text = "#[size(0x3540), align(4)\npub type Foo {}";
        let err = parse_str_for_tests(text).unwrap_err();
        assert!(
            matches!(
                err,
                ParseError::ExpectedToken {
                    found: TokenKind::Pub,
                    ..
                }
            ),
            "Expected ExpectedToken with Pub, got: {err:?}"
        );
    }
}
