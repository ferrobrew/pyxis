use crate::{
    source_store::SourceStore,
    span::ItemLocation,
    tokenizer::{LexError, TokenKind},
};
use ariadne::{Color, Label, Report, ReportKind, Source};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    ExpectedToken {
        expected: TokenKind,
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
    /// Helper to build ariadne report with source
    fn format_ariadne_with_source(
        location: &ItemLocation,
        source_store: &mut dyn SourceStore,
        message: &str,
        label_message: &str,
    ) -> String {
        let (offset, source) = if let Some(source) = source_store.get(location.filename.as_ref()) {
            (crate::span::span_to_offset(source, &location.span), source)
        } else {
            (0, "")
        };

        let report = Report::build(ReportKind::Error, location.filename.as_ref(), offset)
            .with_message(message)
            .with_label(
                Label::new((location.filename.as_ref(), offset..offset + 1))
                    .with_message(label_message)
                    .with_color(Color::Red),
            )
            .finish();

        let mut buffer = Vec::new();
        report
            .write(
                (location.filename.as_ref(), Source::from(source)),
                &mut buffer,
            )
            .expect("writing to Vec should not fail");
        String::from_utf8_lossy(&buffer).to_string()
    }

    /// Format error with ariadne using a source store. Always produces ariadne-formatted output.
    pub fn format_with_ariadne(&self, source_store: &mut dyn SourceStore) -> String {
        match self {
            ParseError::ExpectedToken {
                expected,
                found,
                location,
            } => Self::format_ariadne_with_source(
                location,
                source_store,
                &format!("Expected {expected:?}, found {found:?}"),
                &format!("expected {expected:?} here"),
            ),
            ParseError::ExpectedIdentifier { found, location } => Self::format_ariadne_with_source(
                location,
                source_store,
                &format!("Expected identifier, found {found:?}"),
                "expected identifier here",
            ),
            ParseError::ExpectedType { found, location } => Self::format_ariadne_with_source(
                location,
                source_store,
                &format!("Expected type, found {found:?}"),
                "expected type here",
            ),
            ParseError::ExpectedExpression { found, location } => Self::format_ariadne_with_source(
                location,
                source_store,
                &format!("Expected expression, found {found:?}"),
                "expected expression here",
            ),
            ParseError::ExpectedIntLiteral { found, location } => Self::format_ariadne_with_source(
                location,
                source_store,
                &format!("Expected integer literal, found {found:?}"),
                "expected integer literal here",
            ),
            ParseError::ExpectedStringLiteral { found, location } => {
                Self::format_ariadne_with_source(
                    location,
                    source_store,
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
                source_store,
                &format!("Invalid {kind} literal: {value}"),
                "invalid literal",
            ),
            ParseError::MissingPointerQualifier { location } => Self::format_ariadne_with_source(
                location,
                source_store,
                "Expected const or mut after *",
                "expected 'const' or 'mut' here",
            ),
            ParseError::SuperNotSupported { location } => Self::format_ariadne_with_source(
                location,
                source_store,
                "super not supported",
                "super keyword not supported",
            ),
            ParseError::UnexpectedModuleToken { found, location } => {
                Self::format_ariadne_with_source(
                    location,
                    source_store,
                    &format!("Unexpected token at module level: {found:?}"),
                    "unexpected token",
                )
            }
            ParseError::UnexpectedTokenAfterAttributes { found, location } => {
                Self::format_ariadne_with_source(
                    location,
                    source_store,
                    &format!("Unexpected token after attributes: {found:?}"),
                    "unexpected token after attributes",
                )
            }
            ParseError::ExpectedItemDefinition { found, location } => {
                Self::format_ariadne_with_source(
                    location,
                    source_store,
                    &format!("Expected type, enum, or bitflags, found {found:?}"),
                    "expected type, enum, or bitflags here",
                )
            }
            ParseError::ExpectedBackendContent { found, location } => {
                Self::format_ariadne_with_source(
                    location,
                    source_store,
                    &format!("Expected LBrace, Prologue, or Epilogue, found {found:?}"),
                    "expected backend content here",
                )
            }
            ParseError::ExpectedPrologueOrEpilogue { found, location } => {
                Self::format_ariadne_with_source(
                    location,
                    source_store,
                    &format!("Expected prologue or epilogue, found {found:?}"),
                    "expected prologue or epilogue here",
                )
            }
            ParseError::Tokenizer(err) => err.format_with_ariadne(source_store),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::ExpectedToken {
                expected, found, ..
            } => write!(f, "Expected {expected:?}, found {found:?}"),
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
}
