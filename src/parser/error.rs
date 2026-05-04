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
    /// `backend foo` where `foo` isn't a known backend name. Caught at
    /// parse time so a typo can't slip through to a backend's runtime
    /// "no module here" failure.
    UnknownBackend {
        found: String,
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
            ParseError::UnknownBackend { found, location } => Self::format_ariadne_with_source(
                location,
                file_store,
                &format!(
                    "Unknown backend `{found}`; valid backends are: {}",
                    crate::Backend::ALL
                        .iter()
                        .map(|b| b.name())
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                "this backend name isn't recognized",
            ),
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
            ParseError::UnknownBackend { found, .. } => {
                write!(
                    f,
                    "Unknown backend `{found}`; valid backends are: {}",
                    crate::Backend::ALL
                        .iter()
                        .map(|b| b.name())
                        .collect::<Vec<_>>()
                        .join(", "),
                )
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
