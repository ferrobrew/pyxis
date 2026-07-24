use crate::{
    source_store::FileStore,
    span::{self, ItemLocation},
};
use ariadne::{Color, Label, Report, ReportKind, Source};

#[cfg(test)]
use crate::span::StripLocations;

/// Lexer errors
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub enum LexError {
    /// Unexpected character encountered during tokenization
    UnexpectedCharacter {
        character: char,
        location: ItemLocation,
    },
    /// Unterminated multiline comment (/* without closing */)
    UnterminatedMultilineComment { location: ItemLocation },
    /// Invalid escape sequence in string or char literal
    InvalidEscapeSequence {
        character: char,
        location: ItemLocation,
    },
    /// Unexpected end of file while parsing a string literal
    UnexpectedEofInStringLiteral { location: ItemLocation },
    /// Invalid raw string literal start (expected '"' after 'r' and '#')
    InvalidRawStringStart { location: ItemLocation },
    /// Unterminated raw string literal
    UnterminatedRawString { location: ItemLocation },
    /// Unexpected end of file while parsing a char literal
    UnexpectedEofInCharLiteral { location: ItemLocation },
    /// Unclosed char literal (missing closing quote)
    UnclosedCharLiteral { location: ItemLocation },
}

impl LexError {
    /// Returns the core error message without location prefix
    pub fn error_message(&self) -> String {
        match self {
            LexError::UnexpectedCharacter { character, .. } => {
                format!("Unexpected character: '{character}'")
            }
            LexError::UnterminatedMultilineComment { .. } => {
                "Unterminated multiline comment".to_string()
            }
            LexError::InvalidEscapeSequence { character, .. } => {
                format!("Invalid escape sequence: \\{character}")
            }
            LexError::UnexpectedEofInStringLiteral { .. } => {
                "Unexpected end of file in string literal".to_string()
            }
            LexError::InvalidRawStringStart { .. } => {
                "Expected '\"' after 'r' and '#' in raw string literal".to_string()
            }
            LexError::UnterminatedRawString { .. } => "Unterminated raw string literal".to_string(),
            LexError::UnexpectedEofInCharLiteral { .. } => {
                "Unexpected end of file in char literal".to_string()
            }
            LexError::UnclosedCharLiteral { .. } => {
                "Expected closing '\\'' in char literal".to_string()
            }
        }
    }

    /// Returns the location of the error
    pub fn location(&self) -> &ItemLocation {
        match self {
            LexError::UnexpectedCharacter { location, .. } => location,
            LexError::UnterminatedMultilineComment { location } => location,
            LexError::InvalidEscapeSequence { location, .. } => location,
            LexError::UnexpectedEofInStringLiteral { location } => location,
            LexError::InvalidRawStringStart { location } => location,
            LexError::UnterminatedRawString { location } => location,
            LexError::UnexpectedEofInCharLiteral { location } => location,
            LexError::UnclosedCharLiteral { location } => location,
        }
    }

    /// Format the error using ariadne with the provided file store.
    /// Always produces ariadne-formatted output.
    pub fn format_with_ariadne(&self, file_store: &FileStore) -> String {
        let message = self.error_message();
        let location = self.location();
        let filename = file_store.filename(location.file_id);

        if let Some(source) = file_store.source(location.file_id) {
            let offset = span::span_to_offset(&source, &location.span);
            let length = span::span_length(&source, &location.span).max(1);
            let report = Report::build(ReportKind::Error, (filename, offset..offset + length))
                .with_message(&message)
                .with_label(
                    Label::new((filename, offset..offset + length))
                        .with_message(&message)
                        .with_color(Color::Red),
                )
                .finish();

            let mut buffer = Vec::new();
            if report
                .write((filename, Source::from(source)), &mut buffer)
                .is_ok()
            {
                return String::from_utf8_lossy(&buffer).to_string();
            }
        }

        // Source not available - create a report without source code labels
        let report =
            Report::<(&str, std::ops::Range<usize>)>::build(ReportKind::Error, (filename, 0..0))
                .with_message(&message)
                .with_note(format!("Error location: {filename}:{location}"))
                .finish();

        let mut buffer = Vec::new();
        if report
            .write((filename, Source::from("")), &mut buffer)
            .is_ok()
        {
            return String::from_utf8_lossy(&buffer).to_string();
        }

        message
    }
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Lexer error at {}: {}",
            self.location(),
            self.error_message()
        )
    }
}

impl std::error::Error for LexError {}

impl crate::span::HasLocation for LexError {
    fn location(&self) -> &crate::span::ItemLocation {
        match self {
            Self::UnexpectedCharacter { location, .. }
            | Self::UnterminatedMultilineComment { location }
            | Self::InvalidEscapeSequence { location, .. }
            | Self::UnexpectedEofInStringLiteral { location }
            | Self::InvalidRawStringStart { location }
            | Self::UnterminatedRawString { location }
            | Self::UnexpectedEofInCharLiteral { location }
            | Self::UnclosedCharLiteral { location } => location,
        }
    }
}
