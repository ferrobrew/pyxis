use std::fmt;

use ariadne::{Color, Label, Report, ReportKind, Source};
use crate::{grammar::ItemPath, semantic::SemanticError, span::Span};

/// Backend code generation errors
#[derive(Debug)]
pub enum BackendError {
    /// IO error during code generation
    Io {
        error: std::io::Error,
        context: String,
    },
    /// Formatting error (e.g., rustfmt failed)
    Formatting(String),
    /// Syntax error in generated code
    Syntax(syn::Error),
    /// Semantic analysis error (with full context preserved)
    Semantic(SemanticError),
    /// Failed to generate code for a specific type
    TypeCodeGenFailed {
        type_path: ItemPath,
        reason: String,
        span: Option<Span>,
        filename: Option<Box<str>>,
        source: Option<Box<str>>,
    },
    /// Failed to generate code for a specific field
    FieldCodeGenFailed {
        type_path: ItemPath,
        field_name: String,
        reason: String,
        span: Option<Span>,
        filename: Option<Box<str>>,
        source: Option<Box<str>>,
    },
    /// Failed to generate vftable code
    VftableCodeGenFailed {
        type_path: ItemPath,
        reason: String,
        span: Option<Span>,
        filename: Option<Box<str>>,
        source: Option<Box<str>>,
    },
}

impl BackendError {
    fn span_to_offset(source: &str, span: &Span) -> usize {
        source
            .lines()
            .take(span.start.line.saturating_sub(1))
            .map(|line| line.len() + 1)
            .sum::<usize>()
            + span.start.column.saturating_sub(1)
    }

    fn span_length(source: &str, span: &Span) -> usize {
        if span.start.line == span.end.line {
            span.end.column.saturating_sub(span.start.column)
        } else {
            let first_line_len = source
                .lines()
                .nth(span.start.line.saturating_sub(1))
                .map(|line| line.len())
                .unwrap_or(0)
                .saturating_sub(span.start.column.saturating_sub(1));
            let middle_lines_len: usize = source
                .lines()
                .skip(span.start.line)
                .take(span.end.line.saturating_sub(span.start.line).saturating_sub(1))
                .map(|line| line.len() + 1)
                .sum();
            let last_line_len = span.end.column.saturating_sub(1);
            first_line_len + middle_lines_len + last_line_len
        }
    }
}

impl fmt::Display for BackendError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BackendError::Io { error, context } => {
                write!(f, "IO error: {} ({})", error, context)
            }
            BackendError::Formatting(msg) => write!(f, "Formatting error: {}", msg),
            BackendError::Syntax(err) => write!(f, "Syntax error: {}", err),
            BackendError::Semantic(err) => write!(f, "{}", err),
            BackendError::TypeCodeGenFailed {
                type_path,
                reason,
                span,
                filename,
                source,
            } => {
                if let (Some(span), Some(filename), Some(source)) = (span, filename, source) {
                    let offset = Self::span_to_offset(source, span);
                    let length = Self::span_length(source, span);

                    let report = Report::build(ReportKind::Error, filename.as_ref(), offset)
                        .with_message(format!("Failed to generate code for type `{}`", type_path))
                        .with_label(
                            Label::new((filename.as_ref(), offset..offset + length.max(1)))
                                .with_message(reason)
                                .with_color(Color::Red),
                        )
                        .finish();

                    let mut buffer = Vec::new();
                    report
                        .write((filename.as_ref(), Source::from(source.as_ref())), &mut buffer)
                        .map_err(|_| fmt::Error)?;
                    write!(f, "{}", String::from_utf8_lossy(&buffer))
                } else {
                    write!(
                        f,
                        "Failed to generate code for type `{}`: {}",
                        type_path, reason
                    )
                }
            }
            BackendError::FieldCodeGenFailed {
                type_path,
                field_name,
                reason,
                span,
                filename,
                source,
            } => {
                if let (Some(span), Some(filename), Some(source)) = (span, filename, source) {
                    let offset = Self::span_to_offset(source, span);
                    let length = Self::span_length(source, span);

                    let report = Report::build(ReportKind::Error, filename.as_ref(), offset)
                        .with_message(format!(
                            "Failed to generate code for field `{}` of type `{}`",
                            field_name, type_path
                        ))
                        .with_label(
                            Label::new((filename.as_ref(), offset..offset + length.max(1)))
                                .with_message(reason)
                                .with_color(Color::Red),
                        )
                        .finish();

                    let mut buffer = Vec::new();
                    report
                        .write((filename.as_ref(), Source::from(source.as_ref())), &mut buffer)
                        .map_err(|_| fmt::Error)?;
                    write!(f, "{}", String::from_utf8_lossy(&buffer))
                } else {
                    write!(
                        f,
                        "Failed to generate code for field `{}` of type `{}`: {}",
                        field_name, type_path, reason
                    )
                }
            }
            BackendError::VftableCodeGenFailed {
                type_path,
                reason,
                span,
                filename,
                source,
            } => {
                if let (Some(span), Some(filename), Some(source)) = (span, filename, source) {
                    let offset = Self::span_to_offset(source, span);
                    let length = Self::span_length(source, span);

                    let report = Report::build(ReportKind::Error, filename.as_ref(), offset)
                        .with_message(format!(
                            "Failed to generate vftable code for type `{}`",
                            type_path
                        ))
                        .with_label(
                            Label::new((filename.as_ref(), offset..offset + length.max(1)))
                                .with_message(reason)
                                .with_color(Color::Red),
                        )
                        .finish();

                    let mut buffer = Vec::new();
                    report
                        .write((filename.as_ref(), Source::from(source.as_ref())), &mut buffer)
                        .map_err(|_| fmt::Error)?;
                    write!(f, "{}", String::from_utf8_lossy(&buffer))
                } else {
                    write!(
                        f,
                        "Failed to generate vftable code for type `{}`: {}",
                        type_path, reason
                    )
                }
            }
        }
    }
}

impl std::error::Error for BackendError {}

impl From<syn::Error> for BackendError {
    fn from(err: syn::Error) -> Self {
        BackendError::Syntax(err)
    }
}

impl From<std::fmt::Error> for BackendError {
    fn from(err: std::fmt::Error) -> Self {
        BackendError::Formatting(err.to_string())
    }
}

impl From<SemanticError> for BackendError {
    fn from(err: SemanticError) -> Self {
        BackendError::Semantic(err)
    }
}

/// Result type for backend operations
pub type Result<T> = std::result::Result<T, BackendError>;
