use std::fmt;

use ariadne::{Color, Label, Report, ReportKind, Source};
use crate::{grammar::ItemPath, semantic::SemanticError, source_store::SourceStore, span::{ErrorContext, ErrorLabelColor, Span}};

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
        context: ErrorContext,
    },
    /// Failed to generate code for a specific field
    FieldCodeGenFailed {
        type_path: ItemPath,
        field_name: String,
        reason: String,
        context: ErrorContext,
    },
    /// Failed to generate vftable code
    VftableCodeGenFailed {
        type_path: ItemPath,
        reason: String,
        context: ErrorContext,
    },
}

impl BackendError {
    /// Convert ErrorLabelColor to ariadne Color
    fn label_color_to_ariadne(color: ErrorLabelColor) -> Color {
        match color {
            ErrorLabelColor::Red => Color::Red,
            ErrorLabelColor::Yellow => Color::Yellow,
            ErrorLabelColor::Blue => Color::Blue,
            ErrorLabelColor::Cyan => Color::Cyan,
        }
    }

    /// Format location prefix for error messages
    fn format_location(context: &ErrorContext) -> String {
        match (&context.filename, &context.span) {
            (Some(f), Some(s)) => format!("Error at {}:{}:{}: ", f, s.start.line, s.start.column),
            (Some(f), None) => format!("Error in {}: ", f),
            _ => String::new(),
        }
    }

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
                .take(
                    span.end
                        .line
                        .saturating_sub(span.start.line)
                        .saturating_sub(1),
                )
                .map(|line| line.len() + 1)
                .sum();
            let last_line_len = span.end.column.saturating_sub(1);
            first_line_len + middle_lines_len + last_line_len
        }
    }

    /// Format the error using ariadne with the provided source store
    pub fn format_with_ariadne(&self, source_store: &mut dyn SourceStore) -> String {
        match self {
            // For semantic errors, delegate to SemanticError's format_with_ariadne
            BackendError::Semantic(err) => err.format_with_ariadne(source_store),

            // For backend-specific errors with filename and span
            BackendError::TypeCodeGenFailed { type_path, reason, context } => {
                if let (Some(filename), Some(span)) = (&context.filename, &context.span) {
                    if let Some(source) = source_store.get(filename.as_ref()) {
                        let offset = Self::span_to_offset(source, span);
                        let length = Self::span_length(source, span);

                        let mut report_builder = Report::build(ReportKind::Error, filename.as_ref(), offset)
                            .with_message(format!("Failed to generate code for type `{}`: {}", type_path, reason))
                            .with_label(
                                Label::new((filename.as_ref(), offset..offset + length))
                                    .with_message("error occurred here")
                                    .with_color(Color::Red),
                            );

                        // Add additional labels if present
                        for label in &context.labels {
                            let label_offset = Self::span_to_offset(source, &label.span);
                            let label_length = Self::span_length(source, &label.span);
                            report_builder = report_builder.with_label(
                                Label::new((filename.as_ref(), label_offset..label_offset + label_length))
                                    .with_message(&label.message)
                                    .with_color(Self::label_color_to_ariadne(label.color)),
                            );
                        }

                        // Add notes if present
                        for note in &context.notes {
                            report_builder = report_builder.with_note(note);
                        }

                        // Add help text if present
                        if let Some(help) = &context.help {
                            report_builder = report_builder.with_help(help);
                        }

                        let report = report_builder.finish();

                        let mut buffer = Vec::new();
                        if report.write((filename.as_ref(), Source::from(source)), &mut buffer).is_ok() {
                            return String::from_utf8_lossy(&buffer).to_string();
                        }
                    }
                }
                self.to_string()
            }

            BackendError::FieldCodeGenFailed { type_path, field_name, reason, context } => {
                if let (Some(filename), Some(span)) = (&context.filename, &context.span) {
                    if let Some(source) = source_store.get(filename.as_ref()) {
                        let offset = Self::span_to_offset(source, span);
                        let length = Self::span_length(source, span);

                        let mut report_builder = Report::build(ReportKind::Error, filename.as_ref(), offset)
                            .with_message(format!("Failed to generate code for field `{}` of type `{}`: {}", field_name, type_path, reason))
                            .with_label(
                                Label::new((filename.as_ref(), offset..offset + length))
                                    .with_message("error occurred here")
                                    .with_color(Color::Red),
                            );

                        // Add additional labels if present
                        for label in &context.labels {
                            let label_offset = Self::span_to_offset(source, &label.span);
                            let label_length = Self::span_length(source, &label.span);
                            report_builder = report_builder.with_label(
                                Label::new((filename.as_ref(), label_offset..label_offset + label_length))
                                    .with_message(&label.message)
                                    .with_color(Self::label_color_to_ariadne(label.color)),
                            );
                        }

                        // Add notes if present
                        for note in &context.notes {
                            report_builder = report_builder.with_note(note);
                        }

                        // Add help text if present
                        if let Some(help) = &context.help {
                            report_builder = report_builder.with_help(help);
                        }

                        let report = report_builder.finish();

                        let mut buffer = Vec::new();
                        if report.write((filename.as_ref(), Source::from(source)), &mut buffer).is_ok() {
                            return String::from_utf8_lossy(&buffer).to_string();
                        }
                    }
                }
                self.to_string()
            }

            BackendError::VftableCodeGenFailed { type_path, reason, context } => {
                if let (Some(filename), Some(span)) = (&context.filename, &context.span) {
                    if let Some(source) = source_store.get(filename.as_ref()) {
                        let offset = Self::span_to_offset(source, span);
                        let length = Self::span_length(source, span);

                        let mut report_builder = Report::build(ReportKind::Error, filename.as_ref(), offset)
                            .with_message(format!("Failed to generate vftable code for type `{}`: {}", type_path, reason))
                            .with_label(
                                Label::new((filename.as_ref(), offset..offset + length))
                                    .with_message("error occurred here")
                                    .with_color(Color::Red),
                            );

                        // Add additional labels if present
                        for label in &context.labels {
                            let label_offset = Self::span_to_offset(source, &label.span);
                            let label_length = Self::span_length(source, &label.span);
                            report_builder = report_builder.with_label(
                                Label::new((filename.as_ref(), label_offset..label_offset + label_length))
                                    .with_message(&label.message)
                                    .with_color(Self::label_color_to_ariadne(label.color)),
                            );
                        }

                        // Add notes if present
                        for note in &context.notes {
                            report_builder = report_builder.with_note(note);
                        }

                        // Add help text if present
                        if let Some(help) = &context.help {
                            report_builder = report_builder.with_help(help);
                        }

                        let report = report_builder.finish();

                        let mut buffer = Vec::new();
                        if report.write((filename.as_ref(), Source::from(source)), &mut buffer).is_ok() {
                            return String::from_utf8_lossy(&buffer).to_string();
                        }
                    }
                }
                self.to_string()
            }

            // For other errors without span/filename, fall back to Display
            _ => self.to_string(),
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
                context,
            } => {
                write!(
                    f,
                    "{}Failed to generate code for type `{}`: {}",
                    Self::format_location(context),
                    type_path, reason
                )
            }
            BackendError::FieldCodeGenFailed {
                type_path,
                field_name,
                reason,
                context,
            } => {
                write!(
                    f,
                    "{}Failed to generate code for field `{}` of type `{}`: {}",
                    Self::format_location(context),
                    field_name, type_path, reason
                )
            }
            BackendError::VftableCodeGenFailed {
                type_path,
                reason,
                context,
            } => {
                write!(
                    f,
                    "{}Failed to generate vftable code for type `{}`: {}",
                    Self::format_location(context),
                    type_path, reason
                )
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
