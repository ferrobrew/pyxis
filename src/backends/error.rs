use std::fmt;

use crate::{
    grammar::ItemPath,
    semantic::SemanticError,
    source_store::SourceStore,
    span::{self, ErrorContext, ItemLocation},
};
use ariadne::{Label, Report, ReportKind, Source};

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
    /// Returns the core error message without location prefix
    fn error_message(&self) -> String {
        match self {
            BackendError::Io { error, context } => {
                format!("IO error: {} ({})", error, context)
            }
            BackendError::Formatting(msg) => format!("Formatting error: {}", msg),
            BackendError::Syntax(err) => format!("Syntax error: {}", err),
            BackendError::Semantic(err) => err.to_string(),
            BackendError::TypeCodeGenFailed {
                type_path, reason, ..
            } => {
                format!(
                    "Failed to generate code for type `{}`: {}",
                    type_path, reason
                )
            }
            BackendError::FieldCodeGenFailed {
                type_path,
                field_name,
                reason,
                ..
            } => {
                format!(
                    "Failed to generate code for field `{}` of type `{}`: {}",
                    field_name, type_path, reason
                )
            }
            BackendError::VftableCodeGenFailed {
                type_path, reason, ..
            } => {
                format!(
                    "Failed to generate vftable code for type `{}`: {}",
                    type_path, reason
                )
            }
        }
    }

    /// Returns the error context if available
    fn error_context(&self) -> Option<&ErrorContext> {
        match self {
            BackendError::TypeCodeGenFailed { context, .. }
            | BackendError::FieldCodeGenFailed { context, .. }
            | BackendError::VftableCodeGenFailed { context, .. } => Some(context),
            _ => None,
        }
    }

    /// Format the error using ariadne with the provided source store
    pub fn format_with_ariadne(&self, source_store: &mut dyn SourceStore) -> String {
        // For semantic errors, delegate to SemanticError's format_with_ariadne
        if let BackendError::Semantic(err) = self {
            return err.format_with_ariadne(source_store);
        }

        let message = self.error_message();

        // Get context if available
        let context = match self.error_context() {
            Some(ctx) => ctx,
            None => {
                // No location info - create simple report
                return format_ariadne_simple(&message);
            }
        };

        let location = &context.location;

        // Try to get source
        if let Some(source) = source_store.get(location.filename.as_ref()) {
            format_ariadne_with_source(location, source, &message)
        } else {
            format_ariadne_without_source(location, &message)
        }
    }
}

/// Format an Ariadne report with source code available
fn format_ariadne_with_source(location: &ItemLocation, source: &str, message: &str) -> String {
    let offset = span::span_to_offset(source, &location.span);
    let length = span::span_length(source, &location.span);

    let report_builder = Report::build(ReportKind::Error, location.filename.as_ref(), offset)
        .with_message(message)
        .with_label(
            Label::new((location.filename.as_ref(), offset..offset + length))
                .with_message("error occurred here")
                .with_color(ariadne::Color::Red),
        );

    let report = report_builder.finish();

    let mut buffer = Vec::new();
    report
        .write(
            (location.filename.as_ref(), Source::from(source)),
            &mut buffer,
        )
        .expect("writing to Vec should not fail");
    String::from_utf8_lossy(&buffer).to_string()
}

/// Format an Ariadne report without source code (just location note)
fn format_ariadne_without_source(location: &ItemLocation, message: &str) -> String {
    let placeholder_filename = location.filename.as_ref();
    let report_builder =
        Report::<(&str, std::ops::Range<usize>)>::build(ReportKind::Error, placeholder_filename, 0)
            .with_message(message)
            .with_note(format!(
                "Error location: {}:{}:{}",
                location.filename, location.span.start.line, location.span.start.column
            ));

    let report = report_builder.finish();

    let mut buffer = Vec::new();
    report
        .write((placeholder_filename, Source::from("")), &mut buffer)
        .expect("writing to Vec should not fail");
    String::from_utf8_lossy(&buffer).to_string()
}

/// Format an Ariadne report with no location information at all
fn format_ariadne_simple(message: &str) -> String {
    let report = Report::<(&str, std::ops::Range<usize>)>::build(ReportKind::Error, "", 0)
        .with_message(message)
        .finish();

    let mut buffer = Vec::new();
    report
        .write(("", Source::from("")), &mut buffer)
        .expect("writing to Vec should not fail");
    String::from_utf8_lossy(&buffer).to_string()
}

impl fmt::Display for BackendError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Add location prefix if available
        if let Some(context) = self.error_context() {
            write!(f, "{}", span::format_error_location(context))?;
        }
        write!(f, "{}", self.error_message())
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
