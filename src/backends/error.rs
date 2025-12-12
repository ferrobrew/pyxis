use std::fmt;

use crate::{
    grammar::ItemPath,
    semantic::SemanticError,
    source_store::FileStore,
    span::{self, ItemLocation},
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
        location: ItemLocation,
    },
    /// Failed to generate code for a specific field
    FieldCodeGenFailed {
        type_path: ItemPath,
        field_name: String,
        reason: String,
        location: ItemLocation,
    },
    /// Failed to generate vftable code
    VftableCodeGenFailed {
        type_path: ItemPath,
        reason: String,
        location: ItemLocation,
    },
}
impl fmt::Display for BackendError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Add location prefix if available
        if let Some(location) = self.location() {
            write!(f, "{location}: ")?;
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
impl BackendError {
    /// Returns the core error message without location prefix
    fn error_message(&self) -> String {
        match self {
            BackendError::Io { error, context } => {
                format!("IO error: {error} ({context})")
            }
            BackendError::Formatting(msg) => format!("Formatting error: {msg}"),
            BackendError::Syntax(err) => format!("Syntax error: {err}"),
            BackendError::Semantic(err) => err.to_string(),
            BackendError::TypeCodeGenFailed {
                type_path, reason, ..
            } => {
                format!("Failed to generate code for type `{type_path}`: {reason}")
            }
            BackendError::FieldCodeGenFailed {
                type_path,
                field_name,
                reason,
                ..
            } => {
                format!(
                    "Failed to generate code for field `{field_name}` of type `{type_path}`: {reason}"
                )
            }
            BackendError::VftableCodeGenFailed {
                type_path, reason, ..
            } => {
                format!("Failed to generate vftable code for type `{type_path}`: {reason}")
            }
        }
    }

    fn location(&self) -> Option<&ItemLocation> {
        match self {
            BackendError::TypeCodeGenFailed {
                location: context, ..
            }
            | BackendError::FieldCodeGenFailed {
                location: context, ..
            }
            | BackendError::VftableCodeGenFailed {
                location: context, ..
            } => Some(context),
            _ => None,
        }
    }

    /// Format the error using ariadne with the provided file store
    pub fn format_with_ariadne(&self, file_store: &FileStore) -> String {
        // For semantic errors, delegate to SemanticError's format_with_ariadne
        if let BackendError::Semantic(err) = self {
            return err.format_with_ariadne(file_store);
        }

        let message = self.error_message();

        let (filename, offset, length, source) = if let Some(location) = self.location() {
            let filename = file_store.filename(location.file_id);
            if let Some(source) = file_store.source(location.file_id) {
                (
                    filename,
                    span::span_to_offset(&source, &location.span),
                    span::span_length(&source, &location.span),
                    source,
                )
            } else {
                (filename, 0, 0, String::new())
            }
        } else {
            ("<unknown>", 0, 0, String::new())
        };

        let report_builder = Report::build(ReportKind::Error, (filename, offset..offset + length))
            .with_message(message)
            .with_label(
                Label::new((filename, offset..offset + length))
                    .with_message("error occurred here")
                    .with_color(ariadne::Color::Red),
            );

        let report = report_builder.finish();

        let mut buffer = Vec::new();
        report
            .write((filename, Source::from(source)), &mut buffer)
            .expect("writing to Vec should not fail");
        String::from_utf8_lossy(&buffer).to_string()
    }
}

/// Result type for backend operations
pub type Result<T> = std::result::Result<T, BackendError>;
