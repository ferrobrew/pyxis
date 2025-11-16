use std::fmt;

use crate::{grammar::ItemPath, semantic::SemanticError};

/// Backend code generation errors
#[derive(Debug)]
pub enum BackendError {
    /// IO error during code generation
    Io(std::io::Error),
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
    },
    /// Failed to generate code for a specific field
    FieldCodeGenFailed {
        type_path: ItemPath,
        field_name: String,
        reason: String,
    },
    /// Failed to generate vftable code
    VftableCodeGenFailed {
        type_path: ItemPath,
        reason: String,
    },
}

impl fmt::Display for BackendError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BackendError::Io(err) => write!(f, "IO error: {}", err),
            BackendError::Formatting(msg) => write!(f, "Formatting error: {}", msg),
            BackendError::Syntax(err) => write!(f, "Syntax error: {}", err),
            BackendError::Semantic(err) => write!(f, "{}", err),
            BackendError::TypeCodeGenFailed { type_path, reason } => {
                write!(f, "Failed to generate code for type `{}`: {}", type_path, reason)
            }
            BackendError::FieldCodeGenFailed {
                type_path,
                field_name,
                reason,
            } => {
                write!(
                    f,
                    "Failed to generate code for field `{}` of type `{}`: {}",
                    field_name, type_path, reason
                )
            }
            BackendError::VftableCodeGenFailed { type_path, reason } => {
                write!(
                    f,
                    "Failed to generate vftable code for type `{}`: {}",
                    type_path, reason
                )
            }
        }
    }
}

impl std::error::Error for BackendError {}

impl From<std::io::Error> for BackendError {
    fn from(err: std::io::Error) -> Self {
        BackendError::Io(err)
    }
}

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
