use std::fmt;

/// Backend code generation errors
#[derive(Debug)]
pub enum BackendError {
    /// IO error during code generation
    Io(std::io::Error),
    /// Formatting error (e.g., rustfmt failed)
    Formatting(String),
    /// Code generation error
    CodeGen(String),
    /// Syntax error in generated code
    Syntax(syn::Error),
}

impl fmt::Display for BackendError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BackendError::Io(err) => write!(f, "IO error: {}", err),
            BackendError::Formatting(msg) => write!(f, "Formatting error: {}", msg),
            BackendError::CodeGen(msg) => write!(f, "Code generation error: {}", msg),
            BackendError::Syntax(err) => write!(f, "Syntax error: {}", err),
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

impl From<crate::semantic::SemanticError> for BackendError {
    fn from(err: crate::semantic::SemanticError) -> Self {
        BackendError::CodeGen(err.to_string())
    }
}

/// Result type for backend operations
pub type Result<T> = std::result::Result<T, BackendError>;
