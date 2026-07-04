use std::fmt;

use crate::{
    grammar::ItemPath,
    semantic::SemanticError,
    source_store::FileStore,
    span::{self, ItemLocation},
};
use ariadne::{Label, Report, ReportKind, Source};

/// Reason why type code generation failed
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeCodeGenFailedKind {
    /// Type was not resolved
    TypeNotResolved,
    /// Failed to get last component of item path
    EmptyItemPath,
}

impl fmt::Display for TypeCodeGenFailedKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeCodeGenFailedKind::TypeNotResolved => write!(f, "type was not resolved"),
            TypeCodeGenFailedKind::EmptyItemPath => {
                write!(f, "failed to get last component of item path")
            }
        }
    }
}

/// Reason why field code generation failed
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldCodeGenFailedKind {
    /// Field name was not present
    FieldNameNotPresent,
}

impl fmt::Display for FieldCodeGenFailedKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FieldCodeGenFailedKind::FieldNameNotPresent => write!(f, "field name not present"),
        }
    }
}

/// Backend code generation errors. Per-backend failure modes live in
/// their own enums (e.g. [`CppBackendError`]) and surface via the
/// matching `BackendError` variant, gated on the backend's feature
/// flag so disabled backends impose no compile-time cost.
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
        kind: TypeCodeGenFailedKind,
        location: ItemLocation,
    },
    /// Failed to generate code for a specific field
    FieldCodeGenFailed {
        type_path: ItemPath,
        field_name: String,
        kind: FieldCodeGenFailedKind,
        location: ItemLocation,
    },
    /// Generation was requested for a backend whose codegen isn't compiled
    /// into this build (its cargo feature is off). Parsing defs that mention
    /// the backend is always fine; only emitting its output needs the feature.
    BackendNotCompiledIn(crate::Backend),
    /// C++-backend-specific error. Closed-world (not type-erased) so
    /// callers can pattern-match on the exact failure mode; only present
    /// when the `cpp` feature is enabled.
    #[cfg(feature = "cpp")]
    Cpp(CppBackendError),
}

/// C++-backend-specific failure modes. Lives here (instead of inside
/// `backends::cpp`) so [`BackendError`] can refer to it without a
/// cyclic import, but the rest of the type system treats it as cpp's
/// concern — adding a new variant doesn't touch any other backend.
#[cfg(feature = "cpp")]
#[derive(Debug)]
pub enum CppBackendError {
    /// The cpp backend's dependency analysis found a strongly-connected
    /// component on FullDef edges — a real layout cycle that no amount
    /// of forward-decling can resolve. The cycle is reported as the
    /// list of items (or modules) that form it, in traversal order.
    LayoutCycle {
        scope: CppLayoutCycleScope,
        cycle: Vec<ItemPath>,
        location: ItemLocation,
    },
}

#[cfg(feature = "cpp")]
impl CppBackendError {
    fn error_message(&self) -> String {
        match self {
            CppBackendError::LayoutCycle { scope, cycle, .. } => {
                let chain = cycle
                    .iter()
                    .map(|p| format!("`{p}`"))
                    .collect::<Vec<_>>()
                    .join(" → ");
                format!(
                    "C++ backend: detected a {scope} layout cycle on FullDef edges: {chain}. \
                     A real value-cycle can't be resolved by forward declarations; \
                     break the cycle by introducing a pointer/reference indirection or \
                     by extracting a shared type into a separate module."
                )
            }
        }
    }

    fn location(&self) -> Option<&ItemLocation> {
        match self {
            CppBackendError::LayoutCycle { location, .. } => Some(location),
        }
    }
}

/// What kind of dependency graph the cycle was detected in.
#[cfg(feature = "cpp")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CppLayoutCycleScope {
    /// Cycle inside a single module's items.
    IntraModule,
    /// Cycle between modules' aggregated FullDef dependencies.
    CrossModule,
}

#[cfg(feature = "cpp")]
impl fmt::Display for CppLayoutCycleScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CppLayoutCycleScope::IntraModule => write!(f, "intra-module"),
            CppLayoutCycleScope::CrossModule => write!(f, "cross-module"),
        }
    }
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
                type_path, kind, ..
            } => {
                format!("Failed to generate code for type `{type_path}`: {kind}")
            }
            BackendError::FieldCodeGenFailed {
                type_path,
                field_name,
                kind,
                ..
            } => {
                format!(
                    "Failed to generate code for field `{field_name}` of type `{type_path}`: {kind}"
                )
            }
            BackendError::BackendNotCompiledIn(backend) => {
                format!(
                    "the `{backend}` backend is not compiled into this build; \
                     enable its cargo feature to generate `{backend}` output"
                )
            }
            #[cfg(feature = "cpp")]
            BackendError::Cpp(err) => err.error_message(),
        }
    }

    fn location(&self) -> Option<&ItemLocation> {
        match self {
            BackendError::TypeCodeGenFailed { location, .. }
            | BackendError::FieldCodeGenFailed { location, .. } => Some(location),
            #[cfg(feature = "cpp")]
            BackendError::Cpp(err) => err.location(),
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
