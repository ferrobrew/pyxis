#![allow(clippy::result_large_err)]
#![allow(clippy::collapsible_if)]
#![deny(clippy::uninlined_format_args)]

use std::path::Path;

pub mod backends;
pub mod config;
pub mod grammar;
pub mod parser;
pub mod pretty_print;
pub mod semantic;
pub mod source_store;
pub mod span;
pub mod tokenizer;

pub(crate) mod util;

// Re-export semantic error for convenience
pub use semantic::SemanticError;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Backend {
    Rust,
    #[cfg(feature = "json")]
    Json,
    Cpp,
}

impl Backend {
    /// Lower-case keyword name, used in `backend foo { ... }` syntax,
    /// `#[cfg(backend = "foo")]` predicates, and the driver's `--backend`
    /// flag. The single source of truth - everything else parses or
    /// formats through here, so we don't end up sprinkling string
    /// literals around the codebase.
    pub const fn name(self) -> &'static str {
        match self {
            Backend::Rust => "rust",
            #[cfg(feature = "json")]
            Backend::Json => "json",
            Backend::Cpp => "cpp",
        }
    }

    /// All variants in canonical order. Used by parser-error messages
    /// to enumerate valid backend names. `Json` is conditional on the
    /// `json` feature - without it, parsing `backend json { ... }`
    /// fails as an unknown backend.
    pub const ALL: &'static [Backend] = &[
        Backend::Rust,
        #[cfg(feature = "json")]
        Backend::Json,
        Backend::Cpp,
    ];

    /// Parse a backend keyword. `None` if the input doesn't match any
    /// known backend - callers translate to a typed error appropriate
    /// to their layer (parse error, semantic error, driver CLI error).
    pub fn from_name(name: &str) -> Option<Backend> {
        Self::ALL.iter().copied().find(|b| b.name() == name)
    }
}

impl std::fmt::Display for Backend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

impl std::str::FromStr for Backend {
    type Err = UnknownBackend;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Backend::from_name(s).ok_or_else(|| UnknownBackend(s.to_string()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownBackend(pub String);
impl std::fmt::Display for UnknownBackend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "unknown backend `{}`; valid backends are: {}",
            self.0,
            Backend::ALL
                .iter()
                .map(|b| b.name())
                .collect::<Vec<_>>()
                .join(", "),
        )
    }
}
impl std::error::Error for UnknownBackend {}

#[cfg(test)]
impl crate::span::StripLocations for Backend {
    fn strip_locations(&self) -> Self {
        *self
    }
}

#[cfg(test)]
mod backend_tests {
    use super::Backend;
    use std::str::FromStr;

    #[test]
    fn name_round_trips() {
        for &b in Backend::ALL {
            assert_eq!(Backend::from_name(b.name()), Some(b));
            assert_eq!(b.to_string(), b.name());
        }
    }

    #[test]
    fn unknown_name_yields_none() {
        assert_eq!(Backend::from_name("foobar"), None);
        assert_eq!(Backend::from_name("RUST"), None); // case-sensitive
        assert_eq!(Backend::from_name(""), None);
    }

    #[test]
    fn from_str_yields_unknown_backend_error() {
        let err = Backend::from_str("foobar").unwrap_err();
        assert_eq!(err.0, "foobar");
        // Display includes the list of valid names.
        let msg = err.to_string();
        assert!(msg.contains("foobar"));
        assert!(msg.contains("rust"));
        assert!(msg.contains("cpp"));
    }

    #[test]
    fn known_variants_are_all_in_all() {
        // ALL must be exhaustive over the enum so `from_name` matches
        // every variant.
        let all_names: Vec<_> = Backend::ALL.iter().map(|b| b.name()).collect();
        assert!(all_names.contains(&"rust"));
        assert!(all_names.contains(&"cpp"));
        #[cfg(feature = "json")]
        assert!(all_names.contains(&"json"));
    }

    #[cfg(not(feature = "json"))]
    #[test]
    fn json_excluded_when_feature_disabled() {
        assert_eq!(Backend::from_name("json"), None);
    }
}

/// Build error type that wraps different error sources
#[derive(Debug)]
pub enum BuildError {
    Semantic(semantic::SemanticError),
    Config(config::ConfigError),
    Glob(glob::PatternError),
    Io {
        error: std::io::Error,
        context: String,
    },
    Parser(parser::ParseError),
    Backend(backends::BackendError),
}

impl std::fmt::Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildError::Semantic(err) => write!(f, "{err}"),
            BuildError::Config(err) => write!(f, "{err}"),
            BuildError::Glob(err) => write!(f, "Glob pattern error: {err}"),
            BuildError::Io { error, context } => write!(f, "IO error: {error} ({context})"),
            BuildError::Parser(err) => write!(f, "{err}"),
            BuildError::Backend(err) => write!(f, "{err}"),
        }
    }
}

impl std::error::Error for BuildError {}

impl From<semantic::SemanticError> for BuildError {
    fn from(err: semantic::SemanticError) -> Self {
        BuildError::Semantic(err)
    }
}

impl From<config::ConfigError> for BuildError {
    fn from(err: config::ConfigError) -> Self {
        BuildError::Config(err)
    }
}

impl From<glob::PatternError> for BuildError {
    fn from(err: glob::PatternError) -> Self {
        BuildError::Glob(err)
    }
}

impl From<backends::BackendError> for BuildError {
    fn from(err: backends::BackendError) -> Self {
        BuildError::Backend(err)
    }
}

impl From<parser::ParseError> for BuildError {
    fn from(err: parser::ParseError) -> Self {
        BuildError::Parser(err)
    }
}

impl BuildError {
    /// Returns the core error message without location prefix
    fn error_message(&self) -> String {
        match self {
            BuildError::Semantic(err) => err.to_string(),
            BuildError::Config(err) => err.to_string(),
            BuildError::Glob(err) => format!("Glob pattern error: {err}"),
            BuildError::Io { error, context } => format!("IO error: {error} ({context})"),
            BuildError::Parser(err) => err.to_string(),
            BuildError::Backend(err) => err.to_string(),
        }
    }

    /// Format the error with rich diagnostics using ariadne
    pub fn format_with_ariadne(&self, file_store: &source_store::FileStore) -> String {
        use ariadne::{Report, ReportKind, Source};

        match self {
            // Delegate to inner types that have their own ariadne formatting
            BuildError::Semantic(err) => err.format_with_ariadne(file_store),
            BuildError::Parser(err) => err.format_with_ariadne(file_store),
            BuildError::Backend(err) => err.format_with_ariadne(file_store),
            // Other error types don't have location information - create simple ariadne report
            BuildError::Config(_) | BuildError::Glob(_) | BuildError::Io { .. } => {
                let message = self.error_message();
                let report =
                    Report::<(&str, std::ops::Range<usize>)>::build(ReportKind::Error, ("", 0..0))
                        .with_message(&message)
                        .finish();

                let mut buffer = Vec::new();
                report
                    .write(("", Source::from("")), &mut buffer)
                    .expect("writing to Vec should not fail");
                String::from_utf8_lossy(&buffer).to_string()
            }
        }
    }
}

pub fn build(in_dir: &Path, out_dir: &Path, backend: Backend) -> Result<(), BuildError> {
    let mut file_store = source_store::FileStore::new();
    build_with_store(in_dir, out_dir, backend, &mut file_store)
}

/// Build with an externally-managed FileStore.
///
/// This allows callers to access the file store for error formatting
/// even when the build fails.
pub fn build_with_store(
    in_dir: &Path,
    out_dir: &Path,
    backend: Backend,
    file_store: &mut source_store::FileStore,
) -> Result<(), BuildError> {
    let config = config::Config::load(&in_dir.join("pyxis.toml"))?;

    let mut semantic_state = semantic::SemanticState::new(config.project.pointer_size);

    for path in glob::glob(&format!("{}/**/*.pyxis", in_dir.display()))?.filter_map(Result::ok) {
        semantic_state.add_file(file_store, in_dir, &path)?;
    }

    let resolved_semantic_state = semantic_state.build()?;

    match backend {
        Backend::Rust => {
            for (key, module) in resolved_semantic_state.modules() {
                backends::rust::write_module(out_dir, key, &resolved_semantic_state, module)?;
            }
            Ok(())
        }
        #[cfg(feature = "json")]
        Backend::Json => {
            backends::json::build(
                out_dir,
                &resolved_semantic_state,
                &config.project.name,
                file_store,
            )?;
            Ok(())
        }
        Backend::Cpp => {
            backends::cpp::build(out_dir, &resolved_semantic_state, &config.project)?;
            Ok(())
        }
    }
}

/// Helper for running Pyxis against `in_dir` to produce `out_dir`. If `out_dir` is not provided, it will default to `OUT_DIR`.
pub fn build_script(in_dir: &Path, out_dir: Option<&Path>) -> Result<(), BuildError> {
    println!("cargo:rerun-if-changed={}", in_dir.display());

    let out_dir = out_dir
        .map(|p| p.to_path_buf())
        .or_else(|| {
            std::env::var("OUT_DIR")
                .ok()
                .map(|s| Path::new(&s).to_path_buf())
        })
        .ok_or_else(|| {
            BuildError::Config(config::ConfigError::Other("OUT_DIR not set".to_string()))
        })?;

    let mut file_store = source_store::FileStore::new();
    let result = build_with_store(in_dir, &out_dir, Backend::Rust, &mut file_store);

    if let Err(ref err) = result {
        let formatted = err.format_with_ariadne(&file_store);
        eprintln!("{formatted}");
    }

    result
}
