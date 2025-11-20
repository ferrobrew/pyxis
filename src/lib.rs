#![allow(clippy::result_large_err)]
#![allow(clippy::collapsible_if)]

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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Backend {
    Rust,
    #[cfg(feature = "json")]
    Json,
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
    Backend(backends::BackendError),
}

impl std::fmt::Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildError::Semantic(err) => write!(f, "{}", err),
            BuildError::Config(err) => write!(f, "{}", err),
            BuildError::Glob(err) => write!(f, "Glob pattern error: {}", err),
            BuildError::Io { error, context } => write!(f, "IO error: {} ({})", error, context),
            BuildError::Backend(err) => write!(f, "{}", err),
        }
    }
}

impl std::error::Error for BuildError {}

impl BuildError {
    /// Format the error using ariadne with the provided source store
    pub fn format_with_ariadne(&self, source_store: &mut dyn source_store::SourceStore) -> String {
        match self {
            BuildError::Semantic(err) => err.format_with_ariadne(source_store),
            _ => self.to_string(),
        }
    }
}

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

/// Build result that includes source cache for error reporting
pub type BuildResult = Result<std::collections::BTreeMap<String, String>, (BuildError, std::collections::BTreeMap<String, String>)>;

pub fn build(in_dir: &Path, out_dir: &Path, backend: Backend) -> BuildResult {
    let config = match config::Config::load(&in_dir.join("pyxis.toml")) {
        Ok(c) => c,
        Err(e) => return Err((e.into(), std::collections::BTreeMap::new())),
    };

    let mut semantic_state = semantic::SemanticState::new(config.project.pointer_size);

    for path in glob::glob(&format!("{}/**/*.pyxis", in_dir.display()))
        .map_err(|e| (BuildError::from(e), std::collections::BTreeMap::new()))?
        .filter_map(Result::ok)
    {
        if let Err(e) = semantic_state.add_file(in_dir, &path) {
            // Clone source cache before returning error
            let source_cache = semantic_state.source_cache().clone();
            return Err((e.into(), source_cache));
        }
    }

    // Clone the source cache before building (which consumes semantic_state)
    let source_cache = semantic_state.source_cache().clone();

    let resolved_semantic_state = match semantic_state.build() {
        Ok(state) => state,
        Err(e) => return Err((e.into(), source_cache)),
    };

    let result = match backend {
        Backend::Rust => {
            for (key, module) in resolved_semantic_state.modules() {
                if let Err(e) = backends::rust::write_module(out_dir, key, &resolved_semantic_state, module) {
                    return Err((e.into(), source_cache));
                }
            }
            Ok(())
        }
        #[cfg(feature = "json")]
        Backend::Json => {
            backends::json::build(out_dir, &resolved_semantic_state, &config.project.name)
                .map_err(|e| (e.into(), source_cache.clone()))
        }
    };

    result.map(|_| source_cache)
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

    match build(in_dir, &out_dir, Backend::Rust) {
        Ok(_) => Ok(()),
        Err((err, mut source_cache)) => {
            // Format error with ariadne before returning
            let formatted = err.format_with_ariadne(&mut source_cache);
            eprintln!("{}", formatted);
            Err(err)
        }
    }
}
