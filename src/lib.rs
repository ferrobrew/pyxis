use std::path::Path;

pub mod backends;
pub mod config;
pub mod grammar;
pub mod parser;
pub mod pretty_print;
pub mod semantic;
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
    Config(String),
    Glob(glob::PatternError),
    Io(std::io::Error),
    Backend(String),
}

impl std::fmt::Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildError::Semantic(err) => write!(f, "{}", err),
            BuildError::Config(msg) => write!(f, "Config error: {}", msg),
            BuildError::Glob(err) => write!(f, "Glob pattern error: {}", err),
            BuildError::Io(err) => write!(f, "IO error: {}", err),
            BuildError::Backend(msg) => write!(f, "Backend error: {}", msg),
        }
    }
}

impl std::error::Error for BuildError {}

impl From<semantic::SemanticError> for BuildError {
    fn from(err: semantic::SemanticError) -> Self {
        BuildError::Semantic(err)
    }
}

impl From<glob::PatternError> for BuildError {
    fn from(err: glob::PatternError) -> Self {
        BuildError::Glob(err)
    }
}

impl From<std::io::Error> for BuildError {
    fn from(err: std::io::Error) -> Self {
        BuildError::Io(err)
    }
}

impl From<anyhow::Error> for BuildError {
    fn from(err: anyhow::Error) -> Self {
        BuildError::Backend(err.to_string())
    }
}

pub fn build(in_dir: &Path, out_dir: &Path, backend: Backend) -> Result<(), BuildError> {
    let config = config::Config::load(&in_dir.join("pyxis.toml"))
        .map_err(|e| BuildError::Config(e.to_string()))?;
    let mut semantic_state = semantic::SemanticState::new(config.project.pointer_size);

    for path in glob::glob(&format!("{}/**/*.pyxis", in_dir.display()))?.filter_map(Result::ok) {
        semantic_state.add_file(in_dir, &path)?;
    }

    let resolved_semantic_state = semantic_state.build()?;
    match backend {
        Backend::Rust => {
            for (key, module) in resolved_semantic_state.modules() {
                backends::rust::write_module(out_dir, key, &resolved_semantic_state, module)?;
            }
        }
        #[cfg(feature = "json")]
        Backend::Json => {
            backends::json::build(out_dir, &resolved_semantic_state, &config.project.name)?;
        }
    }

    Ok(())
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
        .ok_or_else(|| BuildError::Config("OUT_DIR not set".to_string()))?;

    build(in_dir, &out_dir, Backend::Rust)
}
