use std::path::Path;

use anyhow::Context as _;

pub mod backends;
pub mod config;
pub mod grammar;
pub mod parser;
pub mod pretty_print;
pub mod semantic;
pub mod span;
pub mod tokenizer;

pub(crate) mod util;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Backend {
    Rust,
}

pub fn build(in_dir: &Path, out_dir: &Path, backend: Backend) -> anyhow::Result<()> {
    let config = config::Config::load(&in_dir.join("pyxis.toml"))
        .context("Failed to load config for project")?;
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
    }

    Ok(())
}

/// Helper for running Pyxis against `in_dir` to produce `out_dir`. If `out_dir` is not provided, it will default to `OUT_DIR`.
pub fn build_script(in_dir: &Path, out_dir: Option<&Path>) -> anyhow::Result<()> {
    println!("cargo:rerun-if-changed={}", in_dir.display());

    build(
        in_dir,
        out_dir.unwrap_or(Path::new(&std::env::var("OUT_DIR")?)),
        Backend::Rust,
    )
}
