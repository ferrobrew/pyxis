use std::path::Path;

use anyhow::Context as _;

pub mod backends;
pub mod config;
pub mod grammar;
pub mod parser;
pub mod semantic;

pub(crate) mod util;

pub fn build(in_dir: &Path, out_dir: &Path) -> anyhow::Result<()> {
    let config = config::Config::load(&in_dir.join("pyxis.toml"))
        .context("Failed to load config for project")?;
    let mut semantic_state = semantic::SemanticState::new(config.project.pointer_size);

    for path in glob::glob(&format!("{}/**/*.pyxis", in_dir.display()))?.filter_map(Result::ok) {
        semantic_state.add_file(Path::new(&in_dir), &path)?;
    }

    let resolved_semantic_state = semantic_state.build()?;
    for (key, module) in resolved_semantic_state.modules() {
        backends::rust::write_module(Path::new(&out_dir), key, &resolved_semantic_state, module)?;
    }

    Ok(())
}

pub fn build_script(out_dir: Option<&Path>) -> anyhow::Result<()> {
    println!("cargo:rerun-if-changed=types");

    let cargo_out_dir = std::env::var("OUT_DIR")?;
    let out_dir = out_dir.unwrap_or(Path::new(&cargo_out_dir));

    build(Path::new("types"), out_dir)
}
