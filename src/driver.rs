use std::path::Path;

use crate::semantic_analysis::semantic_state::SemanticState;

pub fn build(out_dir: Option<&Path>) -> anyhow::Result<()> {
    println!("cargo:rerun-if-changed=types");

    let cargo_out_dir = std::env::var("OUT_DIR")?;
    let out_dir = out_dir.unwrap_or(std::path::Path::new(&cargo_out_dir));

    let pointer_size = std::env::var("CARGO_CFG_TARGET_POINTER_WIDTH")?.parse::<usize>()? / 8;
    let mut semantic_state = SemanticState::new(pointer_size);

    for path in glob::glob("types/**/*.pyx")?.filter_map(Result::ok) {
        semantic_state.add_file(std::path::Path::new("types"), &path)?;
    }

    let resolved_semantic_state = semantic_state.build()?;
    for (key, module) in resolved_semantic_state.modules() {
        super::backends::rust::write_module(out_dir, key, &resolved_semantic_state, module)?;
    }

    Ok(())
}
