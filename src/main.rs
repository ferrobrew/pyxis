//! Very rudimentary CLI application to run Pyxis with the Rust backend
//! to test what the output will look like without having to look at it
//! in situ

use anyhow::Context;

use pyxis::{backends, semantic_analysis::SemanticState};

fn main() -> anyhow::Result<()> {
    // get input directory from first argument
    let in_dir = std::env::args()
        .nth(1)
        .context("No input directory provided")?;

    let out_dir = std::path::Path::new("out");
    let _ = std::fs::remove_dir_all(out_dir);
    std::fs::create_dir(out_dir)?;

    // assume hardcoded pointer size for now
    let pointer_size = 4;
    let mut semantic_state = SemanticState::new(pointer_size);

    for path in glob::glob(&format!("{in_dir}/**/*.pyx"))?.filter_map(Result::ok) {
        semantic_state.add_file(std::path::Path::new(&in_dir), &path)?;
    }

    let resolved_semantic_state = semantic_state.build()?;
    for (key, module) in resolved_semantic_state.modules() {
        backends::rust::write_module(out_dir, key, &resolved_semantic_state, module)?;
    }

    Ok(())
}
