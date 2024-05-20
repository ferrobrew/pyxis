//! Very rudimentary CLI application to run Pyxis with the Rust backend
//! to test what the output will look like without having to look at it
//! in situ

use std::path::Path;

use anyhow::Context;

use pyxis::{backends, semantic_analysis::SemanticState};

fn main() -> anyhow::Result<()> {
    let mut args = std::env::args();
    args.next(); // skip the executable name
    let in_dir = args.next().context("No input directory provided")?;
    let out_dir = args.next().unwrap_or_else(|| String::from("out"));

    let _ = std::fs::remove_dir_all(&out_dir);
    std::fs::create_dir(&out_dir)?;

    // assume hardcoded pointer size for now
    let pointer_size = 4;
    let mut semantic_state = SemanticState::new(pointer_size);

    for path in glob::glob(&format!("{in_dir}/**/*.pyx"))?.filter_map(Result::ok) {
        semantic_state.add_file(Path::new(&in_dir), &path)?;
    }

    let resolved_semantic_state = semantic_state.build()?;
    for (key, module) in resolved_semantic_state.modules() {
        backends::rust::write_module(Path::new(&out_dir), key, &resolved_semantic_state, module)?;
    }

    Ok(())
}
