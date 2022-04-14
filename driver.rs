use super::semantic_analysis::SemanticState;

pub fn build() -> anyhow::Result<()> {
    let pointer_size = std::env::var("CARGO_CFG_TARGET_POINTER_WIDTH")?.parse::<usize>()? / 8;
    let mut semantic_state = SemanticState::new(pointer_size);

    println!("cargo:rerun-if-changed=types");
    for path in glob::glob("types/**/*.rstl")?.filter_map(Result::ok) {
        semantic_state.add_file(&path)?;
    }

    let resolved_semantic_state = semantic_state.build()?;
    for (key, module) in resolved_semantic_state.modules() {
        super::backends::rust::write_module(key, &resolved_semantic_state, module)?;
    }

    Ok(())
}
