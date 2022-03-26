use super::semantic_analysis::SemanticState;

use itertools::Itertools;

pub fn build() -> anyhow::Result<()> {
    let pointer_size = std::env::var("CARGO_CFG_TARGET_POINTER_WIDTH")?.parse::<usize>()? / 8;
    let mut semantic_state = SemanticState::new(pointer_size);

    println!("cargo:rerun-if-changed=types");
    for path in glob::glob("types/**/*.rstl")?.filter_map(Result::ok) {
        semantic_state.add_file(&path)?;
    }
    semantic_state.build()?;

    for (key, group) in semantic_state
        .type_registry()
        .resolved()
        .iter()
        .sorted()
        .group_by(|t| t.parent())
        .into_iter()
        .filter_map(|(key, group)| key.map(|k| (k, group)))
    {
        super::backends::rust::write_module(key, group, &semantic_state)?;
    }

    Ok(())
}
