use crate::{
    grammar::test_aliases::*,
    semantic_analysis::{
        semantic_state::{ResolvedSemanticState, SemanticState},
        types::test_aliases::*,
    },
};

pub fn pointer_size() -> usize {
    static POINTER_SIZE: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
    *POINTER_SIZE.get_or_init(|| {
        std::env::var("PYXIS_TEST_POINTER_SIZE")
            .ok()
            .map(|s| {
                s.parse()
                    .expect("PYXIS_TEST_POINTER_SIZE must be a valid number")
            })
            .unwrap_or_else(|| 4)
    })
}

pub fn build_state(module: &M, module_path: &IP) -> anyhow::Result<ResolvedSemanticState> {
    let mut semantic_state = SemanticState::new(pointer_size());
    semantic_state.add_module(module, module_path)?;
    semantic_state.build()
}

#[track_caller]
pub fn assert_ast_produces_type_definitions(
    module: M,
    type_definitions: impl IntoIterator<Item = SID>,
) {
    let module_path = IP::from("test");

    let state = build_state(&module, &module_path).unwrap();
    let created_module = state.modules().get(&module_path).unwrap();
    let type_registry = state.type_registry();

    let mut expected_type_definitions: Vec<_> = type_definitions.into_iter().collect();
    let mut created_type_definitions: Vec<_> =
        created_module.definitions(type_registry).cloned().collect();

    expected_type_definitions.sort_by_key(|t| t.path.clone());
    created_type_definitions.sort_by_key(|t| t.path.clone());

    assert_eq!(created_type_definitions, expected_type_definitions);
}

#[track_caller]
pub fn assert_ast_produces_failure(module: M, failure: &str) {
    assert_eq!(
        build_state(&module, &IP::from("test"))
            .unwrap_err()
            .to_string(),
        failure
    );
}

pub fn unknown(size: usize) -> ST {
    ST::raw("u8").array(size)
}
