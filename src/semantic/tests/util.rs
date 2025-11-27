use crate::{
    grammar::test_aliases::*,
    semantic::{
        Module,
        error::SemanticError,
        semantic_state::{ResolvedSemanticState, SemanticState},
        types::test_aliases::*,
    },
};

use pretty_assertions::assert_eq;

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

pub fn build_state(
    module: &M,
    module_path: &IP,
) -> crate::semantic::error::Result<ResolvedSemanticState> {
    let mut semantic_state = SemanticState::new(pointer_size());
    semantic_state.add_module(module, module_path, None)?;
    semantic_state.build()
}

#[track_caller]
pub fn assert_ast_produces_type_definitions(
    module: M,
    type_definitions: impl IntoIterator<Item = SID>,
) -> Module {
    let module_path = IP::from("test");

    let state = build_state(&module, &module_path).unwrap();
    let created_module = state.modules().get(&module_path).unwrap();
    let type_registry = state.type_registry();

    let mut expected_type_definitions: Vec<_> = type_definitions.into_iter().collect();
    let mut created_type_definitions: Vec<_> =
        created_module.definitions(type_registry).cloned().collect();

    expected_type_definitions.sort_by_key(|t| t.path.clone());
    created_type_definitions.sort_by_key(|t| t.path.clone());

    // Strip location information for comparison (tests shouldn't care about exact spans)
    use crate::span::StripLocations;
    let created_type_definitions: Vec<_> = created_type_definitions
        .into_iter()
        .map(|def| def.strip_locations())
        .collect();

    assert_eq!(created_type_definitions, expected_type_definitions);

    created_module.clone()
}

/// Assert that the given AST produces a failure matching the given predicate.
/// The predicate receives the error and should return true if it matches expectations.
#[track_caller]
pub fn assert_ast_produces_error(module: M, predicate: impl FnOnce(&SemanticError) -> bool) {
    let err = build_state(&module, &IP::from("test")).unwrap_err();
    assert!(
        predicate(&err),
        "Error did not match predicate. Got error: {err:?}"
    );
}

/// Assert that the given AST produces a specific error variant.
/// This is a convenience macro for common error matching patterns.
#[macro_export]
macro_rules! assert_error_matches {
    ($module:expr, $pattern:pat $(if $guard:expr)?) => {
        $crate::semantic::tests::util::assert_ast_produces_error($module, |err| {
            matches!(err, $pattern $(if $guard)?)
        })
    };
}

pub fn unknown(size: usize) -> ST {
    ST::raw("u8").array(size)
}

pub fn pad_up_to_8_region() -> SR {
    SR::field(
        (SV::Private, format!("_field_{}", pointer_size())),
        ST::array(ST::raw("u8"), 8 - pointer_size()),
    )
}

pub fn filter_out_empty_regions(regions: impl IntoIterator<Item = SR>) -> Vec<SR> {
    regions
        .into_iter()
        .filter(|r| !matches!(&r.type_ref, ST::Array(_, 0)))
        .collect()
}
