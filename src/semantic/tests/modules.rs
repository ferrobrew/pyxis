//! Tests for the module tree: folder-module synthesis and duplicate
//! detection. (The file-path -> module-path mapping for `mod.pyxis` is
//! covered by `parser::paths`.)

use crate::{
    grammar::test_aliases::*,
    semantic::{
        PyxisDatabaseImpl, SourceFile, SourceSet, analyze, builder::SemanticBuilder,
        error::SemanticError,
    },
};

/// A folder that contains `.pyxis` files but has no `mod.pyxis` should still
/// gain a module, so backends can walk a complete tree.
#[test]
fn synthesizes_ancestor_modules_without_a_mod_file() {
    let weather = M::new().with_definitions([ID::new(
        (V::Public, "Weather"),
        TD::new([TS::field((V::Private, "temperature"), T::ident("u32"))]),
    )]);

    let mut builder = SemanticBuilder::new(4);
    builder
        .add_module(&weather, &IP::from("world::weather"))
        .unwrap();
    // No module was added for `world` itself.
    let resolved = builder.build().unwrap();

    // The intermediate `world` module is synthesized during build.
    assert!(resolved.modules().contains_key(&IP::from("world")));
    assert!(resolved.modules().contains_key(&IP::from("world::weather")));
}

/// A folder module (`world/mod.pyxis` -> `world`) cannot coexist with a
/// sibling file mapping to the same path (`world.pyxis` -> `world`).
#[test]
fn rejects_two_files_mapping_to_the_same_module() {
    let folder_module = M::new().with_definitions([ID::new(
        (V::Public, "World"),
        TD::new([TS::field((V::Private, "id"), T::ident("u32"))]),
    )]);
    let sibling = M::new().with_definitions([ID::new(
        (V::Public, "Other"),
        TD::new([TS::field((V::Private, "id"), T::ident("u32"))]),
    )]);

    let mut builder = SemanticBuilder::new(4);
    builder
        .add_module(&folder_module, &IP::from("world"))
        .unwrap();
    let err = builder
        .add_module(&sibling, &IP::from("world"))
        .unwrap_err();

    assert!(matches!(err, SemanticError::DuplicateModule { .. }));
}

/// The same collision must also be caught on the real, file-based compile
/// path (`analyze`), not just through `SemanticBuilder`: `world.pyxis` and
/// `world/mod.pyxis` both reduce to the `world` module.
#[test]
fn analyze_rejects_two_files_mapping_to_the_same_module() {
    let db = PyxisDatabaseImpl::default();
    let folder = SourceFile::new(
        &db,
        "world/mod.pyxis".to_string(),
        1,
        "pub type World { pub id: u32, }".to_string(),
    );
    let sibling = SourceFile::new(
        &db,
        "world.pyxis".to_string(),
        2,
        "pub type Other { pub id: u32, }".to_string(),
    );
    let source_set = SourceSet::new(&db, vec![folder, sibling]);
    let analysis = analyze(&db, 4, source_set);
    let errors = analysis.errors(&db);
    assert!(
        errors
            .iter()
            .any(|e| matches!(e, SemanticError::DuplicateModule { .. })),
        "expected a DuplicateModule error, got: {errors:?}"
    );
}
