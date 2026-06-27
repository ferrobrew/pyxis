//! Salsa query tests — verifies the incremental compilation query graph.
//!
//! These tests exercise the Salsa-backed queries (parse_file, analyze,
//! resolve_item) to ensure they work correctly and cache results.

use crate::semantic::{
    self, Db, PyxisDatabaseImpl, SourceFile, SourceSet, analyze, parse_file, resolve_item,
};
use salsa::Setter;

#[test]
fn spike_parse_file_works() {
    let db = PyxisDatabaseImpl::default();
    let source = SourceFile::new(
        &db,
        "test.pyxis".to_string(),
        1, // file_id
        "pub type Test { pub field: u32, }".to_string(),
    );
    let parsed = parse_file(&db, source);
    let module = parsed.module(&db);
    let defs: Vec<_> = module.definitions().collect();
    assert!(!defs.is_empty(), "should parse a type def");
}

#[test]
fn spike_analyze_returns_no_errors() {
    let db = PyxisDatabaseImpl::default();
    let source = SourceFile::new(
        &db,
        "test.pyxis".to_string(),
        1,
        "pub type Test { pub field: u32, }".to_string(),
    );
    let source_set = SourceSet::new(&db, vec![source]);
    let analysis = analyze(&db, 4, source_set);
    let errors = analysis.errors(&db);
    let parse_errors = analysis.parse_errors(&db);
    assert!(errors.is_empty(), "should have no semantic errors");
    assert!(parse_errors.is_empty(), "should have no parse errors");
}

#[test]
fn spike_incremental_invalidation() {
    let mut db = PyxisDatabaseImpl::default();
    let source = SourceFile::new(
        &db,
        "test.pyxis".to_string(),
        1,
        "pub type A { pub x: u32, }".to_string(),
    );

    let parsed1 = parse_file(&db, source);
    let module1 = parsed1.module(&db);
    let defs1: Vec<_> = module1.definitions().collect();
    assert_eq!(defs1.len(), 1);

    // Change the content — Salsa should invalidate the cached parse
    source
        .set_contents(&mut db)
        .to("pub type A { pub x: u32, } pub type B { pub y: u32, }".to_string());

    let parsed2 = parse_file(&db, source);
    let module2 = parsed2.module(&db);
    let defs2: Vec<_> = module2.definitions().collect();
    assert_eq!(defs2.len(), 2, "should re-parse with new content");
}

#[test]
fn resolve_item_returns_resolved_type() {
    // AC.9: verify resolve_item produces a resolved item with size/alignment
    let db = PyxisDatabaseImpl::default();
    let source = SourceFile::new(
        &db,
        "test.pyxis".to_string(),
        1,
        "pub type Foo { pub x: u32, pub y: u32, }".to_string(),
    );
    let source_set = SourceSet::new(&db, vec![source]);

    // The item path includes the module path derived from the filename.
    // "test.pyxis" → module path "test" → item path "test::Foo"
    let path = crate::grammar::ItemPath::from("test::Foo");
    let resolved = resolve_item(&db, source_set, 4, path);
    let item = resolved.item(&db);

    assert!(
        item.resolved().is_some(),
        "item should be resolved, got: {:?}",
        item.state
    );
    let resolved_state = item.resolved().unwrap();
    assert_eq!(resolved_state.size, 8, "size should be 8 bytes");
    assert_eq!(resolved_state.alignment, 4, "alignment should be 4");
}

#[test]
fn resolve_item_caches_unchanged_results() {
    // AC.9: calling resolve_item twice with the same inputs should
    // return the same cached result (Salsa memoization).
    let db = PyxisDatabaseImpl::default();
    let source = SourceFile::new(
        &db,
        "test.pyxis".to_string(),
        1,
        "pub type Foo { pub x: u32, }".to_string(),
    );
    let source_set = SourceSet::new(&db, vec![source]);
    let path = crate::grammar::ItemPath::from("test::Foo");

    let resolved1 = resolve_item(&db, source_set, 4, path.clone());
    let item1 = resolved1.item(&db).clone();

    // Call again — Salsa should return the cached result
    let resolved2 = resolve_item(&db, source_set, 4, path);
    let item2 = resolved2.item(&db).clone();

    assert_eq!(item1, item2, "cached result should be equal");
}

#[test]
fn resolve_item_incremental_on_change() {
    // AC.9: When one file changes, only that file's items + dependents
    // should re-resolve. We verify this by checking that after changing
    // a file, resolve_item for an unchanged item returns the same result.
    use salsa::Setter;

    let mut db = PyxisDatabaseImpl::default();
    let source1 = SourceFile::new(
        &db,
        "a.pyxis".to_string(),
        10,
        "pub type A { pub x: u32, }".to_string(),
    );
    let source2 = SourceFile::new(
        &db,
        "b.pyxis".to_string(),
        11,
        "pub type B { pub y: u64, }".to_string(),
    );
    let source_set = SourceSet::new(&db, vec![source1, source2]);

    // Resolve both items
    let path_a = crate::grammar::ItemPath::from("a::A");
    let path_b = crate::grammar::ItemPath::from("b::B");

    let resolved_a1 = resolve_item(&db, source_set, 4, path_a.clone());
    let _resolved_b1 = resolve_item(&db, source_set, 4, path_b.clone());

    let item_a1 = resolved_a1.item(&db).clone();

    // Change source2 (type B). Type A doesn't depend on B.
    source2
        .set_contents(&mut db)
        .to("pub type B { pub y: u32, pub z: u32, }".to_string());

    // Resolve A again — should return the cached result (unchanged)
    let source_set2 = SourceSet::new(&db, vec![source1, source2]);
    let resolved_a2 = resolve_item(&db, source_set2, 4, path_a);
    let item_a2 = resolved_a2.item(&db).clone();

    // A should be unchanged because it doesn't depend on B
    assert_eq!(
        item_a1, item_a2,
        "item A should be unchanged when B changes"
    );

    // B should have changed (different size)
    let resolved_b2 = resolve_item(&db, source_set2, 4, path_b);
    let item_b2 = resolved_b2.item(&db);
    assert_eq!(
        item_b2.resolved().unwrap().size,
        8,
        "B should now be 8 bytes (2x u32)"
    );
}
