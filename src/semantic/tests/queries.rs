//! Salsa query tests — verifies the incremental compilation query graph.
//!
//! These tests exercise the Salsa-backed queries (parse_file, analyze,
//! resolve_item) to ensure they work correctly and cache results.

use crate::semantic::{
    PyxisDatabaseImpl, SourceFile, SourceSet, analyze, parse_file, resolve_item,
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
    // AC.9: calling resolve_item twice with no input change must NOT re-execute
    // the query the second time — Salsa returns the memoized result. We assert
    // on the actual WillExecute events, not just value-equality (which would hold
    // even if nothing were cached).
    use std::sync::{Arc, Mutex};

    let executed: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let sink = executed.clone();
    let db = PyxisDatabaseImpl::with_event_logger(Box::new(move |event| {
        if let salsa::EventKind::WillExecute { database_key } = event.kind {
            sink.lock().unwrap().push(format!("{database_key:?}"));
        }
    }));
    let source = SourceFile::new(
        &db,
        "test.pyxis".to_string(),
        1,
        "pub type Foo { pub x: u32, }".to_string(),
    );
    let source_set = SourceSet::new(&db, vec![source]);
    let path = crate::grammar::ItemPath::from("test::Foo");

    // First call warms the cache and must execute resolve_item.
    let item1 = resolve_item(&db, source_set, 4, path.clone())
        .item(&db)
        .clone();
    assert!(
        executed
            .lock()
            .unwrap()
            .iter()
            .any(|k| k.contains("resolve_item")),
        "expected resolve_item to execute during warm-up; keys: {:?}",
        executed.lock().unwrap()
    );

    // Second call with no input change: Salsa must serve it from cache and not
    // re-execute resolve_item.
    executed.lock().unwrap().clear();
    let item2 = resolve_item(&db, source_set, 4, path).item(&db).clone();

    let log = executed.lock().unwrap();
    assert!(
        !log.iter().any(|k| k.contains("resolve_item")),
        "resolve_item must not re-execute when nothing changed; got: {log:?}"
    );
    assert_eq!(item1, item2, "cached result should be equal");
}

#[test]
fn resolve_item_incremental_on_change() {
    // AC.9: editing one file must re-execute resolve_item only for that file's
    // item; an unchanged item in another file must stay cached (no WillExecute).
    // We assert on the actual events, not just value-equality.
    use std::sync::{Arc, Mutex};

    let executed: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let sink = executed.clone();
    let mut db = PyxisDatabaseImpl::with_event_logger(Box::new(move |event| {
        if let salsa::EventKind::WillExecute { database_key } = event.kind {
            sink.lock().unwrap().push(format!("{database_key:?}"));
        }
    }));
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
    let path_a = crate::grammar::ItemPath::from("a::A");
    let path_b = crate::grammar::ItemPath::from("b::B");

    // Resolve both items (warms the cache).
    let item_a1 = {
        let source_set = SourceSet::new(&db, vec![source1, source2]);
        let item_a1 = resolve_item(&db, source_set, 4, path_a.clone())
            .item(&db)
            .clone();
        let _ = resolve_item(&db, source_set, 4, path_b.clone());
        item_a1
    };

    // Change source2 (type B). Type A doesn't depend on B.
    source2
        .set_contents(&mut db)
        .to("pub type B { pub y: u32, pub z: u32, }".to_string());

    // Re-resolve both. A must come from cache (no WillExecute); B must re-execute.
    executed.lock().unwrap().clear();
    let (item_a2, item_b2_size) = {
        let source_set2 = SourceSet::new(&db, vec![source1, source2]);
        let item_a2 = resolve_item(&db, source_set2, 4, path_a).item(&db).clone();
        let item_b2 = resolve_item(&db, source_set2, 4, path_b);
        (item_a2, item_b2.item(&db).resolved().unwrap().size)
    };

    // The Salsa key prints as `resolve_item(Id(..))` with no path, so we can't
    // match items by name — but exactly one resolve_item should re-run, and it
    // must be the edited b::B (a::A stays cached). The value-equality check below
    // confirms A itself is unchanged.
    let log = executed.lock().unwrap();
    let resolve_item_runs = log.iter().filter(|k| k.contains("resolve_item")).count();
    assert_eq!(
        resolve_item_runs, 1,
        "exactly one resolve_item (for the edited b::B) should re-run; got {resolve_item_runs}: {log:?}"
    );

    // A should be unchanged because it doesn't depend on B.
    assert_eq!(
        item_a1, item_a2,
        "item A should be unchanged when B changes"
    );
    // B should have changed (different size).
    assert_eq!(item_b2_size, 8, "B should now be 8 bytes (2x u32)");
}

#[test]
fn extern_value_without_address_is_an_error() {
    // Regression: a missing `#[address]` on an extern value must surface as a
    // diagnostic. Previously analyze() logged "DEBUG: ..." and dropped the
    // whole module, silently losing every type it declared.
    let db = PyxisDatabaseImpl::default();
    let source = SourceFile::new(
        &db,
        "test.pyxis".to_string(),
        1,
        "extern thing: *mut u32;".to_string(),
    );
    let source_set = SourceSet::new(&db, vec![source]);
    let analysis = analyze(&db, 4, source_set);
    assert!(
        analysis.parse_errors(&db).is_empty(),
        "extern value should parse"
    );
    assert!(
        !analysis.errors(&db).is_empty(),
        "extern value without #[address] should produce a diagnostic"
    );
}

#[test]
fn name_index_is_stable_across_body_edits() {
    // The whole incremental story rests on this: editing a field's type (not
    // any name/scope/arity) must leave the NameIndex byte-for-byte equal, so
    // the name_index query backdates and dependent resolve_item stays cached.
    use crate::semantic::queries::name_index;
    let mut db = PyxisDatabaseImpl::default();
    let a = SourceFile::new(
        &db,
        "a.pyxis".to_string(),
        1,
        "pub type A { pub x: u32, }".to_string(),
    );
    let b = SourceFile::new(
        &db,
        "b.pyxis".to_string(),
        2,
        "pub type B { pub y: u32, }".to_string(),
    );
    // SourceSet<'db> borrows db, so recreate it after each &mut edit.
    let index_now = |db: &PyxisDatabaseImpl| {
        let sources = SourceSet::new(db, vec![a, b]);
        (**name_index(db, sources, 4).index(db)).clone()
    };

    let index1 = index_now(&db);

    // Edit B's body — change the field type, keep the name/kind/arity.
    b.set_contents(&mut db)
        .to("pub type B { pub y: u64, }".to_string());
    let index2 = index_now(&db);
    assert_eq!(
        index1, index2,
        "NameIndex must be unchanged by a body-only edit"
    );

    // Sanity: a structural edit (new type) DOES change it.
    b.set_contents(&mut db)
        .to("pub type B { pub y: u64, } pub type C { pub z: u32, }".to_string());
    let index3 = index_now(&db);
    assert_ne!(index2, index3, "adding a type must change the NameIndex");
}

#[test]
fn resolve_item_is_incremental_across_files() {
    // The payoff: editing one file must NOT re-execute resolve_item for an item
    // in a different, unrelated file. We log salsa's WillExecute events and
    // assert resolve_item doesn't fire for the untouched item.
    use std::sync::{Arc, Mutex};

    let executed: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let sink = executed.clone();
    let mut db = PyxisDatabaseImpl::with_event_logger(Box::new(move |event| {
        if let salsa::EventKind::WillExecute { database_key } = event.kind {
            sink.lock().unwrap().push(format!("{database_key:?}"));
        }
    }));

    let a = SourceFile::new(
        &db,
        "a.pyxis".to_string(),
        1,
        "pub type A { pub x: u32, }".to_string(),
    );
    let b = SourceFile::new(
        &db,
        "b.pyxis".to_string(),
        2,
        "pub type B { pub y: u32, }".to_string(),
    );
    let path_a = crate::grammar::ItemPath::from("a::A");
    let path_b = crate::grammar::ItemPath::from("b::B");

    // Resolve both items once (warms the cache).
    {
        let sources = SourceSet::new(&db, vec![a, b]);
        assert!(
            resolve_item(&db, sources, 4, path_a.clone())
                .item(&db)
                .resolved()
                .is_some()
        );
        assert!(
            resolve_item(&db, sources, 4, path_b.clone())
                .item(&db)
                .resolved()
                .is_some()
        );
    }
    // Sanity: the warm-up DID execute resolve_item (so the key format we match
    // on is real, and the post-edit assertion isn't vacuously true).
    assert!(
        executed
            .lock()
            .unwrap()
            .iter()
            .any(|k| k.contains("resolve_item")),
        "expected resolve_item to execute during warm-up; keys: {:?}",
        executed.lock().unwrap()
    );

    // Edit B's body — A does not reference B, so A's resolution must stand.
    b.set_contents(&mut db)
        .to("pub type B { pub y: u64, }".to_string());

    executed.lock().unwrap().clear();
    {
        let sources = SourceSet::new(&db, vec![a, b]);
        let _ = resolve_item(&db, sources, 4, path_a.clone());
    }

    let log = executed.lock().unwrap();
    assert!(
        !log.iter().any(|k| k.contains("resolve_item")),
        "resolve_item must not re-execute for an item in an unrelated file; got: {log:?}"
    );
}

#[test]
fn file_type_references_indexes_and_is_incremental() {
    use crate::semantic::queries::file_type_references;
    use std::sync::{Arc, Mutex};

    let executed: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let sink = executed.clone();
    let mut db = PyxisDatabaseImpl::with_event_logger(Box::new(move |event| {
        if let salsa::EventKind::WillExecute { database_key } = event.kind {
            sink.lock().unwrap().push(format!("{database_key:?}"));
        }
    }));
    let a = SourceFile::new(
        &db,
        "a.pyxis".to_string(),
        1,
        "pub type Bar { pub n: u32, }\npub type Foo { pub b: *mut Bar, }".to_string(),
    );
    let b = SourceFile::new(
        &db,
        "b.pyxis".to_string(),
        2,
        "pub type B { pub y: u32, }".to_string(),
    );

    // The source map for a.pyxis resolves the `Bar` reference inside Foo.
    {
        let sources = SourceSet::new(&db, vec![a, b]);
        let map = file_type_references(&db, a, sources, 4);
        let refs = map.references(&db);
        assert!(
            refs.iter().any(|(_, path)| path.to_string() == "a::Bar"),
            "source map should resolve the *mut Bar reference; got {refs:?}"
        );
    }

    // Editing an unrelated file must not recompute a.pyxis's source map.
    b.set_contents(&mut db)
        .to("pub type B { pub y: u64, }".to_string());
    executed.lock().unwrap().clear();
    {
        let sources = SourceSet::new(&db, vec![a, b]);
        let _ = file_type_references(&db, a, sources, 4);
    }
    assert!(
        !executed
            .lock()
            .unwrap()
            .iter()
            .any(|k| k.contains("file_type_references")),
        "file_type_references must not recompute for an unrelated edit; got {:?}",
        executed.lock().unwrap()
    );
}

#[test]
fn resolve_item_resolves_aliased_generic_instantiations() {
    // Regression: resolving `VecEntry = Entry<u32, V>` where `Entry<K,V> =
    // MapEntry<K,V>` needs MapEntry resolved (reached transitively through the
    // alias), not just registered as a placeholder.
    let db = PyxisDatabaseImpl::default();
    let src = "pub type MapEntry<K, V> { pub k: *mut K, pub v: *mut V, }\n\
               pub type Entry<K, V> = MapEntry<K, V>;\n\
               pub type VecEntry = Entry<u32, u32>;\n\
               pub type Holder { pub e: VecEntry, }";
    let source = SourceFile::new(&db, "m.pyxis".to_string(), 1, src.to_string());
    let sources = SourceSet::new(&db, vec![source]);
    let holder = resolve_item(&db, sources, 4, crate::grammar::ItemPath::from("m::Holder"));
    assert!(
        holder.item(&db).resolved().is_some(),
        "Holder (via alias-of-generic field) should resolve; got {:?}",
        holder.item(&db).state
    );
}

#[test]
fn analyze_is_incremental_per_item() {
    // analyze drives resolution through resolve_item, so editing one file must
    // not re-resolve an item in an unrelated file even when analyze re-runs.
    use std::sync::{Arc, Mutex};
    let executed: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let sink = executed.clone();
    let mut db = PyxisDatabaseImpl::with_event_logger(Box::new(move |event| {
        if let salsa::EventKind::WillExecute { database_key } = event.kind {
            sink.lock().unwrap().push(format!("{database_key:?}"));
        }
    }));
    let a = SourceFile::new(
        &db,
        "a.pyxis".to_string(),
        1,
        "pub type A { pub x: u32, }".to_string(),
    );
    let b = SourceFile::new(
        &db,
        "b.pyxis".to_string(),
        2,
        "pub type B { pub y: u32, }".to_string(),
    );

    {
        let sources = SourceSet::new(&db, vec![a, b]);
        let _ = analyze(&db, 4, sources);
    }
    b.set_contents(&mut db)
        .to("pub type B { pub y: u64, }".to_string());
    executed.lock().unwrap().clear();
    {
        let sources = SourceSet::new(&db, vec![a, b]);
        let _ = analyze(&db, 4, sources);
    }
    // resolve_item for a::A (unaffected) must not re-execute. (resolve_item for
    // b::B will, and analyze itself re-runs — that's expected.)
    let log = executed.lock().unwrap();
    let resolve_item_runs = log.iter().filter(|k| k.contains("resolve_item")).count();
    assert_eq!(
        resolve_item_runs, 1,
        "exactly one resolve_item (for the edited b::B) should re-run; got {resolve_item_runs}: {log:?}"
    );
}

#[test]
fn resolve_item_resolves_generic_instantiated_by_value() {
    // Regression (JC2 AirVehicle): a type that holds `Map<u32>` by value needs
    // Map's concrete field types resolved too — instantiation re-lays-out Map's
    // fields against this item's registry, not just Map's precomputed size.
    let db = PyxisDatabaseImpl::default();
    let src = "pub type Bar {\n    pub x: u64,\n}\npub type Map<T> {\n    pub k: *mut T,\n    pub b: Bar,\n}\npub type X {\n    pub f: Map<u32>,\n}";
    let source = SourceFile::new(&db, "m.pyxis".to_string(), 1, src.to_string());
    let sources = SourceSet::new(&db, vec![source]);
    let x = resolve_item(&db, sources, 8, crate::grammar::ItemPath::from("m::X"));
    assert!(
        x.item(&db).resolved().is_some(),
        "X holding Map<u32> by value should resolve; got {:?}",
        x.item(&db).state
    );
}

#[test]
fn resolve_item_resolves_generic_in_vftable_signature() {
    // Regression (JC2 GameWorld): a type whose only references to a generic are
    // in vftable method signatures (`fn add(&mut self, item: Map<u32>)`) must
    // still resolve that generic — value deps include vftable signatures.
    let db = PyxisDatabaseImpl::default();
    let src = "pub type Bar {\n    pub x: u64,\n}\npub type Map<T> {\n    pub k: *mut T,\n    pub b: Bar,\n}\npub type World {\n    vftable {\n        pub fn add(&mut self, item: Map<u32>);\n    },\n}";
    let source = SourceFile::new(&db, "m.pyxis".to_string(), 1, src.to_string());
    let sources = SourceSet::new(&db, vec![source]);
    let world = resolve_item(&db, sources, 8, crate::grammar::ItemPath::from("m::World"));
    assert!(
        world.item(&db).resolved().is_some(),
        "World with a generic-typed vftable arg should resolve; got {:?}",
        world.item(&db).state
    );
}
