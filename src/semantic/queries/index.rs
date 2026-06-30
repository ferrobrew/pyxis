//! Project-shape indices built from all parsed files: the full
//! `collect_declarations` registry the LSP uses, the lean body/location-free
//! `name_index` that drives incremental resolution, and the memoized
//! `placeholder_base` shared by every `resolve_item`.

use std::sync::Arc;

use crate::{
    grammar::ItemPath,
    semantic::{TypeRegistry, declaration_registry::DeclarationRegistry, name_index::NameIndex},
};

use super::{
    super::{
        db::Db,
        inputs::SourceSet,
        ir::{DeclarationSet, NameIndexSet, PlaceholderBase},
    },
    helpers::{make_extern_from_sig, make_placeholder, register_predefined},
    leaf::parse_file,
};

/// Build the full declaration registry from all parsed files.
/// This is a single tracked function — it re-runs when any file changes.
#[salsa::tracked]
pub fn collect_declarations<'db>(
    db: &'db dyn Db,
    sources: SourceSet<'db>,
    pointer_size: usize,
) -> DeclarationSet<'db> {
    let mut registry = DeclarationRegistry::new();
    registry.set_pointer_size(pointer_size);
    registry.register_predefined();

    for source in sources.sources(db) {
        let parsed = parse_file(db, *source);
        let module = parsed.module(db);
        let path_str = source.path(db);
        let module_path = ItemPath::from_path(std::path::Path::new(path_str.as_str()));
        registry.register_module(module, &module_path);
    }

    DeclarationSet::new(db, Arc::new(registry))
}

/// Build the body- and location-free name index. Re-runs whenever a file
/// changes (it reads every `parse_file`), but its *output* backdates across
/// edits that don't touch names/scopes/arity — so `resolve_item`, which
/// depends on this rather than the full declaration registry, stays cached.
#[salsa::tracked]
pub fn name_index<'db>(
    db: &'db dyn Db,
    sources: SourceSet<'db>,
    pointer_size: usize,
) -> NameIndexSet<'db> {
    let mut index = NameIndex::new(pointer_size);
    index.register_predefined();

    for (file_index, source) in sources.sources(db).iter().enumerate() {
        let parsed = parse_file(db, *source);
        let module = parsed.module(db);
        let path_str = source.path(db);
        let module_path = ItemPath::from_path(std::path::Path::new(path_str.as_str()));
        index.register_module(module, &module_path, file_index);
    }

    NameIndexSet::new(db, Arc::new(index))
}

/// Build the memoized placeholder base — predefined + extern + every declared
/// item as an `Unresolved` placeholder — shared by every `resolve_item` via
/// `TypeRegistry::with_base`. Depends only on `name_index`, so it's rebuilt only
/// when the project's shape changes, and computed once per `(sources, ptr)`
/// rather than per item.
#[salsa::tracked]
pub fn placeholder_base<'db>(
    db: &'db dyn Db,
    sources: SourceSet<'db>,
    pointer_size: usize,
) -> PlaceholderBase<'db> {
    let index = name_index(db, sources, pointer_size).index(db);
    let mut registry = TypeRegistry::new(pointer_size);
    register_predefined(&mut registry);
    for (path, sig) in index.extern_paths() {
        registry.add(make_extern_from_sig(path, sig));
    }
    for path in index.item_paths() {
        let arity = index.item_sig(path).map(|s| s.arity).unwrap_or(0);
        registry.add(make_placeholder(path, arity));
    }
    PlaceholderBase::new(db, Arc::new(registry))
}
