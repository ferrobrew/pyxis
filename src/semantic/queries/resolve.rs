//! The incremental resolution core: `resolve_item` resolves a single item,
//! depending only on `name_index`, the one file that declares it, the shared
//! `placeholder_base`, and `resolve_item` for its value dependencies — so an
//! edit to an unrelated file leaves the memoized result standing.

use std::{collections::BTreeMap, sync::Arc};

use crate::{
    grammar::{self, ItemDefinitionInner, ItemPath},
    semantic::{
        Module, SemanticError, TypeRegistry,
        error::BuildOutcome,
        name_index::{NameIndex, SigKind},
        types::{ItemCategory, ItemDefinition, ItemState, PredefinedItem, Visibility},
    },
};

use super::{
    super::{db::Db, inputs::SourceSet, ir::ResolvedItem},
    helpers::{
        build_item, make_extern_from_sig, make_predefined_definition, make_unresolved_definition,
        value_referenced_types,
    },
    index::{name_index, placeholder_base},
    leaf::parse_file,
};

/// Fetch one item's grammar definition by path, via the file that declares it.
pub(super) fn grammar_def_for<'db>(
    db: &'db dyn Db,
    sources: SourceSet<'db>,
    index: &NameIndex,
    path: &ItemPath,
) -> Option<grammar::ItemDefinition> {
    let source = index
        .file_of(path)
        .and_then(|i| sources.sources(db).get(i).copied())?;
    let module_path = ItemPath::from_path(std::path::Path::new(source.path(db).as_str()));
    find_grammar_def(parse_file(db, source).module(db), &module_path, path)
}

/// Find a grammar definition in a module by path. Handles both top-level items
/// and nested items (declared inside `type` bodies). Recurses through type
/// nesting: `A::B::C` finds `A` at top level, then `B` inside `A`'s body, then
/// `C` inside `B`'s body.
fn find_grammar_def(
    module: &grammar::Module,
    module_path: &ItemPath,
    target_path: &ItemPath,
) -> Option<grammar::ItemDefinition> {
    // 1. Try top-level match
    if let Some(def) = module
        .definitions()
        .find(|d| module_path.join(d.name.as_str().into()) == *target_path)
    {
        return Some(def.clone());
    }

    // 2. Nested-item search: find the parent type's definition, then walk its
    //    body for a nested item whose name matches target_path.last().
    let parent_path = target_path.parent()?;
    let parent_def = find_grammar_def(module, module_path, &parent_path)?;
    if let grammar::ItemDefinitionInner::Type(td) = &parent_def.inner {
        let leaf = target_path.last()?;
        for stmt in td.statements() {
            if let grammar::TypeField::Item(inner) = &stmt.field {
                if inner.name.as_str() == leaf.as_str() {
                    return Some((**inner).clone());
                }
            }
        }
    }
    if let grammar::ItemDefinitionInner::Enum(ed) = &parent_def.inner {
        let leaf = target_path.last()?;
        for item in &ed.items {
            if let grammar::EnumDefItem::Item(inner) = item {
                if inner.name.as_str() == leaf.as_str() {
                    return Some((**inner).clone());
                }
            }
        }
    }
    if let grammar::ItemDefinitionInner::Bitflags(bd) = &parent_def.inner {
        let leaf = target_path.last()?;
        for item in &bd.items {
            if let grammar::BitflagsDefItem::Item(inner) = item {
                if inner.name.as_str() == leaf.as_str() {
                    return Some((**inner).clone());
                }
            }
        }
    }

    None
}

/// Resolve a single item — the incremental core.
///
/// Depends only on: `name_index` (stable across body/location edits), the one
/// file that declares the item, and `resolve_item` for the item's *value*
/// dependencies. An edit to an unrelated file leaves all three unchanged, so
/// the memoized result stands — genuine cross-file incremental resolution.
///
/// Every declared item is registered as an `Unresolved` placeholder (existence
/// + arity, all the index knows), then the item's value dependencies are
/// resolved recursively and injected as `Resolved`. Pointer references resolve
/// against the placeholders, so mutually-pointing types never form a cycle; a
/// genuine by-value cycle is recovered by `resolve_item_cycle` and surfaces as
/// a stalled type.
#[salsa::tracked(cycle_result=resolve_item_cycle)]
pub fn resolve_item<'db>(
    db: &'db dyn Db,
    sources: SourceSet<'db>,
    pointer_size: usize,
    item_path: ItemPath,
) -> ResolvedItem<'db> {
    let index = name_index(db, sources, pointer_size).index(db);

    // Predefined / extern: resolved purely from the signature.
    if index.is_predefined(&item_path)
        && let Some(p) = PredefinedItem::ALL
            .iter()
            .find(|p| ItemPath::from(p.name()) == item_path)
    {
        return ResolvedItem::new(
            db,
            item_path.clone(),
            Arc::new(make_predefined_definition(
                &item_path,
                p.size(),
                p.size().max(1),
            )),
            Arc::new(vec![]),
            Arc::new(vec![]),
        );
    }
    if let Some(sig) = index.extern_sig(&item_path) {
        return ResolvedItem::new(
            db,
            item_path.clone(),
            Arc::new(make_extern_from_sig(&item_path, sig)),
            Arc::new(vec![]),
            Arc::new(vec![]),
        );
    }

    // Locate the declaring file and extract the item's grammar definition.
    let unresolved = |db: &'db dyn Db| {
        ResolvedItem::new(
            db,
            item_path.clone(),
            Arc::new(make_unresolved_definition(&item_path)),
            Arc::new(vec![]),
            Arc::new(vec![]),
        )
    };
    let Some(source) = index
        .file_of(&item_path)
        .and_then(|i| sources.sources(db).get(i).copied())
    else {
        return unresolved(db);
    };
    let parsed = parse_file(db, source);
    let module_ast = parsed.module(db);
    let path_str = source.path(db);
    let module_path = ItemPath::from_path(std::path::Path::new(path_str.as_str()));
    let Some(definition) = find_grammar_def(module_ast, &module_path, &item_path) else {
        return unresolved(db);
    };

    // Overlay the shared, memoized placeholder base (predefined + extern + all
    // declared items as placeholders). Sharing it — rather than rebuilding the n
    // placeholders per call — is what keeps a whole-program analyze O(edges)
    // instead of O(n²), while still giving every reference (including those
    // reached transitively through a type alias) something to resolve against.
    let base = placeholder_base(db, sources, pointer_size)
        .registry(db)
        .clone();
    let mut type_registry = TypeRegistry::with_base(base);

    // Resolve this item's value dependencies (recursively, memoized) and inject
    // them as Resolved so the build sees their layout. A type alias is followed
    // transitively: its resolved form is the *unexpanded* generic, so resolving
    // `VecEntry = Entry<u32, Vector3>` (where `Entry<K,V> = MapEntry<K,V>`) also
    // needs `MapEntry` itself resolved to compute the instantiation's layout.
    let scope = index
        .scope_of(&item_path)
        .map(<[ItemPath]>::to_vec)
        .unwrap_or_default();
    let mut seen: std::collections::BTreeSet<ItemPath> = std::collections::BTreeSet::new();
    let mut worklist = value_referenced_types(&definition, &scope, index);
    while let Some(dep) = worklist.pop() {
        if dep == item_path || !seen.insert(dep.clone()) {
            continue;
        }
        let resolved = resolve_item(db, sources, pointer_size, dep.clone());
        let dep_item = resolved.item(db);
        if dep_item.is_resolved() {
            type_registry.add((**dep_item).clone());
        }
        for generated in resolved.generated_items(db).iter() {
            type_registry.add(generated.clone());
        }
        // A type alias or a *generic* type, when referenced by value, has its
        // fields re-folded against this item's registry at instantiation —
        // `VecEntry = Entry<u32, V>` expands its alias target, and `Map<u32>`
        // re-lays-out `Map`'s fields. So their own value deps (e.g. a concrete
        // field type of a generic struct) must be resolved here too, not just
        // the alias/generic itself.
        if index
            .item_sig(&dep)
            .is_some_and(|s| s.kind == SigKind::TypeAlias || s.arity > 0)
            && let Some(dep_def) = grammar_def_for(db, sources, index, &dep)
        {
            let dep_scope = index
                .scope_of(&dep)
                .map(<[ItemPath]>::to_vec)
                .unwrap_or_default();
            worklist.extend(value_referenced_types(&dep_def, &dep_scope, index));
        }
    }

    // Modules map for name resolution: the item's own module (its scope, impls)
    // plus the root. build_item reads module.scope() and may add generated items
    // to the item's module.
    let mut modules: BTreeMap<ItemPath, Module> = BTreeMap::new();
    modules.insert(ItemPath::empty(), Module::default());
    let impls: Vec<_> = module_ast.impls().cloned().collect();
    let backends: Vec<_> = module_ast.backends().cloned().collect();
    if let Ok(m) = Module::new(
        module_path.clone(),
        module_ast.as_ref().clone(),
        &impls,
        &backends,
    ) {
        modules.insert(module_path, m);
    }

    // Snapshot the overlay paths present *before* the build. `iter()` walks only
    // this registry's own additions (the overlay), not the shared base — so this
    // captures exactly the resolved value-dependencies and their own generated
    // items injected above. A base-membership check is insufficient: a
    // dependency's generated vftable lives in *this* overlay (added at the value-
    // dep loop), not in `placeholder_base`, so it would slip past such a check
    // and be misattributed to this item. Anything appearing in `iter()` after the
    // build that is *not* in this snapshot was generated during this item's own
    // `build_item` call.
    let pre_build_overlay: std::collections::BTreeSet<ItemPath> =
        type_registry.iter().map(|(path, _)| path.clone()).collect();

    // Build the item.
    let visibility: Visibility = definition.visibility.into();
    let def_location = definition.location;
    let type_param_names: Vec<String> = definition
        .type_parameters
        .iter()
        .map(|tp| tp.name.clone())
        .collect();
    let outcome = build_item(
        &definition,
        &item_path,
        visibility,
        &def_location,
        &definition.doc_comments,
        &type_param_names,
        &mut type_registry,
        &mut modules,
    );

    let (item_def, errors) = match outcome {
        Ok(BuildOutcome::Resolved(item)) => {
            let cfg = match &definition.inner {
                ItemDefinitionInner::Type(td) => td.attributes.cfg(),
                ItemDefinitionInner::Enum(e) => e.attributes.cfg(),
                ItemDefinitionInner::Bitflags(b) => b.attributes.cfg(),
                ItemDefinitionInner::TypeAlias(ta) => ta.attributes.cfg(),
                ItemDefinitionInner::Constant(c) => c.attributes.cfg(),
                ItemDefinitionInner::ExternValue(ev) => ev.attributes.cfg(),
            };
            (
                ItemDefinition {
                    visibility,
                    path: item_path.clone(),
                    type_parameters: type_param_names,
                    state: ItemState::Resolved(item),
                    category: ItemCategory::Defined,
                    predefined: None,
                    cfg,
                    location: def_location,
                    declaration_location: definition.declaration_location,
                },
                Vec::new(),
            )
        }
        Ok(BuildOutcome::Deferred) => (make_unresolved_definition(&item_path), vec![]),
        Ok(BuildOutcome::NotFoundType(unresolved_ref)) => (
            make_unresolved_definition(&item_path),
            vec![SemanticError::TypeResolutionStalled {
                unresolved_types: vec![item_path.to_string()],
                resolved_types: vec![],
                unresolved_references: vec![unresolved_ref],
            }],
        ),
        Err(e) => (make_unresolved_definition(&item_path), vec![e]),
    };

    // Collect items generated during the build (e.g. vftable structs) — those
    // resolved entries that aren't predefined/extern/declared or the item itself.
    let mut generated_items = Vec::new();
    for (path, item) in type_registry.iter() {
        if path == &item_path
            || pre_build_overlay.contains(path)
            || index.item_sig(path).is_some()
            || index.is_predefined(path)
            || index.extern_sig(path).is_some()
        {
            continue;
        }
        if item.is_resolved() {
            generated_items.push((*item).clone());
        }
    }

    ResolvedItem::new(
        db,
        item_path,
        Arc::new(item_def),
        Arc::new(errors),
        Arc::new(generated_items),
    )
}

/// Cycle recovery for `resolve_item`: a by-value dependency cycle (illegal —
/// infinite size) yields an Unresolved placeholder, so the build defers and the
/// type is reported as stalled rather than looping forever.
fn resolve_item_cycle<'db>(
    db: &'db dyn Db,
    _id: salsa::Id,
    _sources: SourceSet<'db>,
    _pointer_size: usize,
    item_path: ItemPath,
) -> ResolvedItem<'db> {
    ResolvedItem::new(
        db,
        item_path.clone(),
        Arc::new(make_unresolved_definition(&item_path)),
        Arc::new(vec![]),
        Arc::new(vec![]),
    )
}
