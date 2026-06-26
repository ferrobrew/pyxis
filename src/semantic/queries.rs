//! Salsa tracked functions — the query graph.
//!
//! The pipeline:
//!   `parse_file` (per-file leaf query)
//!   → `collect_declarations` (builds DeclarationRegistry)
//!   → `resolve_item` (per-item, calls resolve_item on dependencies)
//!   → `analyze` (root query, assembles full resolved state)

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::grammar::{ItemDefinition, ItemDefinitionInner, ItemPath, Module};
use crate::parser::ParseError;
use crate::semantic::declaration_registry::DeclarationRegistry;
use crate::semantic::TypeRegistry;
use crate::span::FileId;

use super::db::Db;
use super::inputs::{SourceFile, SourceSet};
use super::ir::{DeclarationSet, ParsedFile, ResolvedItem, SemanticAnalysis};

/// Parse a single file. Leaf query — re-runs only when that file's content changes.
#[salsa::tracked]
pub fn parse_file(db: &dyn Db, source: SourceFile) -> ParsedFile<'_> {
    let contents = source.contents(db);
    let file_id = FileId::new(source.file_id(db));
    match crate::parser::parse_str_with_file_id(contents, file_id) {
        Ok(module) => ParsedFile::new(db, source, Arc::new(module), Arc::new(vec![])),
        Err(e) => ParsedFile::new(
            db,
            source,
            Arc::new(Module::default()),
            Arc::new(vec![e]),
        ),
    }
}

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

/// Resolve a single item (type/enum/bitflags/type-alias).
///
/// This is the per-type query — the core of incremental type resolution.
/// It builds a TypeRegistry with all dependencies resolved (via recursive
/// resolve_item calls that Salsa memoizes), then resolves the target item.
///
/// When type A references type B, resolve_item(A) calls resolve_item(B).
/// Salsa tracks this dependency. If B changes, only A (and its dependents)
/// re-resolve.
/// Resolve a single item (type/enum/bitflags/type-alias).
///
/// This is the per-type query — the core of incremental type resolution.
/// It builds a TypeRegistry with the target item's dependencies resolved
/// (via recursive resolve_item calls that Salsa memoizes), then resolves
/// the target item.
///
/// Dependencies are resolved iteratively: when type_definition::build
/// returns Deferred (because a referenced type isn't resolved), we find
/// the unresolved dependency, call resolve_item on it, insert the result
/// into the registry, and retry. Salsa memoizes each resolve_item call.
///
/// Cycle recovery: when resolve_item(A) calls resolve_item(B) which calls
/// resolve_item(A) (mutual recursion), Salsa detects the cycle and uses
/// the cycle_result function to return a placeholder. The Fixpoint strategy
/// iterates: it starts with the cycle_result, then re-runs the query to
/// see if it converges (the dependency may have been resolved by the time
/// the cycle unwinds).
#[salsa::tracked]
pub fn resolve_item<'db>(
    db: &'db dyn Db,
    sources: SourceSet<'db>,
    pointer_size: usize,
    item_path: ItemPath,
) -> ResolvedItem<'db> {
    let decl_set = collect_declarations(db, sources.clone(), pointer_size);
    let registry = decl_set.registry(db);

    // Check if it's a predefined or extern type (already resolved)
    if let Some(info) = registry.get_predefined(&item_path) {
        return ResolvedItem::new(
            db,
            item_path.clone(),
            Arc::new(make_predefined_definition(&item_path, info.size, info.alignment)),
            Arc::new(vec![]),
            Arc::new(vec![]),
        );
    }
    if let Some(info) = registry.get_extern_type(&item_path) {
        return ResolvedItem::new(
            db,
            item_path.clone(),
            Arc::new(make_extern_definition(&item_path, info)),
            Arc::new(vec![]),
            Arc::new(vec![]),
        );
    }

    // Find this item's declaration
    let Some(definition) = registry.get_definition(&item_path) else {
        return ResolvedItem::new(
            db,
            item_path.clone(),
            Arc::new(make_unresolved_definition(&item_path)),
            Arc::new(vec![]),
            Arc::new(vec![]),
        );
    };

    // Build a TypeRegistry with predefined + extern types resolved,
    // and ALL declared items registered as Unresolved.
    let mut type_registry = TypeRegistry::new(pointer_size);
    register_predefined(&mut type_registry);

    for (path, info) in registry.extern_types_iter() {
        type_registry.add(make_extern_definition(path, info));
    }

    for other_path in registry.item_paths() {
        if let Some(other_def) = registry.get_definition(other_path) {
            register_unresolved(&mut type_registry, other_path, other_def);
        }
    }

    // Build modules map with proper scopes (needed for name resolution)
    let mut modules: BTreeMap<ItemPath, crate::semantic::Module> = BTreeMap::new();
    modules.insert(ItemPath::empty(), crate::semantic::Module::default());
    for (mod_path, mod_info) in registry.modules() {
        let impls: Vec<_> = mod_info.module.impls().cloned().collect();
        let backends: Vec<_> = mod_info.module.backends().cloned().collect();
        if let Ok(m) = crate::semantic::Module::new(
            mod_path.clone(),
            mod_info.module.clone(),
            vec![],
            &impls,
            &backends,
        ) {
            modules.insert(mod_path.clone(), m);
        }
    }

    // Pre-resolve direct dependencies via resolve_item (Salsa tracks the
    // dependency graph). For most types, this is sufficient.
    let module_path = item_path.parent().unwrap_or_else(|| ItemPath::empty());
    let referenced_types = registry.get_referenced_types(definition, &module_path);

    for dep_path in &referenced_types {
        if dep_path == &item_path {
            continue;
        }
        let dep_resolved = resolve_item(db, sources.clone(), pointer_size, dep_path.clone());
        let dep_item = dep_resolved.item(db);
        if dep_item.is_resolved() {
            type_registry.update_item(dep_path.clone(), (**dep_item).clone());
        }
    }

    let visibility: crate::semantic::types::Visibility = definition.visibility.into();
    let def_location = definition.location;
    let type_param_names: Vec<String> = definition
        .type_parameters
        .iter()
        .map(|tp| tp.name.clone())
        .collect();

    // Iteratively resolve dependencies: each time build() returns Deferred,
    // find an unresolved dependency, call resolve_item on it, insert the
    // result, and retry. Salsa memoizes each resolve_item call.
    //
    // The pre-resolution above (via get_referenced_types) handles the common
    // case efficiently, but type alias chains (A → B → C → D) and other
    // transitive dependencies may still leave items unresolved. The loop
    // handles these by repeatedly resolving whatever build() reports as
    // unresolved, until the item resolves or no progress can be made.
    let max_iterations = registry.item_paths().count() + 1;
    let mut outcome = build_item(&definition, &item_path, visibility, &def_location,
        &definition.doc_comments, &type_param_names, &mut type_registry, &mut modules);

    for _ in 0..max_iterations {
        match &outcome {
            Ok(crate::semantic::error::BuildOutcome::Resolved(_))
            | Err(_)
            | Ok(crate::semantic::error::BuildOutcome::NotFoundType(_)) => break,
            Ok(crate::semantic::error::BuildOutcome::Deferred) => {
                // Find an unresolved dependency and resolve it via resolve_item
                let unresolved = type_registry.unresolved();
                let mut resolved_one = false;

                for dep_path in &unresolved {
                    if dep_path == &item_path {
                        continue;
                    }
                    if !registry.contains(dep_path) {
                        continue;
                    }

                    // Call resolve_item on the dependency (Salsa tracks this)
                    let dep_resolved = resolve_item(db, sources.clone(), pointer_size, dep_path.clone());
                    let dep_item = dep_resolved.item(db);
                    let dep_errors = dep_resolved.errors(db);

                    if dep_errors.is_empty() && dep_item.is_resolved() {
                        type_registry.update_item(dep_path.clone(), (**dep_item).clone());
                        resolved_one = true;
                        break;
                    }
                }

                if !resolved_one {
                    // No more dependencies can be resolved — truly stalled
                    break;
                }

                // Retry building with the newly resolved dependency
                outcome = build_item(&definition, &item_path, visibility, &def_location,
                    &definition.doc_comments, &type_param_names, &mut type_registry, &mut modules);
            }
        }
    }

    let (item_def, errors) = match outcome {
        Ok(crate::semantic::error::BuildOutcome::Resolved(item)) => {
            let cfg = match &definition.inner {
                ItemDefinitionInner::Type(td) => td.attributes.cfg(),
                ItemDefinitionInner::Enum(e) => e.attributes.cfg(),
                ItemDefinitionInner::Bitflags(b) => b.attributes.cfg(),
                ItemDefinitionInner::TypeAlias(ta) => ta.attributes.cfg(),
            };
            let resolved_def = crate::semantic::types::ItemDefinition {
                visibility: definition.visibility.into(),
                path: item_path.clone(),
                type_parameters: type_param_names,
                state: crate::semantic::types::ItemState::Resolved(item),
                category: crate::semantic::types::ItemCategory::Defined,
                predefined: None,
                cfg,
                location: def_location,
                declaration_location: definition.declaration_location,
            };
            (resolved_def, Vec::new())
        }
        Ok(crate::semantic::error::BuildOutcome::Deferred) => {
            (
                make_unresolved_definition(&item_path),
                vec![crate::semantic::SemanticError::TypeResolutionStalled {
                    unresolved_types: vec![item_path.to_string()],
                    resolved_types: vec![],
                    unresolved_references: vec![],
                }],
            )
        }
        Ok(crate::semantic::error::BuildOutcome::NotFoundType(unresolved_ref)) => {
            // Match the old SemanticState::build() behavior: a missing type
            // reference produces TypeResolutionStalled with the unresolved
            // reference info, not TypeNotFound.
            (
                make_unresolved_definition(&item_path),
                vec![crate::semantic::SemanticError::TypeResolutionStalled {
                    unresolved_types: vec![item_path.to_string()],
                    resolved_types: vec![],
                    unresolved_references: vec![unresolved_ref],
                }],
            )
        }
        Err(e) => {
            (
                make_unresolved_definition(&item_path),
                vec![e],
            )
        }
    };

    // Collect generated items (e.g., vftable struct types) that were added
    // to the local registry during build_item but aren't declared items.
    // These are items in the local registry whose paths don't appear in the
    // declaration registry's item_paths, predefined, or extern_types.
    let mut generated_items = Vec::new();
    for (path, item) in type_registry.iter() {
        if path == &item_path {
            continue;
        }
        if registry.contains(path) || registry.get_predefined(path).is_some() || registry.get_extern_type(path).is_some() {
            continue;
        }
        // This is a generated item — collect it if resolved
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

/// The root query — builds the full resolved semantic state.
///
/// Uses `resolve_item` for per-item type resolution (Salsa tracks
/// dependencies), then runs the post-resolution passes (extern values,
/// functions, associated functions, doc links) on the resolved registry.
#[salsa::tracked]
pub fn analyze<'db>(
    db: &'db dyn Db,
    pointer_size: usize,
    sources: SourceSet<'db>,
) -> SemanticAnalysis<'db> {
    use crate::semantic::{
        attribute, doc_links,
        error::{AttributeName, ExternKind},
        module::Module as SemanticModule,
        types::{ExternValue, Type, Visibility},
    };
    use crate::span::HasLocation;

    // Collect parse results and errors.
    let mut parse_errors: Vec<ParseError> = Vec::new();
    let mut parsed_modules: Vec<(SourceFile, Arc<Module>)> = Vec::new();

    for source in sources.sources(db) {
        let parsed = parse_file(db, *source);
        let module = parsed.module(db);
        let errors = parsed.errors(db);
        if !errors.is_empty() {
            parse_errors.extend(errors.iter().cloned());
        } else {
            parsed_modules.push((*source, module.clone()));
        }
    }

    if !parse_errors.is_empty() {
        return SemanticAnalysis::new(
            db,
            Arc::new(TypeRegistry::new(pointer_size)),
            Arc::new(BTreeMap::new()),
            Arc::new(doc_links::DocLinkResolver::build(
                &TypeRegistry::new(pointer_size),
                &BTreeMap::new(),
            )),
            Arc::new(vec![]),
            Arc::new(parse_errors),
        );
    }

    // Build the declaration registry
    let decl_set = collect_declarations(db, sources.clone(), pointer_size);
    let decl_registry = decl_set.registry(db);

    // Resolve all items via resolve_item (Salsa tracks dependencies)
    let mut type_registry = TypeRegistry::new(pointer_size);
    register_predefined(&mut type_registry);

    let mut definition_paths: std::collections::BTreeSet<ItemPath> = std::collections::BTreeSet::new();

    // Predefined types live in the root module; add them to definition_paths
    // so the JSON backend (which iterates module.definitions) includes them.
    for predefined in crate::semantic::types::PredefinedItem::ALL {
        definition_paths.insert(ItemPath::from(predefined.name()));
    }

    // Register extern types (resolved at declaration time)
    for (path, info) in decl_registry.extern_types_iter() {
        type_registry.add(make_extern_definition(path, info));
        // Extern types need to be in definition_paths so module.definitions()
        // finds them and the Rust backend emits `pub use ... as Name;`
        definition_paths.insert(path.clone());
    }

    // Register all declared items as unresolved so validate_uses can check
    // their existence and visibility before type resolution runs.
    for item_path in decl_registry.item_paths() {
        if let Some(def) = decl_registry.get_definition(item_path) {
            register_unresolved(&mut type_registry, item_path, def);
            definition_paths.insert(item_path.clone());
        }
    }

    // Build modules from parsed files (extern values, impls, backends)
    let mut modules: BTreeMap<ItemPath, SemanticModule> = BTreeMap::new();
    modules.insert(ItemPath::empty(), SemanticModule::default());

    for (source, module) in &parsed_modules {
        let path_str = source.path(db);
        let module_path = ItemPath::from_path(std::path::Path::new(path_str.as_str()));

        // Parse extern values
        let extern_values: crate::semantic::error::Result<Vec<ExternValue>> = module
            .extern_values()
            .map(|ev| {
                let name = &ev.name;
                let mut address = None;
                for attribute in &ev.attributes {
                    let Some((ident, items)) = attribute.function() else {
                        continue;
                    };
                    if let Some(attr_address) =
                        attribute::parse_address(ident, items, attribute.location())?
                    {
                        address = Some(attr_address);
                    }
                }
                let address = address.ok_or_else(|| crate::semantic::SemanticError::MissingAttribute {
                    attribute_name: AttributeName::Address,
                    extern_kind: ExternKind::Value,
                    item_path: module_path.join(name.as_str().into()),
                    location: ev.location,
                })?;
                Ok(ExternValue {
                    visibility: Visibility::from(ev.visibility),
                    name: name.as_str().to_owned(),
                    type_: Type::Unresolved(ev.type_.clone()),
                    address,
                    doc: ev.doc_comments.clone(),
                    location: ev.location,
                })
            })
            .collect();

        match extern_values {
            Ok(ev) => {
                let impls: Vec<_> = module.impls().cloned().collect();
                let backends: Vec<_> = module.backends().cloned().collect();

                match SemanticModule::new(module_path.clone(), module.as_ref().clone(), ev, &impls, &backends) {
                    Ok(m) => {
                        modules.insert(module_path, m);
                    }
                    Err(e) => {
                        // Will be reported as a semantic error below
                        eprintln!("DEBUG: module build error: {e}");
                    }
                }
            }
            Err(e) => {
                eprintln!("DEBUG: extern value error: {e}");
            }
        }
    }

    // Synthesize ancestor modules: ensure every folder that contains .pyxis
    // files has a module, even if it lacks a mod.pyxis. This lets backends
    // emit a complete module tree (e.g. parent mod/use wiring).
    let existing: Vec<ItemPath> = modules.keys().cloned().collect();
    for path in existing {
        let mut current = path;
        while let Some(parent) = current.parent() {
            if !modules.contains_key(&parent) {
                modules.insert(parent.clone(), SemanticModule::empty(parent.clone()));
            }
            current = parent;
        }
    }

    // Assign definition_paths to each module. Items are assigned to the
    // module whose path is their parent. This includes predefined types
    // (whose parent is the root module) and generated items (vftables).
    for (module_path, module) in modules.iter_mut() {
        module.definition_paths = definition_paths.iter()
            .filter(|p| p.parent().map_or(false, |parent| &parent == module_path))
            .cloned()
            .collect();
    }

    // Run validate_uses BEFORE type resolution. This catches private access
    // and nonexistent imports before resolve_item tries to resolve types
    // that reference them.
    let mut validation_state = crate::semantic::SemanticState::from_registry_and_modules(
        type_registry.clone(),
        modules.clone(),
    );
    if let Err(e) = validation_state.validate_uses() {
        return SemanticAnalysis::new(
            db,
            Arc::new(type_registry.clone()),
            Arc::new(modules.clone()),
            Arc::new(doc_links::DocLinkResolver::build(
                &type_registry,
                &modules,
            )),
            Arc::new(vec![e]),
            Arc::new(vec![]),
        );
    }
    // validate_backend_for_targets also needs to run before type resolution
    // so that for_type paths are resolved before backends read them.
    if let Err(e) = validation_state.validate_backend_for_targets() {
        return SemanticAnalysis::new(
            db,
            Arc::new(type_registry.clone()),
            Arc::new(modules.clone()),
            Arc::new(doc_links::DocLinkResolver::build(
                &type_registry,
                &modules,
            )),
            Arc::new(vec![e]),
            Arc::new(vec![]),
        );
    }
    // Pull back any mutations validate_backend_for_targets made to modules.
    modules = validation_state.modules.clone();

    // Resolve all declared items via resolve_item
    let mut semantic_errors: Vec<crate::semantic::SemanticError> = Vec::new();

    for item_path in decl_registry.item_paths() {
        let resolved = resolve_item(db, sources.clone(), pointer_size, item_path.clone());
        let item = resolved.item(db);
        let errors = resolved.errors(db);

        if !errors.is_empty() {
            semantic_errors.extend(errors.iter().cloned());
        }

        type_registry.add((**item).clone());

        // Collect generated items (e.g., vftable struct types) and add them
        // to the registry. Their module definition_paths are registered
        // after modules are built below.
        for gen_item in resolved.generated_items(db).iter() {
            let gen_path = gen_item.path.clone();
            type_registry.add(gen_item.clone());
            definition_paths.insert(gen_path);
        }
    }

    // Re-assign definition_paths to include generated items (vftables etc.)
    for (module_path, module) in modules.iter_mut() {
        module.definition_paths = definition_paths.iter()
            .filter(|p| p.parent().map_or(false, |parent| &parent == module_path))
            .cloned()
            .collect();
    }

    if !semantic_errors.is_empty() {
        return SemanticAnalysis::new(
            db,
            Arc::new(type_registry.clone()),
            Arc::new(modules.clone()),
            Arc::new(doc_links::DocLinkResolver::build(
                &type_registry,
                &modules,
            )),
            Arc::new(semantic_errors),
            Arc::new(vec![]),
        );
    }

    // 1. Resolve extern values
    for module in modules.values_mut() {
        if let Err(e) = module.resolve_extern_values(&mut type_registry) {
            semantic_errors.push(e);
        }
    }

    // 2. Resolve freestanding functions
    for module in modules.values_mut() {
        if let Err(e) = module.resolve_functions(&type_registry) {
            semantic_errors.push(e);
        }
    }

    if !semantic_errors.is_empty() {
        return SemanticAnalysis::new(
            db,
            Arc::new(type_registry.clone()),
            Arc::new(modules.clone()),
            Arc::new(doc_links::DocLinkResolver::build(
                &type_registry,
                &modules,
            )),
            Arc::new(semantic_errors),
            Arc::new(vec![]),
        );
    }

    // 3. Resolve associated functions (impl blocks + base inheritance)
    let mut temp_state = crate::semantic::SemanticState::from_registry_and_modules(type_registry, modules);

    // validate_backend_definitions rejects `prologue definition`/`epilogue definition`
    // on non-cpp backends.
    if let Err(e) = temp_state.validate_backend_definitions() {
        let type_registry = temp_state.type_registry.clone();
        let modules = temp_state.modules.clone();
        return SemanticAnalysis::new(
            db,
            Arc::new(type_registry.clone()),
            Arc::new(modules.clone()),
            Arc::new(doc_links::DocLinkResolver::build(
                &type_registry,
                &modules,
            )),
            Arc::new(vec![e]),
            Arc::new(vec![]),
        );
    }

    if let Err(e) = temp_state.resolve_associated_functions() {
        let type_registry = temp_state.type_registry.clone();
        let modules = temp_state.modules.clone();
        return SemanticAnalysis::new(
            db,
            Arc::new(type_registry.clone()),
            Arc::new(modules.clone()),
            Arc::new(doc_links::DocLinkResolver::build(
                &type_registry,
                &modules,
            )),
            Arc::new(vec![e]),
            Arc::new(vec![]),
        );
    }

    // 4. Doc link resolution
    let type_registry = temp_state.type_registry.clone();
    let modules = temp_state.modules.clone();
    let doc_link_resolver = doc_links::DocLinkResolver::build(&type_registry, &modules);
    if let Err(e) = doc_links::validate(&doc_link_resolver, &type_registry, &modules) {
        return SemanticAnalysis::new(
            db,
            Arc::new(type_registry),
            Arc::new(modules),
            Arc::new(doc_link_resolver),
            Arc::new(vec![e]),
            Arc::new(vec![]),
        );
    }

    SemanticAnalysis::new(
        db,
        Arc::new(type_registry),
        Arc::new(modules),
        Arc::new(doc_link_resolver),
        Arc::new(vec![]),
        Arc::new(vec![]),
    )
}

// --- Helpers ---

/// Cycle recovery function for resolve_item.
/// Returns an unresolved placeholder when a cycle is detected
/// (e.g., mutually recursive types A↔B).
fn cycle_default<'db>(
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

/// Cycle recovery: decides whether to accept the new value or keep iterating.
/// If the new value is resolved, accept it (convergence). Otherwise, keep
/// the old value (still unresolved, keep iterating).
fn cycle_recover<'db>(
    db: &'db dyn Db,
    _cycle: &salsa::Cycle,
    old_value: &ResolvedItem<'db>,
    new_value: ResolvedItem<'db>,
    _sources: SourceSet<'db>,
    _pointer_size: usize,
    _item_path: ItemPath,
) -> ResolvedItem<'db> {
    // If the new value is resolved, use it (convergence)
    if new_value.item(db).is_resolved() {
        new_value
    } else {
        // Keep the old value — Salsa will iterate again
        old_value.clone()
    }
}

/// Build a single item using the existing type_definition/enum/bitflags/type_alias
/// build functions, with the given registry and modules.
fn build_item(
    definition: &crate::grammar::ItemDefinition,
    item_path: &ItemPath,
    visibility: crate::semantic::types::Visibility,
    def_location: &crate::span::ItemLocation,
    doc_comments: &[String],
    type_param_names: &[String],
    type_registry: &mut TypeRegistry,
    modules: &mut BTreeMap<ItemPath, crate::semantic::Module>,
) -> crate::semantic::error::Result<crate::semantic::error::BuildOutcome> {
    match &definition.inner {
        ItemDefinitionInner::Type(ty) => {
            let mut ctx = crate::semantic::resolution_context::ResolutionContext::new(
                type_registry, modules,
            );
            crate::semantic::type_definition::build(
                &mut ctx, item_path, visibility, ty, def_location,
                doc_comments, type_param_names,
            )
        }
        ItemDefinitionInner::Enum(e) => {
            let ctx_ref = crate::semantic::resolution_context::ResolutionContextRef::new(
                type_registry, modules,
            );
            crate::semantic::enum_definition::build(
                &ctx_ref, item_path, e, def_location, doc_comments,
            )
        }
        ItemDefinitionInner::Bitflags(b) => {
            let ctx_ref = crate::semantic::resolution_context::ResolutionContextRef::new(
                type_registry, modules,
            );
            crate::semantic::bitflags_definition::build(
                &ctx_ref, item_path, b, def_location, doc_comments,
            )
        }
        ItemDefinitionInner::TypeAlias(ta) => {
            let ctx_ref = crate::semantic::resolution_context::ResolutionContextRef::new(
                type_registry, modules,
            );
            crate::semantic::type_alias_definition::build(
                &ctx_ref, item_path, ta, def_location, doc_comments, type_param_names,
            )
        }
    }
}

fn register_predefined(type_registry: &mut TypeRegistry) {
    for predefined in crate::semantic::types::PredefinedItem::ALL {
        let path = ItemPath::from(predefined.name());
        let size = predefined.size();
        let alignment = size.max(1);
        let location = crate::span::ItemLocation::internal();
        type_registry.add(crate::semantic::types::ItemDefinition {
            visibility: crate::semantic::types::Visibility::Public,
            path,
            type_parameters: vec![],
            state: crate::semantic::types::ItemState::Resolved(
                crate::semantic::types::ItemStateResolved {
                    size,
                    alignment,
                    inner: crate::semantic::types::TypeDefinition {
                        copyable: true,
                        cloneable: true,
                        defaultable: true,
                        ..Default::default()
                    }
                    .into(),
                },
            ),
            category: crate::semantic::types::ItemCategory::Predefined,
            predefined: Some(*predefined),
            cfg: None,
            location,
            declaration_location: location,
        });
    }
}

fn register_unresolved(
    type_registry: &mut TypeRegistry,
    path: &ItemPath,
    definition: &crate::grammar::ItemDefinition,
) {
    let type_parameters: Vec<String> = definition
        .type_parameters
        .iter()
        .map(|tp| tp.name.clone())
        .collect();
    let cfg = match &definition.inner {
        ItemDefinitionInner::Type(td) => td.attributes.cfg(),
        ItemDefinitionInner::Enum(e) => e.attributes.cfg(),
        ItemDefinitionInner::Bitflags(b) => b.attributes.cfg(),
        ItemDefinitionInner::TypeAlias(ta) => ta.attributes.cfg(),
    };
    type_registry.add(crate::semantic::types::ItemDefinition {
        visibility: definition.visibility.into(),
        path: path.clone(),
        type_parameters,
        state: crate::semantic::types::ItemState::Unresolved(definition.clone()),
        category: crate::semantic::types::ItemCategory::Defined,
        predefined: None,
        cfg,
        location: definition.location,
        declaration_location: definition.declaration_location,
    });
}

fn make_unresolved_definition(path: &ItemPath) -> crate::semantic::types::ItemDefinition {
    use crate::grammar::{Ident, ItemDefinition as GrammarDef, ItemDefinitionInner, TypeDefinition};
    use crate::span::ItemLocation;

    let grammar_def = GrammarDef {
        visibility: crate::grammar::Visibility::Private,
        name: Ident::from(""),
        type_parameters: vec![],
        inner: ItemDefinitionInner::Type(TypeDefinition {
            items: vec![],
            attributes: Default::default(),
            inline_trailing_comments: vec![],
            following_comments: vec![],
        }),
        doc_comments: vec![],
        location: ItemLocation::internal(),
        declaration_location: ItemLocation::internal(),
    };

    crate::semantic::types::ItemDefinition {
        visibility: crate::semantic::types::Visibility::Public,
        path: path.clone(),
        type_parameters: vec![],
        state: crate::semantic::types::ItemState::Unresolved(grammar_def),
        category: crate::semantic::types::ItemCategory::Defined,
        predefined: None,
        cfg: None,
        location: ItemLocation::internal(),
        declaration_location: ItemLocation::internal(),
    }
}

fn make_predefined_definition(path: &ItemPath, size: usize, alignment: usize) -> crate::semantic::types::ItemDefinition {
    crate::semantic::types::ItemDefinition {
        visibility: crate::semantic::types::Visibility::Public,
        path: path.clone(),
        type_parameters: vec![],
        state: crate::semantic::types::ItemState::Resolved(
            crate::semantic::types::ItemStateResolved {
                size,
                alignment,
                inner: crate::semantic::types::TypeDefinition {
                    copyable: true,
                    cloneable: true,
                    defaultable: true,
                    ..Default::default()
                }
                .into(),
            },
        ),
        category: crate::semantic::types::ItemCategory::Predefined,
        predefined: None,
        cfg: None,
        location: crate::span::ItemLocation::internal(),
        declaration_location: crate::span::ItemLocation::internal(),
    }
}

fn make_extern_definition(path: &ItemPath, info: &crate::semantic::declaration_registry::ExternTypeInfo) -> crate::semantic::types::ItemDefinition {
    crate::semantic::types::ItemDefinition {
        visibility: crate::semantic::types::Visibility::Public,
        path: path.clone(),
        type_parameters: vec![],
        state: crate::semantic::types::ItemState::Resolved(
            crate::semantic::types::ItemStateResolved {
                size: info.size,
                alignment: info.alignment,
                inner: crate::semantic::types::TypeDefinition {
                    doc: info.doc_comments.clone(),
                    ..Default::default()
                }.into(),
            },
        ),
        category: crate::semantic::types::ItemCategory::Extern,
        predefined: None,
        cfg: info.cfg.clone(),
        location: info.location,
        declaration_location: info.declaration_location,
    }
}
