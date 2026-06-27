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
use crate::semantic::TypeRegistry;
use crate::semantic::declaration_registry::DeclarationRegistry;
use crate::span::FileId;

use super::db::Db;
use super::inputs::{SourceFile, SourceSet};
use super::ir::{DeclarationSet, ParsedFile, ResolvedItem, SemanticAnalysis, TokenizedFile};

/// Parse a single file. Leaf query — re-runs only when that file's content changes.
/// Tokenize a file. Memoized so `parse_file` and editor tooling (hover,
/// highlighting) reuse the same token stream instead of re-lexing.
#[salsa::tracked]
pub fn tokenize_file(db: &dyn Db, source: SourceFile) -> TokenizedFile<'_> {
    let contents = source.contents(db);
    let file_id = FileId::new(source.file_id(db));
    match crate::tokenizer::tokenize_with_file_id(contents.to_string(), file_id) {
        Ok(tokens) => TokenizedFile::new(db, Arc::new(tokens), Arc::new(vec![])),
        Err(e) => TokenizedFile::new(db, Arc::new(vec![]), Arc::new(vec![e.into()])),
    }
}

#[salsa::tracked]
pub fn parse_file(db: &dyn Db, source: SourceFile) -> ParsedFile<'_> {
    let tokenized = tokenize_file(db, source);
    let lex_errors = tokenized.errors(db);
    if !lex_errors.is_empty() {
        return ParsedFile::new(db, source, Arc::new(Module::default()), lex_errors.clone());
    }

    let contents = source.contents(db);
    let file_id = FileId::new(source.file_id(db));
    let tokens = (**tokenized.tokens(db)).clone();
    let mut parser = crate::parser::Parser::new(tokens, file_id, contents.to_string());
    match parser.parse_module() {
        Ok(module) => ParsedFile::new(db, source, Arc::new(module), Arc::new(vec![])),
        Err(e) => ParsedFile::new(db, source, Arc::new(Module::default()), Arc::new(vec![e])),
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
/// It builds a TypeRegistry with the target item's dependencies resolved
/// (via recursive resolve_item calls that Salsa memoizes), then resolves
/// the target item.
///
/// When type A references type B, resolve_item(A) calls resolve_item(B).
/// Salsa tracks this dependency. If B changes, only A (and its dependents)
/// re-resolve.
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
/// Resolve a single item (type/enum/bitflags/type-alias).
///
/// This is NOT a Salsa tracked function — it's a plain function that tries
/// to resolve the item given the current state of the type registry. The
/// iterative resolution loop in `analyze` calls this repeatedly until all
/// items are resolved or no progress can be made.
///
/// Salsa incrementality is preserved at the `parse_file` level — when a
/// file changes, `parse_file` re-runs, `collect_declarations` re-runs, and
/// `analyze` re-runs the iterative loop from scratch. Per-item caching is
/// lost, but correctness is maintained.
fn try_resolve_item(
    registry: &DeclarationRegistry,
    pointer_size: usize,
    item_path: &ItemPath,
    resolved_items: &BTreeMap<ItemPath, crate::semantic::types::ItemDefinition>,
) -> (
    crate::semantic::types::ItemDefinition,
    Vec<crate::semantic::SemanticError>,
    Vec<crate::semantic::types::ItemDefinition>,
) {
    // Check if it's a predefined or extern type (already resolved)
    if let Some(info) = registry.get_predefined(item_path) {
        return (
            make_predefined_definition(item_path, info.size, info.alignment),
            vec![],
            vec![],
        );
    }
    if let Some(info) = registry.get_extern_type(item_path) {
        return (make_extern_definition(item_path, info), vec![], vec![]);
    }

    // Find this item's declaration
    let Some(definition) = registry.get_definition(item_path) else {
        return (make_unresolved_definition(item_path), vec![], vec![]);
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

    // Inject items already resolved in prior iterations as Resolved.
    // This lets the current item's build see resolved dependencies without
    // needing recursive resolve_item calls — the iterative loop in analyze()
    // feeds these in.
    for (resolved_path, resolved_def) in resolved_items {
        if resolved_path != item_path {
            type_registry.add(resolved_def.clone());
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

    let visibility: crate::semantic::types::Visibility = definition.visibility.into();
    let def_location = definition.location;
    let type_param_names: Vec<String> = definition
        .type_parameters
        .iter()
        .map(|tp| tp.name.clone())
        .collect();

    // Try to build the item once. Dependencies that aren't resolved yet
    // will cause Deferred — analyze's iterative loop handles re-trying.
    let outcome = build_item(
        &definition,
        item_path,
        visibility,
        &def_location,
        &definition.doc_comments,
        &type_param_names,
        &mut type_registry,
        &mut modules,
    );

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
            (make_unresolved_definition(item_path), vec![])
        }
        Ok(crate::semantic::error::BuildOutcome::NotFoundType(unresolved_ref)) => (
            make_unresolved_definition(item_path),
            vec![crate::semantic::SemanticError::TypeResolutionStalled {
                unresolved_types: vec![item_path.to_string()],
                resolved_types: vec![],
                unresolved_references: vec![unresolved_ref],
            }],
        ),
        Err(e) => (make_unresolved_definition(item_path), vec![e]),
    };

    // Collect generated items (e.g., vftable struct types)
    let mut generated_items = Vec::new();
    for (path, item) in type_registry.iter() {
        if path == item_path {
            continue;
        }
        if registry.contains(path)
            || registry.get_predefined(path).is_some()
            || registry.get_extern_type(path).is_some()
        {
            continue;
        }
        if item.is_resolved() {
            generated_items.push((*item).clone());
        }
    }

    (item_def, errors, generated_items)
}

/// Salsa tracked wrapper for the LSP. Resolves a single item, caching
/// the result. Uses cycle_result to handle mutually recursive types.
#[salsa::tracked(cycle_result=cycle_result_fn)]
pub fn resolve_item<'db>(
    db: &'db dyn Db,
    sources: SourceSet<'db>,
    pointer_size: usize,
    item_path: ItemPath,
) -> ResolvedItem<'db> {
    let decl_set = collect_declarations(db, sources.clone(), pointer_size);
    let registry = decl_set.registry(db);

    // Run a mini iterative resolution loop to resolve dependencies before
    // resolving the target item. This mirrors analyze()'s loop but scoped to
    // resolving just what the target item needs. We resolve all items (cheap
    // enough for LSP single-item queries) so the target sees its dependencies.
    let item_paths: Vec<ItemPath> = registry.item_paths().cloned().collect();
    let mut resolved_items: BTreeMap<ItemPath, crate::semantic::types::ItemDefinition> =
        BTreeMap::new();
    let max_iterations = item_paths.len() + 1;

    for _iteration in 0..max_iterations {
        let mut made_progress = false;

        for path in &item_paths {
            if resolved_items.contains_key(path) {
                continue;
            }

            let (item_def, errors, _gen_items) =
                try_resolve_item(registry, pointer_size, path, &resolved_items);

            // If the item has errors, record it as resolved-with-error so we
            // don't retry it. If it's resolved, record it. If deferred, skip.
            if !errors.is_empty() || item_def.is_resolved() {
                resolved_items.insert(path.clone(), item_def);
                made_progress = true;
            }
        }

        if !made_progress {
            break;
        }
        if item_paths.iter().all(|p| resolved_items.contains_key(p)) {
            break;
        }
    }

    // Now resolve the target item with all dependencies available
    let (item_def, errors, generated_items) =
        try_resolve_item(registry, pointer_size, &item_path, &resolved_items);

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

    let mut definition_paths: std::collections::BTreeSet<ItemPath> =
        std::collections::BTreeSet::new();

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
                let address =
                    address.ok_or_else(|| crate::semantic::SemanticError::MissingAttribute {
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

                match SemanticModule::new(
                    module_path.clone(),
                    module.as_ref().clone(),
                    ev,
                    &impls,
                    &backends,
                ) {
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
        module.definition_paths = definition_paths
            .iter()
            .filter(|p| p.parent().map_or(false, |parent| &parent == module_path))
            .cloned()
            .collect();
    }

    // Run validate_uses BEFORE type resolution. This catches private access
    // and nonexistent imports before resolve_item tries to resolve types
    // that reference them.
    if let Err(e) = crate::semantic::validation::validate_uses(&type_registry, &modules) {
        return SemanticAnalysis::new(
            db,
            Arc::new(type_registry.clone()),
            Arc::new(modules.clone()),
            Arc::new(doc_links::DocLinkResolver::build(&type_registry, &modules)),
            Arc::new(vec![e]),
            Arc::new(vec![]),
        );
    }
    // validate_backend_for_targets also needs to run before type resolution
    // so that for_type paths are resolved before backends read them.
    if let Err(e) =
        crate::semantic::validation::validate_backend_for_targets(&type_registry, &mut modules)
    {
        return SemanticAnalysis::new(
            db,
            Arc::new(type_registry.clone()),
            Arc::new(modules.clone()),
            Arc::new(doc_links::DocLinkResolver::build(&type_registry, &modules)),
            Arc::new(vec![e]),
            Arc::new(vec![]),
        );
    }

    // Resolve all declared items iteratively. try_resolve_item resolves a
    // single item given the current state of already-resolved dependencies.
    // We iterate until no progress is made (mutually-recursive or stalled
    // types) or all items resolve. This replaces the old recursive resolve_item
    // — Salsa incrementality is preserved at the parse_file level.
    let mut semantic_errors: Vec<crate::semantic::SemanticError> = Vec::new();

    // Items resolved so far, keyed by path. Fed back into try_resolve_item so
    // that dependent items see resolved dependencies without recursion.
    let mut resolved_items: BTreeMap<ItemPath, crate::semantic::types::ItemDefinition> =
        BTreeMap::new();

    // Collect generated items (e.g., vftable struct types) to add to the
    // final registry. Their module definition_paths are registered below.
    let mut generated_items: Vec<crate::semantic::types::ItemDefinition> = Vec::new();

    let item_paths: Vec<ItemPath> = decl_registry.item_paths().cloned().collect();
    let max_iterations = item_paths.len() + 1;

    for _iteration in 0..max_iterations {
        let mut made_progress = false;

        for item_path in &item_paths {
            // Skip already-resolved items
            if resolved_items.contains_key(item_path) {
                continue;
            }

            let (item_def, errors, gen_items) =
                try_resolve_item(decl_registry, pointer_size, item_path, &resolved_items);

            if !errors.is_empty() {
                semantic_errors.extend(errors.iter().cloned());
                // Mark as resolved-with-error so we don't retry it
                resolved_items.insert(item_path.clone(), item_def);
                made_progress = true;
                continue;
            }

            if item_def.is_resolved() {
                resolved_items.insert(item_path.clone(), item_def);
                made_progress = true;
                // Collect generated items from this resolution
                for gen_item in gen_items {
                    let gen_path = gen_item.path.clone();
                    definition_paths.insert(gen_path);
                    generated_items.push(gen_item);
                }
            }
            // else: Deferred — will be retried in the next iteration once
            // more dependencies are resolved.
        }

        if !made_progress {
            // No item resolved this iteration — remaining items are stalled
            // (mutual recursion or missing types). Emit a TypeResolutionStalled
            // error for each remaining unresolved item, matching the old
            // batch-compiler behavior.
            for item_path in &item_paths {
                if !resolved_items.contains_key(item_path) {
                    semantic_errors.push(crate::semantic::SemanticError::TypeResolutionStalled {
                        unresolved_types: vec![item_path.to_string()],
                        resolved_types: vec![],
                        unresolved_references: vec![],
                    });
                }
            }
            break;
        }

        // If all items are resolved, we're done
        if item_paths.iter().all(|p| resolved_items.contains_key(p)) {
            break;
        }
    }

    // Merge resolved items + generated items into the type registry
    for item_def in resolved_items.values() {
        type_registry.add(item_def.clone());
    }
    for gen_item in &generated_items {
        type_registry.add(gen_item.clone());
    }

    // Re-assign definition_paths to include generated items (vftables etc.)
    for (module_path, module) in modules.iter_mut() {
        module.definition_paths = definition_paths
            .iter()
            .filter(|p| p.parent().map_or(false, |parent| &parent == module_path))
            .cloned()
            .collect();
    }

    if !semantic_errors.is_empty() {
        return SemanticAnalysis::new(
            db,
            Arc::new(type_registry.clone()),
            Arc::new(modules.clone()),
            Arc::new(doc_links::DocLinkResolver::build(&type_registry, &modules)),
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
            Arc::new(doc_links::DocLinkResolver::build(&type_registry, &modules)),
            Arc::new(semantic_errors),
            Arc::new(vec![]),
        );
    }

    // validate_backend_definitions rejects `prologue definition`/`epilogue definition`
    // on non-cpp backends.
    if let Err(e) = crate::semantic::validation::validate_backend_definitions(&modules) {
        return SemanticAnalysis::new(
            db,
            Arc::new(type_registry.clone()),
            Arc::new(modules.clone()),
            Arc::new(doc_links::DocLinkResolver::build(&type_registry, &modules)),
            Arc::new(vec![e]),
            Arc::new(vec![]),
        );
    }

    // Associated functions are computed lazily by compute_associated_functions
    // (a tracked query). We need them merged into the type registry before
    // building the doc link resolver (which reads associated_functions for
    // method links). So we construct the analysis, compute AF, merge, and
    // rebuild the analysis with the merged registry.

    // Construct the initial analysis (without AF merged)
    let initial_analysis = SemanticAnalysis::new(
        db,
        Arc::new(type_registry.clone()),
        Arc::new(modules.clone()),
        // Placeholder doc link resolver — will be rebuilt after AF merge
        Arc::new(doc_links::DocLinkResolver::build(
            &TypeRegistry::new(pointer_size),
            &BTreeMap::new(),
        )),
        Arc::new(vec![]),
        Arc::new(vec![]),
    );

    // Compute associated functions and merge into the type registry
    let af_result = compute_associated_functions(db, initial_analysis);

    // If there are AF errors, include them
    if !af_result.errors.is_empty() {
        let doc_link_resolver = doc_links::DocLinkResolver::build(&type_registry, &modules);
        return SemanticAnalysis::new(
            db,
            Arc::new(type_registry),
            Arc::new(modules),
            Arc::new(doc_link_resolver),
            Arc::new(af_result.errors.clone()),
            Arc::new(vec![]),
        );
    }

    // Merge associated functions into type definitions
    let mut type_registry = type_registry;
    for (path, fns) in af_result.functions.iter() {
        if let Ok(item) = type_registry.get_mut(path, &crate::span::ItemLocation::internal()) {
            if let crate::semantic::types::ItemState::Resolved(state) = &mut item.state {
                if let crate::semantic::types::ItemDefinitionInner::Type(td) = &mut state.inner {
                    td.associated_functions = fns.clone();
                }
            }
        }
    }

    // Doc link resolution (now with associated functions in the registry)
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

/// Compute associated functions (own impl methods + inherited from base types)
/// for all resolved types. Returns a map from type path to associated functions,
/// plus any errors (e.g., duplicate method names).
///
/// This is a derived query — it reads the resolved type registry and modules
/// from `SemanticAnalysis` and produces a new result without mutating the
/// registry. The map is merged into type definitions by `to_semantic_output`.
#[salsa::tracked]
pub fn compute_associated_functions<'db>(
    db: &'db dyn Db,
    analysis: SemanticAnalysis<'db>,
) -> Arc<AssociatedFunctionsResult> {
    use crate::semantic::{
        SemanticError,
        error::DuplicateDefinitionKind,
        function::{self, FunctionBuildOutcome},
        types::{Function, FunctionBody, ItemDefinitionInner, Type},
    };
    use crate::span::HasLocation;
    use std::collections::{BTreeMap as BTree, BTreeSet, HashMap, HashSet};

    let type_registry = analysis.type_registry(db);
    let modules = analysis.modules(db);
    let location = crate::span::ItemLocation::internal();

    // Build a map from each type-with-regions to its base parents.
    let mut bases_of: BTree<ItemPath, Vec<(String, ItemPath)>> = BTree::new();
    let mut all_type_paths: Vec<ItemPath> = Vec::new();
    for path in type_registry.resolved() {
        let Ok(item) = type_registry.get(&path, &location) else {
            continue;
        };
        let Some(resolved) = item.resolved() else {
            continue;
        };
        let ItemDefinitionInner::Type(td) = &resolved.inner else {
            continue;
        };
        all_type_paths.push(path.clone());
        let mut bases = Vec::new();
        for region in &td.regions {
            if !region.is_base {
                continue;
            }
            let Some(name) = region.name.clone() else {
                continue;
            };
            if let Type::Raw(base_path) = &region.type_ref {
                bases.push((name, base_path.clone()));
            }
        }
        bases_of.insert(path, bases);
    }

    // Topologically sort by inheritance: bases before derived.
    let mut order: Vec<ItemPath> = Vec::new();
    let mut visited: BTreeSet<ItemPath> = BTreeSet::new();
    fn visit(
        path: &ItemPath,
        bases_of: &BTree<ItemPath, Vec<(String, ItemPath)>>,
        visited: &mut BTreeSet<ItemPath>,
        order: &mut Vec<ItemPath>,
    ) {
        if !visited.insert(path.clone()) {
            return;
        }
        if let Some(bases) = bases_of.get(path) {
            for (_, base) in bases {
                visit(base, bases_of, visited, order);
            }
        }
        order.push(path.clone());
    }
    for p in &all_type_paths {
        visit(p, &bases_of, &mut visited, &mut order);
    }

    // Process types in topological order, building up the associated functions map.
    let mut result: BTree<ItemPath, Vec<Function>> = BTree::new();
    let mut errors: Vec<SemanticError> = Vec::new();

    for path in &order {
        let Ok(item) = type_registry.get(path, &location) else {
            continue;
        };
        let item_location = item.location;
        let Some(resolved) = item.resolved() else {
            continue;
        };
        let ItemDefinitionInner::Type(td) = &resolved.inner else {
            continue;
        };
        let regions = td.regions.clone();
        let vftable_function_names: HashSet<String> = td
            .vftable
            .as_ref()
            .map(|v| v.functions.iter().map(|f| f.name.clone()).collect())
            .unwrap_or_default();

        // Find the module for this type (for impl blocks + scope)
        let module_path = path.parent().unwrap_or_else(ItemPath::empty);
        let Some(module) = modules.get(&module_path) else {
            continue;
        };
        let scope = module.scope();

        // Collect impl functions for this type
        struct ImplFunc {
            func: crate::grammar::Function,
            impl_type_parameters: Vec<String>,
            impl_cfg: Option<crate::parser::cfg::CfgPredicate>,
        }
        let impl_funcs: Vec<ImplFunc> = module
            .impls
            .get(path)
            .map(|fbs| {
                fbs.iter()
                    .flat_map(|fb| {
                        let params: Vec<String> = fb
                            .type_parameters
                            .iter()
                            .map(|tp| tp.name.clone())
                            .collect();
                        let block_cfg = fb.attributes.cfg();
                        fb.functions().cloned().map({
                            let params = params.clone();
                            let block_cfg = block_cfg.clone();
                            move |f| ImplFunc {
                                func: f,
                                impl_type_parameters: params.clone(),
                                impl_cfg: block_cfg.clone(),
                            }
                        })
                    })
                    .collect()
            })
            .unwrap_or_default();

        let struct_param_count = item.type_parameters.len();

        let mut associated_functions: Vec<Function> = Vec::new();
        let mut used_names: HashSet<String> = vftable_function_names.clone();

        // Inherit from base regions
        for (i, region) in regions.iter().filter(|r| r.is_base).enumerate() {
            let Some(name) = region.name.clone() else {
                continue;
            };
            let Type::Raw(base_path) = &region.type_ref else {
                continue;
            };

            // Get base's associated functions from our result map
            let Some(base_fns) = result.get(base_path) else {
                continue;
            };

            let add_functions = |functions: &[Function],
                                 associated_functions: &mut Vec<Function>,
                                 used_names: &mut HashSet<String>| {
                for function in functions.iter().filter(|f| f.is_public()) {
                    let has_self = function.arguments.iter().any(|a| {
                        matches!(
                            a,
                            crate::semantic::function::Argument::ConstSelf { .. }
                                | crate::semantic::function::Argument::MutSelf { .. }
                        )
                    });
                    if !has_self {
                        continue;
                    }
                    let mut function = function.clone();
                    let original_name = function.name.clone();
                    if used_names.contains(&original_name) {
                        function.name = format!("{name}_{original_name}");
                    }
                    function.body = FunctionBody::Field {
                        field: name.clone(),
                        function_name: original_name,
                    };
                    used_names.insert(function.name.clone());
                    associated_functions.push(function);
                }
            };

            add_functions(base_fns, &mut associated_functions, &mut used_names);

            // For multiple inheritance, also inherit vftable functions from bases after the first
            if i > 0 {
                if let Ok(base_item) = type_registry.get(base_path, &location) {
                    if let Some(base_resolved) = base_item.resolved() {
                        if let ItemDefinitionInner::Type(base_td) = &base_resolved.inner {
                            if let Some(vftable) = &base_td.vftable {
                                add_functions(
                                    &vftable.functions,
                                    &mut associated_functions,
                                    &mut used_names,
                                );
                            }
                        }
                    }
                }
            }
        }

        // Build own impl methods
        let reserved = used_names.clone();
        let mut own_method_cfgs: HashMap<String, Vec<Option<crate::parser::cfg::CfgPredicate>>> =
            HashMap::new();

        for ImplFunc {
            func: grammar_fn,
            impl_type_parameters,
            impl_cfg,
        } in &impl_funcs
        {
            let method_extras: Vec<String> = if impl_type_parameters.len() > struct_param_count {
                impl_type_parameters[struct_param_count..].to_vec()
            } else {
                Vec::new()
            };

            let function = match function::build(
                type_registry,
                &scope,
                false,
                grammar_fn,
                impl_type_parameters,
            ) {
                Ok(FunctionBuildOutcome::Built(f)) => *f,
                Ok(FunctionBuildOutcome::Deferred) => {
                    errors.push(SemanticError::TypeResolutionStalled {
                        unresolved_types: vec![grammar_fn.name.0.clone()],
                        resolved_types: vec![],
                        unresolved_references: vec![],
                    });
                    continue;
                }
                Err(e) => {
                    errors.push(e);
                    continue;
                }
            };
            let mut function = function;
            function.method_type_parameters = method_extras;

            function.cfg = match (impl_cfg.clone(), function.cfg.take()) {
                (None, None) => None,
                (Some(b), None) => Some(b),
                (None, Some(f)) => Some(f),
                (Some(b), Some(f)) => Some(crate::parser::cfg::CfgPredicate::All {
                    predicates: vec![b, f],
                    location: item_location,
                }),
            };

            let name = function.name.clone();
            if reserved.contains(&name) {
                errors.push(SemanticError::DuplicateDefinition {
                    name: name.clone(),
                    item_path: path.clone(),
                    kind: DuplicateDefinitionKind::FunctionInTypeOrBase,
                    location: item_location,
                });
                continue;
            }
            let disjoint_with_existing = own_method_cfgs
                .get(&name)
                .map(|cfgs| {
                    cfgs.iter().all(|e| {
                        crate::parser::cfg::CfgPredicate::provably_disjoint(
                            e.as_ref(),
                            function.cfg.as_ref(),
                        )
                    })
                })
                .unwrap_or(true);
            if !disjoint_with_existing {
                errors.push(SemanticError::DuplicateDefinition {
                    name: name.clone(),
                    item_path: path.clone(),
                    kind: DuplicateDefinitionKind::FunctionInTypeOrBase,
                    location: item_location,
                });
                continue;
            }

            if !used_names.contains(&name) {
                used_names.insert(name.clone());
            }
            own_method_cfgs
                .entry(name)
                .or_default()
                .push(function.cfg.clone());
            associated_functions.push(function);
        }

        result.insert(path.clone(), associated_functions);
    }

    Arc::new(AssociatedFunctionsResult {
        functions: result,
        errors,
    })
}

/// Result of compute_associated_functions: the functions map plus any errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssociatedFunctionsResult {
    /// Map from type path to associated functions (own + inherited).
    pub functions: BTreeMap<ItemPath, Vec<crate::semantic::types::Function>>,
    /// Errors encountered during computation (e.g., duplicate method names).
    pub errors: Vec<crate::semantic::SemanticError>,
}

// --- Helpers ---

/// Cycle recovery: when resolve_item(A) calls resolve_item(B) which calls
/// resolve_item(A) (mutual recursion), return an unresolved placeholder.
/// The iterative loop in resolve_item handles convergence by resolving
/// dependencies one at a time — when the cycle unwinds, the placeholder
/// causes build_item to return Deferred, and the loop resolves the
/// dependency via the unresolved() list instead of recursing.
fn cycle_result_fn<'db>(
    db: &'db dyn Db,
    _id: salsa::Id,
    sources: SourceSet<'db>,
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
            let mut ctx =
                crate::semantic::resolution_context::ResolutionContext::new(type_registry, modules);
            crate::semantic::type_definition::build(
                &mut ctx,
                item_path,
                visibility,
                ty,
                def_location,
                doc_comments,
                type_param_names,
            )
        }
        ItemDefinitionInner::Enum(e) => {
            let ctx_ref = crate::semantic::resolution_context::ResolutionContextRef::new(
                type_registry,
                modules,
            );
            crate::semantic::enum_definition::build(
                &ctx_ref,
                item_path,
                e,
                def_location,
                doc_comments,
            )
        }
        ItemDefinitionInner::Bitflags(b) => {
            let ctx_ref = crate::semantic::resolution_context::ResolutionContextRef::new(
                type_registry,
                modules,
            );
            crate::semantic::bitflags_definition::build(
                &ctx_ref,
                item_path,
                b,
                def_location,
                doc_comments,
            )
        }
        ItemDefinitionInner::TypeAlias(ta) => {
            let ctx_ref = crate::semantic::resolution_context::ResolutionContextRef::new(
                type_registry,
                modules,
            );
            crate::semantic::type_alias_definition::build(
                &ctx_ref,
                item_path,
                ta,
                def_location,
                doc_comments,
                type_param_names,
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
    use crate::grammar::{
        Ident, ItemDefinition as GrammarDef, ItemDefinitionInner, TypeDefinition,
    };
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

fn make_predefined_definition(
    path: &ItemPath,
    size: usize,
    alignment: usize,
) -> crate::semantic::types::ItemDefinition {
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

fn make_extern_definition(
    path: &ItemPath,
    info: &crate::semantic::declaration_registry::ExternTypeInfo,
) -> crate::semantic::types::ItemDefinition {
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
                }
                .into(),
            },
        ),
        category: crate::semantic::types::ItemCategory::Extern,
        predefined: None,
        cfg: info.cfg.clone(),
        location: info.location,
        declaration_location: info.declaration_location,
    }
}
