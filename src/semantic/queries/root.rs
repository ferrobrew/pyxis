//! The root query. `analyze` drives per-item resolution through `resolve_item`
//! (so it stays incremental) and then runs the post-resolution passes — extern
//! values, functions, associated functions, doc links — producing the
//! `SemanticAnalysis` that both the batch compiler and the LSP consume.
//! `compute_associated_functions` is a plain helper that folds inherited and
//! own impl methods into each type.

use std::{collections::BTreeMap, sync::Arc};

use crate::{
    grammar::{ItemPath, Module},
    parser::ParseError,
    semantic::{SemanticError, TypeRegistry, types::Function},
};

use super::{
    super::{
        db::Db,
        inputs::{SourceFile, SourceSet},
        ir::SemanticAnalysis,
    },
    helpers::{
        make_extern_definition, merge_associated_functions, register_predefined,
        register_unresolved,
    },
    index::collect_declarations,
    leaf::parse_file,
    resolve::resolve_item,
};

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
    use crate::{
        semantic::{
            doc_links, module::Module as SemanticModule, types::PredefinedItem, validation,
        },
        span::HasLocation,
    };

    // Every result of `analyze` is a `SemanticAnalysis` with empty `parse_errors`
    // (parse errors take the dedicated early return below) — these two helpers
    // build one so each call site states only what differs.
    //
    // `bail` is for the error paths that exit before the final doc-link merge: it
    // builds a doc-link resolver with associated (impl) functions merged into a
    // clone of the registry, so `Type::method` links still resolve. Without this,
    // any single semantic error in a project breaks every impl-method doc link
    // across it (common for in-progress reverse-engineering definitions).
    let bail = |type_registry: &TypeRegistry,
                modules: &BTreeMap<ItemPath, SemanticModule>,
                errors: Vec<SemanticError>|
     -> SemanticAnalysis<'db> {
        let mut merged = type_registry.clone();
        let _ = merge_associated_functions(&mut merged, modules);
        let doc_link_resolver = doc_links::DocLinkResolver::build(&merged, modules);
        SemanticAnalysis::new(
            db,
            Arc::new(type_registry.clone()),
            Arc::new(modules.clone()),
            Arc::new(doc_link_resolver),
            Arc::new(errors),
            Arc::new(vec![]),
        )
    };

    // `finish` is for the paths that already hold the final doc-link resolver
    // (the success path and the two passes that build it themselves); it takes
    // the parts by value rather than cloning.
    let finish = |type_registry: TypeRegistry,
                  modules: BTreeMap<ItemPath, SemanticModule>,
                  doc_link_resolver: doc_links::DocLinkResolver,
                  errors: Vec<SemanticError>|
     -> SemanticAnalysis<'db> {
        SemanticAnalysis::new(
            db,
            Arc::new(type_registry),
            Arc::new(modules),
            Arc::new(doc_link_resolver),
            Arc::new(errors),
            Arc::new(vec![]),
        )
    };

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
    let decl_set = collect_declarations(db, sources, pointer_size);
    let decl_registry = decl_set.registry(db);

    // Resolve all items via resolve_item (Salsa tracks dependencies)
    let mut type_registry = TypeRegistry::new(pointer_size);
    register_predefined(&mut type_registry);

    let mut definition_paths: std::collections::BTreeSet<ItemPath> =
        std::collections::BTreeSet::new();

    // Predefined types live in the root module; add them to definition_paths
    // so the JSON backend (which iterates module.definitions) includes them.
    for predefined in PredefinedItem::ALL {
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

    // Errors from building module scopes (missing `#[address]` on an extern
    // value, malformed module) — collected and returned as diagnostics rather
    // than dropped, so the offending module isn't silently elided.
    let mut module_errors: Vec<SemanticError> = Vec::new();

    // Reject value items (`const` / `extern` value) nested inside a generic
    // type: the backends can't emit a correctly-qualified `impl<T> Parent<T>`
    // accessor for them, and a fixed-address / compile-time value scoped under a
    // per-instantiation generic is semantically murky. Nesting types / enums /
    // bitflags inside a generic is fine.
    for item_path in decl_registry.item_paths() {
        let Some(def) = decl_registry.get_definition(item_path) else {
            continue;
        };
        if !matches!(
            def.inner,
            crate::grammar::ItemDefinitionInner::Constant(_)
                | crate::grammar::ItemDefinitionInner::ExternValue(_)
        ) {
            continue;
        }
        let Some(parent) = item_path.parent() else {
            continue;
        };
        if let Some(parent_def) = decl_registry.get_definition(&parent)
            && !parent_def.type_parameters.is_empty()
        {
            module_errors.push(SemanticError::ValueItemInGenericParent {
                item_path: item_path.clone(),
                parent_path: parent,
                location: def.declaration_location,
            });
        }
    }

    // Track module paths produced by the parsed source files so that two
    // distinct files reducing to the same module path (e.g. `world.pyxis` and
    // `world/mod.pyxis` -> `world`) surface a hard `DuplicateModule` error
    // rather than silently overwriting each other. The root module
    // (`ItemPath::empty()`, inserted above) and the synthesized ancestor
    // modules (added below) are deliberately excluded.
    let mut seen_module_paths: std::collections::BTreeSet<ItemPath> =
        std::collections::BTreeSet::new();

    for (source, module) in &parsed_modules {
        let path_str = source.path(db);
        let module_path = ItemPath::from_path(std::path::Path::new(path_str.as_str()));

        if !module_path.is_empty() && !seen_module_paths.insert(module_path.clone()) {
            let location = module
                .items
                .first()
                .map(|item| *item.location())
                .unwrap_or_else(crate::span::ItemLocation::internal);
            module_errors.push(SemanticError::DuplicateModule {
                path: module_path,
                location,
            });
            continue;
        }

        // Extern values are ordinary registry items (`ItemDefinitionInner::ExternValue`),
        // built via the resolution pipeline like any other item; nothing to collect here.
        let impls: Vec<_> = module.impls().cloned().collect();
        let backends: Vec<_> = module.backends().cloned().collect();

        match SemanticModule::new(
            module_path.clone(),
            module.as_ref().clone(),
            &impls,
            &backends,
        ) {
            Ok(m) => {
                modules.insert(module_path, m);
            }
            Err(e) => module_errors.push(e),
        }
    }

    if !module_errors.is_empty() {
        return bail(&type_registry, &modules, module_errors);
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

    // Assign definition_paths to each module. Items are assigned to their
    // declaring module (the module that contains them). For top-level items
    // this is the parent; for nested items (parent is a type, not a module),
    // the declaring module is found via `decl_registry.declaring_module`.
    // This includes predefined types (whose parent is the root module) and
    // generated items (vftables).
    for (module_path, module) in modules.iter_mut() {
        module.definition_paths = definition_paths
            .iter()
            .filter(|p| {
                decl_registry
                    .declaring_module(p)
                    .is_some_and(|dm| dm == module_path)
            })
            .cloned()
            .collect();
    }

    // Run validate_uses BEFORE type resolution. This catches private access
    // and nonexistent imports before resolve_item tries to resolve types
    // that reference them.
    if let Err(e) = validation::validate_uses(&type_registry, &modules) {
        return bail(&type_registry, &modules, vec![e]);
    }
    // validate_backend_for_targets also needs to run before type resolution
    // so that for_type paths are resolved before backends read them.
    if let Err(e) = validation::validate_backend_for_targets(&type_registry, &mut modules) {
        return bail(&type_registry, &modules, vec![e]);
    }
    // validate_backend_definitions rejects `prologue definition`/`epilogue definition`
    // on non-cpp backends. It inspects only backend prologue/epilogue definitions
    // (no resolved-type dependency), so it runs before item resolution to keep its
    // diagnostic ahead of resolution errors — matching the pre-rewrite ordering.
    if let Err(e) = validation::validate_backend_definitions(&modules) {
        return bail(&type_registry, &modules, vec![e]);
    }

    // Resolve every declared item through the per-item `resolve_item` query, so
    // resolution is cached per item: an incremental edit re-resolves only the
    // affected items (and their dependents), not the whole project. Each
    // `resolve_item` overlays the shared, memoized placeholder base, so this
    // stays O(edges), not O(n²). Resolved items and their generated types
    // (vftables) are merged into the registry; generated items' module
    // definition_paths are folded in just below.
    let mut semantic_errors: Vec<SemanticError> = Vec::new();
    let item_paths: Vec<ItemPath> = decl_registry.item_paths().cloned().collect();
    for item_path in &item_paths {
        let resolved = resolve_item(db, sources, pointer_size, item_path.clone());
        semantic_errors.extend(resolved.errors(db).iter().cloned());
        let item = resolved.item(db);
        if item.is_resolved() {
            type_registry.add((**item).clone());
        } else if resolved.errors(db).is_empty() {
            // Unresolved with no error of its own → stalled (mutual by-value
            // recursion or a missing type).
            semantic_errors.push(SemanticError::TypeResolutionStalled {
                unresolved_types: vec![item_path.to_string()],
                resolved_types: vec![],
                unresolved_references: vec![],
            });
        }
        for generated in resolved.generated_items(db).iter() {
            definition_paths.insert(generated.path.clone());
            type_registry.add(generated.clone());
        }
    }

    // Generated items (vftables) are created inside resolve_item's own modules,
    // so re-assign module definition_paths here to include them.
    for (module_path, module) in modules.iter_mut() {
        module.definition_paths = definition_paths
            .iter()
            .filter(|p| {
                decl_registry
                    .declaring_module(p)
                    .is_some_and(|dm| dm == module_path)
            })
            .cloned()
            .collect();
    }

    if !semantic_errors.is_empty() {
        return bail(&type_registry, &modules, semantic_errors);
    }

    // Resolve freestanding functions
    for module in modules.values_mut() {
        if let Err(e) = module.resolve_functions(&type_registry) {
            semantic_errors.push(e);
        }
    }

    if !semantic_errors.is_empty() {
        return bail(&type_registry, &modules, semantic_errors);
    }

    // Associated functions (own impl methods + inherited) are computed by
    // compute_associated_functions and merged into the registry in place, so
    // backends and the doc-link resolver see methods.
    let af_errors = merge_associated_functions(&mut type_registry, &modules);
    if !af_errors.is_empty() {
        let doc_link_resolver = doc_links::DocLinkResolver::build(&type_registry, &modules);
        return finish(type_registry, modules, doc_link_resolver, af_errors);
    }

    // Doc link resolution (now with associated functions in the registry)
    let doc_link_resolver = doc_links::DocLinkResolver::build(&type_registry, &modules);
    if let Err(e) = doc_links::validate(&doc_link_resolver, &type_registry, &modules) {
        return finish(type_registry, modules, doc_link_resolver, vec![e]);
    }

    finish(type_registry, modules, doc_link_resolver, vec![])
}

/// Compute associated functions (own impl methods + inherited from base types)
/// for all resolved types. Returns a map from type path to associated functions,
/// plus any errors (e.g., duplicate method names).
///
/// A plain function over the resolved type registry and modules: it reads them
/// and produces a new result without mutating the registry, and calls no other
/// db query, so it needs no Salsa indirection. The map is merged into type
/// definitions by `merge_associated_functions`.
pub fn compute_associated_functions(
    type_registry: &TypeRegistry,
    modules: &BTreeMap<ItemPath, crate::semantic::Module>,
) -> Arc<AssociatedFunctionsResult> {
    use crate::{
        grammar,
        parser::cfg::CfgPredicate,
        semantic::{
            SemanticError,
            error::DuplicateDefinitionKind,
            function::{self, FunctionBuildOutcome},
            types::{Function, FunctionBody, ItemDefinitionInner, Type},
        },
        span::ItemLocation,
    };
    use std::collections::{BTreeMap as BTree, BTreeSet, HashMap, HashSet};

    let location = ItemLocation::internal();

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
            func: grammar::Function,
            impl_type_parameters: Vec<String>,
            impl_cfg: Option<CfgPredicate>,
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
                            function::Argument::ConstSelf { .. }
                                | function::Argument::MutSelf { .. }
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
            if i > 0
                && let Ok(base_item) = type_registry.get(base_path, &location)
                && let Some(base_resolved) = base_item.resolved()
                && let ItemDefinitionInner::Type(base_td) = &base_resolved.inner
                && let Some(vftable) = &base_td.vftable
            {
                add_functions(
                    &vftable.functions,
                    &mut associated_functions,
                    &mut used_names,
                );
            }
        }

        // Build own impl methods
        let reserved = used_names.clone();
        let mut own_method_cfgs: HashMap<String, Vec<Option<CfgPredicate>>> = HashMap::new();

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
                (Some(b), Some(f)) => Some(CfgPredicate::All {
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
                    cfgs.iter()
                        .all(|e| CfgPredicate::provably_disjoint(e.as_ref(), function.cfg.as_ref()))
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
    pub functions: BTreeMap<ItemPath, Vec<Function>>,
    /// Errors encountered during computation (e.g., duplicate method names).
    pub errors: Vec<SemanticError>,
}
