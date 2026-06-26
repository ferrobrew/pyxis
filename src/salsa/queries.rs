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
        );
    }
    if let Some(info) = registry.get_extern_type(&item_path) {
        return ResolvedItem::new(
            db,
            item_path.clone(),
            Arc::new(make_extern_definition(&item_path, info.size, info.alignment)),
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
        );
    };

    // Build a TypeRegistry with predefined + extern types resolved,
    // and ALL declared items registered as Unresolved.
    let mut type_registry = TypeRegistry::new(pointer_size);
    register_predefined(&mut type_registry);

    for (path, info) in registry.extern_types_iter() {
        type_registry.add(make_extern_definition(path, info.size, info.alignment));
    }

    for other_path in registry.item_paths() {
        if let Some(other_def) = registry.get_definition(other_path) {
            register_unresolved(&mut type_registry, other_path, other_def);
        }
    }

    // Build modules map (needed for ResolutionContext)
    let mut modules = BTreeMap::new();
    modules.insert(ItemPath::empty(), crate::semantic::Module::default());
    let module_path = item_path.parent().unwrap_or_else(|| ItemPath::empty());
    modules.insert(module_path, crate::semantic::Module::default());

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
    let mut max_iterations = registry.item_paths().count() + 1;
    let outcome;

    loop {
        max_iterations -= 1;
        if max_iterations == 0 {
            return ResolvedItem::new(
                db,
                item_path.clone(),
                Arc::new(make_unresolved_definition(&item_path)),
                Arc::new(vec![crate::semantic::SemanticError::TypeResolutionStalled {
                    unresolved_types: vec![item_path.to_string()],
                    resolved_types: vec![],
                    unresolved_references: vec![],
                }]),
            );
        }

        let result = match &definition.inner {
            ItemDefinitionInner::Type(ty) => {
                let mut ctx = crate::semantic::resolution_context::ResolutionContext::new(
                    &mut type_registry,
                    &mut modules,
                );
                crate::semantic::type_definition::build(
                    &mut ctx,
                    &item_path,
                    visibility,
                    ty,
                    &def_location,
                    &definition.doc_comments,
                    &type_param_names,
                )
            }
            ItemDefinitionInner::Enum(e) => {
                let ctx_ref = crate::semantic::resolution_context::ResolutionContextRef::new(
                    &type_registry,
                    &modules,
                );
                crate::semantic::enum_definition::build(
                    &ctx_ref,
                    &item_path,
                    e,
                    &def_location,
                    &definition.doc_comments,
                )
            }
            ItemDefinitionInner::Bitflags(b) => {
                let ctx_ref = crate::semantic::resolution_context::ResolutionContextRef::new(
                    &type_registry,
                    &modules,
                );
                crate::semantic::bitflags_definition::build(
                    &ctx_ref,
                    &item_path,
                    b,
                    &def_location,
                    &definition.doc_comments,
                )
            }
            ItemDefinitionInner::TypeAlias(ta) => {
                let ctx_ref = crate::semantic::resolution_context::ResolutionContextRef::new(
                    &type_registry,
                    &modules,
                );
                crate::semantic::type_alias_definition::build(
                    &ctx_ref,
                    &item_path,
                    ta,
                    &def_location,
                    &definition.doc_comments,
                    &type_param_names,
                )
            }
        };

        match result {
            Ok(crate::semantic::error::BuildOutcome::Resolved(item)) => {
                outcome = Ok(crate::semantic::error::BuildOutcome::Resolved(item));
                break;
            }
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
                    outcome = Ok(crate::semantic::error::BuildOutcome::Deferred);
                    break;
                }
            }
            other => {
                outcome = other;
                break;
            }
        }
    }

    let (item_def, errors) = match outcome {
        Ok(crate::semantic::error::BuildOutcome::Resolved(item)) => {
            let resolved_def = crate::semantic::types::ItemDefinition {
                visibility: definition.visibility.into(),
                path: item_path.clone(),
                type_parameters: type_param_names,
                state: crate::semantic::types::ItemState::Resolved(item),
                category: crate::semantic::types::ItemCategory::Defined,
                predefined: None,
                cfg: None,
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
            (
                make_unresolved_definition(&item_path),
                vec![crate::semantic::SemanticError::TypeNotFound {
                    path: ItemPath::from(unresolved_ref.type_name.as_str()),
                    location: unresolved_ref.location,
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

    ResolvedItem::new(
        db,
        item_path,
        Arc::new(item_def),
        Arc::new(errors),
    )
}

/// The root query — builds the full resolved semantic state.
#[salsa::tracked]
pub fn analyze<'db>(
    db: &'db dyn Db,
    pointer_size: usize,
    sources: SourceSet<'db>,
) -> SemanticAnalysis<'db> {
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
            Arc::new(crate::semantic::doc_links::DocLinkResolver::build(
                &TypeRegistry::new(pointer_size),
                &BTreeMap::new(),
            )),
            Arc::new(vec![]),
            Arc::new(parse_errors),
        );
    }

    // Build the semantic state using the existing pipeline.
    // This uses the hybrid approach for the batch path: SemanticState
    // runs the full iterative resolution. The per-item resolve_item
    // tracked function is available for the LSP to use for finer-grained
    // queries (hover, code lens), with Salsa caching each result.
    let mut semantic_state = crate::semantic::SemanticState::new(pointer_size);
    let mut semantic_errors: Vec<crate::semantic::SemanticError> = Vec::new();

    for (source, module) in &parsed_modules {
        let path_str = source.path(db);
        let module_path = ItemPath::from_path(std::path::Path::new(path_str.as_str()));
        if let Err(e) = semantic_state.add_module(module, &module_path) {
            semantic_errors.push(e);
            break;
        }
    }

    if !semantic_errors.is_empty() {
        return SemanticAnalysis::new(
            db,
            Arc::new(TypeRegistry::new(pointer_size)),
            Arc::new(BTreeMap::new()),
            Arc::new(crate::semantic::doc_links::DocLinkResolver::build(
                &TypeRegistry::new(pointer_size),
                &BTreeMap::new(),
            )),
            Arc::new(semantic_errors),
            Arc::new(vec![]),
        );
    }

    match semantic_state.build() {
        Ok(resolved) => SemanticAnalysis::new(
            db,
            Arc::new(resolved.type_registry().clone()),
            Arc::new(resolved.modules().clone()),
            Arc::new(resolved.doc_link_resolver().clone()),
            Arc::new(vec![]),
            Arc::new(vec![]),
        ),
        Err(e) => {
            semantic_errors.push(e);
            SemanticAnalysis::new(
                db,
                Arc::new(TypeRegistry::new(pointer_size)),
                Arc::new(BTreeMap::new()),
                Arc::new(crate::semantic::doc_links::DocLinkResolver::build(
                    &TypeRegistry::new(pointer_size),
                    &BTreeMap::new(),
                )),
                Arc::new(semantic_errors),
                Arc::new(vec![]),
            )
        }
    }
}

// --- Helpers ---

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

fn make_extern_definition(path: &ItemPath, size: usize, alignment: usize) -> crate::semantic::types::ItemDefinition {
    crate::semantic::types::ItemDefinition {
        visibility: crate::semantic::types::Visibility::Public,
        path: path.clone(),
        type_parameters: vec![],
        state: crate::semantic::types::ItemState::Resolved(
            crate::semantic::types::ItemStateResolved {
                size,
                alignment,
                inner: crate::semantic::types::TypeDefinition::default().into(),
            },
        ),
        category: crate::semantic::types::ItemCategory::Extern,
        predefined: None,
        cfg: None,
        location: crate::span::ItemLocation::internal(),
        declaration_location: crate::span::ItemLocation::internal(),
    }
}
