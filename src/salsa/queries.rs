//! Salsa tracked functions — the query graph.
//!
//! The pipeline:
//!   `parse_file` (per-file leaf query)
//!   → `collect_declarations` (all items across all files)
//!   → `resolve_item` (per-item, the core incremental boundary)
//!   → `analyze` (root query, wires everything together)

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::grammar::{ItemDefinition, ItemDefinitionInner, ItemPath, Module};
use crate::parser::ParseError;
use crate::semantic::TypeRegistry;
use crate::span::{FileId, HasLocation};

use super::db::Db;
use super::inputs::{SourceFile, SourceSet};
use super::ir::{ItemDeclaration, ParsedFile, ResolvedItem, SemanticAnalysis};

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

/// Collect all item declarations across all files. Each file is parsed
/// independently (and cached). This builds the "unresolved registry" —
/// the set of all declared items before type resolution.
#[salsa::tracked]
pub fn collect_declarations<'db>(
    db: &'db dyn Db,
    sources: SourceSet<'db>,
) -> Vec<ItemDeclaration<'db>> {
    let mut declarations = Vec::new();
    for source in sources.sources(db) {
        let parsed = parse_file(db, *source);
        let module = parsed.module(db);
        let path_str = source.path(db);
        let module_path = ItemPath::from_path(std::path::Path::new(path_str.as_str()));
        for def in module.definitions() {
            let item_path = module_path.join(def.name.as_str().into());
            declarations.push(ItemDeclaration::new(
                db,
                *source,
                item_path,
                Arc::new(def.clone()),
                module_path.clone(),
            ));
        }
    }
    declarations
}

/// Resolve a single item (type/enum/bitflags/type-alias).
///
/// This is the per-type query — the core of incremental type resolution.
/// It depends on other items' declarations (for type references) but
/// Salsa caches the result and only re-runs when dependencies change.
///
/// When type A references type B, resolve_item(A) calls resolve_item(B),
/// and Salsa memoizes. If B changes, only A (and its dependents) re-resolve.
#[salsa::tracked]
pub fn resolve_item<'db>(
    db: &'db dyn Db,
    sources: SourceSet<'db>,
    pointer_size: usize,
    item_path: ItemPath,
) -> ResolvedItem<'db> {
    let declarations = collect_declarations(db, sources.clone());

    // Find this item's declaration
    let Some(decl) = declarations.iter().find(|d| d.path(db) == item_path) else {
        // If the item isn't found, it might be a predefined type (u8, u32, etc.)
        // Return a placeholder resolved item.
        return ResolvedItem::new(
            db,
            item_path.clone(),
            Arc::new(make_unresolved_definition(&item_path, pointer_size)),
            Arc::new(vec![]),
        );
    };

    let definition = decl.definition(db);
    let module_path = decl.module_path(db);

    // Build the full semantic state using the existing pipeline.
    // This resolves ALL items (the iterative loop), but Salsa caches
    // each resolve_item call — so subsequent calls for different items
    // return the cached result without re-running the full pipeline.
    //
    // Note: this is still the hybrid approach. The full Salsa-ification
    // would port the iterative loop into per-item dependency tracking,
    // but that requires refactoring resolve_grammar_type to call
    // resolve_item directly instead of checking is_resolved().
    let mut semantic_state = crate::semantic::SemanticState::new(pointer_size);

    // Register all modules
    for source in sources.sources(db) {
        let parsed = parse_file(db, *source);
        let module = parsed.module(db);
        let path_str = source.path(db);
        let mod_path = ItemPath::from_path(std::path::Path::new(path_str.as_str()));
        if let Err(e) = semantic_state.add_module(module, &mod_path) {
            return ResolvedItem::new(
                db,
                item_path.clone(),
                Arc::new(make_unresolved_definition(&item_path, pointer_size)),
                Arc::new(vec![e]),
            );
        }
    }

    // Run the full resolution
    let resolved_state = match semantic_state.build() {
        Ok(state) => state,
        Err(e) => {
            return ResolvedItem::new(
                db,
                item_path.clone(),
                Arc::new(make_unresolved_definition(&item_path, pointer_size)),
                Arc::new(vec![e]),
            );
        }
    };

    // Extract the specific item we want from the resolved registry
    let type_registry = resolved_state.type_registry();
    match type_registry.get(&item_path, &crate::span::ItemLocation::internal()) {
        Ok(item_def) => {
            ResolvedItem::new(
                db,
                item_path.clone(),
                Arc::new(item_def.clone()),
                Arc::new(vec![]),
            )
        }
        Err(e) => {
            ResolvedItem::new(
                db,
                item_path.clone(),
                Arc::new(make_unresolved_definition(&item_path, pointer_size)),
                Arc::new(vec![e]),
            )
        }
    }
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

    // If there are parse errors, return early.
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
    // This uses the hybrid approach: SemanticState::add_module + build,
    // which runs the full iterative resolution. The per-item resolve_item
    // tracked function is available for the LSP to use for finer-grained
    // queries (hover, code lens), but the batch path uses the proven pipeline.
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

/// Create a placeholder unresolved item definition.
fn make_unresolved_definition(path: &ItemPath, _pointer_size: usize) -> crate::semantic::types::ItemDefinition {
    use crate::grammar::{Ident, ItemDefinition, ItemDefinitionInner, TypeDefinition};
    use crate::semantic::types::{ItemCategory, ItemState, ItemStateResolved, Visibility};
    use crate::span::ItemLocation;

    let grammar_def = ItemDefinition {
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

    ItemDefinition_inner(path, grammar_def, Visibility::Public)
}

fn ItemDefinition_inner(
    path: &ItemPath,
    grammar_def: crate::grammar::ItemDefinition,
    vis: crate::semantic::types::Visibility,
) -> crate::semantic::types::ItemDefinition {
    use crate::semantic::types::{ItemCategory, ItemState};
    crate::semantic::types::ItemDefinition {
        visibility: vis,
        path: path.clone(),
        type_parameters: vec![],
        state: ItemState::Unresolved(grammar_def),
        category: ItemCategory::Defined,
        predefined: None,
        cfg: None,
        location: crate::span::ItemLocation::internal(),
        declaration_location: crate::span::ItemLocation::internal(),
    }
}
