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

    // Build a temporary TypeRegistry with all declarations registered as Unresolved,
    // plus predefined types. This gives us the same resolve_grammar_type interface.
    let mut registry = TypeRegistry::new(pointer_size);

    // Register all predefined types
    for predefined in crate::semantic::types::PredefinedItem::ALL {
        let path = ItemPath::from(predefined.name());
        let size = predefined.size();
        let alignment = size.max(1);
        let location = crate::span::ItemLocation::internal();
        registry.add(crate::semantic::types::ItemDefinition {
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

    // Register all other declarations as Unresolved
    for other_decl in &declarations {
        let other_def = other_decl.definition(db);
        let other_path = other_decl.path(db);
        let type_parameters: Vec<String> = other_def
            .type_parameters
            .iter()
            .map(|tp| tp.name.clone())
            .collect();
        let cfg = match &other_def.inner {
            ItemDefinitionInner::Type(td) => td.attributes.cfg(),
            ItemDefinitionInner::Enum(e) => e.attributes.cfg(),
            ItemDefinitionInner::Bitflags(b) => b.attributes.cfg(),
            ItemDefinitionInner::TypeAlias(ta) => ta.attributes.cfg(),
        };
        registry.add(crate::semantic::types::ItemDefinition {
            visibility: other_def.visibility.into(),
            path: other_path.clone(),
            type_parameters,
            state: crate::semantic::types::ItemState::Unresolved(other_def.as_ref().clone()),
            category: crate::semantic::types::ItemCategory::Defined,
            predefined: None,
            cfg,
            location: other_def.location,
            declaration_location: other_def.declaration_location,
        });
    }

    // Register extern types
    for source in sources.sources(db) {
        let parsed = parse_file(db, *source);
        let module = parsed.module(db);
        let path_str = source.path(db);
        let mod_path = ItemPath::from_path(std::path::Path::new(path_str.as_str()));

        for extern_type in module.extern_types() {
            let crate::grammar::ModuleItem::ExternType {
                name: extern_name,
                attributes,
                doc_comments: extern_doc_comments,
                location: extern_location,
                declaration_location: extern_declaration_location,
            } = extern_type
            else {
                continue;
            };
            let mut size = None;
            let mut alignment = None;
            for attribute in attributes {
                let Some((ident, items)) = attribute.function() else {
                    continue;
                };
                let loc = attribute.location();
                if let Some(attr_size) = crate::semantic::attribute::parse_size(ident, items, loc).unwrap_or(None) {
                    size = Some(attr_size);
                } else if let Some(attr_align) = crate::semantic::attribute::parse_align(ident, items, loc).unwrap_or(None) {
                    alignment = Some(attr_align);
                }
            }
            if let (Some(size), Some(alignment)) = (size, alignment) {
                let extern_path = mod_path.join(extern_name.as_str().into());
                registry.add(crate::semantic::types::ItemDefinition {
                    visibility: crate::semantic::types::Visibility::Public,
                    path: extern_path,
                    type_parameters: vec![],
                    state: crate::semantic::types::ItemState::Resolved(
                        crate::semantic::types::ItemStateResolved {
                            size,
                            alignment,
                            inner: crate::semantic::types::TypeDefinition {
                                doc: extern_doc_comments.clone(),
                                ..Default::default()
                            }
                            .into(),
                        },
                    ),
                    category: crate::semantic::types::ItemCategory::Extern,
                    predefined: None,
                    cfg: attributes.cfg(),
                    location: *extern_location,
                    declaration_location: *extern_declaration_location,
                });
            }
        }
    }

    // Now resolve this item using the existing build functions.
    // We use a temporary SemanticState for the resolution, but only for
    // this one item — the registry is built fresh from declarations.
    let mut semantic_state = crate::semantic::SemanticState::from_registry(registry, module_path.clone());

    let visibility: crate::semantic::types::Visibility = definition.visibility.into();
    let def_location = definition.location;
    let type_param_names: Vec<String> = definition
        .type_parameters
        .iter()
        .map(|tp| tp.name.clone())
        .collect();

    let outcome = match &definition.inner {
        ItemDefinitionInner::Type(ty) => crate::semantic::type_definition::build(
            &mut semantic_state,
            &item_path,
            visibility,
            ty,
            &def_location,
            &definition.doc_comments,
            &type_param_names,
        ),
        ItemDefinitionInner::Enum(e) => crate::semantic::enum_definition::build(
            &semantic_state,
            &item_path,
            e,
            &def_location,
            &definition.doc_comments,
        ),
        ItemDefinitionInner::Bitflags(b) => crate::semantic::bitflags_definition::build(
            &semantic_state,
            &item_path,
            b,
            &def_location,
            &definition.doc_comments,
        ),
        ItemDefinitionInner::TypeAlias(ta) => crate::semantic::type_alias_definition::build(
            &semantic_state,
            &item_path,
            ta,
            &def_location,
            &definition.doc_comments,
            &type_param_names,
        ),
    };

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
            // Still unresolved — this shouldn't happen in the Salsa model
            // since all declarations are available upfront. Return as error.
            (
                make_unresolved_definition(&item_path, pointer_size),
                vec![crate::semantic::SemanticError::TypeResolutionStalled {
                    unresolved_types: vec![item_path.to_string()],
                    resolved_types: vec![],
                    unresolved_references: vec![],
                }],
            )
        }
        Ok(crate::semantic::error::BuildOutcome::NotFoundType(unresolved_ref)) => {
            (
                make_unresolved_definition(&item_path, pointer_size),
                vec![crate::semantic::SemanticError::TypeNotFound {
                    path: ItemPath::from(unresolved_ref.type_name.as_str()),
                    location: unresolved_ref.location,
                }],
            )
        }
        Err(e) => {
            (
                make_unresolved_definition(&item_path, pointer_size),
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
