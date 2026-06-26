//! Salsa tracked functions — the query graph.
//!
//! The pipeline:
//!   `parse_file` (per-file leaf query)
//!   → `analyze` (root query, runs the existing resolution pipeline
//!      over all parsed files and produces the full resolved state)

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::grammar::Module;
use crate::parser::ParseError;
use crate::semantic::TypeRegistry;
use crate::span::FileId;

use super::db::Db;
use super::inputs::SourceFile;
use super::ir::{ParsedFile, SemanticAnalysis};

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

/// The root query — builds the full resolved semantic state.
///
/// This runs the existing resolution pipeline (`SemanticState::add_module` +
/// `build`) over all parsed files. The result is memoized by Salsa: if no
/// file contents have changed, the cached result is returned without
/// re-running resolution.
///
/// In the hybrid architecture, file-level parsing is incremental (each
/// file is parsed independently and cached), while the resolution pass
/// runs over all files as a single query. Future work can break resolution
/// into per-item tracked functions for finer-grained incrementality.
#[salsa::tracked]
pub fn analyze(
    db: &dyn Db,
    pointer_size: usize,
    sources: Vec<SourceFile>,
) -> SemanticAnalysis<'_> {
    // Collect parse results and errors.
    let mut parse_errors: Vec<ParseError> = Vec::new();
    let mut parsed_modules: Vec<(SourceFile, Arc<Module>)> = Vec::new();

    for source in &sources {
        let parsed = parse_file(db, *source);
        let module = parsed.module(db);
        let errors = parsed.errors(db);
        if !errors.is_empty() {
            parse_errors.extend(errors.iter().cloned());
        } else {
            parsed_modules.push((*source, module.clone()));
        }
    }

    // If there are parse errors, return early with an empty resolved state.
    // The LSP surfaces parse errors via the parse_errors field.
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
    let mut semantic_state = crate::semantic::SemanticState::new(pointer_size);
    let mut semantic_errors: Vec<crate::semantic::SemanticError> = Vec::new();

    for (source, module) in &parsed_modules {
        let path_str = source.path(db);
        let module_path = crate::grammar::ItemPath::from_path(std::path::Path::new(path_str.as_str()));
        if let Err(e) = semantic_state.add_module(module, &module_path) {
            semantic_errors.push(e);
            break;
        }
    }

    // If add_module failed, return with errors.
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

    // Run the resolution pipeline.
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
