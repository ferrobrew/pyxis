//! Salsa tracked functions — the query graph.

use std::sync::Arc;

use crate::grammar::Module;
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
        Ok(module) => ParsedFile::new(db, source, Arc::new(module)),
        Err(e) => {
            let _ = e;
            ParsedFile::new(db, source, Arc::new(Module::default()))
        }
    }
}

/// The root query — for the spike, just collects parse errors.
#[salsa::tracked]
pub fn analyze(db: &dyn Db, source: SourceFile) -> SemanticAnalysis<'_> {
    let parsed = parse_file(db, source);
    let module = parsed.module(db);
    let _ = module;
    SemanticAnalysis::new(db, Arc::new(vec![]))
}
