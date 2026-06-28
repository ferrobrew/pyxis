//! Per-file leaf queries: `tokenize_file` and `parse_file`. These re-run only
//! when the file's own content changes.

use std::sync::Arc;

use crate::{grammar::Module, span::FileId};

use super::super::{
    db::Db,
    inputs::SourceFile,
    ir::{ParsedFile, TokenizedFile},
};

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
