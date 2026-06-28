//! Salsa tracked structs — the intermediate representation of the query graph.

use std::sync::Arc;

use crate::semantic::declaration_registry::DeclarationRegistry;

/// A tokenized file (tokens + any lex error, reported as a parse error).
/// Shared by `parse_file` and editor tooling so a file is lexed at most once
/// per edit rather than re-tokenized on demand.
#[salsa::tracked]
pub struct TokenizedFile<'db> {
    #[returns(ref)]
    pub tokens: Arc<Vec<crate::tokenizer::Token>>,
    #[returns(ref)]
    pub errors: Arc<Vec<crate::parser::ParseError>>,
}

/// A parsed file (grammar::Module + parse errors).
#[salsa::tracked]
pub struct ParsedFile<'db> {
    pub source: super::SourceFile,
    #[returns(ref)]
    pub module: Arc<crate::grammar::Module>,
    #[returns(ref)]
    pub errors: Arc<Vec<crate::parser::ParseError>>,
}

/// The full declaration set — all items, modules, and scopes.
/// Built once from all parsed files; used by resolve_item for name resolution.
#[salsa::tracked]
pub struct DeclarationSet<'db> {
    #[returns(ref)]
    pub registry: Arc<DeclarationRegistry>,
}

/// The body- and location-free name index — the stable foundation for
/// incremental resolution. Backdates across body/location edits so
/// `resolve_item` is invalidated only by edits that change names/scopes/arity.
#[salsa::tracked]
pub struct NameIndexSet<'db> {
    #[returns(ref)]
    pub index: Arc<crate::semantic::name_index::NameIndex>,
}

/// Per-file source map: the source span of every named type reference in a file
/// paired with the `ItemPath` it resolves to. A single, incrementally-cached
/// source of truth for "what type is referenced at this position", so the LSP
/// can answer cursor→path with a lookup instead of re-walking the AST. Per-file
/// and dependent on `name_index`, so it recomputes only for the edited file.
#[salsa::tracked]
pub struct FileTypeReferences<'db> {
    #[returns(ref)]
    pub references: Arc<Vec<(crate::span::Span, crate::grammar::ItemPath)>>,
}

/// The memoized placeholder base: a `TypeRegistry` of predefined + extern types
/// plus every declared item as an `Unresolved` placeholder. Built once per
/// `(sources, pointer_size)` and shared (overlaid) by every `resolve_item`, so a
/// whole-program analyze stays O(edges) instead of O(n²). Depends only on
/// `name_index`, so it's rebuilt only when the project's shape changes.
#[salsa::tracked]
pub struct PlaceholderBase<'db> {
    #[returns(ref)]
    pub registry: Arc<crate::semantic::TypeRegistry>,
}

/// A resolved item (type/enum/bitflags/type-alias) — the result of
/// per-type resolution via resolve_item.
#[salsa::tracked]
pub struct ResolvedItem<'db> {
    pub path: crate::grammar::ItemPath,
    #[returns(ref)]
    pub item: Arc<crate::semantic::types::ItemDefinition>,
    #[returns(ref)]
    pub errors: Arc<Vec<crate::semantic::SemanticError>>,
    /// Items generated as side effects during resolution (e.g., vftable
    /// struct types created by type_definition::build). These are collected
    /// by analyze() and added to the final type registry.
    #[returns(ref)]
    pub generated_items: Arc<Vec<crate::semantic::types::ItemDefinition>>,
}

/// The full resolved semantic state — the root query result.
#[salsa::tracked]
pub struct SemanticAnalysis<'db> {
    #[returns(ref)]
    pub type_registry: Arc<crate::semantic::TypeRegistry>,
    #[returns(ref)]
    pub modules: Arc<std::collections::BTreeMap<crate::grammar::ItemPath, crate::semantic::Module>>,
    #[returns(ref)]
    pub doc_link_resolver: Arc<crate::semantic::doc_links::DocLinkResolver>,
    #[returns(ref)]
    pub errors: Arc<Vec<crate::semantic::SemanticError>>,
    #[returns(ref)]
    pub parse_errors: Arc<Vec<crate::parser::ParseError>>,
}

impl SemanticAnalysis<'_> {
    pub fn to_semantic_output(
        &self,
        db: &dyn super::Db,
    ) -> Option<crate::semantic::SemanticOutput> {
        use crate::semantic::SemanticOutput;

        if !self.errors(db).is_empty() || !self.parse_errors(db).is_empty() {
            return None;
        }

        // Associated functions are already merged into the type registry
        // by analyze() (which calls compute_associated_functions and merges
        // the results before building the doc link resolver).
        Some(SemanticOutput::from_parts(
            (**self.type_registry(db)).clone(),
            (**self.modules(db)).clone(),
            (**self.doc_link_resolver(db)).clone(),
        ))
    }
}
