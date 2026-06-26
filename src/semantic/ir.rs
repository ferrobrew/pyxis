//! Salsa tracked structs — the intermediate representation of the query graph.

use std::sync::Arc;

use crate::semantic::declaration_registry::DeclarationRegistry;

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
    pub fn to_semantic_output(&self, db: &dyn super::Db) -> Option<crate::semantic::SemanticOutput> {
        use crate::semantic::SemanticOutput;

        if !self.errors(db).is_empty() || !self.parse_errors(db).is_empty() {
            return None;
        }

        Some(SemanticOutput::from_parts(
            (**self.type_registry(db)).clone(),
            (**self.modules(db)).clone(),
            (**self.doc_link_resolver(db)).clone(),
        ))
    }
}
