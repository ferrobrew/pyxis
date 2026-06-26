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

/// A single item declaration extracted from a parsed file, before resolution.
#[salsa::tracked]
pub struct ItemDeclaration<'db> {
    pub source: super::SourceFile,
    pub path: crate::grammar::ItemPath,
    #[returns(ref)]
    pub definition: Arc<crate::grammar::ItemDefinition>,
    pub module_path: crate::grammar::ItemPath,
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
    pub fn to_resolved_state(&self, db: &dyn super::Db) -> Option<crate::semantic::ResolvedSemanticState> {
        use crate::semantic::ResolvedSemanticState;

        if !self.errors(db).is_empty() || !self.parse_errors(db).is_empty() {
            return None;
        }

        Some(ResolvedSemanticState::from_parts(
            (**self.type_registry(db)).clone(),
            (**self.modules(db)).clone(),
            (**self.doc_link_resolver(db)).clone(),
        ))
    }
}
