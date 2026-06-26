//! Salsa tracked structs — the intermediate representation of the query graph.

use std::sync::Arc;

/// A parsed file (grammar::Module + parse errors).
///
/// Created by [`parse_file`](super::queries::parse_file). This is a leaf
/// query — it only depends on the `SourceFile` input's contents.
#[salsa::tracked]
pub struct ParsedFile<'db> {
    /// The source file this was parsed from
    pub source: super::SourceFile,
    /// The parsed module (wrapped in Arc for salsa::Update)
    #[returns(ref)]
    pub module: Arc<crate::grammar::Module>,
    /// Parse errors (empty if parsing succeeded)
    #[returns(ref)]
    pub errors: Arc<Vec<crate::parser::ParseError>>,
}

/// The full resolved semantic state — the root query result.
///
/// Produced by [`analyze`](super::queries::analyze). Contains the complete
/// resolved type registry, modules, doc links, and all collected errors.
#[salsa::tracked]
pub struct SemanticAnalysis<'db> {
    /// The resolved type registry
    #[returns(ref)]
    pub type_registry: Arc<crate::semantic::TypeRegistry>,
    /// All modules (path → Module)
    #[returns(ref)]
    pub modules: Arc<std::collections::BTreeMap<crate::grammar::ItemPath, crate::semantic::Module>>,
    /// Doc link resolver
    #[returns(ref)]
    pub doc_link_resolver: Arc<crate::semantic::doc_links::DocLinkResolver>,
    /// All semantic errors collected during resolution
    #[returns(ref)]
    pub errors: Arc<Vec<crate::semantic::SemanticError>>,
    /// All parse errors collected from all files
    #[returns(ref)]
    pub parse_errors: Arc<Vec<crate::parser::ParseError>>,
}

impl SemanticAnalysis<'_> {
    /// Project to `ResolvedSemanticState` for backend consumption.
    /// Returns `None` if there were errors (parse or semantic).
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
