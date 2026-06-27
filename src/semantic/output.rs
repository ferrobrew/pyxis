//! Semantic output — the projection of the Salsa `SemanticAnalysis` query
//! that backends consume.
//!
//! This is a simple data struct: a `TypeRegistry`, a module map, and a
//! `DocLinkResolver`. It carries no logic of its own; all semantic analysis
//! happens in the Salsa query graph (`queries.rs`).

use std::collections::BTreeMap;

use crate::{
    grammar::ItemPath,
    semantic::{Module, type_registry::TypeRegistry},
};

/// The output of semantic analysis, projected from `SemanticAnalysis`.
/// Backends take `&SemanticOutput` and read its three fields.
#[derive(Debug)]
pub struct SemanticOutput {
    type_registry: TypeRegistry,
    modules: BTreeMap<ItemPath, Module>,
    doc_link_resolver: crate::semantic::doc_links::DocLinkResolver,
}

impl SemanticOutput {
    pub fn type_registry(&self) -> &TypeRegistry {
        &self.type_registry
    }

    pub fn modules(&self) -> &BTreeMap<ItemPath, Module> {
        &self.modules
    }

    pub fn doc_link_resolver(&self) -> &crate::semantic::doc_links::DocLinkResolver {
        &self.doc_link_resolver
    }

    /// Construct a `SemanticOutput` from its parts.
    /// Used by the Salsa query layer to project `SemanticAnalysis`.
    pub(crate) fn from_parts(
        type_registry: TypeRegistry,
        modules: BTreeMap<ItemPath, Module>,
        doc_link_resolver: crate::semantic::doc_links::DocLinkResolver,
    ) -> Self {
        Self {
            type_registry,
            modules,
            doc_link_resolver,
        }
    }
}
