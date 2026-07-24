//! Semantic output — the projection of the Salsa `SemanticAnalysis` query
//! that backends consume.
//!
//! This is a simple data struct: a `TypeRegistry`, a module map, and a
//! `DocLinkResolver`. It carries no logic of its own; all semantic analysis
//! happens in the Salsa query graph (`queries.rs`).

use std::collections::BTreeMap;

use crate::{
    grammar::ItemPath,
    semantic::{
        Module,
        doc_links::{DocLinkResolver, DocLinks, ModuleDocLinks},
        type_registry::TypeRegistry,
    },
};

/// The output of semantic analysis, projected from `SemanticAnalysis`.
/// Backends take `&SemanticOutput` and read its fields.
#[derive(Debug)]
pub struct SemanticOutput {
    type_registry: TypeRegistry,
    modules: BTreeMap<ItemPath, Module>,
    doc_link_resolver: DocLinkResolver,
    doc_links: DocLinks,
}

impl SemanticOutput {
    pub fn type_registry(&self) -> &TypeRegistry {
        &self.type_registry
    }

    pub fn modules(&self) -> &BTreeMap<ItemPath, Module> {
        &self.modules
    }

    pub fn doc_link_resolver(&self) -> &DocLinkResolver {
        &self.doc_link_resolver
    }

    /// The resolved intra-doc links of every module, produced once during
    /// semantic analysis. Backends read link targets from here rather than
    /// re-resolving doc text.
    pub fn doc_links(&self) -> &DocLinks {
        &self.doc_links
    }

    /// The resolved doc links of one module. Every module present in
    /// [`Self::modules`] has an entry (possibly empty).
    pub fn module_doc_links(&self, module_path: &ItemPath) -> &ModuleDocLinks {
        static EMPTY: std::sync::LazyLock<ModuleDocLinks> =
            std::sync::LazyLock::new(ModuleDocLinks::default);
        self.doc_links.get(module_path).unwrap_or(&EMPTY)
    }

    /// Construct a `SemanticOutput` from its parts.
    /// Used by the Salsa query layer to project `SemanticAnalysis`.
    pub(crate) fn from_parts(
        type_registry: TypeRegistry,
        modules: BTreeMap<ItemPath, Module>,
        doc_link_resolver: DocLinkResolver,
        doc_links: DocLinks,
    ) -> Self {
        Self {
            type_registry,
            modules,
            doc_link_resolver,
            doc_links,
        }
    }
}
