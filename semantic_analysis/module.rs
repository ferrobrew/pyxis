use std::collections::HashSet;

use super::{
    super::grammar::{self, ItemPath},
    type_registry, types,
};

#[derive(Debug)]
pub struct Module {
    pub(crate) path: ItemPath,
    pub(crate) ast: grammar::Module,
    pub(crate) definition_paths: HashSet<ItemPath>,
    pub(crate) extern_values: Vec<(String, types::Type, usize)>,
}

impl Module {
    pub(crate) fn new(
        path: ItemPath,
        ast: grammar::Module,
        extern_values: Vec<(String, types::Type, usize)>,
    ) -> Self {
        Self {
            path,
            ast,
            definition_paths: HashSet::new(),
            extern_values,
        }
    }

    pub fn uses(&self) -> &[ItemPath] {
        &self.ast.uses
    }

    pub fn definition_paths(&self) -> &HashSet<ItemPath> {
        &self.definition_paths
    }

    pub fn definitions<'a>(
        &'a self,
        type_registry: &'a type_registry::TypeRegistry,
    ) -> impl Iterator<Item = &'a types::TypeDefinition> {
        self.definition_paths()
            .iter()
            .filter_map(|p| type_registry.get(p))
    }

    pub fn scope(&self) -> Vec<ItemPath> {
        std::iter::once(self.path.clone())
            .chain(self.uses().iter().cloned())
            .collect()
    }

    pub(crate) fn resolve_extern_values(
        &mut self,
        type_registry: &mut type_registry::TypeRegistry,
    ) -> anyhow::Result<()> {
        let scope = self.scope();

        for (name, type_, _) in &mut self.extern_values {
            if let types::Type::Unresolved(type_ref) = type_ {
                *type_ = type_registry
                    .resolve_grammar_typeref(&scope, type_ref)
                    .ok_or_else(|| anyhow::anyhow!("failed to resolve type for {}", name))?;
            }
        }

        Ok(())
    }
}
