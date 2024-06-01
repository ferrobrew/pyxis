use std::collections::{HashMap, HashSet};

use crate::{
    grammar::{self, ItemPath},
    semantic_analysis::{
        type_registry,
        types::{Backend, ItemDefinition, Type},
    },
};

#[derive(Debug, Clone)]
pub struct Module {
    pub(crate) path: ItemPath,
    pub(crate) ast: grammar::Module,
    pub(crate) definition_paths: HashSet<ItemPath>,
    pub(crate) extern_values: Vec<(String, Type, usize)>,
    pub(crate) impls: HashMap<ItemPath, grammar::FunctionBlock>,
    pub(crate) vftables: HashMap<ItemPath, grammar::FunctionBlock>,
    pub(crate) backends: HashMap<String, Backend>,
}

impl Default for Module {
    fn default() -> Self {
        Self {
            path: ItemPath::empty(),
            ast: Default::default(),
            definition_paths: Default::default(),
            extern_values: Default::default(),
            impls: Default::default(),
            vftables: Default::default(),
            backends: Default::default(),
        }
    }
}

impl Module {
    pub(crate) fn new(
        path: ItemPath,
        ast: grammar::Module,
        extern_values: Vec<(String, Type, usize)>,
        impls: &[grammar::FunctionBlock],
        vftables: &[grammar::FunctionBlock],
        backends: &[grammar::Backend],
    ) -> anyhow::Result<Self> {
        fn convert_functions(
            path: &ItemPath,
            functions: &[grammar::FunctionBlock],
        ) -> HashMap<ItemPath, grammar::FunctionBlock> {
            functions
                .iter()
                .map(|f| (path.join(f.name.as_str().into()), f.clone()))
                .collect()
        }

        let impls = convert_functions(&path, impls);
        let vftables = convert_functions(&path, vftables);

        let mut backends_map = HashMap::new();
        for backend in backends {
            if backends_map.contains_key(&backend.name.0) {
                anyhow::bail!("duplicate backend: {}", backend.name.0);
            }

            backends_map.insert(
                backend.name.0.clone(),
                Backend {
                    prelude: backend.prelude.clone(),
                    postlude: backend.postlude.clone(),
                },
            );
        }

        Ok(Self {
            path,
            ast,
            definition_paths: HashSet::new(),
            extern_values,
            impls,
            vftables,
            backends: backends_map,
        })
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
    ) -> impl Iterator<Item = &'a ItemDefinition> {
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
            if let Type::Unresolved(type_ref) = type_ {
                *type_ = type_registry
                    .resolve_grammar_type(&scope, type_ref)
                    .ok_or_else(|| anyhow::anyhow!("failed to resolve type for {}", name))?;
            }
        }

        Ok(())
    }
}
