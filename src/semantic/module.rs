use std::collections::{HashMap, HashSet};

use crate::{
    grammar::{self, ItemPath},
    semantic::{
        type_registry,
        types::{Backend, ExternValue, ItemDefinition, Type},
    },
};

#[derive(Debug, Clone)]
pub struct Module {
    pub(crate) path: ItemPath,
    pub(crate) ast: grammar::Module,
    pub(crate) definition_paths: HashSet<ItemPath>,
    pub(crate) extern_values: Vec<ExternValue>,
    pub(crate) impls: HashMap<ItemPath, grammar::FunctionBlock>,
    pub(crate) backends: HashMap<String, Vec<Backend>>,
    pub(crate) doc: Option<String>,
}

impl Default for Module {
    fn default() -> Self {
        Self {
            path: ItemPath::empty(),
            ast: Default::default(),
            definition_paths: Default::default(),
            extern_values: Default::default(),
            impls: Default::default(),
            backends: Default::default(),
            doc: Default::default(),
        }
    }
}

impl Module {
    pub(crate) fn new(
        path: ItemPath,
        ast: grammar::Module,
        extern_values: Vec<ExternValue>,
        impls: &[grammar::FunctionBlock],
        backends: &[grammar::Backend],
    ) -> anyhow::Result<Self> {
        let impls = impls
            .iter()
            .map(|f| (path.join(f.name.as_str().into()), f.clone()))
            .collect();

        let mut backends_map: HashMap<String, Vec<Backend>> = HashMap::new();
        for backend in backends {
            backends_map
                .entry(backend.name.0.clone())
                .or_default()
                .push(Backend {
                    prologue: backend.prologue.clone(),
                    epilogue: backend.epilogue.clone(),
                });
        }

        let doc = ast.attributes.doc(&path)?;
        Ok(Self {
            path,
            ast,
            definition_paths: HashSet::new(),
            extern_values,
            impls,
            backends: backends_map,
            doc,
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

        for ev in &mut self.extern_values {
            if let Type::Unresolved(type_ref) = &ev.type_ {
                ev.type_ = type_registry
                    .resolve_grammar_type(&scope, type_ref)
                    .ok_or_else(|| anyhow::anyhow!("failed to resolve type for {}", ev.name))?;
            }
        }

        Ok(())
    }

    pub fn doc(&self) -> Option<&str> {
        self.doc.as_deref()
    }
}
