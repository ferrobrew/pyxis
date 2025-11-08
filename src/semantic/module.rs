use std::collections::{HashMap, HashSet};

use crate::{
    grammar::{self, ItemPath},
    span::Spanned,
    semantic::{
        function::Function,
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
    pub(crate) functions: Vec<Function>,
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
            functions: Default::default(),
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
        impls: &[Spanned<grammar::FunctionBlock>],
        backends: &[Spanned<grammar::Backend>],
    ) -> anyhow::Result<Self> {
        let impls = impls
            .iter()
            .map(|f| (path.join(f.node.name.node.as_str().into()), f.node.clone()))
            .collect();

        let mut backends_map: HashMap<String, Vec<Backend>> = HashMap::new();
        for backend in backends {
            backends_map
                .entry(backend.node.name.node.0.clone())
                .or_default()
                .push(Backend {
                    prologue: backend.node.prologue.clone(),
                    epilogue: backend.node.epilogue.clone(),
                });
        }

        let doc = ast.attributes.doc(&path)?;
        Ok(Self {
            path,
            ast,
            definition_paths: HashSet::new(),
            extern_values,
            functions: vec![],
            impls,
            backends: backends_map,
            doc,
        })
    }

    pub fn uses(&self) -> &[Spanned<ItemPath>] {
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
            .chain(self.uses().iter().map(|u| u.node.clone()))
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

    pub(crate) fn resolve_functions(
        &mut self,
        type_registry: &type_registry::TypeRegistry,
    ) -> anyhow::Result<()> {
        let scope = self.scope();

        for function in &self.ast.functions {
            let semantic_function = crate::semantic::function::build(
                type_registry,
                &scope,
                false, // is_vfunc
                function,
            )?;
            self.functions.push(semantic_function);
        }

        Ok(())
    }

    pub fn doc(&self) -> Option<&str> {
        self.doc.as_deref()
    }

    pub fn functions(&self) -> &[Function] {
        &self.functions
    }
}
