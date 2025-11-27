use std::collections::{BTreeMap, BTreeSet};

use crate::{
    grammar::{self, ItemPath},
    semantic::{
        error::{Result, SemanticError, TypeResolutionContext},
        function::Function,
        type_registry::TypeRegistry,
        types::{Backend, ExternValue, ItemDefinition, Type},
    },
    span::{ItemLocation, Located},
};

#[derive(Debug, Clone)]
pub struct Module {
    pub(crate) path: ItemPath,
    pub(crate) ast: grammar::Module,
    pub(crate) definition_paths: BTreeSet<ItemPath>,
    pub(crate) extern_values: Vec<ExternValue>,
    pub(crate) functions: Vec<Located<Function>>,
    pub(crate) impls: BTreeMap<ItemPath, grammar::FunctionBlock>,
    pub(crate) backends: BTreeMap<String, Vec<Backend>>,
    pub(crate) doc: Vec<String>,
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
        impls: &[grammar::FunctionBlock],
        backends: &[grammar::Backend],
    ) -> Result<Self> {
        let impls = impls
            .iter()
            .map(|f| (path.join(f.name.as_str().into()), f.clone()))
            .collect();

        let mut backends_map: BTreeMap<String, Vec<Backend>> = BTreeMap::new();
        for backend in backends {
            backends_map
                .entry(backend.name.0.clone())
                .or_default()
                .push(Backend {
                    prologue: backend.prologue.clone(),
                    epilogue: backend.epilogue.clone(),
                });
        }

        let doc = ast.doc_comments.clone();
        Ok(Self {
            path,
            ast,
            definition_paths: BTreeSet::new(),
            extern_values,
            functions: vec![],
            impls,
            backends: backends_map,
            doc,
        })
    }

    pub fn uses(&self) -> Vec<ItemPath> {
        self.ast.uses().cloned().collect()
    }

    pub fn definition_paths(&self) -> &BTreeSet<ItemPath> {
        &self.definition_paths
    }

    pub fn definitions<'a>(
        &'a self,
        type_registry: &'a TypeRegistry,
    ) -> impl Iterator<Item = &'a ItemDefinition> {
        self.definition_paths()
            .iter()
            .filter_map(|p| type_registry.get(p, &ItemLocation::internal()).ok())
    }

    pub fn scope(&self) -> Vec<ItemPath> {
        std::iter::once(self.path.clone())
            .chain(self.uses().iter().cloned())
            .collect()
    }

    pub(crate) fn resolve_extern_values(&mut self, type_registry: &mut TypeRegistry) -> Result<()> {
        let scope = self.scope();

        for ev in &mut self.extern_values {
            if let Type::Unresolved(type_ref) = &ev.type_ {
                ev.type_ = type_registry
                    .resolve_grammar_type(&scope, type_ref)
                    .ok_or_else(|| SemanticError::TypeResolutionFailed {
                        type_: type_ref.clone(),
                        resolution_context: TypeResolutionContext::ExternValue {
                            extern_name: ev.name.clone(),
                        },
                        location: type_ref.location.clone(),
                    })?;
            }
        }

        Ok(())
    }

    pub(crate) fn resolve_functions(&mut self, type_registry: &TypeRegistry) -> Result<()> {
        let scope = self.scope();

        for function in self.ast.functions().collect::<Vec<_>>() {
            self.functions.push(crate::semantic::function::build(
                type_registry,
                &scope,
                false, // is_vfunc
                function,
            )?);
        }

        Ok(())
    }

    pub fn doc(&self) -> &[String] {
        &self.doc
    }

    pub fn functions(&self) -> &[Located<Function>] {
        &self.functions
    }
}
