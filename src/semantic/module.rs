use std::collections::{BTreeMap, BTreeSet};

use crate::{
    grammar::{self, ItemPath},
    semantic::{
        error::{Result, SemanticError, TypeResolutionContext},
        function::Function,
        type_registry::{TypeLookupResult, TypeRegistry},
        types::{Backend, ExternValue, ItemDefinition, Type},
    },
    span::{HasLocation, ItemLocation},
};

#[derive(Debug, Clone)]
pub struct Module {
    pub(crate) path: ItemPath,
    pub(crate) ast: grammar::Module,
    pub(crate) definition_paths: BTreeSet<ItemPath>,
    pub(crate) extern_values: Vec<ExternValue>,
    pub(crate) functions: Vec<Function>,
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
        grammar_backends: &[grammar::Backend],
    ) -> Result<Self> {
        let impls = impls
            .iter()
            .map(|f| (path.join(f.name.as_str().into()), f.clone()))
            .collect();

        let mut backends: BTreeMap<String, Vec<Backend>> = BTreeMap::new();
        for backend in grammar_backends {
            backends
                .entry(backend.name.0.clone())
                .or_default()
                .push(Backend {
                    prologue: backend.prologue.clone(),
                    epilogue: backend.epilogue.clone(),
                    location: backend.location,
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
            backends,
            doc,
        })
    }

    pub fn uses(&self) -> impl Iterator<Item = &grammar::ModuleItem> {
        self.ast.uses()
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
            .chain(self.uses().flat_map(|u| {
                if let grammar::ModuleItem::Use { tree, .. } = u {
                    tree.flatten()
                } else {
                    vec![]
                }
            }))
            .collect()
    }

    pub(crate) fn resolve_extern_values(&mut self, type_registry: &mut TypeRegistry) -> Result<()> {
        let scope = self.scope();

        for ev in &mut self.extern_values {
            if let Type::Unresolved(type_ref) = &ev.type_ {
                ev.type_ = match type_registry.resolve_grammar_type(&scope, type_ref) {
                    TypeLookupResult::Found(t) => t,
                    TypeLookupResult::NotYetResolved | TypeLookupResult::NotFound { .. } => {
                        return Err(SemanticError::TypeResolutionFailed {
                            type_: type_ref.clone(),
                            resolution_context: TypeResolutionContext::ExternValue {
                                extern_name: ev.name.clone(),
                            },
                            location: *type_ref.location(),
                        });
                    }
                };
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

    pub fn functions(&self) -> &[Function] {
        &self.functions
    }
}
