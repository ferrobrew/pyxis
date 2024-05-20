use std::collections::HashMap;

use crate::{
    grammar::{self, ItemPath},
    semantic_analysis::types::{ItemDefinition, Type},
};

pub struct TypeRegistry {
    pub(crate) types: HashMap<ItemPath, ItemDefinition>,
    pub(crate) pointer_size: usize,
}

impl TypeRegistry {
    pub(crate) fn new(pointer_size: usize) -> TypeRegistry {
        TypeRegistry {
            types: HashMap::new(),
            pointer_size,
        }
    }

    pub fn pointer_size(&self) -> usize {
        self.pointer_size
    }

    pub fn get(&self, item_path: &ItemPath) -> Option<&ItemDefinition> {
        self.types.get(item_path)
    }

    pub fn get_mut(&mut self, item_path: &ItemPath) -> Option<&mut ItemDefinition> {
        self.types.get_mut(item_path)
    }

    pub(crate) fn resolved(&self) -> Vec<ItemPath> {
        self.types
            .iter()
            .filter(|(_, t)| !t.is_predefined() && t.is_resolved())
            .map(|(k, _)| k.clone())
            .collect()
    }

    pub(crate) fn unresolved(&self) -> Vec<ItemPath> {
        self.types
            .iter()
            .filter(|(_, t)| !t.is_predefined() && !t.is_resolved())
            .map(|(k, _)| k.clone())
            .collect()
    }

    pub(crate) fn add(&mut self, type_: ItemDefinition) {
        self.types.insert(type_.path.clone(), type_);
    }

    pub(crate) fn resolve_string(&self, scope: &[ItemPath], name: &str) -> Option<Type> {
        // todo: take scope_modules and scope_types instead of scope so that we don't need
        // to do this partitioning
        let (scope_types, scope_modules): (Vec<&ItemPath>, Vec<&ItemPath>) =
            scope.iter().partition(|ip| self.types.contains_key(ip));

        // If we find the relevant type within our scope, take the last one
        scope_types
            .into_iter()
            .rev()
            .find(|st| st.last().map(|i| i.as_str()) == Some(name))
            .map(|ip| Type::Raw(ip.clone()))
            .or_else(|| {
                // Otherwise, search our scopes
                std::iter::once(&ItemPath::empty())
                    .chain(scope_modules.iter().copied())
                    .map(|ip| ip.join(name.into()))
                    .find(|ip| self.types.contains_key(ip))
                    .map(Type::Raw)
            })
    }

    pub(crate) fn resolve_grammar_type(
        &self,
        scope: &[ItemPath],
        type_: &grammar::Type,
    ) -> Option<Type> {
        // todo: consider building a better module import/scope system
        match type_ {
            grammar::Type::ConstPointer(t) => self
                .resolve_grammar_type(scope, t.as_ref())
                .map(|t| Type::ConstPointer(Box::new(t))),
            grammar::Type::MutPointer(t) => self
                .resolve_grammar_type(scope, t.as_ref())
                .map(|t| Type::MutPointer(Box::new(t))),
            grammar::Type::Array(t, size) => self
                .resolve_grammar_type(scope, t.as_ref())
                .map(|t| Type::Array(Box::new(t), *size as usize)),
            grammar::Type::Ident(ident) => self.resolve_string(scope, ident.as_str()),
            grammar::Type::Unknown(size) => Some(self.padding_type(*size as usize)),
        }
    }

    pub(crate) fn padding_type(&self, bytes: usize) -> Type {
        Type::Array(Box::new(self.resolve_string(&[], "u8").unwrap()), bytes)
    }
}
