use std::collections::HashMap;

use crate::{
    grammar::{self, ItemPath},
    semantic_analysis::types::{Type, TypeDefinition},
};

pub struct TypeRegistry {
    pub(crate) types: HashMap<ItemPath, TypeDefinition>,
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

    pub fn get(&self, item_path: &ItemPath) -> Option<&TypeDefinition> {
        self.types.get(item_path)
    }

    pub fn get_mut(&mut self, item_path: &ItemPath) -> Option<&mut TypeDefinition> {
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

    pub(crate) fn add(&mut self, type_: TypeDefinition) {
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
            grammar::Type::Ident(ident) => self.resolve_string(scope, ident.as_str()),
        }
    }

    pub(crate) fn resolve_grammar_typeref(
        &self,
        scope: &[ItemPath],
        type_ref: &grammar::TypeRef,
    ) -> Option<Type> {
        match type_ref {
            grammar::TypeRef::Type(type_) => self.resolve_grammar_type(scope, type_),
            grammar::TypeRef::Macro(macro_call) => match macro_call.match_repr() {
                ("unk", [grammar::Expr::IntLiteral(size)]) => self
                    .resolve_string(&[], "u8")
                    .map(|t| Type::Array(Box::new(t), *size as usize)),
                _ => panic!("unsupported macro call"),
            },
        }
    }
}
