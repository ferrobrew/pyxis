use std::collections::BTreeMap;

use crate::{
    SemanticError,
    grammar::{self, ItemPath},
    semantic::{
        error::Result,
        types::{ItemDefinition, Type},
    },
    span::ItemLocation,
};

#[derive(Debug)]
pub struct TypeRegistry {
    types: BTreeMap<ItemPath, ItemDefinition>,
    pointer_size: usize,
}

impl TypeRegistry {
    pub(crate) fn new(pointer_size: usize) -> TypeRegistry {
        TypeRegistry {
            types: BTreeMap::new(),
            pointer_size,
        }
    }

    pub fn pointer_size(&self) -> usize {
        self.pointer_size
    }

    /// Check if a type exists in the registry
    pub fn contains(&self, item_path: &ItemPath) -> bool {
        self.types.contains_key(item_path)
    }

    pub fn get(
        &self,
        item_path: &ItemPath,
        from_location: &ItemLocation,
    ) -> Result<&ItemDefinition> {
        self.types
            .get(item_path)
            .ok_or_else(|| SemanticError::TypeNotFound {
                path: item_path.clone(),
                location: *from_location,
            })
    }

    pub fn get_mut(
        &mut self,
        item_path: &ItemPath,
        from_location: &ItemLocation,
    ) -> Result<&mut ItemDefinition> {
        self.types
            .get_mut(item_path)
            .ok_or_else(|| SemanticError::TypeNotFound {
                path: item_path.clone(),
                location: *from_location,
            })
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
        let found_path = scope_types
            .into_iter()
            .rev()
            .find(|st| st.last().map(|i| i.as_str()) == Some(name))
            .cloned()
            .or_else(|| {
                // Otherwise, search our scopes
                std::iter::once(&ItemPath::empty())
                    .chain(scope_modules.iter().copied())
                    .map(|ip| ip.join(name.into()))
                    .find(|ip| self.types.contains_key(ip))
            });

        // If we found a type, check if it's a type alias and follow it
        found_path.map(|ip| self.resolve_type_alias(Type::Raw(ip)))
    }

    /// Follows type aliases recursively to get the final resolved type.
    /// If the type is not an alias, returns it unchanged.
    fn resolve_type_alias(&self, type_: Type) -> Type {
        match &type_ {
            Type::Raw(path) => {
                // Check if this path refers to a type alias
                if let Some(item_def) = self.types.get(path) {
                    if let Some(resolved) = item_def.resolved() {
                        if let Some(type_alias) = resolved.inner.as_type_alias() {
                            // Return the target type (which is already resolved)
                            return type_alias.target.clone();
                        }
                    }
                }
                type_
            }
            // For compound types, we don't need to resolve here since the inner types
            // would have been resolved when they were constructed
            _ => type_,
        }
    }

    /// Resolves a path, checking if it's a qualified path or needs scope lookup.
    pub(crate) fn resolve_path(&self, scope: &[ItemPath], path: &ItemPath) -> Option<Type> {
        // If path has multiple segments, try to resolve it directly first
        if path.len() > 1 {
            if self.types.contains_key(path) {
                return Some(self.resolve_type_alias(Type::Raw(path.clone())));
            }
        }

        // For single-segment paths or unresolved multi-segment paths,
        // try scope-based resolution using the last segment
        if let Some(last_segment) = path.last() {
            self.resolve_string(scope, last_segment.as_str())
        } else {
            None
        }
    }

    pub(crate) fn resolve_grammar_type(
        &self,
        scope: &[ItemPath],
        type_: &grammar::Type,
    ) -> Option<Type> {
        // todo: consider building a better module import/scope system
        match type_ {
            grammar::Type::ConstPointer { pointee, .. } => self
                .resolve_grammar_type(scope, pointee)
                .map(|t| Type::ConstPointer(Box::new(t))),
            grammar::Type::MutPointer { pointee, .. } => self
                .resolve_grammar_type(scope, pointee)
                .map(|t| Type::MutPointer(Box::new(t))),
            grammar::Type::Array { element, size, .. } => self
                .resolve_grammar_type(scope, element)
                .map(|t| Type::Array(Box::new(t), *size)),
            grammar::Type::Ident { path, .. } => self.resolve_path(scope, path),
            grammar::Type::Unknown { size, .. } => Some(self.padding_type(*size)),
        }
    }

    pub(crate) fn padding_type(&self, bytes: usize) -> Type {
        Type::Array(Box::new(self.resolve_string(&[], "u8").unwrap()), bytes)
    }
}
