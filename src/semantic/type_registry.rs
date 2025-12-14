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

/// Result of attempting to look up a type in the registry.
/// This distinguishes between different failure modes for better error reporting.
#[derive(Debug, Clone)]
pub enum TypeLookupResult {
    /// Type was found and resolved successfully
    Found(Type),
    /// Type exists in the registry but is not yet resolved (should defer)
    NotYetResolved,
    /// Type doesn't exist in the registry at all
    NotFound {
        /// The type name that wasn't found (as written in source)
        type_name: String,
    },
}

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

    pub(crate) fn padding_type(&self, bytes: usize) -> Type {
        match self.resolve_string(&[], "u8") {
            TypeLookupResult::Found(t) => Type::Array(Box::new(t), bytes),
            _ => panic!("u8 type not found in type registry"),
        }
    }

    /// Resolves a type name in the given scope, returning detailed information
    /// about why resolution failed if it does.
    pub(crate) fn resolve_string(&self, scope: &[ItemPath], name: &str) -> TypeLookupResult {
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

        match found_path {
            Some(path) => {
                // Check if the type is resolved
                if let Some(item_def) = self.types.get(&path) {
                    if item_def.is_resolved() {
                        TypeLookupResult::Found(self.resolve_type_alias(Type::Raw(path)))
                    } else {
                        TypeLookupResult::NotYetResolved
                    }
                } else {
                    TypeLookupResult::NotFound {
                        type_name: name.to_string(),
                    }
                }
            }
            None => TypeLookupResult::NotFound {
                type_name: name.to_string(),
            },
        }
    }

    /// Resolves a path, checking if it's a qualified path or needs scope lookup.
    /// Returns detailed information about why resolution failed if it does.
    pub(crate) fn resolve_path(&self, scope: &[ItemPath], path: &ItemPath) -> TypeLookupResult {
        // If path has multiple segments, try to resolve it directly first
        if path.len() > 1 {
            if let Some(item_def) = self.types.get(path) {
                if item_def.is_resolved() {
                    return TypeLookupResult::Found(
                        self.resolve_type_alias(Type::Raw(path.clone())),
                    );
                } else {
                    return TypeLookupResult::NotYetResolved;
                }
            }
        }

        // For single-segment paths or unresolved multi-segment paths,
        // try scope-based resolution using the last segment
        if let Some(last_segment) = path.last() {
            self.resolve_string(scope, last_segment.as_str())
        } else {
            TypeLookupResult::NotFound {
                type_name: path.to_string(),
            }
        }
    }

    /// Resolves a grammar type to a semantic type.
    /// Returns detailed information about why resolution failed if it does.
    pub(crate) fn resolve_grammar_type(
        &self,
        scope: &[ItemPath],
        type_: &grammar::Type,
    ) -> TypeLookupResult {
        match type_ {
            grammar::Type::ConstPointer { pointee, .. } => {
                match self.resolve_grammar_type(scope, pointee) {
                    TypeLookupResult::Found(t) => {
                        TypeLookupResult::Found(Type::ConstPointer(Box::new(t)))
                    }
                    other => other,
                }
            }
            grammar::Type::MutPointer { pointee, .. } => {
                match self.resolve_grammar_type(scope, pointee) {
                    TypeLookupResult::Found(t) => {
                        TypeLookupResult::Found(Type::MutPointer(Box::new(t)))
                    }
                    other => other,
                }
            }
            grammar::Type::Array { element, size, .. } => {
                match self.resolve_grammar_type(scope, element) {
                    TypeLookupResult::Found(t) => {
                        TypeLookupResult::Found(Type::Array(Box::new(t), *size))
                    }
                    other => other,
                }
            }
            grammar::Type::Ident { path, .. } => self.resolve_path(scope, path),
            grammar::Type::Unknown { size, .. } => {
                TypeLookupResult::Found(self.padding_type(*size))
            }
        }
    }
}
