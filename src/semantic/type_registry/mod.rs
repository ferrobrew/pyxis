use std::{collections::BTreeMap, sync::Arc};

use crate::{
    SemanticError,
    grammar::ItemPath,
    semantic::{
        error::Result,
        types::{ItemDefinition, Type, Visibility},
    },
    span::ItemLocation,
};

mod aliases;
mod generics;

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
    /// Type exists but is private and not accessible from the requesting module
    PrivateAccess {
        /// The path of the private type
        item_path: ItemPath,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeRegistry {
    types: BTreeMap<ItemPath, ItemDefinition>,
    /// Explicit re-exports (`pub use`): alias path → target path as written.
    /// Only the shared `placeholder_base` registry populates this; overlays
    /// created via [`Self::with_base`] inherit it through `base`. Following the
    /// chain to a fixpoint canonicalizes a re-exported name to its definition.
    reexports: BTreeMap<ItemPath, ItemPath>,
    /// Optional shared read-only base consulted for any path not in `types`.
    /// Lets `resolve_item` overlay just its own resolved dependencies on top
    /// of a memoized placeholder base instead of cloning all n placeholders.
    base: Option<Arc<TypeRegistry>>,
    pointer_size: usize,
}

impl TypeRegistry {
    pub(crate) fn new(pointer_size: usize) -> TypeRegistry {
        TypeRegistry {
            types: BTreeMap::new(),
            reexports: BTreeMap::new(),
            base: None,
            pointer_size,
        }
    }

    /// A registry that overlays `base`: own additions shadow it, and any
    /// lookup for a path not added here falls through to `base`.
    pub(crate) fn with_base(base: Arc<TypeRegistry>) -> TypeRegistry {
        let pointer_size = base.pointer_size;
        TypeRegistry {
            types: BTreeMap::new(),
            reexports: BTreeMap::new(),
            base: Some(base),
            pointer_size,
        }
    }

    /// Record a `pub use` re-export: `alias` (`<module>::<leaf>`) → `target`
    /// (as written). Consulted by [`Self::canonicalize`].
    pub(crate) fn add_reexport(&mut self, alias: ItemPath, target: ItemPath) {
        if alias != target {
            self.reexports.insert(alias, target);
        }
    }

    /// The re-export target for `path`, checking own additions then the base.
    fn reexport_target(&self, path: &ItemPath) -> Option<ItemPath> {
        self.reexports
            .get(path)
            .cloned()
            .or_else(|| self.base.as_ref().and_then(|b| b.reexport_target(path)))
    }

    /// Follow the `pub use` re-export chain from `path` to a fixpoint. Returns
    /// `path` unchanged if it is not a re-export alias. Bounded against cycles.
    pub(crate) fn canonicalize(&self, path: &ItemPath) -> ItemPath {
        let mut current = path.clone();
        for _ in 0..64 {
            match self.reexport_target(&current) {
                Some(next) if next != current => current = next,
                _ => break,
            }
        }
        current
    }

    /// Look up an item — own additions first, then the base. A re-export alias
    /// is canonicalized to its target so the resolved definition (which may live
    /// in this overlay) is found rather than the alias path.
    pub(in crate::semantic) fn lookup(&self, path: &ItemPath) -> Option<&ItemDefinition> {
        let canonical = self.canonicalize(path);
        self.types
            .get(&canonical)
            .or_else(|| self.base.as_ref().and_then(|b| b.lookup(&canonical)))
    }

    /// Whether an item exists in own additions or the base (following re-exports).
    fn has(&self, path: &ItemPath) -> bool {
        let canonical = self.canonicalize(path);
        self.types.contains_key(&canonical) || self.base.as_ref().is_some_and(|b| b.has(&canonical))
    }

    pub fn pointer_size(&self) -> usize {
        self.pointer_size
    }

    /// Check if a type exists in the registry
    pub fn contains(&self, item_path: &ItemPath) -> bool {
        self.has(item_path)
    }

    /// Check if a module can access an item based on visibility rules.
    /// Private items are only visible to:
    /// - The same module
    /// - Child modules (descendants)
    fn can_access(&self, from_module: &ItemPath, item_path: &ItemPath) -> bool {
        if let Some(item_def) = self.lookup(item_path) {
            // Public items are always accessible
            if item_def.visibility == Visibility::Public {
                return true;
            }

            // Private items: check if from_module is the same as or a child of the item's module
            if let Some(item_module) = item_path.parent() {
                // Same module can always access
                if from_module == &item_module {
                    return true;
                }
                // Child modules can access parent's private items
                return from_module.starts_with(&item_module);
            }
        }
        // If item doesn't exist, let the caller handle it
        true
    }

    /// Extract the current module path from the scope.
    /// The first element of the scope is always the current module path.
    fn get_from_module(scope: &[ItemPath]) -> Option<&ItemPath> {
        scope.first()
    }

    pub fn get(
        &self,
        item_path: &ItemPath,
        from_location: &ItemLocation,
    ) -> Result<&ItemDefinition> {
        self.lookup(item_path)
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

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&ItemPath, &ItemDefinition)> {
        self.types.iter()
    }

    pub(crate) fn add(&mut self, type_: ItemDefinition) {
        self.types.insert(type_.path.clone(), type_);
    }
}
