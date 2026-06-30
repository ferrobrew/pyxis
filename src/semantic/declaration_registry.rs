//! Declaration registry — a read-only view of all declared items.
//!
//! This is the "phase 1" data structure: all item paths and their grammar
//! definitions are collected here, without any resolution state. Name
//! resolution (finding an `ItemPath` for a name in a scope) is a pure
//! function of this data.
//!
//! In the Salsa query graph, `collect_declarations` produces this; `analyze`
//! and the LSP read it for name resolution and item lookup. (Per-item
//! incremental resolution uses the leaner, location-free `name_index` instead —
//! this registry carries full definitions and locations, so it changes on every
//! edit and would defeat backdating.)

use std::collections::BTreeMap;

use crate::{
    grammar::{self, ItemPath},
    parser::cfg::CfgPredicate,
    semantic::attribute,
    span::{HasLocation, ItemLocation},
};

/// A read-only registry of all declared items.
///
/// Built once from all parsed files and used for name resolution. It does NOT
/// track resolution state — that's the job of `analyze` / `resolve_item`.
#[derive(Debug, Clone, Default, Hash, PartialEq, Eq)]
pub struct DeclarationRegistry {
    /// All declared items: path → grammar definition
    items: BTreeMap<ItemPath, grammar::ItemDefinition>,
    /// All declared modules: path → (module, scope)
    modules: BTreeMap<ItemPath, ModuleInfo>,
    /// All extern types (resolved at declaration time — they have size/alignment)
    extern_types: BTreeMap<ItemPath, ExternTypeInfo>,
    /// Predefined types (u8, u32, etc.)
    predefined: BTreeMap<ItemPath, PredefinedInfo>,
    /// Pointer size from project config (4 or 8)
    pointer_size: usize,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ModuleInfo {
    pub module: grammar::Module,
    pub scope: Vec<ItemPath>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ExternTypeInfo {
    pub size: usize,
    pub alignment: usize,
    pub location: ItemLocation,
    pub declaration_location: ItemLocation,
    pub doc_comments: Vec<String>,
    pub cfg: Option<CfgPredicate>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct PredefinedInfo {
    pub size: usize,
    pub alignment: usize,
}

/// Result of looking up a type name in a scope.
pub enum NameResolution {
    /// Found a declared item (type/enum/bitflags/type-alias)
    Found(ItemPath),
    /// Found a predefined type (u8, u32, etc.)
    FoundPredefined(ItemPath),
    /// Found an extern type (already has size/alignment)
    FoundExtern(ItemPath),
    /// Name not found in any scope
    NotFound,
}

impl DeclarationRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a module and all its items.
    pub fn register_module(&mut self, module: &grammar::Module, module_path: &ItemPath) {
        // Build the scope: the module's own path plus use'd paths
        let scope = std::iter::once(module_path.clone())
            .chain(module.uses().flat_map(|u| {
                if let grammar::ModuleItem::Use { tree, .. } = u {
                    tree.flatten()
                } else {
                    vec![]
                }
            }))
            .collect::<Vec<_>>();

        self.modules.insert(
            module_path.clone(),
            ModuleInfo {
                module: module.clone(),
                scope,
            },
        );

        // Register all definitions
        for def in module.definitions() {
            let item_path = module_path.join(def.name.as_str().into());
            self.items.insert(item_path.clone(), def.clone());
        }

        // Register extern types
        for extern_type in module.extern_types() {
            if let grammar::ModuleItem::ExternType {
                name: extern_name,
                attributes,
                doc_comments: extern_doc_comments,
                location: extern_location,
                declaration_location: extern_declaration_location,
            } = extern_type
            {
                let mut size = None;
                let mut alignment = None;
                for attribute in attributes {
                    let Some((ident, items)) = attribute.function() else {
                        continue;
                    };
                    let loc = attribute.location();
                    if let Ok(Some(s)) = attribute::parse_size(ident, items, loc) {
                        size = Some(s);
                    } else if let Ok(Some(a)) = attribute::parse_align(ident, items, loc) {
                        alignment = Some(a);
                    }
                }
                if let (Some(size), Some(alignment)) = (size, alignment) {
                    let extern_path = module_path.join(extern_name.as_str().into());
                    self.extern_types.insert(
                        extern_path,
                        ExternTypeInfo {
                            size,
                            alignment,
                            location: *extern_location,
                            declaration_location: *extern_declaration_location,
                            doc_comments: extern_doc_comments.clone(),
                            cfg: attributes.cfg(),
                        },
                    );
                }
            }
        }
    }

    /// Register all predefined types (u8, u32, etc.)
    pub fn register_predefined(&mut self) {
        use crate::semantic::types::PredefinedItem;
        for predefined in PredefinedItem::ALL {
            let path = ItemPath::from(predefined.name());
            let size = predefined.size();
            let alignment = size.max(1);
            self.predefined
                .insert(path, PredefinedInfo { size, alignment });
        }
    }

    /// Check if a path exists in the registry (as any kind of item).
    pub fn contains(&self, path: &ItemPath) -> bool {
        self.items.contains_key(path)
            || self.extern_types.contains_key(path)
            || self.predefined.contains_key(path)
    }

    /// Get the grammar definition for an item path.
    pub fn get_definition(&self, path: &ItemPath) -> Option<&grammar::ItemDefinition> {
        self.items.get(path)
    }

    /// Get extern type info for a path.
    pub fn get_extern_type(&self, path: &ItemPath) -> Option<&ExternTypeInfo> {
        self.extern_types.get(path)
    }

    /// Get predefined type info for a path.
    pub fn get_predefined(&self, path: &ItemPath) -> Option<&PredefinedInfo> {
        self.predefined.get(path)
    }

    /// Get the module scope for an item path (for name resolution).
    pub fn get_scope(&self, item_path: &ItemPath) -> Option<&[ItemPath]> {
        let parent = item_path.parent()?;
        self.modules.get(&parent).map(|m| m.scope.as_slice())
    }

    /// Resolve a type name in the given scope, returning the full path.
    ///
    /// This is a pure function of the declaration data — it doesn't check
    /// resolution state, only whether the name exists as a declaration.
    pub fn resolve_name(&self, scope: &[ItemPath], name: &str) -> NameResolution {
        // Check scope types (explicitly imported via `use`)
        let found = scope
            .iter()
            .rev()
            .find(|s| s.last().map(|i| i.as_str()) == Some(name))
            .cloned()
            .or_else(|| {
                // Search scope modules
                std::iter::once(&ItemPath::empty())
                    .chain(scope.iter())
                    .map(|ip| ip.join(name.into()))
                    .find(|ip| self.contains(ip))
            });

        match found {
            Some(path) => {
                if self.predefined.contains_key(&path) {
                    NameResolution::FoundPredefined(path)
                } else if self.extern_types.contains_key(&path) {
                    NameResolution::FoundExtern(path)
                } else {
                    NameResolution::Found(path)
                }
            }
            None => NameResolution::NotFound,
        }
    }

    /// Get all extern types as an iterator.
    pub fn extern_types_iter(&self) -> impl Iterator<Item = (&ItemPath, &ExternTypeInfo)> {
        self.extern_types.iter()
    }

    /// Get all declared item paths.
    pub fn item_paths(&self) -> impl Iterator<Item = &ItemPath> {
        self.items.keys()
    }

    /// Get all module paths.
    pub fn module_paths(&self) -> impl Iterator<Item = &ItemPath> {
        self.modules.keys()
    }

    /// Get a module by path.
    pub fn get_module(&self, path: &ItemPath) -> Option<&ModuleInfo> {
        self.modules.get(path)
    }

    /// Get all modules.
    pub fn modules(&self) -> &BTreeMap<ItemPath, ModuleInfo> {
        &self.modules
    }

    /// Get the pointer size (always available — it's a config value).
    /// This is stored separately and set during construction.
    pub fn pointer_size(&self) -> usize {
        self.pointer_size
    }

    /// Set the pointer size.
    pub fn set_pointer_size(&mut self, size: usize) {
        self.pointer_size = size;
    }
}
