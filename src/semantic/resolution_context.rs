//! Resolution context — replaces SemanticState for the build functions.
//!
//! This struct holds references to the type registry and modules,
//! providing the same lookup interface that SemanticState's methods
//! provided, without the SemanticState wrapper.

use std::collections::BTreeMap;

use crate::{
    grammar::ItemPath,
    semantic::{
        error::{Result, SemanticError},
        module::Module,
        type_registry::TypeRegistry,
        types::ItemDefinition,
    },
    span::{HasLocation, ItemLocation},
};

/// Mutable context for type resolution. Used by type_definition::build
/// which needs to add vftable types to the registry and track
/// definition_paths on modules.
pub struct ResolutionContext<'a> {
    pub type_registry: &'a mut TypeRegistry,
    pub modules: &'a mut BTreeMap<ItemPath, Module>,
}

impl<'a> ResolutionContext<'a> {
    pub fn new(
        type_registry: &'a mut TypeRegistry,
        modules: &'a mut BTreeMap<ItemPath, Module>,
    ) -> Self {
        Self { type_registry, modules }
    }

    pub fn get_module_for_path(
        &self,
        path: &ItemPath,
        from_location: &ItemLocation,
    ) -> Result<&Module> {
        path.parent()
            .and_then(|parent| self.modules.get(&parent))
            .ok_or_else(|| SemanticError::ModuleNotFound {
                path: path.clone(),
                location: *from_location,
            })
    }

    pub fn add_item(&mut self, item_definition: ItemDefinition) -> Result<()> {
        let parent_path =
            &item_definition
                .path
                .parent()
                .ok_or_else(|| SemanticError::ModuleNotFound {
                    path: item_definition.path.clone(),
                    location: *item_definition.location(),
                })?;
        self.modules
            .get_mut(parent_path)
            .ok_or_else(|| SemanticError::ModuleNotFound {
                path: parent_path.clone(),
                location: *item_definition.location(),
            })?
            .definition_paths
            .insert(item_definition.path.clone());
        self.type_registry.add(item_definition);
        Ok(())
    }
}

/// Immutable view of the resolution context for read-only operations.
/// Used by enum_definition::build and bitflags_definition::build which
/// only need to read the registry, not mutate it.
pub struct ResolutionContextRef<'a> {
    pub type_registry: &'a TypeRegistry,
    pub modules: &'a BTreeMap<ItemPath, Module>,
}

impl<'a> ResolutionContextRef<'a> {
    pub fn new(
        type_registry: &'a TypeRegistry,
        modules: &'a BTreeMap<ItemPath, Module>,
    ) -> Self {
        Self { type_registry, modules }
    }

    pub fn get_module_for_path(
        &self,
        path: &ItemPath,
        from_location: &ItemLocation,
    ) -> Result<&Module> {
        path.parent()
            .and_then(|parent| self.modules.get(&parent))
            .ok_or_else(|| SemanticError::ModuleNotFound {
                path: path.clone(),
                location: *from_location,
            })
    }

    pub fn type_registry(&self) -> &TypeRegistry {
        self.type_registry
    }
}
