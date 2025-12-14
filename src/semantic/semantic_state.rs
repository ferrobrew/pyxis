use std::{collections::BTreeMap, path::Path};

use crate::{
    BuildError,
    grammar::{self, ItemPath},
    parser,
    semantic::{
        attribute, bitflags_definition, enum_definition,
        error::{Result, SemanticError},
        module::Module,
        type_alias_definition, type_definition,
        type_registry::TypeRegistry,
        types::{
            ExternValue, ItemCategory, ItemDefinition, ItemState, ItemStateResolved,
            PredefinedItem, Type, TypeDefinition, Visibility,
        },
    },
    source_store::FileStore,
    span::{HasLocation, ItemLocation},
};

pub struct SemanticState {
    modules: BTreeMap<ItemPath, Module>,
    pub(crate) type_registry: TypeRegistry,
}

impl SemanticState {
    pub fn new(pointer_size: usize) -> Self {
        let mut semantic_state = Self {
            modules: BTreeMap::new(),
            type_registry: TypeRegistry::new(pointer_size),
        };

        // Insert the empty root module.
        semantic_state
            .modules
            .insert(ItemPath::empty(), Module::default());

        for predefined_item in PredefinedItem::ALL {
            let path = ItemPath::from(predefined_item.name());
            let size = predefined_item.size();
            let alignment = size.max(1);
            let location = ItemLocation::internal();
            semantic_state
                .add_item(ItemDefinition {
                    visibility: Visibility::Public,
                    path,
                    state: ItemState::Resolved(ItemStateResolved {
                        size,
                        alignment,
                        inner: TypeDefinition {
                            cloneable: true,
                            copyable: true,
                            defaultable: true,
                            ..Default::default()
                        }
                        .into(),
                    }),
                    category: ItemCategory::Predefined,
                    predefined: Some(*predefined_item),
                    location,
                })
                .expect("failed to add predefined type");
        }

        semantic_state
    }

    pub fn add_file(
        &mut self,
        file_store: &mut FileStore,
        base_path: &Path,
        path: &Path,
    ) -> std::result::Result<(), BuildError> {
        let source = std::fs::read_to_string(path).map_err(|e| BuildError::Io {
            error: e,
            context: format!("reading file {}", path.display()),
        })?;
        // Use relative path from base_path for the filename (for display/linking purposes)
        let relative_path = path.strip_prefix(base_path).unwrap_or(path);
        let filename = relative_path.display().to_string();

        // Register the file and get its ID
        let file_id = file_store.register_path(filename, path.to_path_buf());

        self.add_module(
            &parser::parse_str_with_file_id(&source, file_id)?,
            &ItemPath::from_path(relative_path),
        )
        .map_err(Into::into)
    }

    pub fn add_module(&mut self, module: &grammar::Module, path: &ItemPath) -> Result<()> {
        let extern_values = module
            .extern_values()
            .map(|ev| {
                let name = &ev.name;
                let mut address = None;
                for attribute in &ev.attributes {
                    let Some((ident, items)) = attribute.function() else {
                        continue;
                    };
                    if let Some(attr_address) =
                        attribute::parse_address(ident, items, attribute.location())?
                    {
                        address = Some(attr_address);
                    }
                }

                let address = address.ok_or_else(|| SemanticError::MissingAttribute {
                    attribute_name: "address".into(),
                    item_kind: "extern value".into(),
                    item_path: path.join(name.as_str().into()),
                    location: ev.location,
                })?;

                Ok(ExternValue {
                    visibility: Visibility::from(ev.visibility),
                    name: name.as_str().to_owned(),
                    type_: Type::Unresolved(ev.type_.clone()),
                    address,
                    location: ev.location,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        let impls: Vec<_> = module.impls().cloned().collect();
        let backends: Vec<_> = module.backends().cloned().collect();

        self.modules.insert(
            path.clone(),
            Module::new(
                path.clone(),
                module.clone(),
                extern_values,
                &impls,
                &backends,
            )?,
        );

        for definition in module.definitions() {
            let new_path = path.join(definition.name.as_str().into());

            self.add_item(ItemDefinition {
                visibility: definition.visibility.into(),
                path: new_path,
                state: ItemState::Unresolved(definition.clone()),
                category: ItemCategory::Defined,
                predefined: None,
                location: definition.location,
            })?;
        }

        for extern_type in module.extern_types() {
            let grammar::ModuleItem::ExternType {
                name: extern_name,
                attributes,
                location: extern_location,
                ..
            } = extern_type
            else {
                continue;
            };
            let mut size = None;
            let mut alignment = None;
            for attribute in attributes {
                let Some((ident, items)) = attribute.function() else {
                    continue;
                };
                let location = attribute.location();
                if let Some(attr_size) = attribute::parse_size(ident, items, location)? {
                    size = Some(attr_size);
                } else if let Some(attr_align) = attribute::parse_align(ident, items, location)? {
                    alignment = Some(attr_align);
                }
            }
            let size = size.ok_or_else(|| SemanticError::MissingExternAttribute {
                attribute_name: "size".into(),
                extern_kind: "extern type".into(),
                type_name: extern_name.as_str().into(),
                module_name: path.to_string(),
                location: *extern_location,
            })?;
            let alignment = alignment.ok_or_else(|| SemanticError::MissingExternAttribute {
                attribute_name: "align".into(),
                extern_kind: "extern type".into(),
                type_name: extern_name.as_str().into(),
                module_name: path.to_string(),
                location: *extern_location,
            })?;

            let extern_path = path.join(extern_name.as_str().into());

            self.add_item(ItemDefinition {
                visibility: Visibility::Public,
                path: extern_path.clone(),
                state: ItemState::Resolved(ItemStateResolved {
                    size,
                    alignment,
                    inner: TypeDefinition::default().into(),
                }),
                category: ItemCategory::Extern,
                predefined: None,
                location: *extern_location,
            })?;
        }

        Ok(())
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

    pub fn build(mut self) -> Result<ResolvedSemanticState> {
        // Validate all use statements before resolving types
        self.validate_uses()?;

        loop {
            let to_resolve = self.type_registry.unresolved();
            if to_resolve.is_empty() {
                break;
            }

            for resolvee_path in &to_resolve {
                let item_def = self
                    .type_registry
                    .get(resolvee_path, &ItemLocation::internal())?;
                let ItemState::Unresolved(definition) = item_def.state.clone() else {
                    continue;
                };

                let visibility: Visibility = definition.visibility.into();

                let def_location = &definition.location;
                let item = match &definition.inner {
                    grammar::ItemDefinitionInner::Type(ty) => type_definition::build(
                        &mut self,
                        resolvee_path,
                        visibility,
                        ty,
                        def_location,
                        &definition.doc_comments,
                    )?,
                    grammar::ItemDefinitionInner::Enum(e) => enum_definition::build(
                        &self,
                        resolvee_path,
                        e,
                        def_location,
                        &definition.doc_comments,
                    )?,
                    grammar::ItemDefinitionInner::Bitflags(b) => bitflags_definition::build(
                        &self,
                        resolvee_path,
                        b,
                        def_location,
                        &definition.doc_comments,
                    )?,
                    grammar::ItemDefinitionInner::TypeAlias(ta) => type_alias_definition::build(
                        &self,
                        resolvee_path,
                        ta,
                        def_location,
                        &definition.doc_comments,
                    )?,
                };

                let Some(item) = item else { continue };
                self.type_registry
                    .get_mut(resolvee_path, &definition.location)?
                    .state = ItemState::Resolved(item);
            }

            if to_resolve == self.type_registry.unresolved() {
                // Oh no! We failed to resolve any new types!
                // Bail from the loop.
                return Err(SemanticError::TypeResolutionStalled {
                    unresolved_types: to_resolve.iter().map(|s| s.to_string()).collect(),
                    resolved_types: self
                        .type_registry
                        .resolved()
                        .iter()
                        .map(|s| s.to_string())
                        .collect(),
                });
            }
        }

        // Now that we've finished resolving all of our types, we should be able
        // to resolve our extern values.
        for module in self.modules.values_mut() {
            module.resolve_extern_values(&mut self.type_registry)?;
        }

        // Resolve freestanding functions after types are resolved
        for module in self.modules.values_mut() {
            module.resolve_functions(&self.type_registry)?;
        }

        Ok(ResolvedSemanticState {
            modules: self.modules,
            type_registry: self.type_registry,
        })
    }
}

impl SemanticState {
    pub(super) fn get_module_for_path(
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

    /// Validate that all items referenced in use statements actually exist.
    /// This checks that each path in a use statement refers to either:
    /// - A type in the type registry (whose parent module also exists)
    /// - A module that exists
    fn validate_uses(&self) -> Result<()> {
        for module in self.modules.values() {
            for use_item in module.uses() {
                let grammar::ModuleItem::Use { tree, .. } = use_item else {
                    continue;
                };

                for (path, location) in tree.flatten_with_locations() {
                    // Check if the path is a type in the type registry
                    let is_type = self.type_registry.contains(&path);

                    // Check if the path is a module
                    let is_module = self.modules.contains_key(&path);

                    if is_type || is_module {
                        continue;
                    }

                    // The path doesn't exist directly. Check if the parent module exists
                    // to provide a better error - if parent doesn't exist, the import
                    // definitely can't work.
                    if let Some(parent) = path.parent() {
                        if !self.modules.contains_key(&parent) {
                            return Err(SemanticError::UseItemNotFound { path, location });
                        }
                    }

                    // Parent exists but the item doesn't
                    return Err(SemanticError::UseItemNotFound { path, location });
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct ResolvedSemanticState {
    type_registry: TypeRegistry,
    modules: BTreeMap<ItemPath, Module>,
}

impl ResolvedSemanticState {
    pub fn type_registry(&self) -> &TypeRegistry {
        &self.type_registry
    }

    pub fn modules(&self) -> &BTreeMap<ItemPath, Module> {
        &self.modules
    }
}
