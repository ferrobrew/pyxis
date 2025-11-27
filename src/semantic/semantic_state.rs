use std::{collections::BTreeMap, path::Path};

use crate::{
    BuildError,
    grammar::{self, ItemPath},
    parser,
    semantic::{
        bitflags_definition, enum_definition,
        error::{IntegerConversionContext, Result, SemanticError},
        module::Module,
        type_definition,
        type_registry::TypeRegistry,
        types::{
            ExternValue, ItemCategory, ItemDefinition, ItemState, ItemStateResolved,
            PredefinedItem, Type, TypeDefinition, Visibility,
        },
    },
    span::ItemLocation,
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
            semantic_state
                .add_item(ItemDefinition {
                    visibility: Visibility::Public,
                    path,
                    state: ItemState::Resolved(ItemStateResolved {
                        size,
                        alignment,
                        inner: TypeDefinition::default()
                            .with_cloneable(true)
                            .with_copyable(true)
                            .with_defaultable(true)
                            .into(),
                    }),
                    category: ItemCategory::Predefined,
                    predefined: Some(*predefined_item),
                    location: ItemLocation::internal(),
                })
                .expect("failed to add predefined type");
        }

        semantic_state
    }

    pub fn add_file(
        &mut self,
        base_path: &Path,
        path: &Path,
    ) -> std::result::Result<(), BuildError> {
        let source = std::fs::read_to_string(path).map_err(|e| BuildError::Io {
            error: e,
            context: format!("reading file {}", path.display()),
        })?;
        let filename = path.display().to_string();

        self.add_module(
            &parser::parse_str_with_filename(&source, &filename)?,
            &ItemPath::from_path(path.strip_prefix(base_path).unwrap_or(path)),
            Some(filename.into()),
        )
        .map_err(Into::into)
    }

    /// Attach span and source information to a semantic error if available
    pub fn enhance_error(&self, error: SemanticError, _item_path: &ItemPath) -> SemanticError {
        // Note: with_span_and_source method was removed as part of refactoring
        // to eliminate source fields from error types
        error
    }

    pub fn add_module(
        &mut self,
        module: &grammar::Module,
        path: &ItemPath,
        _filename: Option<std::sync::Arc<str>>,
    ) -> Result<()> {
        let extern_values = module
            .extern_values()
            .map(|ev| {
                let name = &ev.name;
                let mut address = None;
                for attribute in &ev.attributes {
                    let Some((ident, items)) = attribute.function() else {
                        continue;
                    };
                    let exprs = grammar::AttributeItem::extract_exprs(items);
                    if let ("address", [grammar::Expr::IntLiteral { value, .. }]) =
                        (ident.as_str(), &exprs[..])
                    {
                        address = Some(*value as usize);
                    }
                }

                let address = address.ok_or_else(|| {
                    SemanticError::missing_attribute(
                        "address",
                        "extern value",
                        path.join(name.as_str().into()),
                        ev.location.clone(),
                    )
                })?;

                Ok(ExternValue {
                    visibility: Visibility::from(ev.visibility),
                    name: name.as_str().to_owned(),
                    type_: Type::Unresolved(ev.type_.clone()),
                    address,
                    location: ev.location.clone(),
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

        for definition in module.definitions().collect::<Vec<_>>() {
            let new_path = path.join(definition.name.as_str().into());

            self.add_item(ItemDefinition {
                visibility: definition.visibility.into(),
                path: new_path,
                state: ItemState::Unresolved(definition.clone()),
                category: ItemCategory::Defined,
                predefined: None,
                location: definition.location.clone(),
            })?;
        }

        for (extern_name, attributes, extern_location) in module.extern_types().collect::<Vec<_>>()
        {
            let mut size = None;
            let mut alignment = None;
            for attribute in attributes {
                let Some((ident, items)) = attribute.function() else {
                    continue;
                };
                let exprs = grammar::AttributeItem::extract_exprs(items);
                match (ident.as_str(), &exprs[..]) {
                    ("size", [grammar::Expr::IntLiteral { value, .. }]) => {
                        size = Some((*value).try_into().map_err(|_| {
                            SemanticError::integer_conversion(
                                value.to_string(),
                                "usize",
                                IntegerConversionContext::ExternSizeAttribute {
                                    type_name: extern_name.to_string(),
                                    module_name: path.to_string(),
                                },
                                extern_location.clone(),
                            )
                        })?);
                    }
                    ("align", [grammar::Expr::IntLiteral { value, .. }]) => {
                        alignment = Some((*value).try_into().map_err(|_| {
                            SemanticError::integer_conversion(
                                value.to_string(),
                                "usize",
                                IntegerConversionContext::ExternAlignAttribute {
                                    type_name: extern_name.to_string(),
                                    module_name: path.to_string(),
                                },
                                extern_location.clone(),
                            )
                        })?);
                    }
                    _ => {}
                }
            }
            let size = size.ok_or_else(|| {
                SemanticError::missing_extern_attribute(
                    "size",
                    "extern type",
                    extern_name.as_str(),
                    path.to_string(),
                    extern_location.clone(),
                )
            })?;
            let alignment = alignment.ok_or_else(|| {
                SemanticError::missing_extern_attribute(
                    "align",
                    "extern type",
                    extern_name.as_str(),
                    path.to_string(),
                    extern_location.clone(),
                )
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
                location: extern_location.clone(),
            })?;
        }

        Ok(())
    }

    pub fn add_item(&mut self, item_definition: ItemDefinition) -> Result<()> {
        let parent_path = &item_definition.path.parent().ok_or_else(|| {
            SemanticError::module_not_found(
                item_definition.path.clone(),
                item_definition.location.clone(),
            )
        })?;
        self.modules
            .get_mut(parent_path)
            .ok_or_else(|| {
                SemanticError::module_not_found(
                    parent_path.clone(),
                    item_definition.location.clone(),
                )
            })?
            .definition_paths
            .insert(item_definition.path.clone());
        self.type_registry.add(item_definition);
        Ok(())
    }

    pub fn build(mut self) -> Result<ResolvedSemanticState> {
        loop {
            let to_resolve = self.type_registry.unresolved();
            if to_resolve.is_empty() {
                break;
            }

            for resolvee_path in &to_resolve {
                let item_def = self.type_registry.get(resolvee_path).ok_or_else(|| {
                    SemanticError::type_not_found(resolvee_path.clone(), ItemLocation::internal())
                })?;
                let item_location = item_def.location.clone();
                let ItemState::Unresolved(definition) = item_def.state.clone() else {
                    continue;
                };

                let visibility: Visibility = definition.visibility.into();

                let item = match &definition.inner {
                    grammar::ItemDefinitionInner::Type(ty) => type_definition::build(
                        &mut self,
                        resolvee_path,
                        visibility,
                        ty,
                        &definition.doc_comments,
                        definition.location.clone(),
                    )?,
                    grammar::ItemDefinitionInner::Enum(e) => enum_definition::build(
                        &self,
                        resolvee_path,
                        e,
                        &definition.doc_comments,
                        item_location.clone(),
                    )?,
                    grammar::ItemDefinitionInner::Bitflags(b) => bitflags_definition::build(
                        &self,
                        resolvee_path,
                        b,
                        &definition.doc_comments,
                        item_location.clone(),
                    )?,
                };

                let Some(item) = item else { continue };
                self.type_registry.get_mut(resolvee_path).unwrap().state =
                    ItemState::Resolved(item);
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
    pub(super) fn get_module_for_path(&self, path: &ItemPath) -> Option<&Module> {
        self.modules.get(&path.parent()?)
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
