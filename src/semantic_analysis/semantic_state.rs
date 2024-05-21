use std::{collections::HashMap, path::Path};

use crate::{
    grammar::{self, ItemPath},
    parser,
};

use anyhow::Context;

use super::{
    module::Module,
    type_registry::TypeRegistry,
    types::{
        Argument, Attribute, EnumDefinition, Function, ItemCategory, ItemDefinition, ItemState,
        ItemStateResolved, Region, Type, TypeDefinition,
    },
};

pub struct SemanticState {
    pub(crate) modules: HashMap<ItemPath, Module>,
    pub(crate) type_registry: TypeRegistry,
}

impl SemanticState {
    pub fn new(pointer_size: usize) -> Self {
        let mut semantic_state = Self {
            modules: HashMap::new(),
            type_registry: TypeRegistry::new(pointer_size),
        };

        // Insert the empty root module.
        semantic_state
            .modules
            .insert(ItemPath::empty(), Module::default());

        // Insert all of our predefined types.
        let predefined_types = [
            ("void", 0),
            ("bool", 1),
            ("u8", 1),
            ("u16", 2),
            ("u32", 4),
            ("u64", 8),
            ("u128", 16),
            ("i8", 1),
            ("i16", 2),
            ("i32", 4),
            ("i64", 8),
            ("i128", 16),
            ("f32", 4),
            ("f64", 8),
        ];

        for (name, size) in predefined_types {
            let path = ItemPath::from_colon_delimited_str(name);
            semantic_state
                .add_type(ItemDefinition {
                    path,
                    state: ItemState::Resolved(ItemStateResolved {
                        size,
                        inner: TypeDefinition::default().into(),
                    }),
                    category: ItemCategory::Predefined,
                })
                .expect("failed to add predefined type");
        }

        semantic_state
    }

    // todo: define an actual error type
    pub fn add_file(&mut self, base_path: &Path, path: &Path) -> anyhow::Result<()> {
        self.add_module(
            &parser::parse_str(&std::fs::read_to_string(path)?).context(format!("{:?}", path))?,
            &ItemPath::from_path(path.strip_prefix(base_path).unwrap_or(path)),
        )
    }

    pub fn add_module(&mut self, module: &grammar::Module, path: &ItemPath) -> anyhow::Result<()> {
        let extern_values = module
            .extern_values
            .iter()
            .map(|(name, type_, attributes)| {
                let mut address = None;
                for attribute in attributes {
                    let Some((ident, exprs)) = attribute.function() else {
                        anyhow::bail!("unsupported attribute: {attribute:?}");
                    };
                    match (ident.as_str(), &exprs[..]) {
                        ("address", [grammar::Expr::IntLiteral(addr)]) => {
                            address = Some(*addr as usize);
                        }
                        _ => anyhow::bail!("unsupported attribute: {attribute:?}"),
                    }
                }

                let address =
                    address.context("failed to find address attribute for extern value")?;
                Ok((
                    name.as_str().to_owned(),
                    Type::Unresolved(type_.clone()),
                    address,
                ))
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        self.modules.insert(
            path.clone(),
            Module::new(
                path.clone(),
                module.clone(),
                extern_values,
                &module.impls,
                &module.vftables,
            ),
        );

        for definition in &module.definitions {
            let path = path.join(definition.name.as_str().into());
            self.add_type(ItemDefinition {
                path: path.clone(),
                state: ItemState::Unresolved(definition.clone()),
                category: ItemCategory::Defined,
            })?;
        }

        for (extern_path, attributes) in &module.extern_types {
            let mut size = None;
            for attribute in attributes {
                let Some((ident, exprs)) = attribute.function() else {
                    anyhow::bail!("unsupported attribute: {attribute:?}");
                };
                match (ident.as_str(), &exprs[..]) {
                    ("size", [grammar::Expr::IntLiteral(size_)]) => {
                        size = Some(
                            (*size_)
                                .try_into()
                                .context("failed to convert size into usize")?,
                        );
                    }
                    _ => anyhow::bail!("unsupported attribute: {attribute:?}"),
                }
            }
            let size = size.context("failed to find size attribute for extern type")?;

            let extern_path = path.join(extern_path.as_str().into());

            self.add_type(ItemDefinition {
                path: extern_path.clone(),
                state: ItemState::Resolved(ItemStateResolved {
                    size,
                    inner: TypeDefinition::default().into(),
                }),
                category: ItemCategory::Extern,
            })?;
        }

        Ok(())
    }

    pub fn add_type(&mut self, type_definition: ItemDefinition) -> anyhow::Result<()> {
        let parent_path = &type_definition
            .path
            .parent()
            .context("failed to get parent type")?;
        self.modules
            .get_mut(parent_path)
            .context("failed to get module")?
            .definition_paths
            .insert(type_definition.path.clone());
        self.type_registry.add(type_definition);
        Ok(())
    }

    pub fn build(mut self) -> anyhow::Result<ResolvedSemanticState> {
        loop {
            let to_resolve = self.type_registry.unresolved();
            if to_resolve.is_empty() {
                break;
            }

            for resolvee_path in &to_resolve {
                let ItemState::Unresolved(definition) = self
                    .type_registry
                    .get(resolvee_path)
                    .context("failed to get type")?
                    .state
                    .clone()
                else {
                    continue;
                };

                let item = self.build_item(resolvee_path, &definition)?;
                if let Some(item) = item {
                    self.type_registry.get_mut(resolvee_path).unwrap().state =
                        ItemState::Resolved(item);
                }
            }

            if to_resolve == self.type_registry.unresolved() {
                // Oh no! We failed to resolve any new types!
                // Bail from the loop.
                return Err(anyhow::anyhow!(
                    "type resolution will not terminate, failed on types: {:?} (resolved types: {:?})",
                    Vec::from_iter(to_resolve.iter().map(|s| s.to_string())),
                    Vec::from_iter(self.type_registry.resolved().iter().map(|s| s.to_string())),
                ));
            }
        }

        // Now that we've finished resolving all of our types, we should be able
        // to resolve our extern values.
        for module in self.modules.values_mut() {
            module.resolve_extern_values(&mut self.type_registry)?;
        }

        Ok(ResolvedSemanticState {
            modules: self.modules,
            type_registry: self.type_registry,
        })
    }
}

impl SemanticState {
    fn build_function(
        &self,
        scope: &[ItemPath],
        function: &grammar::Function,
    ) -> Result<Function, anyhow::Error> {
        let attributes = function
            .attributes
            .iter()
            .map(|a| match a {
                grammar::Attribute::Function(ident, exprs) => match (ident.as_str(), &exprs[..]) {
                    ("address", [grammar::Expr::IntLiteral(address)]) => {
                        let address = Attribute::Address(*address as usize);
                        Ok(address)
                    }
                    (_, _) => Err(anyhow::anyhow!(
                        "failed to resolve function attribute, unsupported name"
                    )),
                },
            })
            .collect::<Result<Vec<_>, _>>()?;

        let arguments = function
            .arguments
            .iter()
            .map(|a| match a {
                grammar::Argument::ConstSelf => Ok(Argument::ConstSelf),
                grammar::Argument::MutSelf => Ok(Argument::MutSelf),
                grammar::Argument::Field(grammar::TypeField(name, type_)) => Ok(Argument::Field(
                    name.0.clone(),
                    self.type_registry
                        .resolve_grammar_type(scope, type_)
                        .ok_or_else(|| {
                            anyhow::anyhow!(
                                "failed to resolve type of field {:?} ({:?}",
                                name,
                                type_
                            )
                        })?,
                )),
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        let return_type = function
            .return_type
            .as_ref()
            .and_then(|t| self.type_registry.resolve_grammar_type(scope, t));

        Ok(Function {
            name: function.name.0.clone(),
            attributes,
            arguments,
            return_type,
        })
    }

    fn build_item(
        &mut self,
        resolvee_path: &ItemPath,
        definition: &grammar::ItemDefinition,
    ) -> anyhow::Result<Option<ItemStateResolved>> {
        match &definition.inner {
            grammar::ItemDefinitionInner::Type(ty) => self.build_type(resolvee_path, ty),
            grammar::ItemDefinitionInner::Enum(e) => self.build_enum(resolvee_path, e),
        }
    }

    fn build_type(
        &mut self,
        resolvee_path: &ItemPath,
        definition: &grammar::TypeDefinition,
    ) -> anyhow::Result<Option<ItemStateResolved>> {
        let module = self
            .get_module_for_path(resolvee_path)
            .context("failed to get module for path")?;

        let mut target_size: Option<usize> = None;
        let mut singleton = None;
        let mut regions: Vec<(Option<usize>, Region)> = vec![];
        let mut free_functions = vec![];
        let mut vftable_functions = None;
        for statement in &definition.statements {
            let grammar::TypeStatement { field, attributes } = statement;
            let mut address = None;

            for attribute in attributes {
                let Some((ident, exprs)) = attribute.function() else {
                    anyhow::bail!("unsupported attribute: {attribute:?}");
                };
                match (ident.as_str(), &exprs[..]) {
                    ("address", [grammar::Expr::IntLiteral(addr)]) => {
                        address = Some(*addr as usize);
                    }
                    _ => anyhow::bail!("unsupported attribute: {attribute:?}"),
                }
            }

            if let Some(address) = address {
                regions.push((
                    Some(address),
                    Region::unnamed_field(self.type_registry.padding_type(0)),
                ));
            }

            let grammar::TypeField(ident, type_) = field;
            let Some(type_) = self
                .type_registry
                .resolve_grammar_type(&module.scope(), type_)
            else {
                return Ok(None);
            };

            let ident = (ident.0 != "_").then(|| ident.0.clone());
            regions.push((
                None,
                Region {
                    name: ident,
                    type_ref: type_,
                },
            ));
        }

        if let Some(type_impl) = module.impls.get(resolvee_path) {
            for function in type_impl {
                let function = self.build_function(&module.scope(), function)?;
                free_functions.push(function);
            }
        }

        if let Some(vftable_block) = module.vftables.get(resolvee_path) {
            let mut new_vftable_functions = vec![];
            for function in vftable_block {
                let function = self.build_function(&module.scope(), function)?;
                new_vftable_functions.push(function);
            }
            vftable_functions = Some(new_vftable_functions);
        }

        for attribute in &definition.attributes {
            let grammar::Attribute::Function(ident, exprs) = attribute;
            match (ident.as_str(), exprs.as_slice()) {
                ("size", [grammar::Expr::IntLiteral(size)]) => {
                    target_size = Some(*size as usize);
                }
                ("singleton", [grammar::Expr::IntLiteral(value)]) => {
                    singleton = Some(*value as usize);
                }
                _ => anyhow::bail!("unsupported attribute: {attribute:?}"),
            }
        }

        // this resolution algorithm is very simple and doesn't handle overlapping regions
        // or regions that are out of order
        let mut last_address: usize = 0;
        let mut resolved_regions = vec![];

        if let Some(vftable_functions) = &vftable_functions {
            if let Some((type_definition, region, size)) =
                self.build_vftable(resolvee_path, vftable_functions)
            {
                self.add_type(type_definition)?;
                resolved_regions.push(region);
                last_address += size;
            }
        }

        for (offset, region) in regions {
            if let Some(offset) = offset {
                let size = offset - last_address;
                resolved_regions.push(Region::unnamed_field(self.type_registry.padding_type(size)));
                last_address += size;
            }

            let region_size = match region.size(&self.type_registry) {
                Some(size) => size,
                None => return Ok(None),
            };

            if region_size == 0 {
                continue;
            }

            resolved_regions.push(region);
            last_address += region_size;
        }

        if let Some(target_size) = target_size {
            if last_address < target_size {
                resolved_regions.push(Region::unnamed_field(
                    self.type_registry.padding_type(target_size - last_address),
                ));
            }
        }

        let mut size = 0;
        for region in &mut resolved_regions {
            let Some(region_size) = region.size(&self.type_registry) else {
                return Ok(None);
            };

            if let Region {
                name: None,
                type_ref,
            } = region
            {
                *region = Region {
                    name: Some(format!("_field_{size:x}")),
                    type_ref: type_ref.clone(),
                };
            }

            size += region_size;
        }

        for region in &resolved_regions {
            if let Region {
                name: None,
                type_ref,
            } = region
            {
                anyhow::bail!(
                    "unnamed field with type {type_ref} in type definition {resolvee_path}"
                );
            }
        }

        Ok(Some(ItemStateResolved {
            size,
            inner: TypeDefinition {
                regions: resolved_regions,
                free_functions,
                vftable_functions,
                singleton,
            }
            .into(),
        }))
    }

    fn build_enum(
        &mut self,
        resolvee_path: &ItemPath,
        definition: &grammar::EnumDefinition,
    ) -> anyhow::Result<Option<ItemStateResolved>> {
        let module = self
            .get_module_for_path(resolvee_path)
            .context("failed to get module for path")?;

        let Some(ty) = self
            .type_registry
            .resolve_grammar_type(&module.scope(), &definition.type_)
        else {
            return Ok(None);
        };

        // TODO: verify that `ty` actually makes sense for an enum
        let Some(size) = ty.size(&self.type_registry) else {
            return Ok(None);
        };

        let mut singleton = None;
        let mut fields: Vec<(String, isize)> = vec![];
        let mut last_field = 0;
        for statement in &definition.statements {
            let grammar::EnumStatement { name, expr } = statement;
            let value = match expr {
                Some(grammar::Expr::IntLiteral(value)) => *value,
                Some(_) => anyhow::bail!("unsupported enum field value {expr:?}"),
                None => last_field,
            };
            fields.push((name.0.clone(), value));
            last_field = value + 1;
        }

        for attribute in &definition.attributes {
            let grammar::Attribute::Function(ident, exprs) = attribute;
            match (ident.as_str(), exprs.as_slice()) {
                ("singleton", [grammar::Expr::IntLiteral(value)]) => {
                    singleton = Some(*value as usize);
                }
                _ => anyhow::bail!("unsupported attribute: {attribute:?}"),
            }
        }

        Ok(Some(ItemStateResolved {
            size,
            inner: EnumDefinition {
                type_: ty,
                fields,
                singleton,
            }
            .into(),
        }))
    }

    fn build_vftable(
        &self,
        resolvee_path: &ItemPath,
        functions: &[Function],
    ) -> Option<(ItemDefinition, Region, usize)> {
        let name = resolvee_path.last()?;

        let resolvee_vtable_path = resolvee_path
            .parent()?
            .join(format!("{}Vftable", name.as_str()).into());

        let function_to_field = |function: &Function| -> Region {
            let argument_to_type = |argument: &Argument| -> (String, Box<Type>) {
                match argument {
                    Argument::ConstSelf => (
                        "this".to_string(),
                        Box::new(Type::ConstPointer(Box::new(Type::Raw(
                            resolvee_path.clone(),
                        )))),
                    ),
                    Argument::MutSelf => (
                        "this".to_string(),
                        Box::new(Type::MutPointer(Box::new(Type::Raw(resolvee_path.clone())))),
                    ),
                    Argument::Field(name, type_ref) => (name.clone(), Box::new(type_ref.clone())),
                }
            };
            let arguments = function.arguments.iter().map(argument_to_type).collect();
            let return_type = function.return_type.as_ref().map(|t| Box::new(t.clone()));

            Region {
                name: Some(function.name.clone()),
                type_ref: Type::Function(arguments, return_type),
            }
        };

        Some((
            ItemDefinition {
                path: resolvee_vtable_path.clone(),
                state: ItemState::Resolved(ItemStateResolved {
                    size: 0,
                    inner: TypeDefinition {
                        regions: functions.iter().map(function_to_field).collect(),
                        free_functions: vec![],
                        vftable_functions: None,
                        singleton: None,
                    }
                    .into(),
                }),
                category: ItemCategory::Defined,
            },
            Region {
                name: Some("vftable".to_string()),
                type_ref: Type::ConstPointer(Box::new(Type::Raw(resolvee_vtable_path))),
            },
            self.type_registry.pointer_size(),
        ))
    }

    fn get_module_for_path(&self, path: &ItemPath) -> Option<&Module> {
        self.modules.get(&path.parent()?)
    }
}

pub struct ResolvedSemanticState {
    type_registry: TypeRegistry,
    modules: HashMap<ItemPath, Module>,
}

impl ResolvedSemanticState {
    pub fn type_registry(&self) -> &TypeRegistry {
        &self.type_registry
    }

    pub fn modules(&self) -> &HashMap<ItemPath, Module> {
        &self.modules
    }
}
