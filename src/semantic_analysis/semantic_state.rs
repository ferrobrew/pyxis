use std::{collections::HashMap, path::Path};

use anyhow::Context;

use crate::{
    grammar::{self, ItemPath},
    parser,
    semantic_analysis::{
        module::Module,
        type_definition,
        type_registry::TypeRegistry,
        types::{
            EnumDefinition, ExternValue, ItemCategory, ItemDefinition, ItemState,
            ItemStateResolved, Type, TypeDefinition, Visibility,
        },
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
            let path = ItemPath::from(name);
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
                })
                .expect("failed to add predefined type");
        }

        semantic_state
    }

    // todo: define an actual error type
    pub fn add_file(&mut self, base_path: &Path, path: &Path) -> anyhow::Result<()> {
        self.add_module(
            &parser::parse_str(&std::fs::read_to_string(path)?).map_err(|e| {
                let proc_macro2::LineColumn { line, column } = e.span().start();
                anyhow::Error::new(e).context(format!(
                    "failed to parse {}:{}:{}",
                    path.display(),
                    line,
                    column + 1
                ))
            })?,
            &ItemPath::from_path(path.strip_prefix(base_path).unwrap_or(path)),
        )
    }

    pub fn add_module(&mut self, module: &grammar::Module, path: &ItemPath) -> anyhow::Result<()> {
        let extern_values = module
            .extern_values
            .iter()
            .map(|ev| {
                let name = &ev.name;
                let mut address = None;
                for attribute in &ev.attributes {
                    let Some((ident, exprs)) = attribute.function() else {
                        anyhow::bail!(
                            "unsupported attribute for extern value `{}` in module `{}`: {:?}",
                            name,
                            path,
                            attribute
                        );
                    };
                    match (ident.as_str(), &exprs[..]) {
                        ("address", [grammar::Expr::IntLiteral(addr)]) => {
                            address = Some(*addr as usize);
                        }
                        _ => anyhow::bail!(
                            "unsupported attribute for extern value `{}` in module `{}`: {:?}",
                            name,
                            path,
                            attribute
                        ),
                    }
                }

                let address = address.with_context(|| {
                    format!(
                        "failed to find `address` attribute for extern value `{}` in module `{}`",
                        name, path
                    )
                })?;

                Ok(ExternValue {
                    visibility: Visibility::from(ev.visibility),
                    name: name.as_str().to_owned(),
                    type_: Type::Unresolved(ev.type_.clone()),
                    address,
                })
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        self.modules.insert(
            path.clone(),
            Module::new(
                path.clone(),
                module.clone(),
                extern_values,
                &module.impls,
                &module.backends,
            )?,
        );

        for definition in &module.definitions {
            let new_path = path.join(definition.name.as_str().into());
            self.add_item(ItemDefinition {
                visibility: definition.visibility.into(),
                path: new_path,
                state: ItemState::Unresolved(definition.clone()),
                category: ItemCategory::Defined,
            })?;
        }

        for (extern_path, attributes) in &module.extern_types {
            let mut size = None;
            let mut alignment = None;
            for attribute in attributes {
                let Some((ident, exprs)) = attribute.function() else {
                    anyhow::bail!("unsupported attribute for extern type `{extern_path}` in module `{path}`: {attribute:?}");
                };
                match (ident.as_str(), &exprs[..]) {
                    ("size", [grammar::Expr::IntLiteral(size_)]) => {
                        size = Some(
                            (*size_)
                                .try_into()
                                .with_context(|| format!("failed to convert `size` attribute into usize for extern type `{extern_path}` in module `{path}`"))?,
                        );
                    }
                    ("align", [grammar::Expr::IntLiteral(alignment_)]) => {
                        alignment = Some(
                            (*alignment_)
                                .try_into()
                                .with_context(|| format!("failed to convert `align` attribute into usize for extern type `{extern_path}` in module `{path}`"))?,
                        );
                    }
                    _ => anyhow::bail!(
                        "unsupported attribute for extern type `{extern_path}` in module `{path}`: {attribute:?}"
                    ),
                }
            }
            let size = size.with_context(|| {
                format!("failed to find `size` attribute for extern type `{extern_path}` in module `{path}`")
            })?;
            let alignment = alignment.with_context(|| {
                format!("failed to find `align` attribute for extern type `{extern_path}` in module `{path}`")
            })?;

            let extern_path = path.join(extern_path.as_str().into());

            self.add_item(ItemDefinition {
                visibility: Visibility::Public,
                path: extern_path.clone(),
                state: ItemState::Resolved(ItemStateResolved {
                    size,
                    alignment,
                    inner: TypeDefinition::default().into(),
                }),
                category: ItemCategory::Extern,
            })?;
        }

        Ok(())
    }

    pub fn add_item(&mut self, item_definition: ItemDefinition) -> anyhow::Result<()> {
        let parent_path = &item_definition.path.parent().with_context(|| {
            format!(
                "failed to get parent path for type `{}`",
                item_definition.path
            )
        })?;
        self.modules
            .get_mut(parent_path)
            .with_context(|| format!("failed to get module for path `{parent_path}`"))?
            .definition_paths
            .insert(item_definition.path.clone());
        self.type_registry.add(item_definition);
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
                    .with_context(|| format!("failed to get type `{resolvee_path}`"))?
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
    fn build_item(
        &mut self,
        resolvee_path: &ItemPath,
        definition: &grammar::ItemDefinition,
    ) -> anyhow::Result<Option<ItemStateResolved>> {
        let visibility: Visibility = definition.visibility.into();

        match &definition.inner {
            grammar::ItemDefinitionInner::Type(ty) => {
                type_definition::build(self, resolvee_path, visibility, ty)
            }
            grammar::ItemDefinitionInner::Enum(e) => self.build_enum(resolvee_path, e),
        }
    }

    fn build_enum(
        &self,
        resolvee_path: &ItemPath,
        definition: &grammar::EnumDefinition,
    ) -> anyhow::Result<Option<ItemStateResolved>> {
        let module = self
            .get_module_for_path(resolvee_path)
            .with_context(|| format!("failed to get module for path `{resolvee_path}`"))?;

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

        let mut fields: Vec<(String, isize)> = vec![];
        let mut last_field = 0;
        let mut default_index = None;
        for statement in &definition.statements {
            let grammar::EnumStatement {
                name,
                expr,
                attributes,
            } = statement;
            let value = match expr {
                Some(grammar::Expr::IntLiteral(value)) => *value,
                Some(_) => anyhow::bail!(
                    "unsupported enum value for case `{name}` of enum `{resolvee_path}`: {expr:?}"
                ),
                None => last_field,
            };
            fields.push((name.0.clone(), value));

            for attribute in attributes {
                match attribute {
                    grammar::Attribute::Ident(ident) => match ident.as_str() {
                        "default" => {
                            if default_index.is_some() {
                                anyhow::bail!("enum {resolvee_path} has multiple default variants");
                            }
                            default_index = Some(fields.len() - 1);
                        }
                        _ => anyhow::bail!("unsupported attribute for case `{name}` of enum `{resolvee_path}`: {attribute:?}"),
                    },
                    grammar::Attribute::Function(_ident, _exprs) => {
                        anyhow::bail!("unsupported attribute for case `{name}` of enum `{resolvee_path}`: {attribute:?}");
                    }
                }
            }

            last_field = value + 1;
        }

        let mut singleton = None;
        let mut copyable = false;
        let mut cloneable = false;
        let mut defaultable = false;
        for attribute in &definition.attributes {
            match attribute {
                grammar::Attribute::Ident(ident) => match ident.as_str() {
                    "copyable" => {
                        copyable = true;
                        cloneable = true;
                    }
                    "cloneable" => cloneable = true,
                    "defaultable" => defaultable = true,
                    _ => anyhow::bail!(
                        "unsupported attribute for enum `{resolvee_path}`: {attribute:?}"
                    ),
                },
                grammar::Attribute::Function(ident, exprs) => {
                    match (ident.as_str(), exprs.as_slice()) {
                        ("singleton", [grammar::Expr::IntLiteral(value)]) => {
                            singleton = Some(*value as usize);
                        }
                        _ => anyhow::bail!(
                            "unsupported attribute for enum `{resolvee_path}`: {attribute:?}"
                        ),
                    }
                }
            }
        }

        if defaultable && default_index.is_none() {
            anyhow::bail!(
                "enum `{resolvee_path}` is marked as defaultable but has no default variant set"
            );
        }

        if !defaultable && default_index.is_some() {
            anyhow::bail!(
                "enum `{resolvee_path}` has a default variant set but is not marked as defaultable"
            );
        }

        Ok(Some(ItemStateResolved {
            size,
            alignment: ty.alignment(&self.type_registry).with_context(|| {
                format!("failed to get alignment for base type of enum `{resolvee_path}`")
            })?,
            inner: EnumDefinition {
                type_: ty,
                fields,
                singleton,
                copyable,
                cloneable,
                defaultable,
                default_index,
            }
            .into(),
        }))
    }

    pub(super) fn get_module_for_path(&self, path: &ItemPath) -> Option<&Module> {
        self.modules.get(&path.parent()?)
    }
}

#[derive(Debug)]
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
