use std::{collections::BTreeMap, path::Path};

use crate::{
    BuildError,
    grammar::{self, ItemPath},
    parser,
    semantic::{
        attribute, bitflags_definition, enum_definition,
        error::{
            AttributeName, BuildOutcome, ExternKind, Result, SemanticError, UnresolvedTypeReference,
        },
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
                    type_parameters: vec![],
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
                    cfg: None,
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
                    attribute_name: AttributeName::Address,
                    extern_kind: ExternKind::Value,
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

            // Extract type parameter names from the grammar definition
            let type_parameters: Vec<String> = definition
                .type_parameters
                .iter()
                .map(|tp| tp.name.clone())
                .collect();

            // Extract any `#[cfg(...)]` from the definition's attributes.
            let cfg = match &definition.inner {
                grammar::ItemDefinitionInner::Type(td) => td.attributes.cfg(),
                grammar::ItemDefinitionInner::Enum(e) => e.attributes.cfg(),
                grammar::ItemDefinitionInner::Bitflags(b) => b.attributes.cfg(),
                grammar::ItemDefinitionInner::TypeAlias(ta) => ta.attributes.cfg(),
            };

            self.add_item(ItemDefinition {
                visibility: definition.visibility.into(),
                path: new_path,
                type_parameters,
                state: ItemState::Unresolved(definition.clone()),
                category: ItemCategory::Defined,
                predefined: None,
                cfg,
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
                attribute_name: AttributeName::Size,
                extern_kind: ExternKind::Type,
                type_name: extern_name.as_str().into(),
                module_name: path.to_string(),
                location: *extern_location,
            })?;
            let alignment = alignment.ok_or_else(|| SemanticError::MissingExternAttribute {
                attribute_name: AttributeName::Align,
                extern_kind: ExternKind::Type,
                type_name: extern_name.as_str().into(),
                module_name: path.to_string(),
                location: *extern_location,
            })?;

            let extern_path = path.join(extern_name.as_str().into());

            self.add_item(ItemDefinition {
                visibility: Visibility::Public,
                path: extern_path.clone(),
                type_parameters: vec![], // Extern types don't have type parameters (generics are in the name)
                state: ItemState::Resolved(ItemStateResolved {
                    size,
                    alignment,
                    inner: TypeDefinition::default().into(),
                }),
                category: ItemCategory::Extern,
                predefined: None,
                cfg: attributes.cfg(),
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

        // Track unresolved type references across iterations
        let mut unresolved_references: Vec<UnresolvedTypeReference> = vec![];

        loop {
            let to_resolve = self.type_registry.unresolved();
            if to_resolve.is_empty() {
                break;
            }

            // Clear references at start of each iteration - we only care about
            // references that couldn't be resolved in the final iteration
            unresolved_references.clear();

            for resolvee_path in &to_resolve {
                let item_def = self
                    .type_registry
                    .get(resolvee_path, &ItemLocation::internal())?;
                let ItemState::Unresolved(definition) = item_def.state.clone() else {
                    continue;
                };

                let visibility: Visibility = definition.visibility.into();

                let def_location = &definition.location;
                // Extract type parameter names from the definition
                let type_param_names: Vec<String> = definition
                    .type_parameters
                    .iter()
                    .map(|tp| tp.name.clone())
                    .collect();

                let outcome = match &definition.inner {
                    grammar::ItemDefinitionInner::Type(ty) => type_definition::build(
                        &mut self,
                        resolvee_path,
                        visibility,
                        ty,
                        def_location,
                        &definition.doc_comments,
                        &type_param_names,
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
                        &type_param_names,
                    )?,
                };

                match outcome {
                    BuildOutcome::Resolved(item) => {
                        self.type_registry
                            .get_mut(resolvee_path, &definition.location)?
                            .state = ItemState::Resolved(item);
                    }
                    BuildOutcome::Deferred => {
                        // Type exists but not yet resolved - keep trying
                    }
                    BuildOutcome::NotFoundType(unresolved_ref) => {
                        // Track this unresolved reference for better error reporting
                        unresolved_references.push(unresolved_ref);
                    }
                }
            }

            if to_resolve == self.type_registry.unresolved() {
                // Oh no! We failed to resolve any new types!
                // Bail from the loop with collected unresolved references.
                return Err(SemanticError::TypeResolutionStalled {
                    unresolved_types: to_resolve.iter().map(|s| s.to_string()).collect(),
                    resolved_types: self
                        .type_registry
                        .resolved()
                        .iter()
                        .map(|s| s.to_string())
                        .collect(),
                    unresolved_references,
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

        // Resolve associated functions (impl blocks + base inheritance) now
        // that every type is fully resolved. This deferral lets impl methods
        // freely reference their own enclosing type as a return / argument
        // type without trapping the resolver in a defer loop.
        self.resolve_associated_functions()?;

        Ok(ResolvedSemanticState {
            modules: self.modules,
            type_registry: self.type_registry,
        })
    }

    fn resolve_associated_functions(&mut self) -> Result<()> {
        use std::collections::{BTreeMap, BTreeSet};

        // Build a map from each type-with-regions to its `#[base]` parents.
        // We process types in topological order (bases before derived) so
        // that when we copy `base.associated_functions` into a derived type,
        // the base has its own impl methods (and *its* inherited methods)
        // already populated.
        let mut bases_of: BTreeMap<ItemPath, Vec<(String, ItemPath)>> = BTreeMap::new();
        let mut all_type_paths: Vec<ItemPath> = Vec::new();
        for path in self.type_registry.resolved() {
            let item = self
                .type_registry
                .get(&path, &ItemLocation::internal())?
                .clone();
            let Some(resolved) = item.resolved() else {
                continue;
            };
            let crate::semantic::types::ItemDefinitionInner::Type(td) = &resolved.inner else {
                continue;
            };
            all_type_paths.push(path.clone());
            let mut bases = Vec::new();
            for region in &td.regions {
                if !region.is_base {
                    continue;
                }
                let Some(name) = region.name.clone() else {
                    continue;
                };
                if let crate::semantic::types::Type::Raw(base_path) = &region.type_ref {
                    bases.push((name, base_path.clone()));
                }
            }
            bases_of.insert(path, bases);
        }

        // Topologically sort by inheritance: bases before derived.
        let mut order: Vec<ItemPath> = Vec::new();
        let mut visited: BTreeSet<ItemPath> = BTreeSet::new();
        fn visit(
            path: &ItemPath,
            bases_of: &BTreeMap<ItemPath, Vec<(String, ItemPath)>>,
            visited: &mut BTreeSet<ItemPath>,
            order: &mut Vec<ItemPath>,
        ) {
            if !visited.insert(path.clone()) {
                return;
            }
            if let Some(bases) = bases_of.get(path) {
                for (_, base) in bases {
                    visit(base, bases_of, visited, order);
                }
            }
            order.push(path.clone());
        }
        for p in &all_type_paths {
            visit(p, &bases_of, &mut visited, &mut order);
        }

        // Resolve own impl + inherit base methods, in topological order.
        for path in order {
            self.resolve_associated_functions_for(&path)?;
        }

        Ok(())
    }

    fn resolve_associated_functions_for(&mut self, path: &ItemPath) -> Result<()> {
        use std::collections::HashSet;

        use crate::semantic::{
            error::DuplicateDefinitionKind,
            function::{self, FunctionBuildOutcome},
            types::{Function, FunctionBody, ItemDefinitionInner, Type},
        };

        let item = self.type_registry.get(path, &ItemLocation::internal())?;
        let location = item.location;
        let Some(resolved) = item.resolved() else {
            return Ok(());
        };
        let ItemDefinitionInner::Type(td) = &resolved.inner else {
            return Ok(());
        };
        let regions = td.regions.clone();
        let vftable_function_names: HashSet<String> = td
            .vftable
            .as_ref()
            .map(|v| v.functions.iter().map(|f| f.name.clone()).collect())
            .unwrap_or_default();

        let module = self.get_module_for_path(path, &location)?;
        let scope = module.scope();
        // A type can have multiple `impl` blocks — e.g. one per set of
        // method-level type parameters. We iterate them in declaration
        // order so the resolved `associated_functions` reflects the
        // source order, and each function picks up its own impl block's
        // type parameters.
        struct ImplFunc {
            func: grammar::Function,
            impl_type_parameters: Vec<String>,
            /// Cfg from the impl block (if any). Methods conjoin this with
            /// their own per-method cfg.
            impl_cfg: Option<crate::parser::cfg::CfgPredicate>,
        }
        let impl_funcs: Vec<ImplFunc> = module
            .impls
            .get(path)
            .map(|fbs| {
                fbs.iter()
                    .flat_map(|fb| {
                        let params: Vec<String> = fb
                            .type_parameters
                            .iter()
                            .map(|tp| tp.name.clone())
                            .collect();
                        let block_cfg = fb.attributes.cfg();
                        fb.functions().cloned().map({
                            let params = params.clone();
                            let block_cfg = block_cfg.clone();
                            move |f| ImplFunc {
                                func: f,
                                impl_type_parameters: params.clone(),
                                impl_cfg: block_cfg.clone(),
                            }
                        })
                    })
                    .collect()
            })
            .unwrap_or_default();

        let struct_param_count = self
            .type_registry
            .get(path, &location)?
            .type_parameters
            .len();

        let mut associated_functions: Vec<Function> = Vec::new();
        let mut used_names: HashSet<String> = vftable_function_names.clone();

        // Inherit from base regions: copy each base's already-resolved
        // associated_functions and rewrite their bodies to delegate via the
        // base field.
        for (i, region) in regions.iter().filter(|r| r.is_base).enumerate() {
            let Some(name) = region.name.clone() else {
                continue;
            };
            let Type::Raw(base_path) = &region.type_ref else {
                continue;
            };
            let base_item = self.type_registry.get(base_path, &location)?;
            let Some(base_resolved) = base_item.resolved() else {
                continue;
            };
            let ItemDefinitionInner::Type(base_td) = &base_resolved.inner else {
                continue;
            };

            let add_functions = |functions: &[Function],
                                 associated_functions: &mut Vec<Function>,
                                 used_names: &mut HashSet<String>| {
                for function in functions.iter().filter(|f| f.is_public()) {
                    let mut function = function.clone();
                    let original_name = function.name.clone();
                    if used_names.contains(&original_name) {
                        function.name = format!("{name}_{original_name}");
                    }
                    function.body = FunctionBody::Field {
                        field: name.clone(),
                        function_name: original_name,
                    };
                    used_names.insert(function.name.clone());
                    associated_functions.push(function);
                }
            };

            add_functions(
                &base_td.associated_functions,
                &mut associated_functions,
                &mut used_names,
            );

            if i > 0 {
                if let Some(vftable) = &base_td.vftable {
                    add_functions(
                        &vftable.functions,
                        &mut associated_functions,
                        &mut used_names,
                    );
                }
            }
        }

        // Build own impl methods. Now that all types are resolved, function
        // building never defers. Each function picks up its own impl
        // block's type parameters (so different impl blocks can declare
        // different method-level template parameters).
        for ImplFunc {
            func: grammar_fn,
            impl_type_parameters,
            impl_cfg,
        } in &impl_funcs
        {
            if used_names.contains(&grammar_fn.name.0) {
                return Err(SemanticError::DuplicateDefinition {
                    name: grammar_fn.name.0.clone(),
                    item_path: path.clone(),
                    kind: DuplicateDefinitionKind::FunctionInTypeOrBase,
                    location,
                });
            }
            // Method-level extras for this specific function come from its
            // own impl block, not the type's full set of impl blocks.
            let method_extras: Vec<String> = if impl_type_parameters.len() > struct_param_count {
                impl_type_parameters[struct_param_count..].to_vec()
            } else {
                Vec::new()
            };
            let mut function = match function::build(
                &self.type_registry,
                &scope,
                false,
                grammar_fn,
                impl_type_parameters,
            )? {
                FunctionBuildOutcome::Built(f) => *f,
                FunctionBuildOutcome::Deferred => {
                    return Err(SemanticError::TypeResolutionStalled {
                        unresolved_types: vec![grammar_fn.name.0.clone()],
                        resolved_types: vec![],
                        unresolved_references: vec![],
                    });
                }
            };
            function.method_type_parameters = method_extras;
            // Conjoin any block-level cfg with the per-method cfg. Either
            // both, just one, or neither may be present.
            function.cfg = match (impl_cfg.clone(), function.cfg.take()) {
                (None, None) => None,
                (Some(b), None) => Some(b),
                (None, Some(f)) => Some(f),
                (Some(b), Some(f)) => Some(crate::parser::cfg::CfgPredicate::All {
                    predicates: vec![b, f],
                    location,
                }),
            };
            used_names.insert(function.name.clone());
            associated_functions.push(function);
        }

        // Patch the result back into the type definition.
        let item = self.type_registry.get_mut(path, &location)?;
        if let ItemState::Resolved(state) = &mut item.state {
            if let ItemDefinitionInner::Type(td) = &mut state.inner {
                td.associated_functions = associated_functions;
            }
        }

        Ok(())
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
    ///
    /// Additionally, checks that the item is visible from the importing module.
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

                    if is_type {
                        // Check visibility of the type
                        if let Ok(item_def) =
                            self.type_registry.get(&path, &ItemLocation::internal())
                        {
                            if item_def.visibility == Visibility::Private {
                                // Private items are only visible within the same module or child modules
                                if let Some(item_module) = path.parent() {
                                    if !Self::can_access_private(&module.path, &item_module) {
                                        return Err(SemanticError::PrivateItemAccess {
                                            item_path: path,
                                            from_module: module.path.clone(),
                                            location,
                                        });
                                    }
                                }
                            }
                        }
                        continue;
                    }

                    if is_module {
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

    /// Check if a module can access a private item in another module.
    /// In Rust-like visibility rules, private items are visible to:
    /// - The same module
    /// - Child modules (descendants)
    fn can_access_private(from_module: &ItemPath, item_module: &ItemPath) -> bool {
        // Same module can always access
        if from_module == item_module {
            return true;
        }

        // Child modules can access parent's private items
        // Check if from_module starts with item_module (is a descendant)
        from_module.starts_with(item_module)
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
