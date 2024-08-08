use std::{collections::HashMap, path::Path};

use crate::{
    grammar::{self, ItemPath},
    parser, util,
};

use anyhow::Context;

use super::{
    module::Module,
    type_registry::TypeRegistry,
    types::{
        Argument, CallingConvention, EnumDefinition, ExternValue, Function, ItemCategory,
        ItemDefinition, ItemState, ItemStateResolved, Region, Type, TypeDefinition, TypeVftable,
        Visibility,
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
                .add_type(ItemDefinition {
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
            self.add_type(ItemDefinition {
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

            self.add_type(ItemDefinition {
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

    pub fn add_type(&mut self, type_definition: ItemDefinition) -> anyhow::Result<()> {
        let parent_path = &type_definition.path.parent().with_context(|| {
            format!(
                "failed to get parent path for type `{}`",
                type_definition.path
            )
        })?;
        self.modules
            .get_mut(parent_path)
            .with_context(|| format!("failed to get module for path `{parent_path}`"))?
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
    fn build_function(
        &self,
        scope: &[ItemPath],
        function: &grammar::Function,
    ) -> Result<Function, anyhow::Error> {
        let mut address = None;
        let mut calling_convention = None;
        for attribute in &function.attributes {
            let Some((ident, exprs)) = attribute.function() else {
                anyhow::bail!(
                    "unsupported attribute for function `{}`: {attribute:?}",
                    function.name
                );
            };
            match (ident.as_str(), &exprs[..]) {
                ("address", [grammar::Expr::IntLiteral(addr)]) => {
                    address = Some((*addr).try_into().with_context(|| {
                        format!(
                            "failed to convert `address` attribute into usize for function `{}`",
                            function.name
                        )
                    })?);
                }
                ("index", _) => {
                    // ignore index attribute, this is handled by vftable construction
                }
                ("calling_convention", [grammar::Expr::StringLiteral(cc)]) => {
                    calling_convention = Some(cc.parse().map_err(|_| {
                        anyhow::anyhow!(
                            "invalid calling convention for function `{}`: {cc}",
                            function.name
                        )
                    })?);
                }
                _ => anyhow::bail!(
                    "unsupported attribute for function `{}`: {attribute:?}",
                    function.name
                ),
            }
        }

        let arguments = function
            .arguments
            .iter()
            .map(|a| match a {
                grammar::Argument::ConstSelf => Ok(Argument::ConstSelf),
                grammar::Argument::MutSelf => Ok(Argument::MutSelf),
                grammar::Argument::Named(name, type_) => Ok(Argument::Field(
                    name.0.clone(),
                    self.type_registry
                        .resolve_grammar_type(scope, type_)
                        .ok_or_else(|| {
                            anyhow::anyhow!(
                                "failed to resolve type of field `{:?}` ({:?})",
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

        let calling_convention = calling_convention.unwrap_or_else(|| {
            // Assume that if the function has a self argument, it's a thiscall function, otherwise it's "system"
            // for interoperating with system libraries: <https://doc.rust-lang.org/nomicon/ffi.html#foreign-calling-conventions>
            // Bit sus honestly, maybe we should enforce a calling convention for all non-self functions?
            let has_self = arguments
                .iter()
                .any(|a| matches!(a, Argument::ConstSelf | Argument::MutSelf));
            if has_self {
                CallingConvention::Thiscall
            } else {
                CallingConvention::System
            }
        });

        Ok(Function {
            visibility: function.visibility.into(),
            name: function.name.0.clone(),
            address,
            arguments,
            return_type,
            calling_convention,
        })
    }

    fn build_item(
        &mut self,
        resolvee_path: &ItemPath,
        definition: &grammar::ItemDefinition,
    ) -> anyhow::Result<Option<ItemStateResolved>> {
        let visibility: Visibility = definition.visibility.into();

        match &definition.inner {
            grammar::ItemDefinitionInner::Type(ty) => {
                self.build_type(resolvee_path, visibility, ty)
            }
            grammar::ItemDefinitionInner::Enum(e) => self.build_enum(resolvee_path, e),
        }
    }

    fn build_type(
        &mut self,
        resolvee_path: &ItemPath,
        visibility: Visibility,
        definition: &grammar::TypeDefinition,
    ) -> anyhow::Result<Option<ItemStateResolved>> {
        let module = self
            .get_module_for_path(resolvee_path)
            .with_context(|| format!("failed to get module for path `{resolvee_path}`"))?;
        // Handle attributes
        let mut target_size: Option<usize> = None;
        let mut singleton = None;
        let mut copyable = false;
        let mut cloneable = false;
        let mut defaultable = false;
        let mut packed = false;
        let mut align = None;
        for attribute in &definition.attributes {
            if let grammar::Attribute::Function(ident, exprs) = attribute {
                match (ident.as_str(), exprs.as_slice()) {
                    ("size", [grammar::Expr::IntLiteral(value)]) => {
                        target_size = Some(
                            (*value)
                                .try_into()
                                .with_context(|| format!("failed to convert `size` attribute into usize for type `{resolvee_path}`"))?,
                        );
                    }
                    ("singleton", [grammar::Expr::IntLiteral(value)]) => {
                        singleton = Some((*value).try_into().with_context(|| {
                            format!(
                                "failed to convert `singleton` attribute into usize for type `{resolvee_path}`"
                            )
                        })?);
                    }
                    ("align", [grammar::Expr::IntLiteral(value)]) => {
                        align = Some((*value).try_into().with_context(|| {
                            format!("failed to convert `align` attribute into usize for type `{resolvee_path}`")
                        })?);
                    }
                    _ => anyhow::bail!(
                        "unsupported attribute for type `{resolvee_path}`: {attribute:?}"
                    ),
                }
            } else if let grammar::Attribute::Ident(ident) = attribute {
                match ident.as_str() {
                    "copyable" => {
                        copyable = true;
                        cloneable = true;
                    }
                    "cloneable" => cloneable = true,
                    "defaultable" => defaultable = true,
                    "packed" => packed = true,
                    _ => anyhow::bail!(
                        "unsupported attribute for type `{resolvee_path}`: {attribute:?}"
                    ),
                }
            }
        }

        // Handle functions
        let mut free_functions = vec![];
        if let Some(type_impl) = module.impls.get(resolvee_path) {
            for function in &type_impl.functions {
                let function = self
                    .build_function(&module.scope(), function)
                    .with_context(|| {
                        format!(
                            "while building impl function `{}` for type `{resolvee_path}`",
                            function.name
                        )
                    })?;
                free_functions.push(function);
            }
        }

        // Handle fields
        let mut pending_regions: Vec<(Option<usize>, Region)> = vec![];
        let mut vftable_functions = None;
        for (idx, statement) in definition.statements.iter().enumerate() {
            let grammar::TypeStatement { field, attributes } = statement;

            match field {
                grammar::TypeField::Field(visibility, ident, type_) => {
                    // Extract address attribute
                    let mut address: Option<usize> = None;
                    let mut _is_base = false;
                    for attribute in attributes {
                        match attribute {
                            grammar::Attribute::Ident(ident) => match ident.as_str() {
                                "base" => _is_base = true,
                                _ => anyhow::bail!(
                                    "unsupported attribute for type `{resolvee_path}`: {attribute:?}"
                                ),
                            },
                            grammar::Attribute::Function(ident, exprs) => {
                                match (ident.as_str(), &exprs[..]) {
                                    ("address", [grammar::Expr::IntLiteral(addr)]) => {
                                        address = Some(
                                            (*addr)
                                                .try_into()
                                                .with_context(|| format!("failed to convert `address` attribute into usize for field `{ident}` of type `{resolvee_path}`"))?,
                                        );
                                    }
                                    _ => anyhow::bail!("unsupported attribute for type `{resolvee_path}`: {attribute:?}"),
                                }
                            }
                        }
                    }

                    // Handle address
                    if let Some(address) = address {
                        pending_regions.push((
                            Some(address),
                            Region::unnamed_field(self.type_registry.padding_type(0)),
                        ));
                    }

                    // Push field
                    let Some(type_) = self
                        .type_registry
                        .resolve_grammar_type(&module.scope(), type_)
                    else {
                        return Ok(None);
                    };

                    let ident = (ident.0 != "_").then(|| ident.0.clone());
                    pending_regions.push((
                        None,
                        Region {
                            visibility: (*visibility).into(),
                            name: ident,
                            type_ref: type_,
                        },
                    ));
                }
                grammar::TypeField::Vftable(functions) => {
                    // the vftable field is a sentinel field used to ensure that the user has
                    // thought about the presence of vftables in their type. we do not actually
                    // count it as a region; the type will be generated with a vftable field later on
                    if idx != 0 {
                        anyhow::bail!(
                            "vftable field of type `{resolvee_path}` must be the first field"
                        );
                    }

                    // Extract size attribute
                    let mut size = None;
                    for attribute in attributes {
                        let grammar::Attribute::Function(ident, exprs) = attribute else {
                            anyhow::bail!(
                                "unsupported attribute for type `{resolvee_path}`: {attribute:?}"
                            );
                        };
                        match (ident.as_str(), exprs.as_slice()) {
                            ("size", [grammar::Expr::IntLiteral(size_)]) => {
                                size = Some(*size_ as usize);
                            }
                            _ => anyhow::bail!(
                                "unsupported attribute for type `{resolvee_path}`: {attribute:?}"
                            ),
                        }
                    }

                    vftable_functions = Some(
                        self.build_vftable_list(module, size, functions)
                            .with_context(|| {
                                format!("while building vftable for type `{resolvee_path}`")
                            })?,
                    );
                }
            }
        }

        let Some((regions, vftable, size)) = self
            .resolve_regions(
                resolvee_path,
                visibility,
                target_size,
                pending_regions,
                vftable_functions,
            )
            .with_context(|| format!("while processing `{resolvee_path}`"))?
        else {
            return Ok(None);
        };

        // Iterate over all of the regions and ensure their types are defaultable if
        // we have our defaultable attribute set.
        if defaultable {
            for region in &regions {
                let Region {
                    visibility: _,
                    name,
                    type_ref,
                } = region;
                let name = name.as_deref().unwrap_or("unnamed");
                fn get_defaultable_type_path(type_ref: &Type) -> Option<&ItemPath> {
                    match type_ref {
                        Type::Raw(tp) => Some(tp),
                        Type::Array(t, _) => get_defaultable_type_path(t),
                        _ => None,
                    }
                }
                let Some(path) = get_defaultable_type_path(type_ref) else {
                    anyhow::bail!("field `{name}` of type `{resolvee_path}` is not a defaultable type (pointer or function?)");
                };

                let item = self
                    .type_registry
                    .get(path)
                    .with_context(|| {
                        format!(
                            "failed to get type `{path}` for field `{name}` of type `{resolvee_path}`"
                        )
                    })?
                    .state
                    .clone();

                let ItemState::Resolved(ItemStateResolved { inner, .. }) = &item else {
                    continue;
                };

                if !inner.defaultable() {
                    anyhow::bail!(
                        "field `{name}` of type `{resolvee_path}` is not a defaultable type"
                    );
                }
            }
        }

        let alignment = if packed {
            if align.is_some() {
                anyhow::bail!(
                    "cannot specify both `packed` and `align` attributes for type `{resolvee_path}`"
                );
            }

            1
        } else {
            // Determine the final requested alignment.
            // The requested alignment, the alignment of a single-region type, or the pointer size.
            let alignment = align
                .or((regions.len() == 1)
                    .then(|| regions[0].type_ref.alignment(&self.type_registry))
                    .flatten())
                .unwrap_or(self.type_registry.pointer_size());

            // Calculate the minimum required alignment.
            let required_alignment = util::lcm(
                regions
                    .iter()
                    .flat_map(|r| r.type_ref.alignment(&self.type_registry)),
            );

            // Ensure that the alignment is at least the minimum required alignment.
            if required_alignment > alignment {
                anyhow::bail!(
                    "alignment {alignment} is less than minimum required alignment {required_alignment} for type `{resolvee_path}`"
                );
            }

            // Ensure that all fields are aligned.
            {
                let mut last_address = 0;
                for region in &regions {
                    let name = &region.name.as_deref().unwrap_or("unnamed");
                    let alignment = region.type_ref.alignment(&self.type_registry).unwrap();
                    if last_address % alignment != 0 {
                        anyhow::bail!(
                            "field `{name}` of type `{resolvee_path}` is located at 0x{last_address:X}, which is not divisible by {alignment} (the alignment of the type of the field)"
                        );
                    }
                    last_address += region.size(&self.type_registry).unwrap();
                }
            }

            // Ensure that the size is a multiple of the alignment.
            if size % alignment != 0 {
                anyhow::bail!("the type `{resolvee_path}` has a size of {size}, which is not a multiple of its alignment {alignment}");
            }

            alignment
        };

        Ok(Some(ItemStateResolved {
            size,
            alignment,
            inner: TypeDefinition {
                regions,
                free_functions,
                vftable,
                singleton,
                copyable,
                cloneable,
                defaultable,
                packed,
            }
            .into(),
        }))
    }

    fn build_vftable_list(
        &self,
        module: &Module,
        size: Option<usize>,
        functions: &[grammar::Function],
    ) -> anyhow::Result<Vec<Function>> {
        // Insert function, with padding if necessary
        let mut output = vec![];
        for function in functions {
            let mut index = None;
            for attribute in &function.attributes {
                let grammar::Attribute::Function(ident, exprs) = attribute else {
                    anyhow::bail!(
                        "unsupported attribute for function `{}`: {attribute:?}",
                        function.name
                    );
                };
                match (ident.as_str(), exprs.as_slice()) {
                    ("index", [grammar::Expr::IntLiteral(index_)]) => {
                        index = Some(*index_ as usize);
                    }
                    ("calling_convention", _) => {
                        // ignore calling convention attribute, handled by build_function
                    }
                    _ => anyhow::bail!(
                        "unsupported attribute for function `{}`: {attribute:?}",
                        function.name
                    ),
                }
            }

            if let Some(index) = index {
                make_padding_functions(&mut output, index);
            }
            let function = self
                .build_function(&module.scope(), function)
                .with_context(|| format!("while building vftable function `{}`", function.name))?;
            output.push(function);
        }

        // Pad out to target size
        if let Some(size) = size {
            make_padding_functions(&mut output, size);
        }

        fn make_padding_functions(output: &mut Vec<Function>, target_len: usize) {
            let functions_to_add = target_len.saturating_sub(output.len());
            for _ in 0..functions_to_add {
                output.push(Function {
                    visibility: Visibility::Private,
                    name: format!("_vfunc_{}", output.len()),
                    arguments: vec![Argument::MutSelf],
                    return_type: None,
                    address: None,
                    calling_convention: CallingConvention::Thiscall,
                });
            }
        }

        Ok(output)
    }

    fn resolve_regions(
        &mut self,
        resolvee_path: &ItemPath,
        visibility: Visibility,
        target_size: Option<usize>,
        regions: Vec<(Option<usize>, Region)>,
        vftable_functions: Option<Vec<Function>>,
    ) -> anyhow::Result<Option<(Vec<Region>, Option<TypeVftable>, usize)>> {
        // this resolution algorithm is very simple and doesn't handle overlapping regions
        // or regions that are out of order
        #[derive(Default)]
        struct Regions {
            regions: Vec<Region>,
            last_address: usize,
        }
        impl Regions {
            fn push(&mut self, type_registry: &TypeRegistry, region: Region) -> Option<()> {
                let size = region.size(type_registry)?;
                if size == 0 && region.type_ref.is_array() {
                    // zero-sized regions that are arrays are ignored
                    return Some(());
                }

                self.regions.push(region);
                self.last_address += size;
                Some(())
            }
        }
        let mut resolved = Regions::default();

        // Create vftable
        let mut vftable = None;
        if let Some(vftable_functions) = vftable_functions {
            if let Some(vftable_type) = build_vftable_item(
                &self.type_registry,
                resolvee_path,
                visibility,
                &vftable_functions,
            ) {
                let vftable_path = vftable_type.path.clone();
                let vftable_pointer_type = Type::ConstPointer(Box::new(Type::Raw(vftable_path)));
                self.add_type(vftable_type)?;

                let region = Region {
                    visibility: Visibility::Private,
                    name: Some("vftable".to_string()),
                    type_ref: vftable_pointer_type.clone(),
                };
                if resolved.push(&self.type_registry, region).is_none() {
                    return Ok(None);
                }

                vftable = Some(TypeVftable {
                    functions: vftable_functions,
                    field_path: vec!["vftable".to_string()],
                    type_: vftable_pointer_type,
                });
            }
        }

        // Insert each region, including padding if necessary
        for (offset, region) in regions {
            if let Some(offset) = offset {
                let Some(size) = offset.checked_sub(resolved.last_address) else {
                    let existing_region = resolved
                        .regions
                        .last()
                        .unwrap()
                        .name
                        .as_deref()
                        .unwrap_or_default();
                    anyhow::bail!(
                        "attempted to insert padding at 0x{offset:X}, but overlapped with existing region `{existing_region}` that ends at 0x{:X}", resolved.last_address
                    );
                };
                let padding_region = Region::unnamed_field(self.type_registry.padding_type(size));
                if resolved.push(&self.type_registry, padding_region).is_none() {
                    return Ok(None);
                }
            }

            if resolved.push(&self.type_registry, region).is_none() {
                return Ok(None);
            }
        }

        // Pad out to target size
        if let Some(target_size) = target_size {
            if resolved.last_address < target_size {
                let padding_region = Region::unnamed_field(
                    self.type_registry
                        .padding_type(target_size - resolved.last_address),
                );
                if resolved.push(&self.type_registry, padding_region).is_none() {
                    return Ok(None);
                }
            }
        }

        // Find total size, and ensure that all regions have names
        let mut size = 0;
        for region in &mut resolved.regions {
            let Some(region_size) = region.size(&self.type_registry) else {
                return Ok(None);
            };

            if let Region {
                visibility: _,
                name: None,
                type_ref,
            } = region
            {
                *region = Region {
                    visibility: Visibility::Private,
                    name: Some(format!("_field_{size:x}")),
                    type_ref: type_ref.clone(),
                };
            }

            size += region_size;
        }

        Ok(Some((resolved.regions, vftable, size)))
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

    fn get_module_for_path(&self, path: &ItemPath) -> Option<&Module> {
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

fn build_vftable_item(
    type_registry: &TypeRegistry,
    resolvee_path: &ItemPath,
    visibility: Visibility,
    functions: &[Function],
) -> Option<ItemDefinition> {
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
            visibility: function.visibility,
            name: Some(function.name.clone()),
            type_ref: Type::Function(function.calling_convention, arguments, return_type),
        }
    };

    let regions: Vec<_> = functions.iter().map(function_to_field).collect();
    let alignment = type_registry.pointer_size();
    Some(ItemDefinition {
        visibility,
        path: resolvee_vtable_path.clone(),
        state: ItemState::Resolved(ItemStateResolved {
            size: regions.iter().map(|r| r.size(type_registry).unwrap()).sum(),
            alignment,
            inner: TypeDefinition {
                regions,
                free_functions: vec![],
                vftable: None,
                singleton: None,
                cloneable: false,
                copyable: false,
                defaultable: false,
                packed: false,
            }
            .into(),
        }),
        category: ItemCategory::Defined,
    })
}
