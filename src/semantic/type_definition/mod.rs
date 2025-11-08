use std::collections::HashSet;

use crate::{
    grammar::{self, ItemPath},
    semantic::{
        SemanticState, function,
        type_registry::TypeRegistry,
        types::{Function, FunctionBody, ItemState, ItemStateResolved, Type, Visibility},
    },
    util,
};

mod vftable;
use anyhow::Context;
pub use vftable::TypeVftable;

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Region {
    pub visibility: Visibility,
    pub name: Option<String>,
    pub doc: Option<String>,
    pub type_ref: Type,
    pub is_base: bool,
}
impl Region {
    pub fn field((visibility, name): (Visibility, impl Into<String>), type_ref: Type) -> Self {
        Region {
            visibility,
            name: Some(name.into()),
            doc: None,
            type_ref,
            is_base: false,
        }
    }
    pub fn unnamed_field(type_ref: Type) -> Self {
        Region {
            visibility: Visibility::Private,
            name: None,
            doc: None,
            type_ref,
            is_base: false,
        }
    }
    pub fn marked_as_base(mut self) -> Self {
        self.is_base = true;
        self
    }
    pub fn with_doc(mut self, doc: impl Into<String>) -> Self {
        self.doc = Some(doc.into());
        self
    }
    pub fn size(&self, type_registry: &TypeRegistry) -> Option<usize> {
        self.type_ref.size(type_registry)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Default, Hash)]
pub struct TypeDefinition {
    pub regions: Vec<Region>,
    pub doc: Option<String>,
    pub associated_functions: Vec<Function>,
    pub vftable: Option<TypeVftable>,
    pub singleton: Option<usize>,
    pub copyable: bool,
    pub cloneable: bool,
    pub defaultable: bool,
    pub packed: bool,
}
impl TypeDefinition {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn with_regions(mut self, regions: impl Into<Vec<Region>>) -> Self {
        self.regions = regions.into();
        self
    }
    pub fn with_doc(mut self, doc: impl Into<String>) -> Self {
        self.doc = Some(doc.into());
        self
    }
    pub fn with_associated_functions(
        mut self,
        associated_functions: impl Into<Vec<Function>>,
    ) -> Self {
        self.associated_functions = associated_functions.into();
        self
    }
    pub fn with_vftable(mut self, vftable: TypeVftable) -> Self {
        self.vftable = Some(vftable);
        self
    }
    pub fn with_singleton(mut self, singleton: usize) -> Self {
        self.singleton = Some(singleton);
        self
    }
    pub fn with_copyable(mut self, copyable: bool) -> Self {
        self.copyable = copyable;
        self
    }
    pub fn with_cloneable(mut self, cloneable: bool) -> Self {
        self.cloneable = cloneable;
        self
    }
    pub fn with_defaultable(mut self, defaultable: bool) -> Self {
        self.defaultable = defaultable;
        self
    }
    pub fn with_packed(mut self, packed: bool) -> Self {
        self.packed = packed;
        self
    }
    /// Returns the fields and types of everything in this type's hierarchy, starting from the top
    pub fn dfs_hierarchy(
        &self,
        type_registry: &TypeRegistry,
        type_path: &ItemPath,
        fields: &[&str],
    ) -> anyhow::Result<Vec<(Vec<String>, Type)>> {
        let mut output = vec![];
        for region in &self.regions {
            if !region.is_base {
                continue;
            }

            let Some((field_name, type_definition)) =
                get_region_name_and_type_definition(type_registry, type_path, region)?
            else {
                continue;
            };
            let field_path = fields
                .iter()
                .copied()
                .chain(Some(field_name.as_str()))
                .collect::<Vec<_>>();
            output.push((
                field_path.iter().map(|s| s.to_string()).collect(),
                region.type_ref.clone(),
            ));
            output.extend(type_definition.dfs_hierarchy(type_registry, type_path, &field_path)?);
        }

        Ok(output)
    }
    pub fn doc(&self) -> Option<&str> {
        self.doc.as_deref()
    }
}

pub fn build(
    semantic: &mut SemanticState,
    resolvee_path: &ItemPath,
    visibility: Visibility,
    definition: &grammar::TypeDefinition,
) -> anyhow::Result<Option<ItemStateResolved>> {
    let module = semantic
        .get_module_for_path(resolvee_path)
        .with_context(|| format!("failed to get module for path `{resolvee_path}`"))?;

    // Handle attributes
    let mut target_size: Option<usize> = None;
    let mut min_size: Option<usize> = None;
    let mut singleton = None;
    let mut copyable = false;
    let mut cloneable = false;
    let mut defaultable = false;
    let mut packed = false;
    let mut align = None;
    let doc = definition.attributes.doc(resolvee_path)?;
    for attribute in &definition.attributes {
        match &attribute.node {
            grammar::Attribute::Function(ident, exprs) => {
                match (ident.as_str(), exprs.as_slice()) {
                    ("size", [expr]) if matches!(expr.node, grammar::Expr::IntLiteral(_)) => {
                        if let grammar::Expr::IntLiteral(value) = expr.node {
                            target_size = Some(
                                value
                                    .try_into()
                                    .with_context(|| format!("failed to convert `size` attribute into usize for type `{resolvee_path}`"))?,
                            );
                        }
                    }
                    ("min_size", [expr]) if matches!(expr.node, grammar::Expr::IntLiteral(_)) => {
                        if let grammar::Expr::IntLiteral(value) = expr.node {
                            min_size = Some(
                                value
                                    .try_into()
                                    .with_context(|| format!("failed to convert `min_size` attribute into usize for type `{resolvee_path}`"))?,
                            );
                        }
                    }
                    ("singleton", [expr]) if matches!(expr.node, grammar::Expr::IntLiteral(_)) => {
                        if let grammar::Expr::IntLiteral(value) = expr.node {
                            singleton = Some(value.try_into().with_context(|| {
                                format!(
                                    "failed to convert `singleton` attribute into usize for type `{resolvee_path}`"
                                )
                            })?);
                        }
                    }
                    ("align", [expr]) if matches!(expr.node, grammar::Expr::IntLiteral(_)) => {
                        if let grammar::Expr::IntLiteral(value) = expr.node {
                            align = Some(value.try_into().with_context(|| {
                                format!("failed to convert `align` attribute into usize for type `{resolvee_path}`")
                            })?);
                        }
                    }
                    _ => {}
                }
            }
            grammar::Attribute::Ident(ident) => match ident.as_str() {
                "copyable" => {
                    copyable = true;
                    cloneable = true;
                }
                "cloneable" => cloneable = true,
                "defaultable" => defaultable = true,
                "packed" => packed = true,
                _ => {}
            },
            grammar::Attribute::Assign(_, _) => {}
        }
    }

    // Ensure size and min_size are mutually exclusive
    if target_size.is_some() && min_size.is_some() {
        anyhow::bail!(
            "cannot specify both `size` and `min_size` attributes for type `{resolvee_path}`"
        );
    }

    // Handle fields
    let mut pending_regions: Vec<(Option<usize>, Region)> = vec![];
    let mut vftable_functions = None;
    for (idx, statement) in definition.statements.iter().enumerate() {
        let grammar::TypeStatement {
            field,
            attributes,
            doc_comments: _,
        } = &statement.node;

        match field {
            grammar::TypeField::Field(visibility, ident, type_) => {
                // Extract address attribute
                let mut address: Option<usize> = None;
                let mut is_base = false;
                let doc: Option<String> = attributes.doc(resolvee_path)?;
                for attribute in attributes {
                    match &attribute.node {
                        grammar::Attribute::Ident(ident) if ident.as_str() == "base" => {
                            is_base = true
                        }
                        grammar::Attribute::Function(ident, exprs) => {
                            if let ("address", [expr]) = (ident.as_str(), &exprs[..])
                                && let grammar::Expr::IntLiteral(addr) = expr.node {
                                    address = Some(
                                        addr.try_into()
                                            .with_context(|| format!("failed to convert `address` attribute into usize for field `{ident}` of type `{resolvee_path}`"))?,
                                    );
                                }
                        }
                        _ => {}
                    }
                }

                // Push field
                let Some(type_) = semantic
                    .type_registry
                    .resolve_grammar_type(&module.scope(), type_)
                else {
                    return Ok(None);
                };

                let ident = (ident.0 != "_").then(|| ident.0.clone());
                pending_regions.push((
                    address,
                    Region {
                        visibility: (*visibility).into(),
                        name: ident,
                        doc,
                        type_ref: type_,
                        is_base,
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
                    let grammar::Attribute::Function(ident, exprs) = &attribute.node else {
                        continue;
                    };
                    if let ("size", [expr]) = (ident.as_str(), exprs.as_slice())
                        && let grammar::Expr::IntLiteral(size_) = expr.node {
                            size = Some(size_ as usize);
                        }
                }

                vftable_functions = Some(
                    vftable::convert_grammar_functions_to_semantic_functions(
                        &semantic.type_registry,
                        module,
                        size,
                        functions,
                    )
                    .with_context(|| {
                        format!("while building vftable for type `{resolvee_path}`")
                    })?,
                );
            }
        }
    }

    // Handle min_size: pre-calculate alignment and round up min_size
    if let Some(min_size_value) = min_size {
        // Calculate preliminary alignment based on the pending regions
        let preliminary_alignment = if packed {
            1
        } else {
            // Determine the requested alignment
            let requested_alignment = align
                .or((pending_regions.len() == 1)
                    .then(|| pending_regions[0].1.type_ref.alignment(&semantic.type_registry))
                    .flatten())
                .unwrap_or(semantic.type_registry.pointer_size());

            // Calculate the minimum required alignment from field types
            let required_alignment = util::lcm(
                pending_regions
                    .iter()
                    .flat_map(|(_, r)| r.type_ref.alignment(&semantic.type_registry)),
            );

            // Use the maximum of requested and required alignment
            requested_alignment.max(required_alignment)
        };

        // Round min_size up to nearest multiple of alignment
        let rounded_min_size = if min_size_value % preliminary_alignment == 0 {
            min_size_value
        } else {
            ((min_size_value / preliminary_alignment) + 1) * preliminary_alignment
        };

        // Use the rounded min_size as target_size for padding
        target_size = Some(rounded_min_size);
    }

    let Some((regions, vftable, size)) = resolve_regions(
        semantic,
        resolvee_path,
        visibility,
        target_size,
        min_size.is_some(),
        pending_regions,
        vftable_functions,
    )
    .with_context(|| format!("while processing `{resolvee_path}`"))?
    else {
        return Ok(None);
    };

    // Reborrow the module after resolving regions
    let module = semantic.get_module_for_path(resolvee_path).unwrap();

    // Handle functions
    let mut associated_functions = vec![];
    let mut associated_functions_used_names: HashSet<String> = vftable
        .as_ref()
        .map(|v| v.functions.iter().map(|f| f.name.clone()).collect())
        .unwrap_or_default();
    for (i, base_region) in regions.iter().filter(|r| r.is_base).enumerate() {
        // Inject all base associated functions into the type
        let Some((base_name, base_type)) = get_region_name_and_type_definition(
            &semantic.type_registry,
            resolvee_path,
            base_region,
        )?
        else {
            continue;
        };

        let mut add_functions = |functions: &[Function]| {
            for function in functions.iter().filter(|f| f.is_public()) {
                let mut function = function.clone();
                let original_name = function.name.clone();
                if associated_functions_used_names.contains(&original_name) {
                    function.name = format!("{}_{}", base_name, original_name);
                }
                function.body = FunctionBody::field(base_name.clone(), original_name);
                associated_functions_used_names.insert(function.name.clone());
                associated_functions.push(function);
            }
        };

        // Push this base's associated functions into the type
        add_functions(&base_type.associated_functions);

        if i > 0 {
            // Inject all non-first-base vfuncs into the type
            if let Some(vftable) = &base_type.vftable {
                add_functions(&vftable.functions);
            }
        }
    }
    if let Some(type_impl) = module.impls.get(resolvee_path) {
        for function in &type_impl.functions {
            if associated_functions_used_names.contains(&function.name.0) {
                anyhow::bail!(
                    "function `{}` is already defined in type `{}` (or a base type)",
                    function.name,
                    resolvee_path
                );
            }

            let function =
                function::build(&semantic.type_registry, &module.scope(), false, function)
                    .with_context(|| {
                        format!(
                            "while building impl function `{}` for type `{resolvee_path}`",
                            function.name
                        )
                    })?;
            associated_functions_used_names.insert(function.name.clone());
            associated_functions.push(function);
        }
    }

    // Iterate over all of the regions and ensure their types are defaultable if
    // we have our defaultable attribute set.
    if defaultable {
        for region in &regions {
            let Region {
                visibility: _,
                name,
                doc: _,
                type_ref,
                is_base: _,
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
                anyhow::bail!(
                    "field `{name}` of type `{resolvee_path}` is not a defaultable type (pointer or function?)"
                );
            };

            let item = semantic
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
                anyhow::bail!("field `{name}` of type `{resolvee_path}` is not a defaultable type");
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
                .then(|| regions[0].type_ref.alignment(&semantic.type_registry))
                .flatten())
            .unwrap_or(semantic.type_registry.pointer_size());

        // Calculate the minimum required alignment.
        let required_alignment = util::lcm(
            regions
                .iter()
                .flat_map(|r| r.type_ref.alignment(&semantic.type_registry)),
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
                let alignment = region.type_ref.alignment(&semantic.type_registry).unwrap();
                if last_address % alignment != 0 {
                    anyhow::bail!(
                        "field `{name}` of type `{resolvee_path}` is located at 0x{last_address:X}, which is not divisible by {alignment} (the alignment of the type of the field)"
                    );
                }
                last_address += region.size(&semantic.type_registry).unwrap();
            }
        }

        // Ensure that the size is a multiple of the alignment.
        if size % alignment != 0 {
            anyhow::bail!(
                "the type `{resolvee_path}` has a size of {size}, which is not a multiple of its alignment {alignment}"
            );
        }

        alignment
    };

    Ok(Some(ItemStateResolved {
        size,
        alignment,
        inner: TypeDefinition {
            regions,
            doc,
            associated_functions,
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

#[allow(clippy::type_complexity)]
fn resolve_regions(
    semantic: &mut SemanticState,
    resolvee_path: &ItemPath,
    visibility: Visibility,
    target_size: Option<usize>,
    is_min_size: bool,
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
    let first_base = regions.iter().map(|t| &t.1).find(|r| r.is_base);
    let (vftable, vftable_region) = vftable::build(
        semantic,
        resolvee_path,
        visibility,
        first_base,
        vftable_functions,
    )?;
    if let Some(vftable_region) = vftable_region
        && resolved
            .push(&semantic.type_registry, vftable_region)
            .is_none()
    {
        return Ok(None);
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
                    "attempted to insert padding at 0x{offset:X}, but overlapped with existing region `{existing_region}` that ends at 0x{:X}",
                    resolved.last_address
                );
            };
            let padding_region = Region::unnamed_field(semantic.type_registry.padding_type(size));
            if resolved
                .push(&semantic.type_registry, padding_region)
                .is_none()
            {
                return Ok(None);
            }
        }

        if resolved.push(&semantic.type_registry, region).is_none() {
            return Ok(None);
        }
    }

    // Pad out to target size
    if let Some(target_size) = target_size
        && resolved.last_address < target_size
    {
        let padding_region = Region::unnamed_field(
            semantic
                .type_registry
                .padding_type(target_size - resolved.last_address),
        );
        if resolved
            .push(&semantic.type_registry, padding_region)
            .is_none()
        {
            return Ok(None);
        }
    }

    // Find total size, and ensure that all regions have names
    let mut size = 0;
    for region in &mut resolved.regions {
        let Some(region_size) = region.size(&semantic.type_registry) else {
            return Ok(None);
        };

        if let Region {
            visibility: _,
            name: None,
            doc: _,
            type_ref,
            is_base: _,
        } = region
        {
            *region = Region {
                visibility: Visibility::Private,
                name: Some(format!("_field_{size:x}")),
                doc: None,
                type_ref: type_ref.clone(),
                is_base: false,
            };
        }

        size += region_size;
    }

    // Check that the final size is equal to the target size (or >= for min_size)
    if let Some(target_size) = target_size {
        if is_min_size {
            // For min_size, the final size should be >= target_size (which was already rounded)
            if size < target_size {
                anyhow::bail!(
                    "calculated size {size} for type `{resolvee_path}` is less than minimum size {target_size}"
                );
            }
        } else {
            // For exact size, the final size must equal target_size
            if size != target_size {
                anyhow::bail!(
                    "calculated size {size} for type `{resolvee_path}` does not match target size {target_size}; is your target size correct?"
                );
            }
        }
    }

    Ok(Some((resolved.regions, vftable, size)))
}

/// Given a region, attempt to get the region's name and its type definition if available
fn get_region_name_and_type_definition<'a>(
    type_registry: &'a TypeRegistry,
    type_path: &ItemPath,
    region: &Region,
) -> anyhow::Result<Option<(String, &'a TypeDefinition)>> {
    let region_name = region
        .name
        .clone()
        .expect("region had no name, this shouldn't be possible");

    let Type::Raw(path) = &region.type_ref else {
        anyhow::bail!(
            "expected region field `{}` of type `{}` to be a raw type, but it was a {}",
            region_name,
            type_path,
            region.type_ref.human_friendly_type()
        );
    };

    let region_type = type_registry
        .get(path)
        .with_context(|| format!("failed to get region type `{path}` for type `{type_path}`"))?;

    let Some(region_type) = region_type.resolved() else {
        return Ok(None);
    };

    let Some(region_type) = region_type.inner.as_type() else {
        anyhow::bail!(
            "expected region field `{}` of type `{}` to be a type, but it was a {}",
            region_name,
            type_path,
            region_type.inner.human_friendly_type()
        );
    };

    Ok(Some((region_name, region_type)))
}
