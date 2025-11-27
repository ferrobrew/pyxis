use std::collections::HashSet;

use crate::{
    grammar::{self, ItemPath},
    semantic::{
        SemanticState,
        error::{IntegerConversionContext, Result, SemanticError},
        function,
        type_registry::TypeRegistry,
        types::{Function, FunctionBody, ItemState, ItemStateResolved, Type, Visibility},
    },
    span::{ItemLocation, Located},
    util,
};

#[cfg(test)]
use crate::span::StripLocations;

mod vftable;
pub use vftable::TypeVftable;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Region {
    pub visibility: Visibility,
    pub name: Option<String>,
    pub doc: Vec<String>,
    pub type_ref: Type,
    pub is_base: bool,
}
#[cfg(test)]
impl StripLocations for Region {
    fn strip_locations(&self) -> Self {
        Region {
            visibility: self.visibility.strip_locations(),
            name: self.name.strip_locations(),
            doc: self.doc.strip_locations(),
            type_ref: self.type_ref.strip_locations(),
            is_base: self.is_base.strip_locations(),
        }
    }
}
impl Region {
    #[cfg(test)]
    /// Test-only constructor for field that uses a synthetic location
    pub fn field((visibility, name): (Visibility, impl Into<String>), type_ref: Type) -> Self {
        let (visibility, name) = (visibility, name);
        Region {
            visibility,
            name: Some(name.into()),
            doc: vec![],
            type_ref,
            is_base: false,
        }
    }

    pub fn unnamed_field(type_ref: Type) -> Self {
        Region {
            visibility: Visibility::Private,
            name: None,
            doc: vec![],
            type_ref,
            is_base: false,
        }
    }

    pub fn marked_as_base(mut self) -> Self {
        self.is_base = true;
        self
    }
    pub fn with_doc(mut self, doc: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.doc = doc.into_iter().map(|s| s.into()).collect();
        self
    }
    pub fn size(&self, type_registry: &TypeRegistry) -> Option<usize> {
        self.type_ref.size(type_registry)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Default, Hash)]
pub struct TypeDefinition {
    pub regions: Vec<Located<Region>>,
    pub doc: Vec<String>,
    pub associated_functions: Vec<Located<Function>>,
    pub vftable: Option<TypeVftable>,
    pub singleton: Option<usize>,
    pub copyable: bool,
    pub cloneable: bool,
    pub defaultable: bool,
    pub packed: bool,
}
#[cfg(test)]
impl StripLocations for TypeDefinition {
    fn strip_locations(&self) -> Self {
        TypeDefinition {
            regions: self.regions.strip_locations(),
            doc: self.doc.strip_locations(),
            associated_functions: self.associated_functions.strip_locations(),
            vftable: self.vftable.strip_locations(),
            singleton: self.singleton.strip_locations(),
            copyable: self.copyable.strip_locations(),
            cloneable: self.cloneable.strip_locations(),
            defaultable: self.defaultable.strip_locations(),
            packed: self.packed.strip_locations(),
        }
    }
}
#[cfg(test)]
impl TypeDefinition {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn with_regions(mut self, regions: impl IntoIterator<Item = Region>) -> Self {
        self.regions = regions.into_iter().map(Located::test).collect();
        self
    }
    pub fn with_doc(mut self, doc: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.doc = doc.into_iter().map(|s| s.into()).collect();
        self
    }
    pub fn with_associated_functions(
        mut self,
        associated_functions: impl IntoIterator<Item = Function>,
    ) -> Self {
        self.associated_functions = associated_functions
            .into_iter()
            .map(Located::test)
            .collect();
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
}
impl TypeDefinition {
    /// Returns the fields and types of everything in this type's hierarchy, starting from the top
    pub fn dfs_hierarchy(
        &self,
        type_registry: &TypeRegistry,
        type_path: &ItemPath,
        fields: &[&str],
    ) -> Result<Vec<(Vec<String>, Type)>> {
        let mut output = vec![];
        for region in &self.regions {
            if !region.is_base {
                continue;
            }

            let Some((field_name, type_definition)) =
                get_region_name_and_type_definition(type_registry, type_path, region.as_ref())?
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
    pub fn doc(&self) -> &[String] {
        &self.doc
    }
}

pub fn build(
    semantic: &mut SemanticState,
    resolvee_path: &ItemPath,
    visibility: Visibility,
    definition: &grammar::TypeDefinition,
    doc_comments: &[String],
    location: ItemLocation,
) -> Result<Option<ItemStateResolved>> {
    let module = semantic.get_module_for_path(resolvee_path, &location)?;

    // Handle attributes
    let mut target_size: Option<usize> = None;
    let mut min_size: Option<usize> = None;
    let mut singleton = None;
    let mut copyable = false;
    let mut cloneable = false;
    let mut defaultable = false;
    let mut packed = false;
    let mut align = None;
    let doc = doc_comments.to_vec();
    for attribute in &definition.attributes {
        match attribute {
            grammar::Attribute::Function(ident, items) => {
                let exprs = grammar::AttributeItem::extract_exprs(items);
                match (ident.as_str(), exprs.as_slice()) {
                    ("size", [grammar::Expr::IntLiteral { value, .. }]) => {
                        target_size = Some((*value).try_into().map_err(|_| {
                            SemanticError::IntegerConversion {
                                value: value.to_string(),
                                target_type: "usize".into(),
                                conversion_context: IntegerConversionContext::SizeAttribute {
                                    type_path: resolvee_path.clone(),
                                },
                                location: location.clone(),
                            }
                        })?);
                    }
                    ("min_size", [grammar::Expr::IntLiteral { value, .. }]) => {
                        min_size = Some((*value).try_into().map_err(|_| {
                            SemanticError::IntegerConversion {
                                value: value.to_string(),
                                target_type: "usize".into(),
                                conversion_context: IntegerConversionContext::MinSizeAttribute {
                                    type_path: resolvee_path.clone(),
                                },
                                location: location.clone(),
                            }
                        })?);
                    }
                    ("singleton", [grammar::Expr::IntLiteral { value, .. }]) => {
                        singleton = Some((*value).try_into().map_err(|_| {
                            SemanticError::IntegerConversion {
                                value: value.to_string(),
                                target_type: "usize".into(),
                                conversion_context: IntegerConversionContext::SingletonAttribute {
                                    type_path: resolvee_path.clone(),
                                },
                                location: location.clone(),
                            }
                        })?);
                    }
                    ("align", [grammar::Expr::IntLiteral { value, .. }]) => {
                        align = Some((*value).try_into().map_err(|_| {
                            SemanticError::IntegerConversion {
                                value: value.to_string(),
                                target_type: "usize".into(),
                                conversion_context: IntegerConversionContext::AlignAttribute {
                                    type_path: resolvee_path.clone(),
                                },
                                location: location.clone(),
                            }
                        })?);
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
        return Err({
            SemanticError::ConflictingAttributes {
                attr1: "size".into(),
                attr2: "min_size".into(),
                item_path: resolvee_path.clone(),
                location: location.clone(),
            }
        });
    }

    // Handle fields
    let mut pending_regions: Vec<(Option<usize>, Located<Region>)> = vec![];
    let mut vftable_functions = None;
    for (idx, statement) in definition.statements().enumerate() {
        let grammar::TypeStatement {
            field,
            attributes,
            doc_comments,
            location,
            ..
        } = statement;

        match field {
            grammar::TypeField::Field(visibility, field_ident, type_) => {
                // Extract address attribute
                let mut address: Option<usize> = None;
                let mut is_base = false;
                let doc = doc_comments.to_vec();
                for attribute in attributes {
                    match attribute {
                        grammar::Attribute::Ident(attr_ident) if attr_ident.as_str() == "base" => {
                            is_base = true
                        }
                        grammar::Attribute::Function(attr_ident, items) => {
                            let exprs = grammar::AttributeItem::extract_exprs(items);
                            if let ("address", [grammar::Expr::IntLiteral { value, .. }]) =
                                (attr_ident.as_str(), &exprs[..])
                            {
                                address = Some((*value).try_into().map_err(|_| {
                                    SemanticError::IntegerConversion {
                                        value: value.to_string(),
                                        target_type: "usize".into(),
                                        conversion_context:
                                            IntegerConversionContext::FieldAddressAttribute {
                                                field_name: field_ident.0.clone(),
                                                type_path: resolvee_path.clone(),
                                            },
                                        location: location.clone(),
                                    }
                                })?);
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

                let ident = (field_ident.0 != "_").then(|| field_ident.0.clone());
                pending_regions.push((
                    address,
                    Located::new(
                        Region {
                            visibility: (*visibility).into(),
                            name: ident,
                            doc,
                            type_ref: type_,
                            is_base,
                        },
                        location.clone(),
                    ),
                ));
            }
            grammar::TypeField::Vftable(functions) => {
                // the vftable field is a sentinel field used to ensure that the user has
                // thought about the presence of vftables in their type. we do not actually
                // count it as a region; the type will be generated with a vftable field later on
                if idx != 0 {
                    return Err(SemanticError::VftableMustBeFirst {
                        item_path: resolvee_path.clone(),
                        location: location.clone(),
                    });
                }

                // Extract size attribute
                let mut size = None;
                for attribute in attributes {
                    let grammar::Attribute::Function(ident, items) = attribute else {
                        continue;
                    };
                    let exprs = grammar::AttributeItem::extract_exprs(items);
                    if let ("size", [grammar::Expr::IntLiteral { value, .. }]) =
                        (ident.as_str(), exprs.as_slice())
                    {
                        size = Some(*value as usize);
                    }
                }

                vftable_functions = Some(vftable::convert_grammar_functions_to_semantic_functions(
                    &semantic.type_registry,
                    module,
                    size,
                    functions,
                    location,
                )?);
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
                    .then(|| {
                        pending_regions[0]
                            .1
                            .type_ref
                            .alignment(&semantic.type_registry)
                    })
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
        &location,
    )?
    else {
        return Ok(None);
    };

    // Reborrow the module after resolving regions
    let module = semantic.get_module_for_path(resolvee_path, &location)?;

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
            base_region.as_ref(),
        )?
        else {
            continue;
        };

        let mut add_functions = |functions: &[Located<Function>]| {
            for function in functions.iter().filter(|f| f.is_public()) {
                let mut function = function.clone();
                let original_name = function.name.clone();
                if associated_functions_used_names.contains(&original_name) {
                    function.name = format!("{base_name}_{original_name}");
                }
                function.body = FunctionBody::Field {
                    field: base_name.clone(),
                    function_name: original_name,
                };
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
        for function in type_impl.functions().collect::<Vec<_>>() {
            if associated_functions_used_names.contains(&function.name.0) {
                return Err(SemanticError::DuplicateDefinition {
                    name: function.name.0.clone(),
                    item_path: resolvee_path.clone(),
                    message: "function already defined in type or base type".into(),
                    location: location.clone(),
                });
            }

            let function =
                function::build(&semantic.type_registry, &module.scope(), false, function)?;
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
            } = &region.value;
            let name = name.as_deref().unwrap_or("unnamed");
            fn get_defaultable_type_path(type_ref: &Type) -> Option<&ItemPath> {
                match type_ref {
                    Type::Raw(tp) => Some(tp),
                    Type::Array(t, _) => get_defaultable_type_path(t),
                    _ => None,
                }
            }
            let Some(path) = get_defaultable_type_path(type_ref) else {
                return Err(SemanticError::DefaultableError {
                    field_name: name.into(),
                    item_path: resolvee_path.clone(),
                    message: "is not a defaultable type (pointer or function?)".into(),
                    location: region.location.clone(),
                });
            };

            let item = semantic
                .type_registry
                .get(path, &region.location)?
                .state
                .clone();

            let ItemState::Resolved(ItemStateResolved { inner, .. }) = &item else {
                continue;
            };

            if !inner.defaultable() {
                return Err(SemanticError::DefaultableError {
                    field_name: name.into(),
                    item_path: resolvee_path.clone(),
                    message: "is not a defaultable type".into(),
                    location: region.location.clone(),
                });
            }
        }
    }

    let alignment = if packed {
        if align.is_some() {
            return Err(SemanticError::ConflictingAttributes {
                attr1: "packed".into(),
                attr2: "align".into(),
                item_path: resolvee_path.clone(),
                location: location.clone(),
            });
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
            return Err(SemanticError::AlignmentBelowMinimum {
                alignment,
                required_alignment,
                item_path: resolvee_path.clone(),
                location: location.clone(),
            });
        }

        // Ensure that all fields are aligned.
        {
            let mut last_address = 0;
            for region in &regions {
                let name = region.name.as_deref().unwrap_or("unnamed");
                let field_alignment = region.type_ref.alignment(&semantic.type_registry).unwrap();
                if last_address % field_alignment != 0 {
                    return Err(SemanticError::FieldNotAligned {
                        field_name: name.into(),
                        item_path: resolvee_path.clone(),
                        address: last_address,
                        required_alignment: field_alignment,
                        location: location.clone(),
                    });
                }
                last_address += region.size(&semantic.type_registry).unwrap();
            }
        }

        // Ensure that the size is a multiple of the alignment.
        if size % alignment != 0 {
            return Err(SemanticError::SizeNotAlignmentMultiple {
                size,
                alignment,
                item_path: resolvee_path.clone(),
                location: location.clone(),
            });
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

#[allow(clippy::type_complexity, clippy::too_many_arguments)]
fn resolve_regions(
    semantic: &mut SemanticState,
    resolvee_path: &ItemPath,
    visibility: Visibility,
    target_size: Option<usize>,
    is_min_size: bool,
    regions: Vec<(Option<usize>, Located<Region>)>,
    vftable_functions: Option<Vec<Located<Function>>>,
    type_location: &ItemLocation,
) -> Result<Option<(Vec<Located<Region>>, Option<TypeVftable>, usize)>> {
    // this resolution algorithm is very simple and doesn't handle overlapping regions
    // or regions that are out of order
    #[derive(Default)]
    struct Regions {
        regions: Vec<Located<Region>>,
        last_address: usize,
    }
    impl Regions {
        fn push(&mut self, type_registry: &TypeRegistry, region: Located<Region>) -> Option<()> {
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
        type_location,
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
                    .unwrap_or_default()
                    .to_string();
                return Err(SemanticError::OverlappingRegions {
                    item_path: resolvee_path.clone(),
                    region_name: existing_region,
                    address: offset,
                    existing_end: resolved.last_address,
                    location: region.location.clone(),
                });
            };
            let padding_region = Located::new(
                Region::unnamed_field(semantic.type_registry.padding_type(size)),
                region.location.clone(),
            );
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
        let padding_region = Located::new(
            Region::unnamed_field(
                semantic
                    .type_registry
                    .padding_type(target_size - resolved.last_address),
            ),
            type_location.clone(),
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
        } = &region.value
        {
            region.value = Region {
                visibility: Visibility::Private,
                name: Some(format!("_field_{size:x}")),
                doc: vec![],
                type_ref: type_ref.clone(),
                is_base: false,
            };
        }

        size += region_size;
    }

    // Check that the final size is equal to the target size (or >= for min_size)
    if let Some(target_size) = target_size {
        // Use the first region's location if available for error context, fallback to type location
        let error_location = resolved
            .regions
            .first()
            .map(|r| r.location.clone())
            .unwrap_or_else(|| type_location.clone());
        if is_min_size {
            // For min_size, the final size should be >= target_size (which was already rounded)
            if size < target_size {
                return Err(SemanticError::SizeBelowMinimum {
                    minimum_size: target_size,
                    actual_size: size,
                    item_path: resolvee_path.clone(),
                    location: error_location.clone(),
                });
            }
        } else {
            // For exact size, the final size must equal target_size
            if size != target_size {
                return Err(SemanticError::SizeMismatch {
                    expected: target_size,
                    actual: size,
                    item_path: resolvee_path.clone(),
                    location: error_location,
                });
            }
        }
    }

    Ok(Some((resolved.regions, vftable, size)))
}

/// Given a region, attempt to get the region's name and its type definition if available
fn get_region_name_and_type_definition<'a>(
    type_registry: &'a TypeRegistry,
    type_path: &ItemPath,
    region: Located<&Region>,
) -> Result<Option<(String, &'a TypeDefinition)>> {
    let region_name = region
        .name
        .clone()
        .expect("region had no name, this shouldn't be possible");

    let Type::Raw(path) = &region.type_ref else {
        return Err({
            SemanticError::InvalidType {
                expected: "raw type".into(),
                found: region.type_ref.human_friendly_type().into(),
                item_path: type_path.clone(),
                context_description: format!("region field `{region_name}`"),
                location: region.location.clone(),
            }
        });
    };

    let region_type = type_registry.get(path, &region.location)?;

    let Some(region_type) = region_type.resolved() else {
        return Ok(None);
    };

    let Some(region_type) = region_type.inner.as_type() else {
        return Err({
            SemanticError::InvalidType {
                expected: "type".into(),
                found: region_type.inner.human_friendly_type().into(),
                item_path: type_path.clone(),
                context_description: format!("region field `{region_name}`"),
                location: region.location.clone(),
            }
        });
    };

    Ok(Some((region_name, region_type)))
}
