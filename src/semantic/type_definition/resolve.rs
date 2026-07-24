use super::{Region, TypeDefinition};
use crate::{
    grammar::ItemPath,
    semantic::{
        error::{ItemKind, Result, SemanticError, TypeRefKind},
        resolution_context::ResolutionContext,
        type_registry::TypeRegistry,
        types::{Function, ItemDefinitionInner, ItemState, ItemStateResolved, Type, Visibility},
    },
    span::{HasLocation, ItemLocation},
};

use super::vftable::{self, TypeVftable};

#[allow(clippy::type_complexity, clippy::too_many_arguments)]
pub(super) fn resolve_regions(
    semantic: &mut ResolutionContext<'_>,
    resolvee_path: &crate::grammar::ItemPath,
    visibility: Visibility,
    target_size: Option<usize>,
    is_min_size: bool,
    regions: Vec<(Option<usize>, Region)>,
    vftable_functions: Option<Vec<Function>>,
    type_location: &ItemLocation,
) -> Result<Option<(Vec<Region>, Option<TypeVftable>, usize)>> {
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
        type_location,
    )?;
    if let Some(vftable_region) = vftable_region
        && resolved
            .push(semantic.type_registry, vftable_region)
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
                    location: *region.location(),
                });
            };
            let padding_region = Region::unnamed_field(
                semantic.type_registry.padding_type(size),
                *region.location(),
            );
            if resolved
                .push(semantic.type_registry, padding_region)
                .is_none()
            {
                return Ok(None);
            }
        }

        // Reject embedding a zero-size type as a concrete field. A fieldless
        // type (or a generic instantiation that resolves to size 0, e.g.
        // `Pair<Empty, Empty>`) has no representable layout in backends that
        // lack zero-size objects — the member would silently shift every
        // subsequent field. Pointers, arrays, and type parameters are excluded
        // — only concrete type references (raw or generic) can trigger this.
        // Zero-size arrays are excluded explicitly (they're handled by `push`'s
        // special case) because `Type::Array(T, 0).size()` returns `Some(0)`.
        if !region.type_ref.is_array()
            && let Some(0) = region.type_ref.size(semantic.type_registry)
        {
            return Err(SemanticError::ZeroSizeFieldEmbedding {
                field_name: region
                    .name
                    .clone()
                    .unwrap_or_else(|| "<anonymous>".to_string()),
                item_path: resolvee_path.clone(),
                field_type: region.type_ref.clone(),
                location: *region.location(),
            });
        }

        if resolved.push(semantic.type_registry, region).is_none() {
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
            *type_location,
        );
        if resolved
            .push(semantic.type_registry, padding_region)
            .is_none()
        {
            return Ok(None);
        }
    }

    // Find total size, and ensure that all regions have names
    let mut size = 0;
    for region in &mut resolved.regions {
        let Some(region_size) = region.size(semantic.type_registry) else {
            return Ok(None);
        };

        if region.name.is_none() {
            let type_ref = region.type_ref.clone();
            let location = region.location;
            *region = Region {
                visibility: Visibility::Private,
                name: Some(format!("_field_{size:x}")),
                doc: vec![],
                type_ref,
                is_base: false,
                location,
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
            .map(|r| *r.location())
            .unwrap_or_else(|| *type_location);
        if is_min_size {
            // For min_size, the final size should be >= target_size (which was already rounded)
            if size < target_size {
                return Err(SemanticError::SizeBelowMinimum {
                    minimum_size: target_size,
                    actual_size: size,
                    item_path: resolvee_path.clone(),
                    location: error_location,
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
pub(in crate::semantic) fn get_region_name_and_type_definition<'a>(
    type_registry: &'a TypeRegistry,
    type_path: &ItemPath,
    region: &Region,
) -> Result<Option<(String, &'a TypeDefinition)>> {
    let region_name = region
        .name
        .clone()
        .expect("region had no name, this shouldn't be possible");

    let Type::Raw(path) = &region.type_ref else {
        return Err({
            SemanticError::RegionFieldNotRawType {
                field_name: region_name.clone(),
                item_path: type_path.clone(),
                found: TypeRefKind::from_type(&region.type_ref),
                location: *region.location(),
            }
        });
    };

    let region_type = type_registry.get(path, region.location())?;

    let Some(region_type) = region_type.resolved() else {
        return Ok(None);
    };

    let Some(region_type) = region_type.inner.as_type() else {
        return Err({
            SemanticError::RegionFieldNotStructType {
                field_name: region_name.clone(),
                item_path: type_path.clone(),
                found: ItemKind::from_inner(&region_type.inner),
                location: region.location,
            }
        });
    };

    Ok(Some((region_name, region_type)))
}

/// Check if a type satisfies a trait requirement (copyable or cloneable),
/// recursively handling generics and arrays.
/// Returns Ok(true) if satisfied, Ok(false) if not, Err if type lookup fails.
pub(super) fn is_type_trait_satisfied(
    type_ref: &Type,
    type_registry: &TypeRegistry,
    location: &ItemLocation,
    check_trait: fn(&ItemDefinitionInner) -> bool,
) -> Result<bool> {
    match type_ref {
        // Raw types: check if the type itself satisfies the trait
        Type::Raw(path) => {
            let item = type_registry.get(path, location)?.state.clone();
            let ItemState::Resolved(ItemStateResolved { inner, .. }) = &item else {
                // If not resolved yet, assume it's ok (will be checked later)
                return Ok(true);
            };
            Ok(check_trait(inner))
        }
        // Arrays: check if element type satisfies the trait
        Type::Array(inner, _) => {
            is_type_trait_satisfied(inner, type_registry, location, check_trait)
        }
        // Generic types: check base type AND all type arguments
        Type::Generic(base, args) => {
            // Check the base type
            let item = type_registry.get(base, location)?.state.clone();
            let ItemState::Resolved(ItemStateResolved { inner, .. }) = &item else {
                return Ok(true);
            };
            if !check_trait(inner) {
                return Ok(false);
            }
            // Check all type arguments
            for arg in args {
                if !is_type_trait_satisfied(arg, type_registry, location, check_trait)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        // Pointers and functions satisfy Copy/Clone (like raw pointers in Rust)
        Type::ConstPointer(_) | Type::MutPointer(_) | Type::Function(_, _, _) => Ok(true),
        // Type parameters: assume ok (will be checked at instantiation time)
        Type::TypeParameter(_) => Ok(true),
        // Unresolved: assume ok (will be resolved and checked later)
        Type::Unresolved(_) => Ok(true),
    }
}

/// Check if a resolved type is `str`.
pub(super) fn is_str_type(type_: &Type) -> bool {
    match type_ {
        Type::Raw(path) if path.len() == 1 => path.iter().next().unwrap().as_str() == "str",
        _ => false,
    }
}
