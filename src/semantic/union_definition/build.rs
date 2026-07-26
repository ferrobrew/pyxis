use crate::{
    grammar::{self, ItemPath},
    semantic::{
        attribute,
        error::{
            AttributeName, BuildOutcome, Result, SemanticError, UnresolvedTypeContext,
            UnresolvedTypeReference,
        },
        resolution_context::ResolutionContext,
        type_definition::{Region, TypeAttributes, check_trait_constraints, parse_type_attributes},
        type_registry::TypeLookupResult,
        types::{ItemCategory, ItemDefinition, ItemState, ItemStateResolved, Type, Visibility},
    },
    span::{HasLocation, ItemLocation},
    util,
};

use super::{UnionDefinition, inline_union_name};

/// Build a standalone `union Name { … }` item.
pub fn build(
    semantic: &mut ResolutionContext<'_>,
    resolvee_path: &ItemPath,
    definition: &grammar::UnionDefinition,
    location: &ItemLocation,
    doc_comments: &[String],
    type_parameters: &[String],
) -> Result<BuildOutcome> {
    let mut generated = Vec::new();
    let outcome = build_state(
        semantic,
        resolvee_path,
        &definition.attributes,
        definition,
        location,
        doc_comments,
        type_parameters,
        Nesting::Named,
        &mut generated,
    )?;

    // Only publish the inline unions declared inside this body once the union
    // itself is known good, so a deferral leaves the registry untouched.
    if matches!(outcome, BuildOutcome::Resolved(_)) {
        register_generated(semantic, resolvee_path, generated)?;
    }

    Ok(outcome)
}

/// Whether a union body came from a named `union Name { … }` item or from an
/// inline `pub field: union { … }`. Inline bodies are more restricted: their
/// item is generated at a synthesised path, so nothing may be declared under it.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Nesting {
    Named,
    Inline,
}

fn register_generated(
    semantic: &mut ResolutionContext<'_>,
    owner: &ItemPath,
    generated: Vec<ItemDefinition>,
) -> Result<()> {
    // Two fields in one body can generate the same name (`a_b` and `a__b` both
    // give `AB`), so the second add collides with the first, not the registry.
    for item in generated {
        semantic.add_generated_item(owner, item)?;
    }
    Ok(())
}

/// A `pub name: union { … }` field awaiting desugaring, collected during a type
/// body walk (which holds the resolution context immutably) and processed by the
/// caller once it can register items.
pub struct InlineUnionRequest<'a> {
    pub path: ItemPath,
    pub visibility: Visibility,
    pub attributes: &'a grammar::Attributes,
    pub body: &'a grammar::UnionDefinition,
    pub doc: Vec<String>,
    pub location: ItemLocation,
}

impl InlineUnionRequest<'_> {
    /// The generated item path for an inline union field on `parent_path`:
    /// a module-scope sibling, not a nested item — see [`inline_union_name`].
    pub fn path_for(parent_path: &ItemPath, field_name: &str) -> Option<ItemPath> {
        let parent_name = parent_path.last()?;
        Some(
            parent_path
                .parent()?
                .join(inline_union_name(parent_name.as_str(), field_name).into()),
        )
    }
}

/// Build and register every inline union collected from a type body.
///
/// Returns `None` if any of them deferred, in which case nothing is registered
/// and the enclosing item should defer too.
pub fn build_inline_union(
    semantic: &mut ResolutionContext<'_>,
    owner: &ItemPath,
    requests: &[InlineUnionRequest<'_>],
    type_parameters: &[String],
) -> Result<Option<()>> {
    let mut built = Vec::new();
    for request in requests {
        let mut generated = Vec::new();
        let outcome = build_state(
            semantic,
            &request.path,
            request.attributes,
            request.body,
            &request.location,
            &request.doc,
            type_parameters,
            Nesting::Inline,
            &mut generated,
        )?;
        let BuildOutcome::Resolved(state) = outcome else {
            return Ok(None);
        };
        built.extend(generated);
        built.push(ItemDefinition {
            visibility: request.visibility,
            path: request.path.clone(),
            type_parameters: vec![],
            state: ItemState::Resolved(state),
            category: ItemCategory::Defined,
            predefined: None,
            cfg: request.attributes.cfg(),
            location: request.location,
            declaration_location: request.location,
        });
    }

    // Two fields in one body can PascalCase to the same generated name
    // (`a_b` and `ab` both give `AB`), so check the batch against itself as well
    // as against the registry.
    let mut claimed: Vec<&ItemPath> = Vec::new();
    for item in &built {
        if claimed.contains(&&item.path) {
            return Err(SemanticError::GeneratedNameCollision {
                generated_path: item.path.clone(),
                item_path: owner.clone(),
                location: item.location,
            });
        }
        claimed.push(&item.path);
    }

    register_generated(semantic, owner, built)?;
    Ok(Some(()))
}

/// The layout-bearing content collected from a union body.
struct UnionBody {
    regions: Vec<Region>,
    nested_item_paths: Vec<ItemPath>,
}

/// Compute a union's resolved state.
///
/// `generated` accumulates the items for any inline unions declared inside this
/// body (at any depth). They are registered by the caller rather than here so
/// that a deferral doesn't leave partially-built items behind.
#[allow(clippy::too_many_arguments)]
fn build_state(
    semantic: &ResolutionContext<'_>,
    resolvee_path: &ItemPath,
    attributes: &grammar::Attributes,
    definition: &grammar::UnionDefinition,
    location: &ItemLocation,
    doc_comments: &[String],
    type_parameters: &[String],
    nesting: Nesting,
    generated: &mut Vec<ItemDefinition>,
) -> Result<BuildOutcome> {
    let mut parsed = parse_type_attributes(attributes)?;

    if parsed.target_size.is_some() && parsed.min_size.is_some() {
        return Err(SemanticError::ConflictingAttributes {
            attr1: AttributeName::Size,
            attr2: AttributeName::MinSize,
            item_path: resolvee_path.clone(),
            location: *location,
        });
    }

    let Some(mut body) = build_body(
        semantic,
        resolvee_path,
        definition,
        type_parameters,
        nesting,
        generated,
    )?
    else {
        return Ok(BuildOutcome::Deferred);
    };

    if body.regions.is_empty() {
        return Err(SemanticError::EmptyUnion {
            item_path: resolvee_path.clone(),
            location: *location,
        });
    }

    let alignment = resolve_alignment(semantic, resolvee_path, location, &body.regions, &parsed)?;
    let Some((size, largest_member)) = resolve_size(
        semantic,
        resolvee_path,
        location,
        &body.regions,
        &parsed,
        alignment,
        generated,
    )?
    else {
        return Ok(BuildOutcome::Deferred);
    };

    // `#[size]`/`#[min_size]` can ask for more room than any member needs. A
    // type pads at the tail; a union has no tail, so the padding is another
    // whole-width reading of the same bytes. Without it the backends would emit
    // a union that is `largest_member` bytes wide alongside a size assertion
    // demanding `size`, and fail to compile.
    //
    // Rounding up to the alignment needs no help — both Rust and C++ do that
    // for a `union` themselves — so only an explicitly requested excess counts.
    if size > round_up(largest_member, alignment) {
        body.regions.push(Region {
            visibility: Visibility::Private,
            name: Some("_padding".to_string()),
            doc: vec![],
            type_ref: semantic.type_registry.padding_type(size),
            is_base: false,
            location: *location,
        });
    }

    // `#[singleton]` is meaningless on a union; drop it so it can't leak into
    // the shared trait-constraint checks with any effect.
    parsed.singleton = None;
    check_trait_constraints(semantic, resolvee_path, &body.regions, &parsed)?;

    Ok(BuildOutcome::Resolved(ItemStateResolved {
        size,
        alignment,
        inner: UnionDefinition {
            regions: body.regions,
            doc: doc_comments.to_vec(),
            copyable: parsed.copyable,
            cloneable: parsed.cloneable,
            defaultable: parsed.defaultable,
            packed: parsed.packed,
            pinned: parsed.pinned,
            nested_item_paths: body.nested_item_paths,
        }
        .into(),
    }))
}

/// Walk a union body, resolving member types and rejecting the constructs a
/// union cannot express. Returns `None` to defer.
fn build_body(
    semantic: &ResolutionContext<'_>,
    resolvee_path: &ItemPath,
    definition: &grammar::UnionDefinition,
    type_parameters: &[String],
    nesting: Nesting,
    generated: &mut Vec<ItemDefinition>,
) -> Result<Option<UnionBody>> {
    let mut regions: Vec<Region> = vec![];
    let mut nested_item_paths: Vec<ItemPath> = Vec::new();

    for statement in definition.statements() {
        let grammar::TypeStatement {
            field,
            attributes,
            doc_comments,
            ..
        } = statement;

        match field {
            grammar::TypeField::Field(visibility, field_ident, type_) => {
                check_member(
                    attributes,
                    resolvee_path,
                    &field_ident.0,
                    &statement.location,
                )?;

                let module = semantic.get_module_for_path(resolvee_path, &statement.location)?;
                let scope: Vec<ItemPath> = std::iter::once(resolvee_path.clone())
                    .chain(module.scope())
                    .collect();
                let type_ = match semantic.type_registry.resolve_grammar_type(
                    &scope,
                    type_,
                    type_parameters,
                ) {
                    TypeLookupResult::Found(t) => t,
                    TypeLookupResult::NotYetResolved => return Ok(None),
                    TypeLookupResult::InvalidAttribute { error } => return Err(*error),
                    TypeLookupResult::NotFound { type_name } => {
                        return Err(unresolved(type_name, type_, field_ident, resolvee_path));
                    }
                    TypeLookupResult::PrivateAccess { item_path } => {
                        return Err(unresolved(
                            item_path.to_string(),
                            type_,
                            field_ident,
                            resolvee_path,
                        ));
                    }
                };

                regions.push(Region {
                    visibility: (*visibility).into(),
                    name: Some(field_ident.0.clone()),
                    doc: doc_comments.to_vec(),
                    type_ref: type_,
                    is_base: false,
                    location: statement.location,
                });
            }
            grammar::TypeField::UnionField {
                visibility,
                name,
                body,
            } => {
                check_member(attributes, resolvee_path, &name.0, &statement.location)?;

                let Some(path) = InlineUnionRequest::path_for(resolvee_path, name.as_str()) else {
                    return Err(SemanticError::ModuleNotFound {
                        path: resolvee_path.clone(),
                        location: statement.location,
                    });
                };

                // Depth-first: the child is fully built before the parent needs
                // its size, and `resolve_size` finds it in `generated`.
                let outcome = build_state(
                    semantic,
                    &path,
                    attributes,
                    body,
                    &statement.location,
                    doc_comments,
                    type_parameters,
                    Nesting::Inline,
                    generated,
                )?;
                let BuildOutcome::Resolved(state) = outcome else {
                    return Ok(None);
                };
                generated.push(ItemDefinition {
                    visibility: (*visibility).into(),
                    path: path.clone(),
                    type_parameters: vec![],
                    state: ItemState::Resolved(state),
                    category: ItemCategory::Defined,
                    predefined: None,
                    cfg: attributes.cfg(),
                    location: statement.location,
                    declaration_location: statement.location,
                });

                regions.push(Region {
                    visibility: (*visibility).into(),
                    name: Some(name.0.clone()),
                    doc: doc_comments.to_vec(),
                    type_ref: Type::Raw(path),
                    is_base: false,
                    location: statement.location,
                });
            }
            grammar::TypeField::Vftable(_) => {
                return Err(SemanticError::UnionVftableNotAllowed {
                    item_path: resolvee_path.clone(),
                    location: statement.location,
                });
            }
            grammar::TypeField::Item(inner_def) => {
                // A named union is walked by the grammar passes that populate
                // `item_scopes`, so its nested items get real paths. An inline
                // union's item is synthesised after those passes have run, so a
                // nested item under it would be registered nowhere and reach no
                // backend — reject it rather than drop it silently.
                if nesting == Nesting::Inline {
                    return Err(SemanticError::InlineUnionNestedItem {
                        item_name: inner_def.name.as_str().to_string(),
                        item_path: resolvee_path.clone(),
                        location: statement.location,
                    });
                }
                nested_item_paths.push(resolvee_path.join(inner_def.name.as_str().into()));
            }
        }
    }

    Ok(Some(UnionBody {
        regions,
        nested_item_paths,
    }))
}

/// Reject what a union member can't be: named `_` (padding, which a union has no
/// room for), `#[base]` (a base must sit at a known offset), or `#[address]`
/// (every member starts at offset 0).
fn check_member(
    attributes: &grammar::Attributes,
    resolvee_path: &ItemPath,
    member_name: &str,
    location: &ItemLocation,
) -> Result<()> {
    if member_name == "_" {
        return Err(SemanticError::UnionAnonymousMember {
            item_path: resolvee_path.clone(),
            location: *location,
        });
    }

    for attribute in attributes {
        match attribute {
            grammar::Attribute::Ident { ident, .. } if ident.as_str() == "base" => {
                return Err(SemanticError::UnionBaseNotAllowed {
                    item_path: resolvee_path.clone(),
                    location: *location,
                });
            }
            grammar::Attribute::Function { name, items, .. }
                if attribute::parse_address(name, items, attribute.location())?.is_some() =>
            {
                return Err(SemanticError::UnionMemberAddress {
                    member_name: member_name.to_string(),
                    item_path: resolvee_path.clone(),
                    location: *location,
                });
            }
            _ => {}
        }
    }
    Ok(())
}

fn unresolved(
    type_name: String,
    type_: &grammar::Type,
    field_ident: &grammar::Ident,
    resolvee_path: &ItemPath,
) -> SemanticError {
    let field_name = if field_ident.0 == "_" {
        "<anonymous>".to_string()
    } else {
        field_ident.0.clone()
    };
    SemanticError::TypeResolutionStalled {
        unresolved_types: vec![resolvee_path.to_string()],
        resolved_types: vec![],
        unresolved_references: vec![UnresolvedTypeReference {
            type_name,
            location: *type_.location(),
            context: UnresolvedTypeContext::StructField {
                field_name,
                type_path: resolvee_path.clone(),
            },
        }],
    }
}

/// A union's alignment is the strictest of its members', unless `#[align]` asks
/// for more or `#[packed]` asks for none.
///
/// Note the default differs from a type's: a type with no explicit alignment
/// falls back to the pointer size, but a union of two `u8`s is genuinely
/// 1-aligned, and forcing it wider would inflate its size.
fn resolve_alignment(
    semantic: &ResolutionContext<'_>,
    resolvee_path: &ItemPath,
    location: &ItemLocation,
    regions: &[Region],
    attributes: &TypeAttributes,
) -> Result<usize> {
    if attributes.packed {
        if attributes.align.is_some() {
            return Err(SemanticError::ConflictingAttributes {
                attr1: AttributeName::Packed,
                attr2: AttributeName::Align,
                item_path: resolvee_path.clone(),
                location: *location,
            });
        }
        return Ok(1);
    }

    let required_alignment = util::lcm(
        regions
            .iter()
            .flat_map(|r| r.type_ref.alignment(semantic.type_registry)),
    )
    .max(1);

    let Some(requested) = attributes.align else {
        return Ok(required_alignment);
    };

    if required_alignment > requested {
        return Err(SemanticError::AlignmentBelowMinimum {
            alignment: requested,
            required_alignment,
            item_path: resolvee_path.clone(),
            location: *location,
        });
    }
    Ok(requested)
}

/// A union is as large as its largest member, rounded up to its alignment.
/// `#[size]` fixes the size exactly (and every member must fit); `#[min_size]`
/// raises the floor. Returns the final size alongside the largest member's size,
/// which the caller needs to decide whether to pad — or `None` to defer, if a
/// member's size isn't known yet.
#[allow(clippy::too_many_arguments)]
fn resolve_size(
    semantic: &ResolutionContext<'_>,
    resolvee_path: &ItemPath,
    location: &ItemLocation,
    regions: &[Region],
    attributes: &TypeAttributes,
    alignment: usize,
    generated: &[ItemDefinition],
) -> Result<Option<(usize, usize)>> {
    let mut largest = 0usize;
    for region in regions {
        let Some(size) = region_size(semantic, generated, region) else {
            return Ok(None);
        };
        if let Some(declared) = attributes.target_size
            && size > declared
        {
            return Err(SemanticError::UnionMemberExceedsSize {
                member_name: region.name.clone().unwrap_or_else(|| "unnamed".to_string()),
                member_size: size,
                declared_size: declared,
                item_path: resolvee_path.clone(),
                location: region.location,
            });
        }
        largest = largest.max(size);
    }

    let size = match (attributes.target_size, attributes.min_size) {
        (Some(declared), _) => declared,
        (None, Some(min)) => round_up(largest.max(min), alignment),
        (None, None) => round_up(largest, alignment),
    };

    if !size.is_multiple_of(alignment) {
        return Err(SemanticError::SizeNotAlignmentMultiple {
            size,
            alignment,
            item_path: resolvee_path.clone(),
            location: *location,
        });
    }

    Ok(Some((size, largest)))
}

/// A region's size, consulting the not-yet-registered inline unions built during
/// this walk before falling back to the registry.
fn region_size(
    semantic: &ResolutionContext<'_>,
    generated: &[ItemDefinition],
    region: &Region,
) -> Option<usize> {
    if let Type::Raw(path) = &region.type_ref
        && let Some(item) = generated.iter().find(|item| &item.path == path)
    {
        return item.size();
    }
    region.size(semantic.type_registry)
}

fn round_up(value: usize, alignment: usize) -> usize {
    if alignment == 0 || value.is_multiple_of(alignment) {
        value
    } else {
        value.div_ceil(alignment) * alignment
    }
}
