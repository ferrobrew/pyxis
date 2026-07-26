use super::{
    Region,
    resolve::{is_str_type, is_type_trait_satisfied, resolve_regions},
};
use crate::{
    grammar::{self, ItemPath},
    semantic::{
        attribute,
        error::{
            AttributeName, BuildOutcome, DefaultableErrorKind, Result, SemanticError,
            UnresolvedTypeContext, UnresolvedTypeReference,
        },
        module::Module,
        resolution_context::ResolutionContext,
        type_registry::TypeLookupResult,
        types::{Function, ItemDefinitionInner, ItemState, ItemStateResolved, Type, Visibility},
    },
    span::{HasLocation, ItemLocation},
    util,
};

use super::{TypeDefinition, vftable};
use crate::semantic::union_definition::{self, InlineUnionRequest};

/// Type-level attributes parsed from a `TypeDefinition`'s attribute list.
///
/// Shared with `union_definition`, which accepts the same layout attributes on
/// the same terms — a union is laid out by different rules, but `#[size]`,
/// `#[align]`, `#[packed]` and the trait attributes mean exactly what they do on
/// a type.
pub(in crate::semantic) struct TypeAttributes {
    pub target_size: Option<usize>,
    pub min_size: Option<usize>,
    pub singleton: Option<usize>,
    pub copyable: bool,
    pub cloneable: bool,
    pub defaultable: bool,
    pub packed: bool,
    pub pinned: bool,
    pub align: Option<usize>,
}

/// The layout-bearing content collected from a type body: the pending field
/// regions, the resolved vftable functions (if any), the nested item paths, and
/// the inline `union { … }` fields awaiting desugaring.
struct TypeBody<'a> {
    pending_regions: Vec<(Option<usize>, Region)>,
    vftable_functions: Option<Vec<Function>>,
    nested_item_paths: Vec<ItemPath>,
    /// Inline unions can't be built during the walk — registering the generated
    /// items needs the resolution context mutably, and the walk holds it
    /// immutably. They're collected here and built by `build` immediately after.
    inline_unions: Vec<InlineUnionRequest<'a>>,
}

pub fn build(
    semantic: &mut ResolutionContext<'_>,
    resolvee_path: &ItemPath,
    visibility: Visibility,
    definition: &grammar::TypeDefinition,
    location: &ItemLocation,
    doc_comments: &[String],
    type_parameters: &[String],
) -> Result<BuildOutcome> {
    let module = semantic.get_module_for_path(resolvee_path, location)?;

    let mut attributes = parse_type_attributes(&definition.attributes)?;

    // Ensure size and min_size are mutually exclusive
    if attributes.target_size.is_some() && attributes.min_size.is_some() {
        return Err({
            SemanticError::ConflictingAttributes {
                attr1: AttributeName::Size,
                attr2: AttributeName::MinSize,
                item_path: resolvee_path.clone(),
                location: *location,
            }
        });
    }

    let body = match build_type_body(semantic, module, resolvee_path, definition, type_parameters)?
    {
        BuildFlow::Continue(body) => body,
        BuildFlow::Outcome(outcome) => return Ok(*outcome),
    };
    let TypeBody {
        pending_regions,
        vftable_functions,
        nested_item_paths,
        inline_unions,
    } = body;

    // Desugar the inline `union { … }` fields into generated sibling items. Each
    // field's region already points at the item's path, so this only has to make
    // that path real before layout asks for its size.
    if !inline_unions.is_empty()
        && union_definition::build_inline_union(
            semantic,
            resolvee_path,
            &inline_unions,
            type_parameters,
        )?
        .is_none()
    {
        return Ok(BuildOutcome::Deferred);
    }

    // Handle min_size: pre-calculate alignment and round up min_size
    if let Some(min_size_value) = attributes.min_size {
        attributes.target_size = Some(round_up_min_size(
            semantic,
            min_size_value,
            attributes.packed,
            attributes.align,
            &pending_regions,
        ));
    }

    let Some((regions, vftable, size)) = resolve_regions(
        semantic,
        resolvee_path,
        visibility,
        attributes.target_size,
        attributes.min_size.is_some(),
        pending_regions,
        vftable_functions,
        location,
    )?
    else {
        return Ok(BuildOutcome::Deferred);
    };

    // Reborrow the module after resolving regions
    let module = semantic.get_module_for_path(resolvee_path, location)?;

    // Associated-function resolution (own impl block + inheritance from base
    // types) is deferred to `compute_associated_functions` after every type has
    // fully resolved. This prevents the resolver from defer-looping
    // when an impl method's signature references its own enclosing type
    // (`impl Foo { fn make() -> Foo; }`).
    let associated_functions: Vec<Function> = Vec::new();
    let _ = module;

    check_trait_constraints(semantic, resolvee_path, &regions, &attributes)?;

    let alignment = resolve_alignment(
        semantic,
        resolvee_path,
        location,
        &regions,
        &attributes,
        size,
    )?;

    Ok(BuildOutcome::Resolved(ItemStateResolved {
        size,
        alignment,
        inner: TypeDefinition {
            regions,
            doc: doc_comments.to_vec(),
            associated_functions,
            vftable,
            singleton: attributes.singleton,
            copyable: attributes.copyable,
            cloneable: attributes.cloneable,
            defaultable: attributes.defaultable,
            packed: attributes.packed,
            pinned: attributes.pinned,
            nested_item_paths,
        }
        .into(),
    }))
}

/// Result of a phase that may short-circuit the whole `build` with an outcome.
enum BuildFlow<T> {
    Continue(T),
    Outcome(Box<BuildOutcome>),
}

/// Parse the type-level attributes (`#[size]`, `#[copyable]`, `#[align]`, ...).
pub(in crate::semantic) fn parse_type_attributes(
    attributes: &[grammar::Attribute],
) -> Result<TypeAttributes> {
    let mut target_size: Option<usize> = None;
    let mut min_size: Option<usize> = None;
    let mut singleton = None;
    let mut copyable = false;
    let mut cloneable = false;
    let mut defaultable = false;
    let mut packed = false;
    let mut pinned = false;
    let mut align = None;
    for attribute in attributes {
        match attribute {
            grammar::Attribute::Function { name, items, .. } => {
                let loc = attribute.location();
                if let Some(attr_size) = attribute::parse_size(name, items, loc)? {
                    target_size = Some(attr_size);
                } else if let Some(attr_min_size) = attribute::parse_min_size(name, items, loc)? {
                    min_size = Some(attr_min_size);
                } else if let Some(attr_singleton) = attribute::parse_singleton(name, items, loc)? {
                    singleton = Some(attr_singleton);
                } else if let Some(attr_align) = attribute::parse_align(name, items, loc)? {
                    align = Some(attr_align);
                }
            }
            grammar::Attribute::Ident { ident, .. } => match ident.as_str() {
                "copyable" => {
                    copyable = true;
                    cloneable = true;
                }
                "cloneable" => cloneable = true,
                "defaultable" => defaultable = true,
                "packed" => packed = true,
                "pinned" => pinned = true,
                _ => {}
            },
            grammar::Attribute::Assign { .. } | grammar::Attribute::Cfg { .. } => {}
        }
    }

    Ok(TypeAttributes {
        target_size,
        min_size,
        singleton,
        copyable,
        cloneable,
        defaultable,
        packed,
        pinned,
        align,
    })
}

/// Walk the type body's statements, resolving field types, the vftable, and
/// collecting nested item paths. Returns `BuildFlow::Outcome` when a field's
/// type is unresolved (deferring or reporting a not-found type).
fn build_type_body<'a>(
    semantic: &ResolutionContext<'_>,
    module: &Module,
    resolvee_path: &ItemPath,
    definition: &'a grammar::TypeDefinition,
    type_parameters: &[String],
) -> Result<BuildFlow<TypeBody<'a>>> {
    let mut pending_regions: Vec<(Option<usize>, Region)> = vec![];
    let mut vftable_functions = None;
    let mut nested_item_paths: Vec<ItemPath> = Vec::new();
    let mut inline_unions: Vec<InlineUnionRequest<'a>> = Vec::new();
    for statement in definition.statements() {
        let grammar::TypeStatement {
            field,
            attributes,
            doc_comments,
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
                        grammar::Attribute::Ident {
                            ident: attr_ident, ..
                        } if attr_ident.as_str() == "base" => {
                            is_base = true;
                        }
                        grammar::Attribute::Function {
                            name: attr_ident,
                            items,
                            ..
                        } => {
                            if let Some(attr_address) =
                                attribute::parse_address(attr_ident, items, attribute.location())?
                            {
                                address = Some(attr_address);
                            }
                        }
                        _ => {}
                    }
                }

                // Push field - use type parameters for resolution inside generic types
                // Extend the module scope with the parent type's path so bare
                // references to nested items (e.g. `MyEnum` inside `Outer`)
                // resolve to `Outer::MyEnum`.
                let type_body_scope: Vec<ItemPath> = std::iter::once(resolvee_path.clone())
                    .chain(module.scope())
                    .collect();
                let type_ = match semantic.type_registry.resolve_grammar_type(
                    &type_body_scope,
                    type_,
                    type_parameters,
                ) {
                    TypeLookupResult::Found(t) => t,
                    TypeLookupResult::NotYetResolved => {
                        return Ok(BuildFlow::Outcome(Box::new(BuildOutcome::Deferred)));
                    }
                    TypeLookupResult::NotFound { type_name } => {
                        let field_name = if field_ident.0 == "_" {
                            "<anonymous>".to_string()
                        } else {
                            field_ident.0.clone()
                        };
                        return Ok(BuildFlow::Outcome(Box::new(BuildOutcome::NotFoundType(
                            UnresolvedTypeReference {
                                type_name,
                                location: *type_.location(),
                                context: UnresolvedTypeContext::StructField {
                                    field_name,
                                    type_path: resolvee_path.clone(),
                                },
                            },
                        ))));
                    }
                    TypeLookupResult::PrivateAccess { item_path } => {
                        let field_name = if field_ident.0 == "_" {
                            "<anonymous>".to_string()
                        } else {
                            field_ident.0.clone()
                        };
                        return Ok(BuildFlow::Outcome(Box::new(BuildOutcome::NotFoundType(
                            UnresolvedTypeReference {
                                type_name: item_path.to_string(),
                                location: *type_.location(),
                                context: UnresolvedTypeContext::StructField {
                                    field_name,
                                    type_path: resolvee_path.clone(),
                                },
                            },
                        ))));
                    }
                };

                // `str` type is only allowed on `const` declarations, not on fields.
                if is_str_type(&type_) {
                    return Err(SemanticError::StrTypeNotConst {
                        location: statement.location,
                    });
                }

                let ident = (field_ident.0 != "_").then(|| field_ident.0.clone());
                pending_regions.push((
                    address,
                    Region {
                        visibility: (*visibility).into(),
                        name: ident,
                        doc,
                        type_ref: type_,
                        is_base,
                        location: statement.location,
                    },
                ));
            }
            grammar::TypeField::Vftable(functions) => {
                // the vftable field is a sentinel field used to ensure that the user has
                // thought about the presence of vftables in their type. we do not actually
                // count it as a region; the type will be generated with a vftable field later on.
                // It occupies offset 0, so it must precede every layout field — but nested
                // items (consts, extern values, nested type/enum/bitflags definitions) carry no
                // layout and may appear before it.
                if !pending_regions.is_empty() {
                    return Err(SemanticError::VftableMustBeFirst {
                        item_path: resolvee_path.clone(),
                        location: statement.location,
                    });
                }

                // Extract size attribute
                let mut size = None;
                for attribute in attributes {
                    if let grammar::Attribute::Function {
                        name: ident, items, ..
                    } = attribute
                    {
                        if let Some(attr_size) =
                            attribute::parse_size(ident, items, attribute.location())?
                        {
                            size = Some(attr_size);
                        }
                    }
                }

                vftable_functions = match vftable::convert_grammar_functions_to_semantic_functions(
                    semantic.type_registry,
                    module,
                    resolvee_path,
                    size,
                    functions,
                    &statement.location,
                )? {
                    Some(funcs) => Some(funcs),
                    None => return Ok(BuildFlow::Outcome(Box::new(BuildOutcome::Deferred))),
                };
            }
            grammar::TypeField::Item(inner_def) => {
                // Compute the nested item's path as Parent::Child
                let nested_path = resolvee_path.join(inner_def.name.as_str().into());
                nested_item_paths.push(nested_path);
                // Nested items are registered by name_index/declaration_registry (Phase 3),
                // not during build. We only collect the paths here.
            }
            grammar::TypeField::UnionField {
                visibility,
                name,
                body,
            } => {
                // `pub payload: union { … }` is one ordinary field whose type is
                // a generated sibling item. Recording the region here (rather
                // than after the union is built) keeps it in body order, so the
                // field lands at the offset the source implies.
                //
                // The field name is load-bearing twice over — it names the Rust
                // field *and* the generated item — so it can't be the anonymous
                // `_` that padding uses.
                if name.as_str() == "_" {
                    return Err(SemanticError::UnionAnonymousMember {
                        item_path: resolvee_path.clone(),
                        location: statement.location,
                    });
                }

                let Some(path) = InlineUnionRequest::path_for(resolvee_path, name.as_str()) else {
                    return Err(SemanticError::ModuleNotFound {
                        path: resolvee_path.clone(),
                        location: statement.location,
                    });
                };

                let mut address: Option<usize> = None;
                for attribute in attributes {
                    if let grammar::Attribute::Function {
                        name: attr_ident,
                        items,
                        ..
                    } = attribute
                        && let Some(attr_address) =
                            attribute::parse_address(attr_ident, items, attribute.location())?
                    {
                        address = Some(attr_address);
                    }
                }

                inline_unions.push(InlineUnionRequest {
                    path: path.clone(),
                    visibility: (*visibility).into(),
                    attributes,
                    body,
                    doc: doc_comments.to_vec(),
                    location: statement.location,
                });

                pending_regions.push((
                    address,
                    Region {
                        visibility: (*visibility).into(),
                        name: Some(name.0.clone()),
                        doc: doc_comments.to_vec(),
                        type_ref: Type::Raw(path),
                        is_base: false,
                        location: statement.location,
                    },
                ));
            }
        }
    }

    Ok(BuildFlow::Continue(TypeBody {
        pending_regions,
        vftable_functions,
        nested_item_paths,
        inline_unions,
    }))
}

/// Round a `#[min_size]` up to the type's preliminary alignment so it can be
/// used as a padding target.
fn round_up_min_size(
    semantic: &ResolutionContext<'_>,
    min_size_value: usize,
    packed: bool,
    align: Option<usize>,
    pending_regions: &[(Option<usize>, Region)],
) -> usize {
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
                        .alignment(semantic.type_registry)
                })
                .flatten())
            .unwrap_or(semantic.type_registry.pointer_size());

        // Calculate the minimum required alignment from field types
        let required_alignment = util::lcm(
            pending_regions
                .iter()
                .flat_map(|(_, r)| r.type_ref.alignment(semantic.type_registry)),
        );

        // Use the maximum of requested and required alignment
        requested_alignment.max(required_alignment)
    };

    // Round min_size up to nearest multiple of alignment
    if min_size_value.is_multiple_of(preliminary_alignment) {
        min_size_value
    } else {
        ((min_size_value / preliminary_alignment) + 1) * preliminary_alignment
    }
}

/// Enforce the `#[defaultable]`, `#[copyable]`, and `#[cloneable]` constraints
/// against every region's type.
pub(in crate::semantic) fn check_trait_constraints(
    semantic: &ResolutionContext<'_>,
    resolvee_path: &ItemPath,
    regions: &[Region],
    attributes: &TypeAttributes,
) -> Result<()> {
    // Iterate over all of the regions and ensure their types are defaultable if
    // we have our defaultable attribute set.
    if attributes.defaultable {
        for region in regions {
            let Region {
                visibility: _,
                name,
                doc: _,
                type_ref,
                is_base: _,
                location: _,
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
                return Err(SemanticError::DefaultableError {
                    field_name: name.into(),
                    item_path: resolvee_path.clone(),
                    kind: DefaultableErrorKind::PointerOrFunction,
                    location: region.location,
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
                    kind: DefaultableErrorKind::TypeNotDefaultable,
                    location: region.location,
                });
            }
        }
    }

    // Iterate over all of the regions and ensure their types are copyable if
    // we have our copyable attribute set.
    // NOTE: Check copyable first, before cloneable, because copyable implies cloneable.
    // If we checked cloneable first, a type marked #[copyable] with a non-copyable field
    // would report "not cloneable" instead of "not copyable".
    if attributes.copyable {
        for region in regions {
            let Region {
                visibility: _,
                name,
                doc: _,
                type_ref,
                is_base: _,
                location: _,
            } = region;
            let name = name.as_deref().unwrap_or("unnamed");

            // Check if the type is copyable, recursively handling generics and arrays
            if !is_type_trait_satisfied(
                type_ref,
                semantic.type_registry,
                &region.location,
                ItemDefinitionInner::copyable,
            )? {
                return Err(SemanticError::CopyableError {
                    field_name: name.into(),
                    item_path: resolvee_path.clone(),
                    location: region.location,
                });
            }
        }
    }

    // Iterate over all of the regions and ensure their types are cloneable if
    // we have our cloneable attribute set (and not already covered by copyable check above).
    if attributes.cloneable && !attributes.copyable {
        for region in regions {
            let Region {
                visibility: _,
                name,
                doc: _,
                type_ref,
                is_base: _,
                location: _,
            } = region;
            let name = name.as_deref().unwrap_or("unnamed");

            // Check if the type is cloneable, recursively handling generics and arrays
            if !is_type_trait_satisfied(
                type_ref,
                semantic.type_registry,
                &region.location,
                ItemDefinitionInner::cloneable,
            )? {
                return Err(SemanticError::CloneableError {
                    field_name: name.into(),
                    item_path: resolvee_path.clone(),
                    location: region.location,
                });
            }
        }
    }

    Ok(())
}

/// Compute the type's final alignment, validating `#[packed]`/`#[align]`
/// conflicts, field alignment, and that the size is an alignment multiple.
fn resolve_alignment(
    semantic: &ResolutionContext<'_>,
    resolvee_path: &ItemPath,
    location: &ItemLocation,
    regions: &[Region],
    attributes: &TypeAttributes,
    size: usize,
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

    // Determine the final requested alignment.
    // The requested alignment, the alignment of a single-region type, or the pointer size.
    let alignment = attributes
        .align
        .or((regions.len() == 1)
            .then(|| regions[0].type_ref.alignment(semantic.type_registry))
            .flatten())
        .unwrap_or(semantic.type_registry.pointer_size());

    // Calculate the minimum required alignment.
    let required_alignment = util::lcm(
        regions
            .iter()
            .flat_map(|r| r.type_ref.alignment(semantic.type_registry)),
    );

    // Ensure that the alignment is at least the minimum required alignment.
    if required_alignment > alignment {
        return Err(SemanticError::AlignmentBelowMinimum {
            alignment,
            required_alignment,
            item_path: resolvee_path.clone(),
            location: *location,
        });
    }

    // Ensure that all fields are aligned.
    {
        let mut last_address = 0;
        for region in regions {
            let name = region.name.as_deref().unwrap_or("unnamed");
            let field_alignment = region.type_ref.alignment(semantic.type_registry).unwrap();
            if last_address % field_alignment != 0 {
                return Err(SemanticError::FieldNotAligned {
                    field_name: name.into(),
                    item_path: resolvee_path.clone(),
                    address: last_address,
                    required_alignment: field_alignment,
                    location: *location,
                });
            }
            last_address += region.size(semantic.type_registry).unwrap();
        }
    }

    // Ensure that the size is a multiple of the alignment.
    if !size.is_multiple_of(alignment) {
        return Err(SemanticError::SizeNotAlignmentMultiple {
            size,
            alignment,
            item_path: resolvee_path.clone(),
            location: *location,
        });
    }

    Ok(alignment)
}
