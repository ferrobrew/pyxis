use crate::{
    grammar::{self, ItemPath},
    semantic::{
        error::{
            BuildOutcome, Result, SemanticError, UnresolvedTypeContext, UnresolvedTypeReference,
        },
        resolution_context::ResolutionContextRef,
        type_registry::{TypeLookupResult, TypeRegistry},
        types::{
            ConstDefinition as SemanticConstDefinition, ConstValue, ItemStateResolved,
            PredefinedItem, Type,
        },
    },
    span::{HasLocation, ItemLocation},
};

/// Build a semantic `ConstDefinition` from the grammar `ConstDefinition`.
///
/// Resolves the const's type annotation and validates the value expression
/// against that type. Constants have no runtime size/alignment — size 0 and
/// alignment 1 are sentinels.
pub fn build(
    semantic: &ResolutionContextRef<'_>,
    resolvee_path: &ItemPath,
    definition: &grammar::ConstDefinition,
    _location: &ItemLocation,
    doc_comments: &[String],
) -> Result<BuildOutcome> {
    let module = semantic.get_module_for_path(resolvee_path, &definition.location)?;
    let scope: Vec<ItemPath> = module.scope();

    // Resolve the const's type annotation
    let resolved_type = match semantic.type_registry.resolve_grammar_type(
        &scope,
        &definition.type_,
        &[], // Constants don't have type parameters
    ) {
        TypeLookupResult::Found(t) => t,
        TypeLookupResult::NotYetResolved => return Ok(BuildOutcome::Deferred),
        TypeLookupResult::NotFound { type_name } => {
            return Ok(BuildOutcome::NotFoundType(UnresolvedTypeReference {
                type_name,
                location: *definition.type_.location(),
                context: UnresolvedTypeContext::ConstType {
                    const_path: resolvee_path.clone(),
                },
            }));
        }
        TypeLookupResult::PrivateAccess { item_path } => {
            return Ok(BuildOutcome::NotFoundType(UnresolvedTypeReference {
                type_name: item_path.to_string(),
                location: *definition.type_.location(),
                context: UnresolvedTypeContext::ConstType {
                    const_path: resolvee_path.clone(),
                },
            }));
        }
    };

    // Resolve the value expression based on the Expr variant
    let value = match validate_const_expr(
        semantic,
        &scope,
        &definition.expr,
        &resolved_type,
        resolvee_path,
    )? {
        Some(v) => v,
        None => return Ok(BuildOutcome::Deferred),
    };

    Ok(BuildOutcome::Resolved(ItemStateResolved {
        size: 0,
        alignment: 1,
        inner: SemanticConstDefinition {
            type_: resolved_type,
            value,
            doc: doc_comments.to_vec(),
        }
        .into(),
    }))
}

/// Validate a const value expression against an expected type and produce a
/// `ConstValue`. This is recursive: struct fields and array elements are
/// validated by re-entering this function. Returns `Ok(None)` if a referenced
/// constant hasn't been resolved yet (caller should defer).
fn validate_const_expr(
    semantic: &ResolutionContextRef<'_>,
    scope: &[ItemPath],
    expr: &grammar::Expr,
    expected_type: &Type,
    resolvee_path: &ItemPath,
) -> Result<Option<ConstValue>> {
    let mismatch = |expected: String, found: String| SemanticError::ConstValueTypeMismatch {
        item_path: resolvee_path.clone(),
        expected,
        found,
        location: *expr.location(),
    };

    match expr {
        grammar::Expr::IntLiteral { value, .. } => {
            if !type_is_predefined_integer(semantic.type_registry, expected_type) {
                return Err(mismatch(
                    "an integer type".to_string(),
                    format!("{expected_type}"),
                ));
            }
            Ok(Some(ConstValue::Int(*value)))
        }
        grammar::Expr::FloatLiteral { raw_text, .. } => {
            if !type_is_predefined_float(semantic.type_registry, expected_type) {
                return Err(mismatch(
                    "a float type (f32 or f64)".to_string(),
                    format!("{expected_type}"),
                ));
            }
            let f: f64 = raw_text
                .parse()
                .map_err(|_| mismatch("a valid float".to_string(), raw_text.clone()))?;
            Ok(Some(ConstValue::Float(f.to_bits())))
        }
        grammar::Expr::StringLiteral { value, .. } => {
            if !type_is_predefined_str(semantic.type_registry, expected_type) {
                return Err(mismatch("`str`".to_string(), format!("{expected_type}")));
            }
            Ok(Some(ConstValue::String(value.clone())))
        }
        grammar::Expr::CStringLiteral { value, .. } => {
            if !type_is_predefined_cstr(semantic.type_registry, expected_type) {
                return Err(mismatch("`cstr`".to_string(), format!("{expected_type}")));
            }
            // CStr is NUL-terminated; interior NUL bytes are invalid.
            if value.contains('\0') {
                return Err(SemanticError::ConstValueTypeMismatch {
                    item_path: resolvee_path.clone(),
                    expected: "a cstr without interior NUL bytes".to_string(),
                    found: format!(
                        "a string with a NUL byte at position {}",
                        value.find('\0').unwrap()
                    ),
                    location: *expr.location(),
                });
            }
            Ok(Some(ConstValue::CString(value.clone())))
        }
        grammar::Expr::Path { path, .. } => {
            // First try enum-value resolution (existing behavior).
            match resolve_enum_value(semantic, scope, path, resolvee_path, expr.location()) {
                Ok(enum_type_path) => {
                    // The const's type should be the enum type.
                    if *expected_type != Type::Raw(enum_type_path.clone()) {
                        return Err(mismatch(
                            format!("enum `{enum_type_path}`"),
                            format!("{expected_type}"),
                        ));
                    }
                    Ok(Some(ConstValue::EnumValue(path.clone())))
                }
                Err(_) => {
                    // Not an enum value — try constant reference resolution.
                    resolve_const_ref(semantic, scope, path, expected_type, resolvee_path, expr)
                }
            }
        }
        grammar::Expr::Ident { ident, .. } => {
            // Constant alias: resolve the ident as a constant in scope.
            let path = ItemPath::from(ident.as_str());
            resolve_const_ref(semantic, scope, &path, expected_type, resolvee_path, expr)
        }
        grammar::Expr::StructLiteral {
            type_name, fields, ..
        } => {
            // Resolve the struct type through the scope.
            let struct_type_path = match semantic.type_registry.resolve_path(scope, type_name) {
                TypeLookupResult::Found(Type::Raw(p)) => p,
                _ => {
                    return Err(mismatch(
                        format!("a struct type `{type_name}`"),
                        format!("{expected_type}"),
                    ));
                }
            };

            // The struct type must match the expected type.
            if *expected_type != Type::Raw(struct_type_path.clone()) {
                return Err(mismatch(
                    format!("`{struct_type_path}`"),
                    format!("{expected_type}"),
                ));
            }

            // Look up the type definition and validate POD-ness.
            let type_def = semantic
                .type_registry
                .get(&struct_type_path, &ItemLocation::internal())
                .map_err(|_| {
                    mismatch(
                        format!("type `{struct_type_path}`"),
                        format!("{expected_type}"),
                    )
                })?;

            // If the type hasn't been resolved yet, defer.
            let resolved = match type_def.resolved() {
                Some(r) => r,
                None => return Ok(None),
            };

            let inner = match resolved.inner.as_type() {
                Some(t) => t,
                None => {
                    return Err(mismatch(
                        format!(
                            "a struct type, got {}",
                            resolved.inner.human_friendly_type()
                        ),
                        format!("{expected_type}"),
                    ));
                }
            };

            // POD validation: must be copyable, not pinned, no vftable, no base regions.
            if !inner.copyable {
                return Err(SemanticError::ConstValueTypeMismatch {
                    item_path: resolvee_path.clone(),
                    expected: format!(
                        "a `#[copyable]` type (struct `{struct_type_path}` is not copyable)"
                    ),
                    found: format!("{expected_type}"),
                    location: *expr.location(),
                });
            }
            if inner.pinned {
                return Err(SemanticError::ConstValueTypeMismatch {
                    item_path: resolvee_path.clone(),
                    expected: format!(
                        "a non-`#[pinned]` type (struct `{struct_type_path}` is pinned)"
                    ),
                    found: format!("{expected_type}"),
                    location: *expr.location(),
                });
            }
            if inner.vftable.is_some() {
                return Err(SemanticError::ConstValueTypeMismatch {
                    item_path: resolvee_path.clone(),
                    expected: format!(
                        "a type without a vftable (struct `{struct_type_path}` has a vftable)"
                    ),
                    found: format!("{expected_type}"),
                    location: *expr.location(),
                });
            }
            if inner.regions.iter().any(|r| r.is_base) {
                return Err(SemanticError::ConstValueTypeMismatch {
                    item_path: resolvee_path.clone(),
                    expected: format!(
                        "a type without base regions (struct `{struct_type_path}` has base regions)"
                    ),
                    found: format!("{expected_type}"),
                    location: *expr.location(),
                });
            }

            // Collect named fields from the type definition, in declaration order.
            let named_fields: Vec<(&str, &Type)> = inner
                .regions
                .iter()
                .filter_map(|r| r.name.as_deref().map(|n| (n, &r.type_ref)))
                .collect();

            // Validate that every literal field corresponds to a named field,
            // and check for duplicate field names.
            let mut seen_fields = std::collections::HashSet::new();
            for field in fields {
                let field_name = field.ident_as_str();
                if !seen_fields.insert(field_name) {
                    return Err(SemanticError::ConstValueTypeMismatch {
                        item_path: resolvee_path.clone(),
                        expected: format!("unique fields in `{struct_type_path}`"),
                        found: format!("duplicate field `{field_name}`"),
                        location: *expr.location(),
                    });
                }
                if !named_fields.iter().any(|(n, _)| *n == field_name) {
                    return Err(SemanticError::ConstValueTypeMismatch {
                        item_path: resolvee_path.clone(),
                        expected: format!("a field of `{struct_type_path}`"),
                        found: format!("unknown field `{field_name}`"),
                        location: *expr.location(),
                    });
                }
            }

            // Validate that every named field is covered (full initialization).
            for (name, _) in &named_fields {
                if !fields.iter().any(|f| f.ident_as_str() == *name) {
                    return Err(SemanticError::ConstValueTypeMismatch {
                        item_path: resolvee_path.clone(),
                        expected: format!("all fields of `{struct_type_path}` to be initialized"),
                        found: format!("missing field `{name}`"),
                        location: *expr.location(),
                    });
                }
            }

            // Build the ordered field values (in declaration order) by recursing.
            let mut ordered_fields = Vec::with_capacity(named_fields.len());
            for (name, field_type) in &named_fields {
                let field_expr = fields
                    .iter()
                    .find(|f| f.ident_as_str() == *name)
                    .expect("checked above");
                let field_value = match validate_const_expr(
                    semantic,
                    scope,
                    &field_expr.1,
                    field_type,
                    resolvee_path,
                )? {
                    Some(v) => v,
                    None => return Ok(None),
                };
                ordered_fields.push((name.to_string(), field_value));
            }

            Ok(Some(ConstValue::Struct {
                type_path: struct_type_path,
                fields: ordered_fields,
            }))
        }
        grammar::Expr::ArrayLiteral { elements, .. } => {
            // Resolve the expected type as an array.
            let (elem_type, count) = match expected_type {
                Type::Array(elem, count) => (elem.as_ref(), *count),
                _ => {
                    return Err(mismatch(
                        "an array type".to_string(),
                        format!("{expected_type}"),
                    ));
                }
            };

            // Validate element count.
            if elements.len() != count {
                return Err(SemanticError::ConstValueTypeMismatch {
                    item_path: resolvee_path.clone(),
                    expected: format!("an array of {count} elements"),
                    found: format!("an array of {} elements", elements.len()),
                    location: *expr.location(),
                });
            }

            // Recursively validate each element.
            let mut values = Vec::with_capacity(count);
            for elem_expr in elements {
                let v = match validate_const_expr(
                    semantic,
                    scope,
                    elem_expr,
                    elem_type,
                    resolvee_path,
                )? {
                    Some(v) => v,
                    None => return Ok(None),
                };
                values.push(v);
            }

            Ok(Some(ConstValue::Array(values)))
        }
    }
}

/// Resolve a constant path through the scope, returning the canonical
/// `ItemPath` of the constant. Unlike `TypeRegistry::resolve_path`, this does
/// not require the item to be resolved — it only checks that the item exists.
/// This allows forward references between constants.
fn resolve_const_path(
    type_registry: &TypeRegistry,
    scope: &[ItemPath],
    path: &ItemPath,
) -> Option<ItemPath> {
    // For multi-segment paths, try direct lookup and canonicalization.
    if path.len() > 1 {
        let canonical = type_registry.canonicalize(path);
        if type_registry.contains(&canonical) {
            return Some(canonical);
        }
    }

    // For single-segment paths or unresolved multi-segment paths,
    // try scope-based resolution using the last segment.
    let last_segment = path.last()?;
    let name = last_segment.as_str();

    // Search through scope paths: try `scope::name` for each scope entry.
    // This mirrors `resolve_string` but without the `is_resolved` check.
    for scope_path in scope {
        let candidate = scope_path.join(name.into());
        if type_registry.contains(&candidate) {
            return Some(type_registry.canonicalize(&candidate));
        }
    }
    // Also try the bare name.
    let bare = ItemPath::from(name);
    if type_registry.contains(&bare) {
        return Some(type_registry.canonicalize(&bare));
    }

    None
}

/// Resolve a path as a constant reference and validate type compatibility.
/// Returns `Ok(None)` if the referenced constant hasn't been resolved yet
/// (caller should defer).
fn resolve_const_ref(
    semantic: &ResolutionContextRef<'_>,
    scope: &[ItemPath],
    path: &ItemPath,
    expected_type: &Type,
    resolvee_path: &ItemPath,
    expr: &grammar::Expr,
) -> Result<Option<ConstValue>> {
    let mismatch = |expected: String, found: String| SemanticError::ConstValueTypeMismatch {
        item_path: resolvee_path.clone(),
        expected,
        found,
        location: *expr.location(),
    };

    // Resolve the path through the scope. We can't use `resolve_path` directly
    // because it returns `NotYetResolved` for constants that haven't been built
    // yet, which would prevent forward references. Instead, we construct
    // candidate paths from the scope and look them up directly.
    let resolved_path = match resolve_const_path(semantic.type_registry, scope, path) {
        Some(p) => p,
        None => {
            return Err(mismatch(format!("a constant `{path}`"), path.to_string()));
        }
    };

    // Look up the item and check it's a constant.
    let item_def = match semantic
        .type_registry
        .get(&resolved_path, &ItemLocation::internal())
    {
        Ok(def) => def,
        Err(_) => return Err(mismatch(format!("a constant `{path}`"), path.to_string())),
    };

    // If the referenced constant hasn't been resolved yet, return None so the
    // caller can defer and retry after the dependency is available.
    let resolved = match item_def.resolved() {
        Some(r) => r,
        None => return Ok(None),
    };

    let const_inner = resolved.inner.as_constant().ok_or_else(|| {
        mismatch(
            format!("a constant `{path}`"),
            resolved.inner.human_friendly_type().to_string(),
        )
    })?;

    // Validate type compatibility.
    if const_inner.type_ != *expected_type {
        return Err(mismatch(
            format!("a constant of type `{expected_type}`"),
            format!("`{}` (type `{}`)", path, const_inner.type_),
        ));
    }

    Ok(Some(ConstValue::ConstRef(resolved_path)))
}

/// Look up a resolved `Type` in the type registry and return its
/// `PredefinedItem` if it is a predefined type (e.g. `u32`, `f64`, `str`).
fn predefined_for_type<'a>(
    type_registry: &'a TypeRegistry,
    type_: &Type,
) -> Option<&'a PredefinedItem> {
    let path = match type_ {
        Type::Raw(path) => path,
        _ => return None,
    };
    let item_def = type_registry.get(path, &ItemLocation::internal()).ok()?;
    item_def.predefined.as_ref()
}

/// Check if a resolved type is a predefined integer type (u8/i32/…).
fn type_is_predefined_integer(type_registry: &TypeRegistry, type_: &Type) -> bool {
    predefined_for_type(type_registry, type_).is_some_and(PredefinedItem::is_integer)
}

/// Check if a resolved type is a predefined float type (f32 or f64).
fn type_is_predefined_float(type_registry: &TypeRegistry, type_: &Type) -> bool {
    predefined_for_type(type_registry, type_).is_some_and(PredefinedItem::is_float)
}

/// Check if a resolved type is the predefined `str` type.
fn type_is_predefined_str(type_registry: &TypeRegistry, type_: &Type) -> bool {
    predefined_for_type(type_registry, type_).is_some_and(PredefinedItem::is_str)
}

/// Check if a resolved type is the predefined `cstr` type.
fn type_is_predefined_cstr(type_registry: &TypeRegistry, type_: &Type) -> bool {
    predefined_for_type(type_registry, type_).is_some_and(PredefinedItem::is_cstr)
}

/// Resolve an enum-value path (e.g. `Color::Red`, or module-qualified
/// `gfx::Color::Red`) and validate that the variant exists. Returns the
/// resolved absolute path of the enum type on success.
///
/// The final segment is the variant name; everything before it is the enum's
/// path, resolved through the scope (so it may be module-qualified).
fn resolve_enum_value(
    semantic: &ResolutionContextRef<'_>,
    scope: &[ItemPath],
    path: &ItemPath,
    resolvee_path: &ItemPath,
    expr_location: &ItemLocation,
) -> Result<ItemPath> {
    let mismatch = |expected: String| SemanticError::ConstValueTypeMismatch {
        item_path: resolvee_path.clone(),
        expected,
        found: path.to_string(),
        location: *expr_location,
    };

    // Split into `<enum-path>::<variant>`. Need at least an enum segment and a
    // variant segment.
    let variant_name = path
        .last()
        .map(|s| s.as_str())
        .ok_or_else(|| mismatch("an enum-value reference (EnumName::VariantName)".to_string()))?;
    let enum_grammar_path = path
        .parent()
        .filter(|p| !p.is_empty())
        .ok_or_else(|| mismatch("an enum-value reference (EnumName::VariantName)".to_string()))?;

    // Resolve the enum path through the scope (handles module-qualified paths).
    let enum_path = match semantic
        .type_registry
        .resolve_path(scope, &enum_grammar_path)
    {
        TypeLookupResult::Found(Type::Raw(p)) => p,
        _ => return Err(mismatch(format!("enum `{enum_grammar_path}`"))),
    };

    // Look up the enum in the type registry
    let enum_def = semantic
        .type_registry
        .get(&enum_path, &ItemLocation::internal())
        .map_err(|_| mismatch(format!("enum `{enum_grammar_path}`")))?;

    // Check that it's actually an enum and find the variant
    let enum_inner = match enum_def.resolved() {
        Some(r) => match r.inner.as_enum() {
            Some(e) => e,
            None => {
                return Err(mismatch(r.inner.human_friendly_type().to_string()));
            }
        },
        None => {
            return Err(mismatch(format!("enum `{enum_grammar_path}` (unresolved)")));
        }
    };

    // Find the matching variant
    if !enum_inner.variants.iter().any(|v| v.name == variant_name) {
        return Err(mismatch(format!(
            "variant `{variant_name}` in enum `{enum_grammar_path}`"
        )));
    }

    Ok(enum_path)
}
