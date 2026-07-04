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
    let value = match &definition.expr {
        grammar::Expr::IntLiteral { value, .. } => {
            if !type_is_predefined_integer(semantic.type_registry, &resolved_type) {
                return Err(SemanticError::ConstValueTypeMismatch {
                    item_path: resolvee_path.clone(),
                    expected: "an integer type".to_string(),
                    found: format!("{resolved_type}"),
                    location: *definition.expr.location(),
                });
            }
            ConstValue::Int(*value)
        }
        grammar::Expr::FloatLiteral { raw_text, .. } => {
            if !type_is_predefined_float(semantic.type_registry, &resolved_type) {
                return Err(SemanticError::ConstValueTypeMismatch {
                    item_path: resolvee_path.clone(),
                    expected: "a float type (f32 or f64)".to_string(),
                    found: format!("{resolved_type}"),
                    location: *definition.expr.location(),
                });
            }
            let f: f64 = raw_text
                .parse()
                .map_err(|_| SemanticError::ConstValueTypeMismatch {
                    item_path: resolvee_path.clone(),
                    expected: "a valid float".to_string(),
                    found: raw_text.clone(),
                    location: *definition.expr.location(),
                })?;
            ConstValue::Float(f.to_bits())
        }
        grammar::Expr::StringLiteral { value, .. } => {
            if !type_is_predefined_str(semantic.type_registry, &resolved_type) {
                return Err(SemanticError::ConstValueTypeMismatch {
                    item_path: resolvee_path.clone(),
                    expected: "`str`".to_string(),
                    found: format!("{resolved_type}"),
                    location: *definition.expr.location(),
                });
            }
            ConstValue::String(value.clone())
        }
        grammar::Expr::Path { path, .. } => {
            // Enum-value reference: resolve the path against enum variants.
            // The path is `<enum-path>::VariantName`, where `<enum-path>` may be
            // module-qualified (e.g. `gfx::Color::Red`).
            let enum_type_path = resolve_enum_value(
                semantic,
                &scope,
                path,
                resolvee_path,
                definition.expr.location(),
            )?;

            // The const's type should be the enum type (Type::Raw pointing to the enum).
            if resolved_type != Type::Raw(enum_type_path.clone()) {
                return Err(SemanticError::ConstValueTypeMismatch {
                    item_path: resolvee_path.clone(),
                    expected: format!("enum `{enum_type_path}`"),
                    found: format!("{resolved_type}"),
                    location: *definition.expr.location(),
                });
            }

            ConstValue::EnumValue(path.clone())
        }
        _ => {
            return Err(SemanticError::ConstValueTypeMismatch {
                item_path: resolvee_path.clone(),
                expected: "a literal or enum-value reference".to_string(),
                found: "an unsupported expression".to_string(),
                location: *definition.expr.location(),
            });
        }
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
