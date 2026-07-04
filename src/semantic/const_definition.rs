use crate::{
    grammar::{self, ItemPath},
    semantic::{
        error::{
            BuildOutcome, Result, SemanticError, UnresolvedTypeContext, UnresolvedTypeReference,
        },
        resolution_context::ResolutionContextRef,
        type_registry::TypeLookupResult,
        types::{ConstDefinition as SemanticConstDefinition, ConstValue, ItemStateResolved, Type},
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
            if !is_integer_type(&resolved_type) {
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
            if !is_float_type(&resolved_type) {
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
            if !is_str_type(&resolved_type) {
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
            // The path should be `EnumName::VariantName`.
            resolve_enum_value(
                semantic,
                &scope,
                path,
                resolvee_path,
                definition.expr.location(),
            )?;

            // Validate that the const's type annotation matches the enum type.
            // The path is `EnumName::VariantName`; the enum is the first segment.
            let enum_name = path.iter().next().unwrap().as_str();
            let enum_type_path = match semantic.type_registry.resolve_string(&scope, enum_name) {
                TypeLookupResult::Found(Type::Raw(p)) => p,
                _ => {
                    return Err(SemanticError::ConstValueTypeMismatch {
                        item_path: resolvee_path.clone(),
                        expected: format!("enum `{enum_name}`"),
                        found: path.to_string(),
                        location: *definition.expr.location(),
                    });
                }
            };
            // The const's type should be the enum type (Type::Raw pointing to the enum)
            if resolved_type != Type::Raw(enum_type_path.clone()) {
                return Err(SemanticError::ConstValueTypeMismatch {
                    item_path: resolvee_path.clone(),
                    expected: format!("enum `{enum_name}` ({enum_type_path})"),
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

/// Check if a resolved type is an integer type by looking up the predefined item.
fn is_integer_type(type_: &Type) -> bool {
    match type_ {
        Type::Raw(path) if path.len() == 1 => {
            let name = path.iter().next().unwrap().as_str();
            matches!(
                name,
                "u8" | "u16" | "u32" | "u64" | "u128" | "i8" | "i16" | "i32" | "i64" | "i128"
            )
        }
        _ => false,
    }
}

/// Check if a resolved type is a float type (f32 or f64).
fn is_float_type(type_: &Type) -> bool {
    match type_ {
        Type::Raw(path) if path.len() == 1 => {
            let name = path.iter().next().unwrap().as_str();
            matches!(name, "f32" | "f64")
        }
        _ => false,
    }
}

/// Check if a resolved type is `str`.
fn is_str_type(type_: &Type) -> bool {
    match type_ {
        Type::Raw(path) if path.len() == 1 => path.iter().next().unwrap().as_str() == "str",
        _ => false,
    }
}

/// Resolve an enum-value path (e.g., `Color::Red`) and validate it exists.
fn resolve_enum_value(
    semantic: &ResolutionContextRef<'_>,
    scope: &[ItemPath],
    path: &ItemPath,
    resolvee_path: &ItemPath,
    expr_location: &ItemLocation,
) -> Result<()> {
    let segments: Vec<&str> = path.iter().map(|s| s.as_str()).collect();
    if segments.len() != 2 {
        return Err(SemanticError::ConstValueTypeMismatch {
            item_path: resolvee_path.clone(),
            expected: "an enum-value reference (EnumName::VariantName)".to_string(),
            found: path.to_string(),
            location: *expr_location,
        });
    }

    let enum_name = segments[0];
    let variant_name = segments[1];

    // Resolve the enum name through the scope using the type registry
    let enum_path = match semantic.type_registry.resolve_string(scope, enum_name) {
        TypeLookupResult::Found(Type::Raw(p)) => p,
        _ => {
            return Err(SemanticError::ConstValueTypeMismatch {
                item_path: resolvee_path.clone(),
                expected: format!("enum `{enum_name}`"),
                found: path.to_string(),
                location: *expr_location,
            });
        }
    };

    // Look up the enum in the type registry
    let enum_def = semantic
        .type_registry
        .get(&enum_path, &ItemLocation::internal())
        .map_err(|_| SemanticError::ConstValueTypeMismatch {
            item_path: resolvee_path.clone(),
            expected: format!("enum `{enum_name}`"),
            found: path.to_string(),
            location: *expr_location,
        })?;

    // Check that it's actually an enum and find the variant
    let enum_inner = match enum_def.resolved() {
        Some(r) => match r.inner.as_enum() {
            Some(e) => e,
            None => {
                return Err(SemanticError::ConstValueTypeMismatch {
                    item_path: resolvee_path.clone(),
                    expected: format!("enum `{enum_name}`"),
                    found: r.inner.human_friendly_type().to_string(),
                    location: *expr_location,
                });
            }
        },
        None => {
            return Err(SemanticError::ConstValueTypeMismatch {
                item_path: resolvee_path.clone(),
                expected: format!("enum `{enum_name}` (unresolved)"),
                found: path.to_string(),
                location: *expr_location,
            });
        }
    };

    // Find the matching variant
    if !enum_inner.variants.iter().any(|v| v.name == variant_name) {
        return Err(SemanticError::ConstValueTypeMismatch {
            item_path: resolvee_path.clone(),
            expected: format!("variant `{variant_name}` in enum `{enum_name}`"),
            found: path.to_string(),
            location: *expr_location,
        });
    }

    Ok(())
}
