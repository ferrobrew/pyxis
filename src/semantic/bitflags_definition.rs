use crate::{
    grammar::{self, ItemPath},
    semantic::{
        SemanticState, attribute,
        error::{
            BitflagsExpectedType, BuildOutcome, Result, SemanticError, TypeResolutionContext,
            UnresolvedTypeReference,
        },
        type_registry::TypeLookupResult,
        types::{ItemStateResolved, Type},
    },
    span::{HasLocation, ItemLocation},
};

#[cfg(test)]
use crate::span::StripLocations;

/// A single flag in a bitflags definition
#[derive(PartialEq, Eq, Debug, Clone, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct BitflagField {
    pub name: String,
    pub value: usize,
    pub location: ItemLocation,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct BitflagsDefinition {
    pub type_: Type,
    pub doc: Vec<String>,
    pub flags: Vec<BitflagField>,
    pub singleton: Option<usize>,
    pub copyable: bool,
    pub cloneable: bool,
    pub default: Option<usize>,
}
#[cfg(test)]
impl BitflagsDefinition {
    pub fn new(type_: Type) -> Self {
        BitflagsDefinition {
            type_,
            doc: vec![],
            flags: Vec::new(),
            singleton: None,
            copyable: false,
            cloneable: false,
            default: None,
        }
    }
    pub fn with_doc(mut self, doc: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.doc = doc.into_iter().map(|s| s.into()).collect();
        self
    }
    pub fn with_flags<'a>(mut self, flags: impl IntoIterator<Item = (&'a str, usize)>) -> Self {
        self.flags = flags
            .into_iter()
            .map(|(n, v)| BitflagField {
                name: n.to_string(),
                value: v,
                location: ItemLocation::test(),
            })
            .collect();
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
    pub fn with_default(mut self, default: usize) -> Self {
        self.default = Some(default);
        self
    }
}
impl BitflagsDefinition {
    pub fn doc(&self) -> &[String] {
        &self.doc
    }
}

pub fn build(
    semantic: &SemanticState,
    resolvee_path: &ItemPath,
    definition: &grammar::BitflagsDefinition,
    location: &ItemLocation,
    doc_comments: &[String],
) -> Result<BuildOutcome> {
    let module = semantic.get_module_for_path(resolvee_path, location)?;

    let type_location = *definition.type_.location();

    // Retrieve the type for this bitflags, and validate it, before getting its size
    let ty =
        match semantic
            .type_registry
            .resolve_grammar_type(&module.scope(), &definition.type_, &[])
        {
            TypeLookupResult::Found(t) => t,
            TypeLookupResult::NotYetResolved => return Ok(BuildOutcome::Deferred),
            TypeLookupResult::NotFound { type_name } => {
                return Ok(BuildOutcome::NotFoundType(UnresolvedTypeReference {
                    type_name,
                    location: type_location,
                    context: format!("base type of bitflags `{resolvee_path}`"),
                }));
            }
            TypeLookupResult::PrivateAccess { item_path } => {
                return Ok(BuildOutcome::NotFoundType(UnresolvedTypeReference {
                    type_name: item_path.to_string(),
                    location: type_location,
                    context: format!("base type of bitflags `{resolvee_path}`"),
                }));
            }
        };
    let ty_raw_path = ty
        .as_raw()
        .ok_or_else(|| SemanticError::BitflagsInvalidType {
            expected: BitflagsExpectedType::RawType,
            found: ty.clone(),
            item_path: resolvee_path.clone(),
            location: type_location,
        })?;
    let Ok(ty_item) = semantic.type_registry.get(ty_raw_path, &type_location) else {
        return Ok(BuildOutcome::Deferred);
    };
    let Some(predefined_item) = ty_item.predefined else {
        return Err({
            SemanticError::BitflagsInvalidType {
                expected: BitflagsExpectedType::PredefinedType,
                found: ty.clone(),
                item_path: resolvee_path.clone(),
                location: type_location,
            }
        });
    };
    if !predefined_item.is_unsigned_integer() {
        return Err({
            SemanticError::BitflagsInvalidType {
                expected: BitflagsExpectedType::UnsignedInteger,
                found: ty.clone(),
                item_path: resolvee_path.clone(),
                location: type_location,
            }
        });
    }
    let size = predefined_item.size();
    let alignment = ty_item
        .alignment()
        .ok_or_else(|| SemanticError::TypeResolutionFailed {
            type_: definition.type_.clone(),
            resolution_context: TypeResolutionContext::BitflagsBaseTypeAlignment {
                bitflags_path: resolvee_path.clone(),
            },
            location: type_location,
        })?;

    let mut flags: Vec<BitflagField> = vec![];
    let mut default = None;
    for statement in definition.statements() {
        let grammar::BitflagsStatement {
            name,
            expr,
            attributes,
            location: stmt_location,
            ..
        } = statement;
        let value = match expr {
            grammar::Expr::IntLiteral { value, .. } => *value,
            _ => {
                return Err(SemanticError::BitflagsUnsupportedValue {
                    item_path: resolvee_path.clone(),
                    case_name: name.0.clone(),
                    location: *expr.location(),
                });
            }
        };
        flags.push(BitflagField {
            name: name.0.clone(),
            value: value
                .try_into()
                .map_err(|_| SemanticError::IntegerConversion {
                    value: value.to_string(),
                    target_type: "usize".into(),
                    location: *expr.location(),
                })?,
            location: *stmt_location,
        });

        for attribute in attributes {
            match attribute {
                grammar::Attribute::Ident { ident, .. } if ident.as_str() == "default" => {
                    if default.is_some() {
                        return Err(SemanticError::BitflagsMultipleDefaults {
                            item_path: resolvee_path.clone(),
                            location: *location,
                        });
                    }
                    default = Some(flags.len() - 1);
                }
                _ => {}
            }
        }
    }

    let mut singleton = None;
    let mut copyable = false;
    let mut cloneable = false;
    let mut defaultable = false;
    let doc = doc_comments.to_vec();
    for attribute in &definition.attributes {
        match attribute {
            grammar::Attribute::Ident { ident, .. } => match ident.as_str() {
                "copyable" => {
                    copyable = true;
                    cloneable = true;
                }
                "cloneable" => cloneable = true,
                "defaultable" => defaultable = true,
                _ => {}
            },
            grammar::Attribute::Function { name, items, .. } => {
                if let Some(attr_singleton) =
                    attribute::parse_singleton(name, items, attribute.location())?
                {
                    singleton = Some(attr_singleton);
                }
            }
            grammar::Attribute::Assign { .. } => {}
        }
    }

    if !defaultable && default.is_some() {
        return Err(SemanticError::BitflagsDefaultWithoutDefaultable {
            item_path: resolvee_path.clone(),
            location: *location,
        });
    }

    if defaultable && default.is_none() {
        return Err(SemanticError::BitflagsDefaultableMissingDefault {
            item_path: resolvee_path.clone(),
            location: *location,
        });
    }

    Ok(BuildOutcome::Resolved(ItemStateResolved {
        size,
        alignment,
        inner: BitflagsDefinition {
            type_: ty,
            doc,
            flags,
            singleton,
            copyable,
            cloneable,
            default,
        }
        .into(),
    }))
}
