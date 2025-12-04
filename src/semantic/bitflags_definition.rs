use crate::{
    grammar::{self, ItemPath},
    semantic::{
        SemanticState, attribute,
        error::{BitflagsExpectedType, Result, SemanticError, TypeResolutionContext},
        types::{ItemStateResolved, Type},
    },
    span::{HasLocation, Located},
};

#[cfg(test)]
use crate::span::StripLocations;

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct BitflagsDefinition {
    pub type_: Type,
    pub doc: Vec<String>,
    pub fields: Vec<(String, usize)>,
    pub singleton: Option<usize>,
    pub copyable: bool,
    pub cloneable: bool,
    pub default: Option<usize>,
}
#[cfg(test)]
impl StripLocations for BitflagsDefinition {
    fn strip_locations(&self) -> Self {
        BitflagsDefinition {
            type_: self.type_.strip_locations(),
            doc: self.doc.strip_locations(),
            fields: self.fields.strip_locations(),
            singleton: self.singleton.strip_locations(),
            copyable: self.copyable.strip_locations(),
            cloneable: self.cloneable.strip_locations(),
            default: self.default.strip_locations(),
        }
    }
}
impl BitflagsDefinition {
    pub fn new(type_: Type) -> Self {
        BitflagsDefinition {
            type_,
            doc: vec![],
            fields: Vec::new(),
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
    pub fn with_fields<'a>(mut self, fields: impl IntoIterator<Item = (&'a str, usize)>) -> Self {
        self.fields = fields
            .into_iter()
            .map(|(n, v)| (n.to_string(), v))
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
    pub fn doc(&self) -> &[String] {
        &self.doc
    }
}

pub fn build(
    semantic: &SemanticState,
    resolvee_path: &ItemPath,
    definition: Located<&grammar::BitflagsDefinition>,
    doc_comments: &[String],
) -> Result<Option<ItemStateResolved>> {
    let module = semantic.get_module_for_path(resolvee_path, &definition.location)?;

    let type_location = definition.type_.location().clone();

    // Retrieve the type for this bitflags, and validate it, before getting its size
    let Some(ty) = semantic
        .type_registry
        .resolve_grammar_type(&module.scope(), &definition.type_)
    else {
        return Ok(None);
    };
    let ty_raw_path = ty
        .as_raw()
        .ok_or_else(|| SemanticError::BitflagsInvalidType {
            expected: BitflagsExpectedType::RawType,
            found: ty.clone(),
            item_path: resolvee_path.clone(),
            location: type_location.clone(),
        })?;
    let Ok(ty_item) = semantic.type_registry.get(ty_raw_path, &type_location) else {
        return Ok(None);
    };
    let Some(predefined_item) = ty_item.predefined else {
        return Err({
            SemanticError::BitflagsInvalidType {
                expected: BitflagsExpectedType::PredefinedType,
                found: ty.clone(),
                item_path: resolvee_path.clone(),
                location: type_location.clone(),
            }
        });
    };
    if !predefined_item.is_unsigned_integer() {
        return Err({
            SemanticError::BitflagsInvalidType {
                expected: BitflagsExpectedType::UnsignedInteger,
                found: ty.clone(),
                item_path: resolvee_path.clone(),
                location: type_location.clone(),
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
            location: type_location.clone(),
        })?;

    let mut fields: Vec<(String, usize)> = vec![];
    let mut default = None;
    for statement in definition.statements() {
        let grammar::BitflagsStatement {
            name,
            expr,
            attributes,
            ..
        } = &statement.value;
        let value = match expr {
            grammar::Expr::IntLiteral { value, .. } => *value,
            _ => {
                return Err(SemanticError::BitflagsUnsupportedValue {
                    item_path: resolvee_path.clone(),
                    case_name: name.0.clone(),
                    location: expr.location().clone(),
                });
            }
        };
        fields.push((
            name.0.clone(),
            value
                .try_into()
                .map_err(|_| SemanticError::IntegerConversion {
                    value: value.to_string(),
                    target_type: "usize".into(),
                    location: expr.location().clone(),
                })?,
        ));

        for attribute in attributes {
            match attribute {
                grammar::Attribute::Ident { ident, .. } if ident.as_str() == "default" => {
                    if default.is_some() {
                        return Err(SemanticError::BitflagsMultipleDefaults {
                            item_path: resolvee_path.clone(),
                            location: definition.location.clone(),
                        });
                    }
                    default = Some(fields.len() - 1);
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
            location: definition.location.clone(),
        });
    }

    if defaultable && default.is_none() {
        return Err(SemanticError::BitflagsDefaultableMissingDefault {
            item_path: resolvee_path.clone(),
            location: definition.location.clone(),
        });
    }

    Ok(Some(ItemStateResolved {
        size,
        alignment,
        inner: BitflagsDefinition {
            type_: ty,
            doc,
            fields,
            singleton,
            copyable,
            cloneable,
            default,
        }
        .into(),
    }))
}
