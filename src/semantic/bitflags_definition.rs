use crate::{
    grammar::{self, ItemPath},
    semantic::{
        SemanticState,
        error::{
            BitflagsExpectedType, IntegerConversionContext, Result, SemanticError,
            TypeResolutionContext,
        },
        types::{ItemStateResolved, Type},
    },
    span::ItemLocation,
};

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
    definition: &grammar::BitflagsDefinition,
    doc_comments: &[String],
    location: ItemLocation,
) -> Result<Option<ItemStateResolved>> {
    let module = semantic
        .get_module_for_path(resolvee_path)
        .ok_or_else(|| SemanticError::module_not_found(resolvee_path.clone(), location.clone()))?;

    // Retrieve the type for this bitflags, and validate it, before getting its size
    let Some(ty) = semantic
        .type_registry
        .resolve_grammar_type(&module.scope(), &definition.type_)
    else {
        return Ok(None);
    };
    let ty_raw_path = ty.as_raw().ok_or_else(|| {
        SemanticError::bitflags_invalid_type(
            BitflagsExpectedType::RawType,
            ty.clone(),
            resolvee_path.clone(),
            location.clone(),
        )
    })?;
    let Some(ty_item) = semantic.type_registry.get(ty_raw_path) else {
        return Ok(None);
    };
    let Some(predefined_item) = ty_item.predefined else {
        return Err(SemanticError::bitflags_invalid_type(
            BitflagsExpectedType::PredefinedType,
            ty.clone(),
            resolvee_path.clone(),
            location.clone(),
        ));
    };
    if !predefined_item.is_unsigned_integer() {
        return Err(SemanticError::bitflags_invalid_type(
            BitflagsExpectedType::UnsignedInteger,
            ty.clone(),
            resolvee_path.clone(),
            location.clone(),
        ));
    }
    let size = predefined_item.size();

    let mut fields: Vec<(String, usize)> = vec![];
    let mut default = None;
    for statement in definition.statements() {
        let grammar::BitflagsStatement {
            name,
            expr,
            attributes,
            ..
        } = statement;
        let value = match expr {
            grammar::Expr::IntLiteral { value, .. } => *value,
            _ => {
                return Err(SemanticError::bitflags_unsupported_value(
                    resolvee_path.clone(),
                    name.0.clone(),
                    location.clone(),
                ));
            }
        };
        fields.push((
            name.0.clone(),
            value.try_into().map_err(|_| {
                SemanticError::integer_conversion(
                    value.to_string(),
                    "usize",
                    IntegerConversionContext::BitflagsValue {
                        field_name: name.0.clone(),
                        type_path: resolvee_path.clone(),
                    },
                    location.clone(),
                )
            })?,
        ));

        for attribute in attributes {
            match attribute {
                grammar::Attribute::Ident(ident) if ident.as_str() == "default" => {
                    if default.is_some() {
                        return Err(SemanticError::bitflags_multiple_defaults(
                            resolvee_path.clone(),
                            location.clone(),
                        ));
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
            grammar::Attribute::Ident(ident) => match ident.as_str() {
                "copyable" => {
                    copyable = true;
                    cloneable = true;
                }
                "cloneable" => cloneable = true,
                "defaultable" => defaultable = true,
                _ => {}
            },
            grammar::Attribute::Function(ident, items) => {
                let exprs = grammar::AttributeItem::extract_exprs(items);
                if let ("singleton", [grammar::Expr::IntLiteral { value, .. }]) =
                    (ident.as_str(), exprs.as_slice())
                {
                    singleton = Some(*value as usize);
                }
            }
            grammar::Attribute::Assign(_ident, _expr) => {}
        }
    }

    if !defaultable && default.is_some() {
        return Err(SemanticError::bitflags_default_without_defaultable(
            resolvee_path.clone(),
            location.clone(),
        ));
    }

    if defaultable && default.is_none() {
        return Err(SemanticError::bitflags_defaultable_missing_default(
            resolvee_path.clone(),
            location.clone(),
        ));
    }

    Ok(Some(ItemStateResolved {
        size,
        alignment: ty.alignment(&semantic.type_registry).ok_or_else(|| {
            SemanticError::type_resolution_failed(
                definition.type_.clone(),
                TypeResolutionContext::BitflagsBaseTypeAlignment {
                    bitflags_path: resolvee_path.clone(),
                },
                location.clone(),
            )
        })?,
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
