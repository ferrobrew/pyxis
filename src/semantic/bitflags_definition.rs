use anyhow::Context;

use crate::{
    grammar::{self, ItemPath},
    semantic::{
        SemanticState,
        types::{ItemStateResolved, Type},
    },
};

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct BitflagsDefinition {
    pub type_: Type,
    pub doc: Option<String>,
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
            doc: None,
            fields: Vec::new(),
            singleton: None,
            copyable: false,
            cloneable: false,
            default: None,
        }
    }
    pub fn with_doc(mut self, doc: impl Into<String>) -> Self {
        self.doc = Some(doc.into());
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
    pub fn doc(&self) -> Option<&str> {
        self.doc.as_deref()
    }
}

pub fn build(
    semantic: &SemanticState,
    resolvee_path: &ItemPath,
    definition: &grammar::BitflagsDefinition,
) -> anyhow::Result<Option<ItemStateResolved>> {
    let module = semantic
        .get_module_for_path(resolvee_path)
        .with_context(|| format!("failed to get module for path `{resolvee_path}`"))?;

    // Retrieve the type for this bitflags, and validate it, before getting its size
    let Some(ty) = semantic
        .type_registry
        .resolve_grammar_type(&module.scope(), &definition.type_)
    else {
        return Ok(None);
    };
    let ty_raw_path = ty.as_raw().with_context(|| {
        format!("bitflags definition `{resolvee_path}` has a type that is not a raw type: {ty}")
    })?;
    let Some(ty_item) = semantic.type_registry.get(ty_raw_path) else {
        return Ok(None);
    };
    let Some(predefined_item) = ty_item.predefined else {
        anyhow::bail!(
            "bitflags definition `{resolvee_path}` has a type that is not a predefined type: {ty}"
        );
    };
    if !predefined_item.is_unsigned_integer() {
        anyhow::bail!(
            "bitflags definition `{resolvee_path}` has a type that is not an unsigned integer: {ty}"
        );
    }
    let size = predefined_item.size();

    let mut fields: Vec<(String, usize)> = vec![];
    let mut default = None;
    for statement in &definition.statements {
        let grammar::BitflagsStatement {
            name,
            expr,
            attributes,
            doc_comments: _,
        } = &statement.node;
        let value = match &expr.node {
            grammar::Expr::IntLiteral(value) => *value,
            _ => anyhow::bail!(
                "unsupported bitflags value for case `{name}` of bitflags `{resolvee_path}`: {expr:?}"
            ),
        };
        fields.push((
            name.node.0.clone(),
            value.try_into().with_context(|| {
                format!("bitflags value `{value}` is too large for `{name}` of `{resolvee_path}`")
            })?,
        ));

        for attribute in attributes {
            match &attribute.node {
                grammar::Attribute::Ident(ident) if ident.as_str() == "default" => {
                    if default.is_some() {
                        anyhow::bail!("bitflags {resolvee_path} has multiple default values");
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
    let doc = definition.attributes.doc(resolvee_path)?;
    for attribute in &definition.attributes {
        match &attribute.node {
            grammar::Attribute::Ident(ident) => match ident.as_str() {
                "copyable" => {
                    copyable = true;
                    cloneable = true;
                }
                "cloneable" => cloneable = true,
                "defaultable" => defaultable = true,
                _ => {}
            },
            grammar::Attribute::Function(ident, exprs) => {
                if let ("singleton", [expr]) = (ident.as_str(), exprs.as_slice()) {
                    if let grammar::Expr::IntLiteral(value) = expr.node {
                        singleton = Some(value as usize);
                    }
                }
            }
            grammar::Attribute::Assign(_ident, _expr) => {}
        }
    }

    if !defaultable && default.is_some() {
        anyhow::bail!(
            "bitflags `{resolvee_path}` has a default value set but is not marked as defaultable"
        );
    }

    if defaultable && default.is_none() {
        anyhow::bail!(
            "bitflags `{resolvee_path}` is marked as defaultable but has no default value set"
        );
    }

    Ok(Some(ItemStateResolved {
        size,
        alignment: ty.alignment(&semantic.type_registry).with_context(|| {
            format!("failed to get alignment for base type of bitflags `{resolvee_path}`")
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
