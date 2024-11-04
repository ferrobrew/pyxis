use anyhow::Context;

use crate::{
    grammar::{self, ItemPath},
    semantic::{
        types::{ItemStateResolved, Type},
        SemanticState,
    },
};

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct EnumDefinition {
    pub type_: Type,
    pub doc: Option<String>,
    pub fields: Vec<(String, isize)>,
    pub singleton: Option<usize>,
    pub copyable: bool,
    pub cloneable: bool,
    pub defaultable: bool,
    pub bitflags: bool,
    pub default_index: Option<usize>,
}
impl EnumDefinition {
    pub fn new(type_: Type) -> Self {
        EnumDefinition {
            type_,
            doc: None,
            fields: Vec::new(),
            singleton: None,
            copyable: false,
            cloneable: false,
            defaultable: false,
            bitflags: false,
            default_index: None,
        }
    }
    pub fn with_doc(mut self, doc: impl Into<String>) -> Self {
        self.doc = Some(doc.into());
        self
    }
    pub fn with_fields<'a>(mut self, fields: impl IntoIterator<Item = (&'a str, isize)>) -> Self {
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
    pub fn with_defaultable(mut self, defaultable: bool) -> Self {
        self.defaultable = defaultable;
        self
    }
    pub fn with_bitflags(mut self, bitflags: bool) -> Self {
        self.bitflags = bitflags;
        self
    }
    pub fn with_default_index(mut self, default_index: usize) -> Self {
        self.default_index = Some(default_index);
        self
    }
    pub fn doc(&self) -> Option<&str> {
        self.doc.as_deref()
    }
}

pub fn build(
    semantic: &SemanticState,
    resolvee_path: &ItemPath,
    definition: &grammar::EnumDefinition,
) -> anyhow::Result<Option<ItemStateResolved>> {
    let module = semantic
        .get_module_for_path(resolvee_path)
        .with_context(|| format!("failed to get module for path `{resolvee_path}`"))?;

    let Some(ty) = semantic
        .type_registry
        .resolve_grammar_type(&module.scope(), &definition.type_)
    else {
        return Ok(None);
    };

    // TODO: verify that `ty` actually makes sense for an enum
    let Some(size) = ty.size(&semantic.type_registry) else {
        return Ok(None);
    };

    let mut fields: Vec<(String, isize)> = vec![];
    let mut last_field = 0;
    let mut default_index = None;
    for statement in &definition.statements {
        let grammar::EnumStatement {
            name,
            expr,
            attributes,
        } = statement;
        let value = match expr {
            Some(grammar::Expr::IntLiteral(value)) => *value,
            Some(_) => anyhow::bail!(
                "unsupported enum value for case `{name}` of enum `{resolvee_path}`: {expr:?}"
            ),
            None => last_field,
        };
        fields.push((name.0.clone(), value));

        for attribute in attributes {
            match attribute {
                grammar::Attribute::Ident(ident) if ident.as_str() == "default" => {
                    if default_index.is_some() {
                        anyhow::bail!("enum {resolvee_path} has multiple default variants");
                    }
                    default_index = Some(fields.len() - 1);
                }
                _ => {}
            }
        }

        last_field = value + 1;
    }

    let mut singleton = None;
    let mut copyable = false;
    let mut cloneable = false;
    let mut defaultable = false;
    let mut bitflags = false;
    let doc = definition.attributes.doc(resolvee_path)?;
    for attribute in &definition.attributes {
        match attribute {
            grammar::Attribute::Ident(ident) => match ident.as_str() {
                "copyable" => {
                    copyable = true;
                    cloneable = true;
                }
                "cloneable" => cloneable = true,
                "defaultable" => defaultable = true,
                "bitflags" => bitflags = true,
                _ => {}
            },
            grammar::Attribute::Function(ident, exprs) => {
                if let ("singleton", [grammar::Expr::IntLiteral(value)]) =
                    (ident.as_str(), exprs.as_slice())
                {
                    singleton = Some(*value as usize);
                }
            }
            grammar::Attribute::Assign(_ident, _expr) => {}
        }
    }

    if bitflags && default_index.is_some() {
        anyhow::bail!("enum `{resolvee_path}` has a default variant set but is marked as bitflags");
    }

    if !defaultable && default_index.is_some() {
        anyhow::bail!(
            "enum `{resolvee_path}` has a default variant set but is not marked as defaultable"
        );
    }

    if !bitflags && defaultable && default_index.is_none() {
        anyhow::bail!(
            "enum `{resolvee_path}` is marked as defaultable but has no default variant set"
        );
    }

    Ok(Some(ItemStateResolved {
        size,
        alignment: ty.alignment(&semantic.type_registry).with_context(|| {
            format!("failed to get alignment for base type of enum `{resolvee_path}`")
        })?,
        inner: EnumDefinition {
            type_: ty,
            doc,
            fields,
            singleton,
            copyable,
            cloneable,
            bitflags,
            defaultable,
            default_index,
        }
        .into(),
    }))
}
