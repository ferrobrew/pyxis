use crate::{
    grammar::{self, ItemPath},
    semantic::{
        SemanticState,
        error::{Result, SemanticError},
        types::{Function, ItemStateResolved, Type},
    },
};

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct EnumDefinition {
    pub type_: Type,
    pub doc: Vec<String>,
    pub fields: Vec<(String, isize)>,
    pub associated_functions: Vec<Function>,
    pub singleton: Option<usize>,
    pub copyable: bool,
    pub cloneable: bool,
    pub default: Option<usize>,
}
impl EnumDefinition {
    pub fn new(type_: Type) -> Self {
        EnumDefinition {
            type_,
            doc: vec![],
            fields: Vec::new(),
            associated_functions: Vec::new(),
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
    pub fn with_fields<'a>(mut self, fields: impl IntoIterator<Item = (&'a str, isize)>) -> Self {
        self.fields = fields
            .into_iter()
            .map(|(n, v)| (n.to_string(), v))
            .collect();
        self
    }
    pub fn with_associated_functions(
        mut self,
        associated_functions: impl Into<Vec<Function>>,
    ) -> Self {
        self.associated_functions = associated_functions.into();
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
    definition: &grammar::EnumDefinition,
    doc_comments: &[String],
) -> Result<Option<ItemStateResolved>> {
    let module = semantic
        .get_module_for_path(resolvee_path)
        .ok_or_else(|| SemanticError::module_not_found(resolvee_path.clone()))?;

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
    let mut default = None;
    for statement in definition.statements() {
        let grammar::EnumStatement {
            name,
            expr,
            attributes,
            ..
        } = statement;
        let value = match expr {
            Some(grammar::Expr::IntLiteral { value, .. }) => *value,
            Some(_) => {
                return Err(SemanticError::enum_error(
                    resolvee_path.clone(),
                    format!("unsupported enum value for case `{name}`: {:?}", expr),
                ));
            }
            None => last_field,
        };
        fields.push((name.0.clone(), value));

        for attribute in attributes {
            match attribute {
                grammar::Attribute::Ident(ident) if ident.as_str() == "default" => {
                    if default.is_some() {
                        return Err(SemanticError::enum_error(
                            resolvee_path.clone(),
                            "enum has multiple default variants",
                        ));
                    }
                    default = Some(fields.len() - 1);
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
        let mut error = SemanticError::enum_error(
            resolvee_path.clone(),
            "has a default variant set but is not marked as defaultable",
        );
        if let SemanticError::EnumError { context, .. } = &mut error {
            *context = context.clone()
                .with_help("Add the #[defaultable] attribute to the enum declaration")
                .with_note("Only enums marked as defaultable can have default variants");
        }
        return Err(error);
    }

    if defaultable && default.is_none() {
        let mut error = SemanticError::enum_error(
            resolvee_path.clone(),
            "is marked as defaultable but has no default variant set",
        );
        if let SemanticError::EnumError { context, .. } = &mut error {
            *context = context.clone()
                .with_help("Add the #[default] attribute to one of the enum variants")
                .with_note("Defaultable enums must have exactly one variant marked with #[default]");
        }
        return Err(error);
    }

    // Handle associated functions
    let mut associated_functions = vec![];
    if let Some(enum_impl) = module.impls.get(resolvee_path) {
        for function in enum_impl.functions().collect::<Vec<_>>() {
            let function = crate::semantic::function::build(
                &semantic.type_registry,
                &module.scope(),
                false,
                function,
            )?;
            associated_functions.push(function);
        }
    }

    Ok(Some(ItemStateResolved {
        size,
        alignment: ty.alignment(&semantic.type_registry).ok_or_else(|| {
            SemanticError::type_resolution_failed(
                format!("{:?}", ty),
                format!("alignment for base type of enum `{resolvee_path}`"),
            )
        })?,
        inner: EnumDefinition {
            type_: ty,
            doc,
            fields,
            associated_functions,
            singleton,
            copyable,
            cloneable,
            default,
        }
        .into(),
    }))
}
