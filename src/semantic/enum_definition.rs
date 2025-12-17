#[cfg(test)]
use crate::span::StripLocations;
use crate::{
    grammar::{self, ItemPath},
    semantic::{
        SemanticState, attribute,
        error::{
            BuildOutcome, Result, SemanticError, TypeResolutionContext, UnresolvedTypeReference,
        },
        type_registry::TypeLookupResult,
        types::{Function, ItemStateResolved, Type},
    },
    span::{HasLocation, ItemLocation},
};

/// A single variant in an enum definition
#[derive(PartialEq, Eq, Debug, Clone, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct EnumVariant {
    pub name: String,
    pub value: isize,
    pub location: ItemLocation,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct EnumDefinition {
    pub type_: Type,
    pub doc: Vec<String>,
    pub variants: Vec<EnumVariant>,
    pub associated_functions: Vec<Function>,
    pub singleton: Option<usize>,
    pub copyable: bool,
    pub cloneable: bool,
    pub default: Option<usize>,
}
#[cfg(test)]
impl EnumDefinition {
    pub fn new(type_: Type) -> Self {
        EnumDefinition {
            type_,
            doc: vec![],
            variants: Vec::new(),
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
    pub fn with_variants<'a>(
        mut self,
        variants: impl IntoIterator<Item = (&'a str, isize)>,
    ) -> Self {
        self.variants = variants
            .into_iter()
            .map(|(n, v)| EnumVariant {
                name: n.to_string(),
                value: v,
                location: ItemLocation::test(),
            })
            .collect();
        self
    }
    pub fn with_associated_functions(
        mut self,
        associated_functions: impl IntoIterator<Item = Function>,
    ) -> Self {
        self.associated_functions = associated_functions.into_iter().collect();
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
    location: &ItemLocation,
    doc_comments: &[String],
) -> Result<BuildOutcome> {
    let module = semantic.get_module_for_path(resolvee_path, location)?;

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
                    location: *definition.type_.location(),
                    context: format!("base type of enum `{resolvee_path}`"),
                }));
            }
        };

    // TODO: verify that `ty` actually makes sense for an enum
    let Some(size) = ty.size(&semantic.type_registry) else {
        return Ok(BuildOutcome::Deferred);
    };

    let mut variants: Vec<EnumVariant> = vec![];
    let mut last_value = 0;
    let mut default = None;
    for statement in definition.statements() {
        let grammar::EnumStatement {
            name,
            expr,
            attributes,
            location: stmt_location,
            ..
        } = statement;
        let value = match expr {
            Some(grammar::Expr::IntLiteral { value, .. }) => *value,
            Some(e) => {
                return Err(SemanticError::EnumUnsupportedValue {
                    item_path: resolvee_path.clone(),
                    case_name: name.0.clone(),
                    location: *e.location(),
                });
            }
            None => last_value,
        };
        variants.push(EnumVariant {
            name: name.0.clone(),
            value,
            location: *stmt_location,
        });

        for attribute in attributes {
            match attribute {
                grammar::Attribute::Ident { ident, .. } if ident.as_str() == "default" => {
                    if default.is_some() {
                        return Err(SemanticError::EnumMultipleDefaults {
                            item_path: resolvee_path.clone(),
                            location: *location,
                        });
                    }
                    default = Some(variants.len() - 1);
                }
                _ => {}
            }
        }

        last_value = value + 1;
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
        return Err(SemanticError::EnumDefaultWithoutDefaultable {
            item_path: resolvee_path.clone(),
            location: *location,
        });
    }

    if defaultable && default.is_none() {
        return Err(SemanticError::EnumDefaultableMissingDefault {
            item_path: resolvee_path.clone(),
            location: *location,
        });
    }

    // Handle associated functions
    let mut associated_functions = vec![];
    if let Some(enum_impl) = module.impls.get(resolvee_path) {
        for function in enum_impl.functions().collect::<Vec<_>>() {
            let function = match crate::semantic::function::build(
                &semantic.type_registry,
                &module.scope(),
                false,
                function,
            )? {
                crate::semantic::function::FunctionBuildOutcome::Built(f) => *f,
                crate::semantic::function::FunctionBuildOutcome::Deferred => {
                    return Ok(BuildOutcome::Deferred);
                }
            };
            associated_functions.push(function);
        }
    }

    Ok(BuildOutcome::Resolved(ItemStateResolved {
        size,
        alignment: ty.alignment(&semantic.type_registry).ok_or_else(|| {
            SemanticError::TypeResolutionFailed {
                type_: definition.type_.clone(),
                resolution_context: TypeResolutionContext::EnumBaseTypeAlignment {
                    enum_path: resolvee_path.clone(),
                },
                location: *location,
            }
        })?,
        inner: EnumDefinition {
            type_: ty,
            doc,
            variants,
            associated_functions,
            singleton,
            copyable,
            cloneable,
            default,
        }
        .into(),
    }))
}
