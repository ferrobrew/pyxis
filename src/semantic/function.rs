use std::{fmt, str::FromStr};

use crate::{
    grammar::{self, Expr, ItemPath},
    semantic::{
        attribute,
        error::{AttributeNotSupportedContext, Result, SemanticError, TypeResolutionContext},
        type_registry::TypeRegistry,
        types::{Type, Visibility},
    },
    span::{EqualsIgnoringLocations, HasLocation, Located},
};

#[cfg(test)]
use crate::span::StripLocations;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Argument {
    ConstSelf,
    MutSelf,
    Field(String, Type),
}
#[cfg(test)]
impl StripLocations for Argument {
    fn strip_locations(&self) -> Self {
        match self {
            Argument::ConstSelf => Argument::ConstSelf,
            Argument::MutSelf => Argument::MutSelf,
            Argument::Field(name, ty) => {
                Argument::Field(name.strip_locations(), ty.strip_locations())
            }
        }
    }
}
impl EqualsIgnoringLocations for Argument {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        match (self, other) {
            (Argument::ConstSelf, Argument::ConstSelf) => true,
            (Argument::MutSelf, Argument::MutSelf) => true,
            (Argument::Field(name, ty), Argument::Field(name2, ty2)) => {
                name.equals_ignoring_locations(name2) && ty.equals_ignoring_locations(ty2)
            }
            _ => false,
        }
    }
}
impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Argument::ConstSelf => write!(f, "&self"),
            Argument::MutSelf => write!(f, "&mut self"),
            Argument::Field(name, ty) => write!(f, "{name}: {ty}"),
        }
    }
}
#[cfg(test)]
impl Argument {
    pub fn const_self() -> Self {
        Argument::ConstSelf
    }
    pub fn mut_self() -> Self {
        Argument::MutSelf
    }
    pub fn field(name: impl Into<String>, type_ref: impl Into<Type>) -> Self {
        Argument::Field(name.into(), type_ref.into())
    }
}
impl Argument {
    pub fn is_self(&self) -> bool {
        matches!(self, Argument::ConstSelf | Argument::MutSelf)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum CallingConvention {
    C,
    Cdecl,
    Stdcall,
    Fastcall,
    Thiscall,
    Vectorcall,
    System,
}
#[cfg(test)]
impl StripLocations for CallingConvention {
    fn strip_locations(&self) -> Self {
        *self
    }
}
impl EqualsIgnoringLocations for CallingConvention {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self == other
    }
}
impl CallingConvention {
    pub const ALL: &'static [CallingConvention] = &[
        CallingConvention::C,
        CallingConvention::Cdecl,
        CallingConvention::Stdcall,
        CallingConvention::Fastcall,
        CallingConvention::Thiscall,
        CallingConvention::Vectorcall,
        CallingConvention::System,
    ];

    pub fn as_str(&self) -> &'static str {
        match self {
            CallingConvention::C => "C",
            CallingConvention::Cdecl => "cdecl",
            CallingConvention::Stdcall => "stdcall",
            CallingConvention::Fastcall => "fastcall",
            CallingConvention::Thiscall => "thiscall",
            CallingConvention::Vectorcall => "vectorcall",
            CallingConvention::System => "system",
        }
    }

    // Assume that if the function is 4-byte pointer-sized, it's a thiscall function, otherwise it's "system"
    //
    // This is very sus; we should probably have global configuration per-project for this kind of thing
    pub fn for_member_function(pointer_size: usize) -> Self {
        if pointer_size == 4 {
            CallingConvention::Thiscall
        } else {
            CallingConvention::System
        }
    }
}
impl fmt::Display for CallingConvention {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
impl FromStr for CallingConvention {
    type Err = ();
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "C" => Ok(CallingConvention::C),
            "cdecl" => Ok(CallingConvention::Cdecl),
            "stdcall" => Ok(CallingConvention::Stdcall),
            "fastcall" => Ok(CallingConvention::Fastcall),
            "thiscall" => Ok(CallingConvention::Thiscall),
            "vectorcall" => Ok(CallingConvention::Vectorcall),
            "system" => Ok(CallingConvention::System),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FunctionBody {
    Address {
        address: usize,
    },
    Field {
        field: String,
        /// The function to call on this field. *Usually* the same as
        /// the original function's name, but functions can be renamed
        /// for inheritance reasons
        function_name: String,
    },
    Vftable {
        /// The function to call on this field. *Usually* the same as
        /// the original function's name, but functions can be renamed
        /// for inheritance reasons
        function_name: String,
    },
}
#[cfg(test)]
impl StripLocations for FunctionBody {
    fn strip_locations(&self) -> Self {
        match self {
            FunctionBody::Address { address } => FunctionBody::Address {
                address: address.strip_locations(),
            },
            FunctionBody::Field {
                field,
                function_name,
            } => FunctionBody::Field {
                field: field.strip_locations(),
                function_name: function_name.strip_locations(),
            },
            FunctionBody::Vftable { function_name } => FunctionBody::Vftable {
                function_name: function_name.strip_locations(),
            },
        }
    }
}
impl EqualsIgnoringLocations for FunctionBody {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        match (self, other) {
            (FunctionBody::Address { address }, FunctionBody::Address { address: address2 }) => {
                address.equals_ignoring_locations(address2)
            }
            (
                FunctionBody::Field {
                    field,
                    function_name,
                },
                FunctionBody::Field {
                    field: field2,
                    function_name: function_name2,
                },
            ) => {
                field.equals_ignoring_locations(field2)
                    && function_name.equals_ignoring_locations(function_name2)
            }
            (
                FunctionBody::Vftable { function_name },
                FunctionBody::Vftable {
                    function_name: function_name2,
                },
            ) => function_name.equals_ignoring_locations(function_name2),
            _ => false,
        }
    }
}
#[cfg(test)]
impl FunctionBody {
    pub fn address(address: usize) -> Self {
        FunctionBody::Address { address }
    }
    pub fn field(field: impl Into<String>, function_name: impl Into<String>) -> Self {
        FunctionBody::Field {
            field: field.into(),
            function_name: function_name.into(),
        }
    }
    pub fn vftable(function_name: impl Into<String>) -> Self {
        FunctionBody::Vftable {
            function_name: function_name.into(),
        }
    }
}
impl FunctionBody {
    pub fn is_field(&self) -> bool {
        matches!(self, FunctionBody::Field { .. })
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub visibility: Visibility,
    pub name: String,
    pub doc: Vec<String>,
    pub body: FunctionBody,
    pub arguments: Vec<Located<Argument>>,
    pub return_type: Option<Type>,
    pub calling_convention: CallingConvention,
}
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.doc.is_empty() {
            let doc = self.doc.join("\n");
            write!(f, "#[doc = r#{doc:?}#] ")?;
        }
        match self.visibility {
            Visibility::Public => write!(f, "pub "),
            Visibility::Private => Ok(()),
        }?;
        write!(f, "extern \"{}\" ", self.calling_convention)?;
        write!(f, "fn {}(", self.name)?;
        for (i, arg) in self.arguments.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{arg}")?;
        }
        write!(f, ")")?;
        if let Some(ty) = &self.return_type {
            write!(f, " -> {ty}")?;
        }
        write!(f, " = ")?;
        match &self.body {
            FunctionBody::Address { address } => write!(f, "0x{address:X})")?,
            FunctionBody::Field {
                field,
                function_name,
            } => write!(f, "self.{field}.{function_name}")?,
            FunctionBody::Vftable { function_name } => write!(f, "self.vftable.{function_name}")?,
        }
        Ok(())
    }
}
impl EqualsIgnoringLocations for Function {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self.visibility.equals_ignoring_locations(&other.visibility)
            && self.name.equals_ignoring_locations(&other.name)
            && self.body.equals_ignoring_locations(&other.body)
            && self.arguments.equals_ignoring_locations(&other.arguments)
            && self
                .return_type
                .equals_ignoring_locations(&other.return_type)
            && self
                .calling_convention
                .equals_ignoring_locations(&other.calling_convention)
    }
}
impl Function {
    pub fn is_internal(&self) -> bool {
        self.name.starts_with("_")
    }
    pub fn is_public(&self) -> bool {
        matches!(self.visibility, Visibility::Public)
    }
}
#[cfg(test)]
impl StripLocations for Function {
    fn strip_locations(&self) -> Self {
        Function {
            visibility: self.visibility.strip_locations(),
            name: self.name.strip_locations(),
            doc: self.doc.strip_locations(),
            body: self.body.strip_locations(),
            arguments: self.arguments.strip_locations(),
            return_type: self.return_type.strip_locations(),
            calling_convention: self.calling_convention.strip_locations(),
        }
    }
}
#[cfg(test)]
impl Function {
    pub fn new(
        (visibility, name): (Visibility, impl Into<String>),
        body: FunctionBody,
        calling_convention: CallingConvention,
    ) -> Self {
        Function {
            visibility,
            name: name.into(),
            doc: vec![],
            body,
            arguments: Vec::new(),
            return_type: None,
            calling_convention,
        }
    }
    pub fn with_arguments(mut self, arguments: impl IntoIterator<Item = Argument>) -> Self {
        self.arguments = arguments.into_iter().map(Located::test).collect();
        self
    }
    pub fn with_return_type(mut self, return_type: Type) -> Self {
        self.return_type = Some(return_type);
        self
    }
    pub fn with_calling_convention(mut self, calling_convention: CallingConvention) -> Self {
        self.calling_convention = calling_convention;
        self
    }
    pub fn with_body(mut self, body: FunctionBody) -> Self {
        self.body = body;
        self
    }
    pub fn with_doc(mut self, doc: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.doc = doc.into_iter().map(|s| s.into()).collect();
        self
    }
}

pub fn build(
    type_registry: &TypeRegistry,
    scope: &[ItemPath],
    is_vfunc: bool,
    function: Located<&grammar::Function>,
) -> Result<Located<Function>> {
    let mut body = is_vfunc.then(|| FunctionBody::Vftable {
        function_name: function.name.0.clone(),
    });
    let doc = function.doc_comments.clone();
    let mut calling_convention = None;
    for attribute in &function.attributes {
        let Some((ident, items)) = attribute.function() else {
            continue;
        };
        if let Some(attr_address) = attribute::parse_address(ident, items, attribute.location())? {
            if is_vfunc {
                return Err(SemanticError::AttributeNotSupported {
                    attribute_name: "address".into(),
                    attribute_context: AttributeNotSupportedContext::VirtualFunction {
                        function_name: function.name.0.clone(),
                    },
                    location: function.location.clone(),
                });
            }
            body = Some(FunctionBody::Address {
                address: attr_address,
            });
        } else if let Some(_attr_index) =
            attribute::parse_index(ident, items, attribute.location())?
        {
            if !is_vfunc {
                return Err(SemanticError::AttributeNotSupported {
                    attribute_name: "index".into(),
                    attribute_context: AttributeNotSupportedContext::NonVirtualFunction {
                        function_name: function.name.0.clone(),
                    },
                    location: function.location.clone(),
                });
            }
            // ignore index attribute, this is handled by vftable construction
        } else if ident.as_str() == "calling_convention" {
            let exprs = attribute::assert_function_argument_count(
                items,
                "calling_convention",
                1,
                attribute.location(),
            )?;
            let expr = exprs[0];
            let Expr::StringLiteral { value, .. } = expr else {
                return Err(SemanticError::InvalidAttributeValue {
                    attribute_name: "calling_convention".into(),
                    expected_type: std::any::type_name::<CallingConvention>().into(),
                    location: expr.location().clone(),
                });
            };

            calling_convention =
                Some(
                    value
                        .parse()
                        .map_err(|_| SemanticError::InvalidCallingConvention {
                            convention: value.clone(),
                            function_name: function.name.0.clone(),
                            location: expr.location().clone(),
                        })?,
                );
        }
    }

    if !is_vfunc && body.is_none() {
        return Err(SemanticError::FunctionMissingImplementation {
            function_name: function.name.0.clone(),
            location: function.location.clone(),
        });
    }

    let Some(body) = body else {
        panic!(
            "function `{}` had no body assigned: {:?}",
            function.name, function
        );
    };

    let arguments = function
        .arguments
        .iter()
        .map(|a| {
            let location = a.location().clone();
            let arg = match a {
                grammar::Argument::ConstSelf { .. } => Argument::ConstSelf,
                grammar::Argument::MutSelf { .. } => Argument::MutSelf,
                grammar::Argument::Named { ident, type_, .. } => Argument::Field(
                    ident.0.clone(),
                    type_registry
                        .resolve_grammar_type(scope, type_)
                        .ok_or_else(|| SemanticError::TypeResolutionFailed {
                            type_: Located::new(type_.clone(), type_.location().clone()),
                            resolution_context: TypeResolutionContext::FunctionArgument {
                                argument_name: ident.0.clone(),
                                function_name: function.name.0.clone(),
                            },
                            location: location.clone(),
                        })?,
                ),
            };
            Ok(Located::new(arg, location))
        })
        .collect::<Result<Vec<_>>>()?;

    let return_type = function
        .return_type
        .as_ref()
        .and_then(|t| type_registry.resolve_grammar_type(scope, t));

    let calling_convention = calling_convention.unwrap_or_else(|| {
        let has_self = arguments
            .iter()
            .any(|a| matches!(&a.value, Argument::ConstSelf | Argument::MutSelf));
        // probably a bit sus
        if has_self {
            CallingConvention::for_member_function(type_registry.pointer_size())
        } else {
            CallingConvention::System
        }
    });

    Ok(Located::new(
        Function {
            visibility: function.visibility.into(),
            name: function.name.0.clone(),
            doc,
            body,
            arguments,
            return_type,
            calling_convention,
        },
        function.location.clone(),
    ))
}
