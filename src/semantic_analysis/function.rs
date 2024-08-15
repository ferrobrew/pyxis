use std::{fmt, str::FromStr};

use anyhow::Context;

use crate::{
    grammar::{self, ItemPath},
    semantic_analysis::{
        type_registry::TypeRegistry,
        types::{Type, Visibility},
    },
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Argument {
    ConstSelf,
    MutSelf,
    Field(String, Type),
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
impl Argument {
    pub fn field(name: impl Into<String>, type_ref: impl Into<Type>) -> Self {
        Argument::Field(name.into(), type_ref.into())
    }
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
impl CallingConvention {
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
}
impl fmt::Display for CallingConvention {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
impl FromStr for CallingConvention {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
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
    Address { address: usize },
    Field { field: String },
    Vftable,
}

impl FunctionBody {
    pub fn address(address: usize) -> Self {
        FunctionBody::Address { address }
    }
    pub fn field(field: impl Into<String>) -> Self {
        FunctionBody::Field {
            field: field.into(),
        }
    }
    pub fn is_field(&self) -> bool {
        matches!(self, FunctionBody::Field { .. })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub visibility: Visibility,
    pub name: String,
    pub body: FunctionBody,
    pub arguments: Vec<Argument>,
    pub return_type: Option<Type>,
    pub calling_convention: CallingConvention,
}
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.body {
            FunctionBody::Address { address } => write!(f, "#[address(0x{address:X})] ")?,
            FunctionBody::Field { field } => write!(f, "#[field({field:?})] ")?,
            FunctionBody::Vftable => {}
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
            write!(f, " -> {}", ty)?;
        }
        Ok(())
    }
}
impl Function {
    pub fn new((visibility, name): (Visibility, impl Into<String>), body: FunctionBody) -> Self {
        Function {
            visibility,
            name: name.into(),
            body,
            arguments: Vec::new(),
            return_type: None,
            // ehh. This is not really always going to be true,
            // but I also don't want to specify it in all of the tests
            calling_convention: CallingConvention::Thiscall,
        }
    }
    pub fn with_arguments(mut self, arguments: impl Into<Vec<Argument>>) -> Self {
        self.arguments = arguments.into();
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
    pub fn is_internal(&self) -> bool {
        self.name.starts_with("_")
    }
}

pub fn build(
    type_registry: &TypeRegistry,
    scope: &[ItemPath],
    is_vfunc: bool,
    function: &grammar::Function,
) -> Result<Function, anyhow::Error> {
    let mut body = is_vfunc.then_some(FunctionBody::Vftable);
    let mut calling_convention = None;
    for attribute in &function.attributes {
        let Some((ident, exprs)) = attribute.function() else {
            continue;
        };
        match (ident.as_str(), &exprs[..]) {
            ("address", [grammar::Expr::IntLiteral(addr)]) => {
                if is_vfunc {
                    anyhow::bail!(
                        "address attribute is not supported for virtual function `{}`",
                        function.name
                    );
                }

                body = Some(FunctionBody::Address {
                    address: (*addr).try_into().with_context(|| {
                        format!(
                            "failed to convert `address` attribute into usize for function `{}`",
                            function.name
                        )
                    })?,
                });
            }
            ("index", _) => {
                // ignore index attribute, this is handled by vftable construction
                if !is_vfunc {
                    anyhow::bail!(
                        "index attribute is only supported for virtual functions, not `{}`",
                        function.name
                    );
                }
            }
            ("calling_convention", [grammar::Expr::StringLiteral(cc)]) => {
                calling_convention = Some(cc.parse().map_err(|_| {
                    anyhow::anyhow!(
                        "invalid calling convention for function `{}`: {cc}",
                        function.name
                    )
                })?);
            }
            _ => {}
        }
    }

    if !is_vfunc && body.is_none() {
        anyhow::bail!(
            "function `{}` has no implementation available; did you forget to assign an `address` attribute?",
            function.name,
        );
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
        .map(|a| match a {
            grammar::Argument::ConstSelf => Ok(Argument::ConstSelf),
            grammar::Argument::MutSelf => Ok(Argument::MutSelf),
            grammar::Argument::Named(name, type_) => Ok(Argument::Field(
                name.0.clone(),
                type_registry
                    .resolve_grammar_type(scope, type_)
                    .ok_or_else(|| {
                        anyhow::anyhow!(
                            "failed to resolve type of field `{:?}` ({:?})",
                            name,
                            type_
                        )
                    })?,
            )),
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let return_type = function
        .return_type
        .as_ref()
        .and_then(|t| type_registry.resolve_grammar_type(scope, t));

    let calling_convention = calling_convention.unwrap_or_else(|| {
        // Assume that if the function has a self argument, it's a thiscall function, otherwise it's "system"
        // for interoperating with system libraries: <https://doc.rust-lang.org/nomicon/ffi.html#foreign-calling-conventions>
        // Bit sus honestly, maybe we should enforce a calling convention for all non-self functions?
        let has_self = arguments
            .iter()
            .any(|a| matches!(a, Argument::ConstSelf | Argument::MutSelf));
        if has_self {
            CallingConvention::Thiscall
        } else {
            CallingConvention::System
        }
    });

    Ok(Function {
        visibility: function.visibility.into(),
        name: function.name.0.clone(),
        body,
        arguments,
        return_type,
        calling_convention,
    })
}
