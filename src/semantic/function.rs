use std::{fmt, str::FromStr};

use anyhow::Context;

use crate::{
    grammar::{self, ItemPath},
    semantic::{
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
    pub arguments: Vec<Argument>,
    pub return_type: Option<Type>,
    pub calling_convention: CallingConvention,
}
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for doc in &self.doc {
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
            write!(f, " -> {}", ty)?;
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
impl Function {
    pub fn new(
        (visibility, name): (Visibility, impl Into<String>),
        body: FunctionBody,
        calling_convention: CallingConvention,
    ) -> Self {
        Function {
            visibility,
            name: name.into(),
            doc: Vec::new(),
            body,
            arguments: Vec::new(),
            return_type: None,
            calling_convention,
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
    pub fn with_doc(mut self, doc: impl Into<String>) -> Self {
        self.doc.push(doc.into());
        self
    }
    pub fn is_internal(&self) -> bool {
        self.name.starts_with("_")
    }
    pub fn is_public(&self) -> bool {
        matches!(self.visibility, Visibility::Public)
    }
}

pub fn build(
    type_registry: &TypeRegistry,
    scope: &[ItemPath],
    is_vfunc: bool,
    function: &grammar::Function,
) -> Result<Function, anyhow::Error> {
    let mut body = is_vfunc.then(|| FunctionBody::Vftable {
        function_name: function.name.0.clone(),
    });
    let doc = function
        .doc_comments
        .iter()
        .map(|s| s.node.clone())
        .collect::<Vec<_>>();
    let mut calling_convention = None;
    for attribute in &function.attributes {
        let Some((ident, exprs)) = attribute.function() else {
            continue;
        };
        match (ident.as_str(), exprs.as_slice()) {
            ("address", [addr_expr]) if matches!(addr_expr.node, grammar::Expr::IntLiteral(_)) => {
                let grammar::Expr::IntLiteral(addr) = addr_expr.node else {
                    unreachable!()
                };
                if is_vfunc {
                    anyhow::bail!(
                        "address attribute is not supported for virtual function `{}`",
                        function.name
                    );
                }

                body = Some(FunctionBody::Address {
                    address: addr.try_into().with_context(|| {
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
            ("calling_convention", [cc_expr])
                if matches!(cc_expr.node, grammar::Expr::StringLiteral(_)) =>
            {
                let grammar::Expr::StringLiteral(cc) = &cc_expr.node else {
                    unreachable!()
                };
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
        .filter_map(|a| match &a.node {
            grammar::ArgumentChild::Argument(arg) => Some(arg),
            grammar::ArgumentChild::Comment(_) => None,
        })
        .map(|arg| match &arg.node {
            grammar::Argument::ConstSelf => Ok(Argument::ConstSelf),
            grammar::Argument::MutSelf => Ok(Argument::MutSelf),
            grammar::Argument::Named(name, type_) => Ok(Argument::Field(
                name.node.0.clone(),
                type_registry
                    .resolve_grammar_type(scope, &type_.node)
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
        .and_then(|t| type_registry.resolve_grammar_type(scope, &t.node));

    let calling_convention = calling_convention.unwrap_or_else(|| {
        let has_self = arguments
            .iter()
            .any(|a| matches!(a, Argument::ConstSelf | Argument::MutSelf));
        // probably a bit sus
        if has_self {
            CallingConvention::for_member_function(type_registry.pointer_size())
        } else {
            CallingConvention::System
        }
    });

    Ok(Function {
        visibility: function.visibility.into(),
        name: function.name.0.clone(),
        doc,
        body,
        arguments,
        return_type,
        calling_convention,
    })
}
