use anyhow::Context;

use crate::{
    grammar::{self, ItemPath},
    semantic_analysis::{
        type_registry::TypeRegistry,
        types::{Argument, CallingConvention, Function},
    },
};

pub fn build(
    type_registry: &TypeRegistry,
    scope: &[ItemPath],
    function: &grammar::Function,
) -> Result<Function, anyhow::Error> {
    let mut address = None;
    let mut calling_convention = None;
    for attribute in &function.attributes {
        let Some((ident, exprs)) = attribute.function() else {
            anyhow::bail!(
                "unsupported attribute for function `{}`: {attribute:?}",
                function.name
            );
        };
        match (ident.as_str(), &exprs[..]) {
            ("address", [grammar::Expr::IntLiteral(addr)]) => {
                address = Some((*addr).try_into().with_context(|| {
                    format!(
                        "failed to convert `address` attribute into usize for function `{}`",
                        function.name
                    )
                })?);
            }
            ("index", _) => {
                // ignore index attribute, this is handled by vftable construction
            }
            ("calling_convention", [grammar::Expr::StringLiteral(cc)]) => {
                calling_convention = Some(cc.parse().map_err(|_| {
                    anyhow::anyhow!(
                        "invalid calling convention for function `{}`: {cc}",
                        function.name
                    )
                })?);
            }
            _ => anyhow::bail!(
                "unsupported attribute for function `{}`: {attribute:?}",
                function.name
            ),
        }
    }

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
        address,
        arguments,
        return_type,
        calling_convention,
    })
}
