use anyhow::Context;

use crate::{
    grammar::{self, ItemPath},
    semantic_analysis::{
        function,
        module::Module,
        type_definition::get_region_name_and_type_definition,
        type_registry::TypeRegistry,
        types::{
            Argument, CallingConvention, Function, FunctionBody, ItemCategory, ItemDefinition,
            ItemState, ItemStateResolved, Region, Type, TypeDefinition, Visibility,
        },
        SemanticState,
    },
};

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct TypeVftable {
    pub functions: Vec<Function>,
    pub base_field: Option<String>,
    pub type_: Type,
}
impl TypeVftable {
    pub fn new(
        functions: impl Into<Vec<Function>>,
        base_field: impl Into<Option<String>>,
        type_: Type,
    ) -> Self {
        Self {
            functions: functions.into(),
            base_field: base_field.into(),
            type_,
        }
    }
}

/// Given a parsed size/list of functions, construct the list of semantic functions
pub fn convert_grammar_functions_to_semantic_functions(
    type_registry: &TypeRegistry,
    module: &Module,
    size: Option<usize>,
    functions: &[grammar::Function],
) -> anyhow::Result<Vec<Function>> {
    // Insert function, with padding if necessary
    let mut output = vec![];
    for function in functions {
        let mut index = None;
        for attribute in &function.attributes {
            let grammar::Attribute::Function(ident, exprs) = attribute else {
                anyhow::bail!(
                    "unsupported attribute for function `{}`: {attribute:?}",
                    function.name
                );
            };
            match (ident.as_str(), exprs.as_slice()) {
                ("index", [grammar::Expr::IntLiteral(index_)]) => {
                    index = Some(*index_ as usize);
                }
                ("calling_convention", _) => {
                    // ignore calling convention attribute, handled by function::build
                }
                _ => anyhow::bail!(
                    "unsupported attribute for function `{}`: {attribute:?}",
                    function.name
                ),
            }
        }

        if let Some(index) = index {
            make_padding_functions(&mut output, index);
        }
        let function = function::build(type_registry, &module.scope(), true, function)
            .with_context(|| format!("while building vftable function `{}`", function.name))?;
        output.push(function);
    }

    // Pad out to target size
    if let Some(size) = size {
        make_padding_functions(&mut output, size);
    }

    fn make_padding_functions(output: &mut Vec<Function>, target_len: usize) {
        let functions_to_add = target_len.saturating_sub(output.len());
        for _ in 0..functions_to_add {
            output.push(Function {
                visibility: Visibility::Private,
                name: format!("_vfunc_{}", output.len()),
                arguments: vec![Argument::MutSelf],
                return_type: None,
                body: FunctionBody::Vftable,
                calling_convention: CallingConvention::Thiscall,
            });
        }
    }

    Ok(output)
}

pub fn build(
    semantic: &mut SemanticState,
    resolvee_path: &ItemPath,
    visibility: Visibility,
    first_base: Option<&Region>,
    vftable_functions: Option<Vec<Function>>,
) -> anyhow::Result<(Option<TypeVftable>, Option<Region>)> {
    if let Some(vftable_functions) = vftable_functions {
        // There are functions defined for this vftable.
        let vftable_item = build_type(
            &semantic.type_registry,
            resolvee_path,
            visibility,
            &vftable_functions,
        );

        let Some(vftable_type) = vftable_item else {
            return Ok((None, None));
        };

        let vftable_path = vftable_type.path.clone();
        let vftable_pointer_type = Type::ConstPointer(Box::new(Type::Raw(vftable_path)));
        semantic.add_item(vftable_type)?;

        if let Some((base_name, base_vftable)) = get_optional_region_name_and_vftable(
            &semantic.type_registry,
            resolvee_path,
            first_base,
        )? {
            // There is a base class with a vftable. Let's use its field.

            // Ensure that all of the base classes's vfuncs are included in the derived class's vftable
            if vftable_functions.len() < base_vftable.functions.len() {
                anyhow::bail!(
                    "vftable for `{}` is missing functions from base class `{}`",
                    resolvee_path,
                    base_name
                );
            }
            for (idx, (base_vfunc, derived_vfunc)) in base_vftable
                .functions
                .iter()
                .zip(vftable_functions.iter())
                .enumerate()
            {
                if base_vfunc != derived_vfunc {
                    anyhow::bail!(
                        "vftable for `{}` has function `{}` at index {} but base class `{}` has function `{}`",
                        resolvee_path,
                        derived_vfunc,
                        idx,
                        base_name,
                        base_vfunc
                    );
                }
            }

            Ok((
                Some(TypeVftable {
                    functions: vftable_functions,
                    base_field: Some(base_name),
                    type_: vftable_pointer_type,
                }),
                None,
            ))
        } else {
            // There is no base class with a vftable. Let's create a new field.
            let region = Region {
                visibility: Visibility::Private,
                name: Some("vftable".to_string()),
                type_ref: vftable_pointer_type.clone(),
                is_base: false,
            };

            Ok((
                Some(TypeVftable {
                    functions: vftable_functions,
                    base_field: None,
                    type_: vftable_pointer_type,
                }),
                Some(region),
            ))
        }
    } else if let Some((base_name, base_vftable)) =
        get_optional_region_name_and_vftable(&semantic.type_registry, resolvee_path, first_base)?
    {
        // There are no functions defined for this vftable, but there is a base class with a vftable.
        // Let's use its field.
        Ok((
            Some(TypeVftable {
                functions: base_vftable.functions.clone(),
                base_field: Some(base_name),
                type_: base_vftable.type_.clone(),
            }),
            None,
        ))
    } else {
        Ok((None, None))
    }
}

/// Given a list of functions, create the type definition for the vftable containing them
fn build_type(
    type_registry: &TypeRegistry,
    resolvee_path: &ItemPath,
    visibility: Visibility,
    functions: &[Function],
) -> Option<ItemDefinition> {
    let name = resolvee_path.last()?;

    let resolvee_vtable_path = resolvee_path
        .parent()?
        .join(format!("{}Vftable", name.as_str()).into());

    let regions: Vec<_> = functions
        .iter()
        .map(|f| function_to_region(resolvee_path, f))
        .collect();

    Some(ItemDefinition {
        visibility,
        path: resolvee_vtable_path.clone(),
        state: ItemState::Resolved(ItemStateResolved {
            size: regions.iter().map(|r| r.size(type_registry).unwrap()).sum(),
            alignment: type_registry.pointer_size(),
            inner: TypeDefinition {
                regions,
                associated_functions: vec![],
                vftable: None,
                singleton: None,
                cloneable: false,
                copyable: false,
                defaultable: false,
                packed: false,
            }
            .into(),
        }),
        category: ItemCategory::Defined,
    })
}

/// Given a function, create a region representing it
fn function_to_region(resolvee_path: &ItemPath, function: &Function) -> Region {
    let arguments = function
        .arguments
        .iter()
        .map(|a| match a {
            Argument::ConstSelf => (
                "this".to_string(),
                Box::new(Type::ConstPointer(Box::new(Type::Raw(
                    resolvee_path.clone(),
                )))),
            ),
            Argument::MutSelf => (
                "this".to_string(),
                Box::new(Type::MutPointer(Box::new(Type::Raw(resolvee_path.clone())))),
            ),
            Argument::Field(name, type_ref) => (name.clone(), Box::new(type_ref.clone())),
        })
        .collect();
    let return_type = function.return_type.as_ref().map(|t| Box::new(t.clone()));

    Region {
        visibility: function.visibility,
        name: Some(function.name.clone()),
        type_ref: Type::Function(function.calling_convention, arguments, return_type),
        is_base: false,
    }
}

/// Given an optional region, attempt to get the region's name and its type's vftable if available
fn get_optional_region_name_and_vftable<'a>(
    type_registry: &'a TypeRegistry,
    resolvee_path: &ItemPath,
    region: Option<&Region>,
) -> anyhow::Result<Option<(String, &'a TypeVftable)>> {
    Ok(region
        .map(|b| get_region_name_and_vftable(type_registry, resolvee_path, b))
        .transpose()?
        .flatten())
}

/// Given a region, attempt to get the region's name and its type's vftable if available
fn get_region_name_and_vftable<'a>(
    type_registry: &'a TypeRegistry,
    resolvee_path: &ItemPath,
    region: &Region,
) -> anyhow::Result<Option<(String, &'a TypeVftable)>> {
    Ok(
        get_region_name_and_type_definition(type_registry, resolvee_path, region)?
            .and_then(|(name, td)| td.vftable.as_ref().map(|vftable| (name, vftable))),
    )
}
