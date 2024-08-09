use anyhow::Context;

use crate::{
    grammar::{self, ItemPath},
    semantic_analysis::{
        function,
        module::Module,
        type_registry::TypeRegistry,
        types::{
            Argument, CallingConvention, Function, ItemCategory, ItemDefinition, ItemState,
            ItemStateResolved, Region, Type, TypeDefinition, TypeVftable, Visibility,
        },
        SemanticState,
    },
};

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
        let function = function::build(type_registry, &module.scope(), function)
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
                address: None,
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
        semantic.add_type(vftable_type)?;

        if let Some((base_name, base_vftable)) = first_base
            .map(|b| get_region_name_and_vftable(&semantic.type_registry, resolvee_path, b))
            .transpose()?
            .flatten()
        {
            // There is a base class with a vftable. Let's use its field.
            Ok((
                Some(TypeVftable {
                    functions: vftable_functions,
                    field_path: std::iter::once(base_name)
                        .chain(base_vftable.field_path.clone())
                        .collect(),
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
                    field_path: vec!["vftable".to_string()],
                    type_: vftable_pointer_type,
                }),
                Some(region),
            ))
        }
    } else if let Some((base_name, base_vftable)) = first_base
        .map(|b| get_region_name_and_vftable(&semantic.type_registry, resolvee_path, b))
        .transpose()?
        .flatten()
    {
        // There are no functions defined for this vftable, but there is a base class with a vftable.
        // Let's use its field.
        Ok((
            Some(TypeVftable {
                functions: base_vftable.functions.clone(),
                field_path: std::iter::once(base_name)
                    .chain(base_vftable.field_path.clone())
                    .collect(),
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
                free_functions: vec![],
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

/// Given a region, attempt to get its' type's name and vftable if available
fn get_region_name_and_vftable<'a>(
    type_registry: &'a TypeRegistry,
    resolvee_path: &ItemPath,
    base: &Region,
) -> anyhow::Result<Option<(String, &'a TypeVftable)>> {
    // If there is no vftable specified, and there is a base field,
    // attempt to use its vftable if available
    let base_name = base
        .name
        .clone()
        .expect("first base had no name, this shouldn't be possible");

    let Type::Raw(path) = &base.type_ref else {
        anyhow::bail!(
            "expected base field `{}` of type `{}` to be a raw type, but it was a {}",
            base_name,
            resolvee_path,
            base.type_ref.human_friendly_type()
        );
    };

    let base_type = type_registry
        .get(path)
        .with_context(|| format!("failed to get base type `{path}` for type `{resolvee_path}`",))?;

    let Some(base_type) = base_type.resolved() else {
        return Ok(None);
    };

    let Some(base_type) = base_type.inner.as_type() else {
        anyhow::bail!(
            "expected base field `{}` of type `{}` to be a type, but it was a {}",
            base_name,
            resolvee_path,
            base_type.inner.human_friendly_type()
        );
    };

    Ok(base_type
        .vftable
        .as_ref()
        .map(|vftable| (base_name, vftable)))
}
