use crate::{
    grammar::{self, ItemPath},
    semantic::{
        SemanticState,
        attribute::parse_index,
        error::{Result, SemanticError},
        function,
        module::Module,
        type_definition::get_region_name_and_type_definition,
        type_registry::TypeRegistry,
        types::{
            Argument, CallingConvention, Function, FunctionBody, ItemCategory, ItemDefinition,
            ItemState, ItemStateResolved, Region, Type, TypeDefinition, Visibility,
        },
    },
    span::{EqualsIgnoringLocations as _, HasLocation, ItemLocation},
};

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct TypeVftable {
    pub functions: Vec<Function>,
    pub base_field: Option<String>,
    pub type_: Type,
}
#[cfg(test)]
impl TypeVftable {
    pub fn new(
        functions: impl IntoIterator<Item = Function>,
        base_field: impl Into<Option<String>>,
        type_: Type,
    ) -> Self {
        Self {
            functions: functions.into_iter().collect(),
            base_field: base_field.into(),
            type_,
        }
    }
}

#[cfg(test)]
impl crate::span::StripLocations for TypeVftable {
    fn strip_locations(&self) -> Self {
        TypeVftable {
            functions: self.functions.strip_locations(),
            base_field: self.base_field.strip_locations(),
            type_: self.type_.strip_locations(),
        }
    }
}

/// Given a parsed size/list of functions, construct the list of semantic functions
pub fn convert_grammar_functions_to_semantic_functions(
    type_registry: &TypeRegistry,
    module: &Module,
    size: Option<usize>,
    functions: &[grammar::Function],
    location: &ItemLocation,
) -> Result<Vec<Function>> {
    // Insert function, with padding if necessary
    let mut output = vec![];
    let calling_convention = CallingConvention::for_member_function(type_registry.pointer_size());
    for function in functions {
        let mut index = None;
        for attribute in &function.attributes {
            let grammar::Attribute::Function {
                name: ident, items, ..
            } = attribute
            else {
                continue;
            };
            if let Some(attr_index) = parse_index(ident, items, attribute.location())? {
                index = Some(attr_index);
            }
        }

        if let Some(index) = index {
            make_padding_functions(&mut output, index, calling_convention, location);
        }
        output.push(function::build(
            type_registry,
            &module.scope(),
            true,
            function,
        )?);
    }

    // Pad out to target size
    if let Some(size) = size {
        make_padding_functions(&mut output, size, calling_convention, location);
    }

    fn make_padding_functions(
        output: &mut Vec<Function>,
        target_len: usize,
        calling_convention: CallingConvention,
        location: &ItemLocation,
    ) {
        let functions_to_add = target_len.saturating_sub(output.len());
        for _ in 0..functions_to_add {
            let name = format!("_vfunc_{}", output.len());
            output.push(Function {
                visibility: Visibility::Private,
                name: name.clone(),
                doc: vec![],
                arguments: vec![Argument::MutSelf {
                    location: *location,
                }],
                return_type: None,
                body: FunctionBody::Vftable {
                    function_name: name,
                },
                calling_convention,
                location: *location,
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
    location: &ItemLocation,
) -> Result<(Option<TypeVftable>, Option<Region>)> {
    if let Some(vftable_functions) = vftable_functions {
        // There are functions defined for this vftable.
        let vftable_item = build_type(
            &semantic.type_registry,
            resolvee_path,
            visibility,
            &vftable_functions,
            location,
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
                // Use the last derived function's location, or the first base function's location
                // Note: We always expect at least one function in either list, so the fallback should never trigger
                let error_location = vftable_functions
                    .last()
                    .map(|f| *f.location())
                    .or_else(|| base_vftable.functions.first().map(|f| *f.location()))
                    .unwrap_or(*location);
                return Err({
                    SemanticError::VftableMissingFunctions {
                        item_path: resolvee_path.clone(),
                        base_name,
                        expected_count: base_vftable.functions.len(),
                        actual_count: vftable_functions.len(),
                        location: error_location,
                    }
                });
            }
            for (idx, (base_vfunc, derived_vfunc)) in base_vftable
                .functions
                .iter()
                .zip(vftable_functions.iter())
                .enumerate()
            {
                if !base_vfunc.equals_ignoring_locations(derived_vfunc) {
                    // Use the derived function's location for the error
                    return Err(SemanticError::VftableFunctionMismatch {
                        item_path: resolvee_path.clone(),
                        base_name,
                        index: idx,
                        derived_function: derived_vfunc.to_string(),
                        base_function: base_vfunc.to_string(),
                        location: *derived_vfunc.location(),
                    });
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
            // Use the first vftable function's location for the generated vftable field
            let vftable_location = vftable_functions
                .first()
                .map(|f| *f.location())
                .unwrap_or_else(|| *location);

            let region = Region {
                visibility: Visibility::Private,
                name: Some("vftable".to_string()),
                doc: vec![],
                type_ref: vftable_pointer_type.clone(),
                is_base: false,
                location: vftable_location,
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
    location: &ItemLocation,
) -> Option<ItemDefinition> {
    let name = resolvee_path.last()?;

    let resolvee_vtable_path = resolvee_path
        .parent()?
        .join(format!("{}Vftable", name.as_str()).into());

    let regions: Vec<_> = functions
        .iter()
        .map(|f| function_to_region(resolvee_path, f))
        .collect();

    // Use the first function's location for the generated vftable type
    let vftable_type_location = functions
        .first()
        .map(|f| *f.location())
        .unwrap_or_else(|| *location);

    Some(ItemDefinition {
        visibility,
        path: resolvee_vtable_path.clone(),
        type_parameters: vec![], // Generated vftable types are not generic
        state: ItemState::Resolved(ItemStateResolved {
            size: regions.iter().map(|r| r.size(type_registry).unwrap()).sum(),
            alignment: type_registry.pointer_size(),
            inner: TypeDefinition {
                regions,
                doc: vec![],
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
        predefined: None,
        location: vftable_type_location,
    })
}

/// Given a function, create a region representing it
fn function_to_region(resolvee_path: &ItemPath, function: &Function) -> Region {
    let arguments = function
        .arguments
        .iter()
        .map(|a| match a {
            Argument::ConstSelf { .. } => (
                "this".to_string(),
                Box::new(Type::ConstPointer(Box::new(Type::Raw(
                    resolvee_path.clone(),
                )))),
            ),
            Argument::MutSelf { .. } => (
                "this".to_string(),
                Box::new(Type::MutPointer(Box::new(Type::Raw(resolvee_path.clone())))),
            ),
            Argument::Field { name, type_, .. } => (name.clone(), Box::new(type_.clone())),
        })
        .collect();
    let return_type = function.return_type.as_ref().map(|t| Box::new(t.clone()));

    Region {
        visibility: function.visibility,
        name: Some(function.name.clone()),
        doc: function.doc.clone(),
        type_ref: Type::Function(function.calling_convention, arguments, return_type),
        is_base: false,
        location: function.location,
    }
}

/// Given an optional region, attempt to get the region's name and its type's vftable if available
fn get_optional_region_name_and_vftable<'a>(
    type_registry: &'a TypeRegistry,
    resolvee_path: &ItemPath,
    region: Option<&Region>,
) -> Result<Option<(String, &'a TypeVftable)>> {
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
) -> Result<Option<(String, &'a TypeVftable)>> {
    Ok(
        get_region_name_and_type_definition(type_registry, resolvee_path, region)?
            .and_then(|(name, td)| td.vftable.as_ref().map(|vftable| (name, vftable))),
    )
}
