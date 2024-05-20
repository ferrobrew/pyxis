use std::{io::Write, path::Path, process::Command};

use crate::{
    grammar::{ItemPath, ItemPathSegment},
    semantic_analysis::{module, semantic_state, types},
};

use anyhow::Context;
use itertools::Itertools;
use quote::quote;

fn str_to_ident(s: &str) -> syn::Ident {
    quote::format_ident!("{}", s)
}

fn fully_qualified_type_ref_impl(
    out: &mut String,
    type_ref: &types::Type,
) -> Result<(), std::fmt::Error> {
    use std::fmt::Write;
    use types::Type;

    match type_ref {
        Type::Unresolved(_) => panic!("received unresolved type {:?}", type_ref),
        Type::Raw(path) => {
            if path.len() == 1 && path.last() == Some(&"void".into()) {
                write!(out, "::std::ffi::c_void")
            } else {
                // todo: re-evaluate this hack
                if path.len() > 1 {
                    write!(out, "crate::")?;
                }
                write!(out, "{}", path)
            }
        }
        Type::ConstPointer(tr) => {
            write!(out, "*const ")?;
            fully_qualified_type_ref_impl(out, tr.as_ref())
        }
        Type::MutPointer(tr) => {
            write!(out, "*mut ")?;
            fully_qualified_type_ref_impl(out, tr.as_ref())
        }
        Type::Array(tr, size) => {
            write!(out, "[")?;
            fully_qualified_type_ref_impl(out, tr.as_ref())?;
            write!(out, "; {}]", size)
        }
        Type::Function(args, return_type) => {
            // todo: revisit the thiscall here when we have non-thiscall functions
            write!(out, r#"unsafe extern "thiscall" fn ("#)?;
            for (field, type_ref) in args.iter() {
                write!(out, "{field}: ")?;
                fully_qualified_type_ref_impl(out, type_ref)?;
                write!(out, ", ")?;
            }
            write!(out, ")")?;
            if let Some(type_ref) = return_type {
                write!(out, " -> ")?;
                fully_qualified_type_ref_impl(out, type_ref)?;
            }
            Ok(())
        }
    }
}

fn fully_qualified_type_ref(type_ref: &types::Type) -> Result<String, std::fmt::Error> {
    let mut out = String::new();
    fully_qualified_type_ref_impl(&mut out, type_ref)?;
    Ok(out)
}

fn sa_type_to_syn_type(type_ref: &types::Type) -> anyhow::Result<syn::Type> {
    Ok(syn::parse_str(&fully_qualified_type_ref(type_ref)?)?)
}

fn build_function(
    function: &types::Function,
    is_vftable: bool,
) -> Result<proc_macro2::TokenStream, anyhow::Error> {
    use types::Argument;

    let name = str_to_ident(&function.name);

    let arguments = function
        .arguments
        .iter()
        .map(|a| {
            Ok(match a {
                Argument::ConstSelf => quote! { &self },
                Argument::MutSelf => quote! { &mut self },
                Argument::Field(name, type_ref) => {
                    let name = str_to_ident(name);
                    let syn_type = sa_type_to_syn_type(type_ref)?;
                    quote! {
                        #name: #syn_type
                    }
                }
            })
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let lambda_arguments = function
        .arguments
        .iter()
        .map(|a| {
            Ok(match a {
                Argument::ConstSelf => quote! { this: *const Self },
                Argument::MutSelf => quote! { this: *mut Self },
                Argument::Field(name, type_ref) => {
                    let name = str_to_ident(name);
                    let syn_type = sa_type_to_syn_type(type_ref)?;
                    quote! {
                        #name: #syn_type
                    }
                }
            })
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let call_arguments = function
        .arguments
        .iter()
        .map(|a| {
            Ok(match a {
                Argument::ConstSelf => quote! { self as *const Self},
                Argument::MutSelf => quote! { self as *mut Self },
                Argument::Field(name, _) => {
                    let name = str_to_ident(name);
                    quote! { #name }
                }
            })
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let return_type = function
        .return_type
        .as_ref()
        .map(|type_ref| -> anyhow::Result<proc_macro2::TokenStream> {
            let syn_type = sa_type_to_syn_type(type_ref)?;
            Ok(quote! { -> #syn_type })
        })
        .transpose()?;

    #[derive(Debug)]
    enum FunctionGetter {
        Address(usize),
        Vftable,
    }
    let mut function_getter = None;
    if is_vftable {
        function_getter = Some(FunctionGetter::Vftable);
    } else {
        for attribute in &function.attributes {
            let types::Attribute::Address(address) = attribute;
            if function_getter.is_some() {
                return Err(anyhow::anyhow!(
                    "function getter already set: {:?}",
                    function_getter
                ));
            }

            function_getter = Some(FunctionGetter::Address(*address));
        }
    }
    if function_getter.is_none() {
        return Err(anyhow::anyhow!("no function getter set"));
    }
    let function_getter_impl = function_getter.map(|fg| match fg {
        FunctionGetter::Address(address) => quote! {
            ::std::mem::transmute(#address)
        },
        FunctionGetter::Vftable => quote! {
            std::ptr::addr_of!((*self.vftable).#name).read()
        },
    });

    Ok(quote! {
        pub unsafe fn #name(#(#arguments),*) #return_type {
            let f: unsafe extern "thiscall" fn(#(#lambda_arguments),*) #return_type = #function_getter_impl;
            f(#(#call_arguments),*)
        }
    })
}

fn build_type(
    name: &ItemPathSegment,
    size: usize,
    type_definition: &types::TypeDefinition,
) -> anyhow::Result<proc_macro2::TokenStream> {
    let types::TypeDefinition {
        metadata,
        regions,
        functions,
    } = type_definition;

    let fields = regions
        .iter()
        .map(|r| {
            Ok(match r {
                types::Region {
                    name: field,
                    type_ref,
                } => {
                    let field_name = field.as_deref().context("field name not present")?;
                    let field_ident = str_to_ident(field_name);
                    let visibility = (!field_name.starts_with("_")).then(|| quote! { pub });
                    let syn_type = sa_type_to_syn_type(type_ref)?;
                    quote! {
                        #visibility #field_ident: #syn_type
                    }
                }
            })
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let name_ident = str_to_ident(name.as_str());
    let size_check_ident = quote::format_ident!("_{}_size_check", name.as_str());
    let size_check_impl = (size > 0).then(|| {
        quote! {
            #[allow(non_snake_case)]
            #[allow(dead_code)]
            fn #size_check_ident() {
                unsafe {
                    ::std::mem::transmute::<_, #name_ident>([0u8; #size]);
                }
                unreachable!()
            }
        }
    });

    let singleton_impl = metadata
        .iter()
        .find(|(k, _)| k.as_str() == "singleton")
        .map(|(_, address)| match address {
            types::MetadataValue::Integer(ref address) => {
                let address = *address as usize;
                quote! {
                    #[allow(dead_code)]
                    impl #name_ident {
                        pub unsafe fn get() -> &'static mut Self {
                            unsafe {
                                &mut **(#address as *mut *mut Self)
                            }
                        }
                    }
                }
            }
        });

    let free_functions_impl = functions
        .get("free")
        .cloned()
        .unwrap_or_default()
        .iter()
        .map(|f| build_function(f, false))
        .collect::<anyhow::Result<Vec<_>>>()?;

    let vftable_function_impl = functions
        .get("vftable")
        .cloned()
        .unwrap_or_default()
        .iter()
        .map(|f| build_function(f, true))
        .collect::<anyhow::Result<Vec<_>>>()?;

    Ok(quote! {
        #[repr(C)]
        pub struct #name_ident {
            #(#fields),*
        }
        #size_check_impl
        #singleton_impl

        #[allow(dead_code)]
        impl #name_ident {
            #(#free_functions_impl)*
            #(#vftable_function_impl)*
        }
    })
}

fn build_enum(
    name: &ItemPathSegment,
    size: usize,
    enum_definition: &types::EnumDefinition,
) -> anyhow::Result<proc_macro2::TokenStream> {
    let types::EnumDefinition {
        metadata,
        fields,
        ty,
    } = enum_definition;

    let syn_type = sa_type_to_syn_type(ty)?;
    let name_ident = str_to_ident(name.as_str());

    let syn_fields = fields.iter().map(|(name, value)| {
        let name_ident = str_to_ident(name);
        quote! {
            #name_ident = #value as _
        }
    });

    let size_check_ident = quote::format_ident!("_{}_size_check", name.as_str());
    let size_check_impl = (size > 0).then(|| {
        quote! {
            #[allow(non_snake_case)]
            #[allow(dead_code)]
            fn #size_check_ident() {
                unsafe {
                    ::std::mem::transmute::<_, #name_ident>([0u8; #size]);
                }
                unreachable!()
            }
        }
    });

    let singleton_impl = metadata
        .iter()
        .find(|(k, _)| k.as_str() == "singleton")
        .map(|(_, address)| match address {
            types::MetadataValue::Integer(ref address) => {
                let address = *address as usize;
                quote! {
                    #[allow(dead_code)]
                    impl #name_ident {
                        pub unsafe fn get() -> Self {
                            unsafe {
                                *(#address as *const Self)
                            }
                        }
                    }
                }
            }
        });

    Ok(quote! {
        #[repr(#syn_type)]
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
        pub enum #name_ident {
            #(#syn_fields),*
        }
        #size_check_impl
        #singleton_impl
    })
}

fn build_item(definition: &types::ItemDefinition) -> anyhow::Result<proc_macro2::TokenStream> {
    use types::ItemCategory;

    let name = definition
        .path
        .last()
        .context("failed to get last of item path")?;

    let types::ItemStateResolved { size, inner } =
        &definition.resolved().context("type was not resolved")?;

    match definition.category() {
        ItemCategory::Defined => match inner {
            types::ItemDefinitionInner::Type(td) => build_type(name, *size, td),
            types::ItemDefinitionInner::Enum(ed) => build_enum(name, *size, ed),
        },
        ItemCategory::Predefined => Ok(quote! {}),
        ItemCategory::Extern => Ok(quote! {}),
    }
}

fn build_extern_value(
    name: &str,
    type_: &types::Type,
    address: usize,
) -> anyhow::Result<proc_macro2::TokenStream> {
    let function_ident = quote::format_ident!("get_{name}");
    let type_ = sa_type_to_syn_type(type_)?;

    Ok(quote! {
        #[allow(dead_code)]
        pub unsafe fn #function_ident() -> Option<::std::ptr::NonNull<#type_>> {
            unsafe { ::std::ptr::NonNull::new(::std::mem::transmute::<_, *mut _>(#address)) }
        }
    })
}

pub fn write_module(
    out_dir: &Path,
    key: &ItemPath,
    semantic_state: &semantic_state::ResolvedSemanticState,
    module: &module::Module,
) -> anyhow::Result<()> {
    const FORMAT_OUTPUT: bool = true;

    if key.is_empty() {
        return Ok(());
    }

    let mut path = out_dir.to_path_buf();
    for segment in key.iter() {
        path.push(segment.as_str());
    }
    path.set_extension("rs");

    let directory_path = path.parent().map(|p| p.to_path_buf()).unwrap_or_default();
    std::fs::create_dir_all(directory_path)?;

    let mut file = std::fs::File::create(&path)?;
    for definition in module
        .definitions(semantic_state.type_registry())
        .sorted_by_key(|d| &d.path)
    {
        writeln!(file, "{}", build_item(definition)?)?;
    }

    for (name, type_, address) in module
        .extern_values
        .iter()
        .sorted_by_key(|(name, _, _)| name)
    {
        writeln!(file, "{}", build_extern_value(name, type_, *address)?)?;
    }

    if FORMAT_OUTPUT {
        Command::new("rustfmt").args([&path]).output()?;
    }

    Ok(())
}
