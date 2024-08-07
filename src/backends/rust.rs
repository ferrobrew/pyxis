use std::{fmt::Write as _, path::Path};

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
        Type::Function(calling_convention, args, return_type) => {
            write!(out, r#"unsafe extern "{calling_convention}" fn ("#)?;
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

fn visibility_to_tokens(visibility: types::Visibility) -> proc_macro2::TokenStream {
    match visibility {
        types::Visibility::Public => quote! { pub },
        types::Visibility::Private => quote! {},
    }
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
                Argument::ConstSelf => quote! { self as *const Self },
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
    let function_getter = if is_vftable {
        Some(FunctionGetter::Vftable)
    } else {
        function.address.map(FunctionGetter::Address)
    };

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

    let calling_convention = function.calling_convention.as_str();
    let visibility = visibility_to_tokens(function.visibility);

    Ok(quote! {
        #visibility unsafe fn #name(#(#arguments),*) #return_type {
            let f: unsafe extern #calling_convention fn(#(#lambda_arguments),*) #return_type = #function_getter_impl;
            f(#(#call_arguments),*)
        }
    })
}

fn build_type(
    name: &ItemPathSegment,
    size: usize,
    alignment: usize,
    visibility: types::Visibility,
    type_definition: &types::TypeDefinition,
) -> anyhow::Result<proc_macro2::TokenStream> {
    let types::TypeDefinition {
        singleton,
        regions,
        free_functions,
        vftable_functions,
        copyable,
        cloneable,
        defaultable,
        packed,
    } = type_definition;

    let visibility = visibility_to_tokens(visibility);

    let fields = regions
        .iter()
        .map(|r| {
            let types::Region {
                visibility,
                name: field,
                type_ref,
            } = r;
            let field_name = field.as_deref().context("field name not present")?;
            let field_ident = str_to_ident(field_name);
            let visibility = visibility_to_tokens(*visibility);
            let syn_type = sa_type_to_syn_type(type_ref)?;
            Ok(quote! {
                #visibility #field_ident: #syn_type
            })
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let name_ident = str_to_ident(name.as_str());
    let size_check_ident = quote::format_ident!("_{}_size_check", name.as_str());
    let size_check_impl = (size > 0).then(|| {
        quote! {
            #[allow(non_snake_case)]
            fn #size_check_ident() {
                unsafe {
                    ::std::mem::transmute::<_, #name_ident>([0u8; #size]);
                }
                unreachable!()
            }
        }
    });

    let singleton_impl = singleton.map(|address| {
        quote! {
            impl #name_ident {
                #visibility unsafe fn get() -> Option<&'static mut Self> {
                    unsafe {
                        let ptr: *mut Self = *(#address as *mut *mut Self);
                        if ptr.is_null() {
                            None
                        } else {
                            Some(&mut *ptr)
                        }
                    }
                }
            }
        }
    });

    let free_functions_impl = free_functions
        .iter()
        .map(|f| build_function(f, false))
        .collect::<anyhow::Result<Vec<_>>>()?;

    let vftable_function_impl = vftable_functions
        .as_ref()
        .map(|fns| {
            fns.iter()
                .filter(|f| !f.name.starts_with('_'))
                .map(|f| build_function(f, true))
                .collect::<anyhow::Result<Vec<_>>>()
        })
        .transpose()?
        .unwrap_or_default();

    let mut extra_derives = vec![];
    if *copyable {
        extra_derives.push(quote! { Copy });
    }
    if *cloneable {
        extra_derives.push(quote! { Clone });
    }
    if *defaultable {
        extra_derives.push(quote! { Default });
    }

    let derives = if extra_derives.is_empty() {
        quote! {}
    } else {
        quote! { #[derive(#(#extra_derives),*)] }
    };

    // Packing and alignment are mutually exclusive
    let (packed, alignment) = if *packed {
        (quote! { , packed }, quote! {})
    } else {
        let alignment: syn::Index = alignment.into();
        (quote! {}, quote! { , align(#alignment) })
    };

    Ok(quote! {
        #derives
        #[repr(C #packed #alignment)]
        #visibility struct #name_ident {
            #(#fields),*
        }
        #size_check_impl
        #singleton_impl

        impl #name_ident {
            #(#free_functions_impl)*
            #(#vftable_function_impl)*
        }
    })
}

fn build_enum(
    name: &ItemPathSegment,
    size: usize,
    visibility: types::Visibility,
    enum_definition: &types::EnumDefinition,
) -> anyhow::Result<proc_macro2::TokenStream> {
    let types::EnumDefinition {
        singleton,
        fields,
        type_,
        copyable,
        cloneable,
        defaultable,
        default_index,
    } = enum_definition;

    let syn_type = sa_type_to_syn_type(type_)?;
    let name_ident = str_to_ident(name.as_str());

    let visibility = visibility_to_tokens(visibility);

    let syn_fields = fields.iter().enumerate().map(|(idx, (name, value))| {
        let name_ident = str_to_ident(name);
        let field = quote! {
            #name_ident = #value as _
        };

        if default_index.is_some_and(|i| i == idx) {
            quote! {
                #[default]
                #field
            }
        } else {
            field
        }
    });

    let size_check_ident = quote::format_ident!("_{}_size_check", name.as_str());
    let size_check_impl = (size > 0).then(|| {
        quote! {
            #[allow(non_snake_case)]
            fn #size_check_ident() {
                unsafe {
                    ::std::mem::transmute::<_, #name_ident>([0u8; #size]);
                }
                unreachable!()
            }
        }
    });

    let singleton_impl = singleton.map(|address| {
        quote! {
            impl #name_ident {
                #visibility unsafe fn get() -> Self {
                    unsafe {
                        *(#address as *const Self)
                    }
                }
            }
        }
    });

    let mut extra_derives = vec![];
    if *copyable {
        extra_derives.push(quote! { Copy });
    }
    if *cloneable {
        extra_derives.push(quote! { Clone });
    }
    if *defaultable {
        extra_derives.push(quote! { Default });
    }

    Ok(quote! {
        #[repr(#syn_type)]
        #[derive(PartialEq, Eq, PartialOrd, Ord, Debug, #(#extra_derives),*)]
        #visibility enum #name_ident {
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

    let types::ItemStateResolved {
        size,
        inner,
        alignment,
    } = &definition.resolved().context("type was not resolved")?;

    let visibility = definition.visibility;

    match definition.category() {
        ItemCategory::Defined => match inner {
            types::ItemDefinitionInner::Type(td) => {
                build_type(name, *size, *alignment, visibility, td)
            }
            types::ItemDefinitionInner::Enum(ed) => build_enum(name, *size, visibility, ed),
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
        pub unsafe fn #function_ident() -> &'static mut #type_ {
            unsafe { &mut *::std::mem::transmute::<_, *mut _>(#address) }
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

    let mut raw_output = String::new();

    writeln!(raw_output, "#![allow(dead_code)]")?;

    let backends = module.backends.get("rust");
    let prologues = backends
        .iter()
        .flat_map(|bs| bs.iter().flat_map(|b| &b.prologue))
        .join("\n");
    let epilogues = backends
        .iter()
        .flat_map(|bs| bs.iter().flat_map(|b| &b.epilogue))
        .join("\n");

    writeln!(raw_output, "{prologues}")?;

    for definition in module
        .definitions(semantic_state.type_registry())
        .sorted_by_key(|d| &d.path)
    {
        writeln!(raw_output, "{}", build_item(definition)?)?;
    }

    for (name, type_, address) in module
        .extern_values
        .iter()
        .sorted_by_key(|(name, _, _)| name)
    {
        writeln!(raw_output, "{}", build_extern_value(name, type_, *address)?)?;
    }

    writeln!(raw_output, "{epilogues}")?;

    let output = if FORMAT_OUTPUT {
        // You may think that this is inefficient. It probably is.
        // It's still probably faster than running `rustfmt`.
        prettyplease::unparse(&syn::parse_file(&raw_output)?)
    } else {
        raw_output
    };

    std::fs::write(&path, output).context("failed to write file")?;

    Ok(())
}
