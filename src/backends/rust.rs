use std::{fmt::Write as _, path::Path};

use crate::{
    grammar::{ItemPath, ItemPathSegment},
    semantic_analysis::{
        types::{
            Argument, EnumDefinition, ExternValue, Function, FunctionBody, ItemCategory,
            ItemDefinition, ItemDefinitionInner, ItemStateResolved, Type, TypeDefinition,
            Visibility,
        },
        Module, ResolvedSemanticState,
    },
};

use anyhow::Context;
use quote::quote;

pub fn write_module(
    out_dir: &Path,
    key: &ItemPath,
    semantic_state: &ResolvedSemanticState,
    module: &Module,
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

    writeln!(
        raw_output,
        "#![allow(dead_code, non_snake_case, clippy::missing_safety_doc)]"
    )?;

    let backends = module.backends.get("rust");
    let prologues = backends
        .iter()
        .flat_map(|bs| bs.iter().flat_map(|b| &b.prologue))
        .map(|s| s.as_str())
        .collect::<Vec<_>>()
        .join("\n");
    let epilogues = backends
        .iter()
        .flat_map(|bs| bs.iter().flat_map(|b| &b.epilogue))
        .map(|s| s.as_str())
        .collect::<Vec<_>>()
        .join("\n");

    writeln!(raw_output, "{prologues}")?;

    let mut definitions = module
        .definitions(semantic_state.type_registry())
        .collect::<Vec<_>>();
    definitions.sort_by_key(|d| &d.path);
    for definition in definitions {
        writeln!(raw_output, "{}", build_item(definition)?)?;
    }

    let mut extern_values = module.extern_values.clone();
    extern_values.sort_by_key(|ev| ev.name.clone());
    for ev in &extern_values {
        writeln!(raw_output, "{}", build_extern_value(ev)?)?;
    }

    writeln!(raw_output, "{epilogues}")?;

    let mut error = None;
    let output = if FORMAT_OUTPUT {
        // You may think that this is inefficient. It probably is.
        // It's still probably faster than running `rustfmt`.
        match syn::parse_file(&raw_output) {
            Ok(parsed_file) => prettyplease::unparse(&parsed_file),
            Err(err) => {
                let lc = err.span().start();
                error = Some(format!(
                    concat!(
                        "Could not parse generated Rust code to pretty-print. The code has been emitted as-is.\n",
                        "This may be due to a bug in Pyxis or an issue with one of your backend definitions.\n",
                        "\n",
                        "Error: {}\n",
                        "  --> {}:{}:{}\n",
                        "   | {}\n",
                        "   | {}"
                    ),
                    err,
                    path.display(),
                    lc.line,
                    lc.column,
                    raw_output.lines().nth(lc.line - 1).unwrap(),
                    format!("{}^", " ".repeat(lc.column))
                ));
                raw_output
            }
        }
    } else {
        raw_output
    };

    std::fs::write(&path, output).context("failed to write file")?;

    if let Some(error) = error {
        anyhow::bail!("{error}");
    }

    Ok(())
}

fn build_item(definition: &ItemDefinition) -> anyhow::Result<proc_macro2::TokenStream> {
    let name = definition
        .path
        .last()
        .context("failed to get last of item path")?;

    let ItemStateResolved {
        size,
        inner,
        alignment,
    } = &definition.resolved().context("type was not resolved")?;

    let visibility = definition.visibility;

    match definition.category() {
        ItemCategory::Defined => match inner {
            ItemDefinitionInner::Type(td) => build_type(name, *size, *alignment, visibility, td),
            ItemDefinitionInner::Enum(ed) => build_enum(name, *size, visibility, ed),
        },
        ItemCategory::Predefined => Ok(quote! {}),
        ItemCategory::Extern => Ok(quote! {}),
    }
}

fn build_type(
    name: &ItemPathSegment,
    size: usize,
    alignment: usize,
    visibility: Visibility,
    type_definition: &TypeDefinition,
) -> anyhow::Result<proc_macro2::TokenStream> {
    let TypeDefinition {
        singleton,
        regions,
        associated_functions,
        vftable,
        copyable,
        cloneable,
        defaultable,
        packed,
    } = type_definition;

    let visibility = visibility_to_tokens(visibility);

    let fields = regions
        .iter()
        .map(|r| {
            let crate::semantic_analysis::types::Region {
                visibility,
                name: field,
                type_ref,
                is_base: _,
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
            fn #size_check_ident() {
                unsafe {
                    ::std::mem::transmute::<[u8; #size], #name_ident>([0u8; #size]);
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

    let vftable_fn_impl = vftable
        .as_ref()
        .map(|v| {
            let accessor = if let Some(field) = &v.base_field {
                let field = str_to_ident(field);
                quote! { #field . vftable() }
            } else {
                quote! { vftable }
            };
            let vftable_type = sa_type_to_syn_type(&v.type_)?;
            anyhow::Ok(quote! {
                pub fn vftable(&self) -> #vftable_type {
                    self. #accessor as #vftable_type
                }
            })
        })
        .transpose()?;

    let associated_functions_impl = associated_functions
        .iter()
        .map(build_function)
        .collect::<anyhow::Result<Vec<_>>>()?;

    let vftable_function_impl = vftable
        .as_ref()
        .map(|v| {
            v.functions
                .iter()
                .filter(|f| !f.name.starts_with('_'))
                .map(build_function)
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
            #vftable_fn_impl
            #(#associated_functions_impl)*
            #(#vftable_function_impl)*
        }
    })
}

fn build_enum(
    name: &ItemPathSegment,
    size: usize,
    visibility: Visibility,
    enum_definition: &EnumDefinition,
) -> anyhow::Result<proc_macro2::TokenStream> {
    let EnumDefinition {
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

fn build_function(function: &Function) -> Result<proc_macro2::TokenStream, anyhow::Error> {
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

    let is_field_function = function.body.is_field();
    let call_arguments = function
        .arguments
        .iter()
        // Only pass `self` to the function if it's not a field function
        .filter(|a| !is_field_function || !a.is_self())
        .map(|a| match a {
            Argument::ConstSelf => quote! { self as *const Self as _ },
            Argument::MutSelf => quote! { self as *mut Self as _ },
            Argument::Field(name, _) => {
                let name = str_to_ident(name);
                quote! { #name }
            }
        })
        .collect::<Vec<_>>();

    let return_type = function
        .return_type
        .as_ref()
        .map(|type_ref| -> anyhow::Result<proc_macro2::TokenStream> {
            let syn_type = sa_type_to_syn_type(type_ref)?;
            Ok(quote! { -> #syn_type })
        })
        .transpose()?;

    let calling_convention = function.calling_convention.as_str();
    let function_body = match &function.body {
        FunctionBody::Address { address } => quote! {
            let f:
                unsafe extern #calling_convention
                fn(#(#lambda_arguments),*) #return_type
            = ::std::mem::transmute(#address);
            f(#(#call_arguments),*)
        },
        FunctionBody::Field { field } => {
            let field_ident = str_to_ident(field);
            quote! {
                self.#field_ident.#name(#(#call_arguments),*)
            }
        }
        FunctionBody::Vftable => quote! {
            let f = std::ptr::addr_of!((*self.vftable()).#name).read();
            f(#(#call_arguments),*)
        },
    };

    let visibility = visibility_to_tokens(function.visibility);
    Ok(quote! {
        #visibility unsafe fn #name(#(#arguments),*) #return_type {
            #function_body
        }
    })
}

fn build_extern_value(ev: &ExternValue) -> anyhow::Result<proc_macro2::TokenStream> {
    let visibility = visibility_to_tokens(ev.visibility);
    let function_ident = quote::format_ident!("get_{}", ev.name);
    let type_ = sa_type_to_syn_type(&ev.type_)?;
    let address = ev.address;

    Ok(quote! {
        #visibility unsafe fn #function_ident() -> &'static mut #type_ {
            unsafe { &mut *(#address as *mut #type_) }
        }
    })
}

fn str_to_ident(s: &str) -> syn::Ident {
    quote::format_ident!("{}", s)
}

fn fully_qualified_type_ref_impl(out: &mut String, type_ref: &Type) -> Result<(), std::fmt::Error> {
    use std::fmt::Write;

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

fn fully_qualified_type_ref(type_ref: &Type) -> Result<String, std::fmt::Error> {
    let mut out = String::new();
    fully_qualified_type_ref_impl(&mut out, type_ref)?;
    Ok(out)
}

fn sa_type_to_syn_type(type_ref: &Type) -> anyhow::Result<syn::Type> {
    Ok(syn::parse_str(&fully_qualified_type_ref(type_ref)?)?)
}

fn visibility_to_tokens(visibility: Visibility) -> proc_macro2::TokenStream {
    match visibility {
        Visibility::Public => quote! { pub },
        Visibility::Private => quote! {},
    }
}
