use std::{collections::HashMap, fmt::Write as _, path::Path, str::FromStr};

use crate::{
    grammar::ItemPath,
    semantic::{
        Module, ResolvedSemanticState, TypeRegistry,
        types::{
            Argument, BitflagsDefinition, EnumDefinition, ExternValue, Function, FunctionBody,
            ItemCategory, ItemDefinition, ItemDefinitionInner, ItemStateResolved, Region, Type,
            TypeDefinition, Visibility,
        },
    },
};

use anyhow::Context;
use quote::{ToTokens, quote};

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
        "#![allow(dead_code, non_snake_case, clippy::missing_safety_doc, clippy::unnecessary_cast)]"
    )?;
    // Disable rustfmt on generated files to prevent the prettyplease-formatted code being reformatted
    // by a stray project-wide `cargo fmt` invocation.
    // <https://stackoverflow.com/questions/59247458/is-there-a-stable-way-to-tell-rustfmt-to-skip-an-entire-file#comment138279076_75910283>
    writeln!(raw_output, "#![cfg_attr(any(), rustfmt::skip)]")?;
    writeln!(raw_output, "{}", doc_to_tokens(true, module.doc()))?;

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
        writeln!(
            raw_output,
            "{}",
            build_item(semantic_state.type_registry(), definition)?
        )?;
    }

    let mut extern_values = module.extern_values.clone();
    extern_values.sort_by_key(|ev| ev.name.clone());
    for ev in &extern_values {
        writeln!(raw_output, "{}", build_extern_value(ev)?)?;
    }

    // Generate freestanding functions
    let freestanding_functions = module
        .functions()
        .iter()
        .filter(|f| !f.is_internal())
        .map(build_function)
        .collect::<anyhow::Result<Vec<_>>>()?;
    for func in freestanding_functions {
        writeln!(raw_output, "{}", func)?;
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

fn build_item(
    type_registry: &TypeRegistry,
    definition: &ItemDefinition,
) -> anyhow::Result<proc_macro2::TokenStream> {
    let ItemStateResolved {
        size,
        inner,
        alignment,
    } = &definition.resolved().context("type was not resolved")?;
    let visibility = definition.visibility;
    let path = &definition.path;

    use ItemDefinitionInner as IDI;
    match definition.category() {
        ItemCategory::Defined => match inner {
            IDI::Type(td) => build_type(type_registry, path, *size, *alignment, visibility, td),
            IDI::Enum(ed) => build_enum(path, *size, visibility, ed),
            IDI::Bitflags(bd) => build_bitflags(path, *size, visibility, bd),
        },
        ItemCategory::Predefined => Ok(quote! {}),
        ItemCategory::Extern => Ok(quote! {}),
    }
}

fn build_type(
    type_registry: &TypeRegistry,
    path: &ItemPath,
    size: usize,
    alignment: usize,
    visibility: Visibility,
    type_definition: &TypeDefinition,
) -> anyhow::Result<proc_macro2::TokenStream> {
    let name = path.last().context("failed to get last of item path")?;

    let TypeDefinition {
        singleton,
        regions,
        doc,
        associated_functions,
        vftable,
        copyable,
        cloneable,
        defaultable,
        packed,
    } = type_definition;

    let visibility = visibility_to_tokens(visibility);
    let doc = doc_to_tokens(false, doc);
    let fields = regions
        .iter()
        .map(|r| {
            let Region {
                visibility,
                name: field,
                doc,
                type_ref,
                is_base: _,
            } = r;
            let field_name = field.as_deref().context("field name not present")?;
            let field_ident = str_to_ident(field_name);
            let visibility = visibility_to_tokens(*visibility);
            let syn_type = sa_type_to_syn_type(type_ref)?;
            let doc = doc_to_tokens(false, doc);
            Ok(quote! {
                #doc
                #visibility #field_ident: #syn_type
            })
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let name_ident = str_to_ident(name.as_str());
    let size_check_ident = quote::format_ident!("_{}_size_check", name.as_str());
    let size_check_impl = (size > 0).then(|| {
        let size = hex_literal(size);
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
                        ptr.as_mut()
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

    // Not sure about filtering out internal functions at this level,
    // might be better to do it in semantic?
    let associated_functions_impl = associated_functions
        .iter()
        .filter(|f| !f.is_internal())
        .map(build_function)
        .collect::<anyhow::Result<Vec<_>>>()?;

    let vftable_function_impl = vftable
        .as_ref()
        .map(|v| {
            v.functions
                .iter()
                .filter(|f| !f.is_internal())
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

    let as_ref_conversions = {
        let types_to_field_paths = type_definition
            .dfs_hierarchy(type_registry, path, &[])?
            .into_iter()
            .map(|(field_path, type_)| {
                let field_path = field_path
                    .into_iter()
                    .map(|s| str_to_ident(&s))
                    .collect::<Vec<_>>();
                let type_ = sa_type_to_syn_type(&type_)?;

                Ok((type_, field_path))
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        let types_to_field_paths_vec: HashMap<_, Vec<_>> =
            types_to_field_paths
                .iter()
                .fold(HashMap::new(), |mut acc, (type_, field_path)| {
                    acc.entry(type_).or_default().push(field_path);
                    acc
                });

        types_to_field_paths
            .iter()
            .map(|(type_, field_path)| {
                let implementations = &types_to_field_paths_vec[type_];
                if implementations.len() > 1 {
                    let mut conflicting_impl_message = format!(
                        concat!(
                        "`AsRef` and `AsMut` implementations were not generated for `{}` to `{}`,\n",
                        "as there are multiple implementations of the same type in the hierarchy:\n"
                    ),
                        name,
                        type_.to_token_stream()
                    );
                    for ident_path in implementations {
                        conflicting_impl_message.push_str("  - `");
                        conflicting_impl_message.push_str(
                            &ident_path
                                .iter()
                                .map(|i| i.to_string())
                                .collect::<Vec<_>>()
                                .join("."),
                        );
                        conflicting_impl_message.push_str("`\n");
                    }
                    let conflicting_impl_doc = doc_to_tokens(false, &[conflicting_impl_message.trim().to_string()]);
                    let conflicting_impl_ident = quote::format_ident!(
                        "_CONFLICTING_{}_{}",
                        name.as_str().to_uppercase(),
                        field_path
                            .iter()
                            .map(|f| f.to_string().to_uppercase())
                            .collect::<Vec<_>>()
                            .join("_")
                    );

                    quote! {
                        #conflicting_impl_doc
                        const #conflicting_impl_ident: () = ();
                    }
                } else {
                    quote! {
                        impl std::convert::AsRef<#type_> for #name_ident {
                            fn as_ref(&self) -> & #type_ {
                                &self #(. #field_path)*
                            }
                        }
                        impl std::convert::AsMut<#type_> for #name_ident {
                            fn as_mut(&mut self) -> &mut #type_ {
                                &mut self #(. #field_path)*
                            }
                        }
                    }
                }
            })
            // Inject conversions from T to T to make it easier to work with traits that rely on AsRef/AsMut
            .chain(std::iter::once(quote! {
                impl std::convert::AsRef<#name_ident> for #name_ident {
                    fn as_ref(&self) -> & #name_ident {
                        self
                    }
                }
                impl std::convert::AsMut<#name_ident> for #name_ident {
                    fn as_mut(&mut self) -> &mut #name_ident {
                        self
                    }
                }
            }))
            .collect::<Vec<_>>()
    };

    Ok(quote! {
        #derives
        #[repr(C #packed #alignment)]
        #doc
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
        #(#as_ref_conversions)*
    })
}

fn build_enum(
    path: &ItemPath,
    size: usize,
    visibility: Visibility,
    enum_definition: &EnumDefinition,
) -> anyhow::Result<proc_macro2::TokenStream> {
    let name = path.last().context("failed to get last of item path")?;

    let EnumDefinition {
        singleton,
        fields,
        doc,
        type_,
        copyable,
        cloneable,
        default,
        associated_functions,
    } = enum_definition;

    let syn_type = sa_type_to_syn_type(type_)?;
    let name_ident = str_to_ident(name.as_str());

    let visibility = visibility_to_tokens(visibility);
    let doc = doc_to_tokens(false, doc);

    let size_check_ident = quote::format_ident!("_{}_size_check", name.as_str());
    let size_check_impl = (size > 0).then(|| {
        let size = hex_literal(size);
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
        let address = hex_literal(address);
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
    if default.is_some() {
        extra_derives.push(quote! { Default });
    }

    let syn_fields = fields.iter().enumerate().map(|(idx, (name, value))| {
        let name_ident = str_to_ident(name);
        let field = quote! {
            #name_ident = #value as _
        };

        if default.is_some_and(|i| i == idx) {
            quote! {
                #[default]
                #field
            }
        } else {
            field
        }
    });

    // Build associated functions
    let associated_functions_impl = associated_functions
        .iter()
        .filter(|f| !f.is_internal())
        .map(build_function)
        .collect::<anyhow::Result<Vec<_>>>()?;

    let associated_impl = if !associated_functions_impl.is_empty() {
        Some(quote! {
            impl #name_ident {
                #(#associated_functions_impl)*
            }
        })
    } else {
        None
    };

    Ok(quote! {
        #[repr(#syn_type)]
        #[derive(PartialEq, Eq, PartialOrd, Ord, Debug, #(#extra_derives),*)]
        #doc
        #visibility enum #name_ident {
            #(#syn_fields),*
        }
        #size_check_impl
        #singleton_impl
        #associated_impl
    })
}

fn build_bitflags(
    path: &ItemPath,
    size: usize,
    visibility: Visibility,
    bitflags_definition: &BitflagsDefinition,
) -> anyhow::Result<proc_macro2::TokenStream> {
    let name = path.last().context("failed to get last of item path")?;

    let BitflagsDefinition {
        singleton,
        fields,
        doc,
        type_,
        copyable,
        cloneable,
        default,
    } = bitflags_definition;

    let syn_type = sa_type_to_syn_type(type_)?;
    let name_ident = str_to_ident(name.as_str());

    let visibility = visibility_to_tokens(visibility);
    let doc = doc_to_tokens(false, doc);

    let size_check_ident = quote::format_ident!("_{}_size_check", name.as_str());
    let size_check_impl = (size > 0).then(|| {
        let size = hex_literal(size);
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
        let address = hex_literal(address);
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

    let default_impl = default.map(|idx| {
        let field_ident = str_to_ident(&fields[idx].0);
        quote! {
            impl Default for #name_ident {
                fn default() -> Self {
                    Self::#field_ident
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

    let syn_fields = fields.iter().map(|(name, value)| {
        let name_ident = str_to_ident(name);
        quote! {
            const #name_ident = #value as _;
        }
    });

    Ok(quote! {
        bitflags::bitflags! {
            #[derive(PartialEq, Eq, PartialOrd, Ord, Debug, #(#extra_derives),*)]
            #doc
            #visibility struct #name_ident: #syn_type {
                #(#syn_fields)*
            }
        }
        #size_check_impl
        #singleton_impl
        #default_impl
    })
}

fn build_function(function: &Function) -> Result<proc_macro2::TokenStream, anyhow::Error> {
    let name = str_to_ident(&function.name);
    let doc = doc_to_tokens(false, &function.doc);

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
        FunctionBody::Address { address } => {
            let address = hex_literal(*address);
            quote! {
                let f:
                    unsafe extern #calling_convention
                    fn(#(#lambda_arguments),*) #return_type
                = ::std::mem::transmute(#address as usize);
                f(#(#call_arguments),*)
            }
        }
        FunctionBody::Field {
            field,
            function_name,
        } => {
            let field_ident = str_to_ident(field);
            let function_to_call_name = str_to_ident(function_name);
            quote! {
                self.#field_ident.#function_to_call_name(#(#call_arguments),*)
            }
        }
        FunctionBody::Vftable { function_name } => {
            let function_to_call_name = str_to_ident(function_name);
            quote! {
                let f = (&raw const (*self.vftable()).#function_to_call_name).read();
                f(#(#call_arguments),*)
            }
        }
    };

    let visibility = visibility_to_tokens(function.visibility);
    Ok(quote! {
        #doc
        #visibility unsafe fn #name(#(#arguments),*) #return_type {
            unsafe {
                #function_body
            }
        }
    })
}

fn build_extern_value(ev: &ExternValue) -> anyhow::Result<proc_macro2::TokenStream> {
    let visibility = visibility_to_tokens(ev.visibility);
    let function_ident = quote::format_ident!("get_{}", ev.name);
    let type_ = sa_type_to_syn_type(&ev.type_)?;
    let address = hex_literal(ev.address);

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

fn doc_to_tokens(is_module_doc: bool, doc: &[String]) -> proc_macro2::TokenStream {
    if doc.is_empty() {
        return proc_macro2::TokenStream::new();
    }
    // Split each string by lines to handle multi-line doc comments
    // Preserve empty strings as they represent empty doc comment lines (/// or //!)
    let doc_attrs = doc
        .iter()
        .flat_map(|s| {
            if s.is_empty() {
                vec![""]
            } else {
                s.lines().collect::<Vec<_>>()
            }
        })
        .map(|line| {
            if is_module_doc {
                quote! { #![doc = #line] }
            } else {
                quote! { #[doc = #line] }
            }
        });
    quote! {
        #(#doc_attrs)*
    }
}

fn hex_literal(value: impl Into<usize>) -> proc_macro2::Literal {
    // https://stackoverflow.com/a/78902864
    proc_macro2::Literal::from_str(&format!("0x{:X}", value.into())).unwrap()
}
