use std::collections::{BTreeSet, HashMap};

use crate::{
    backends::{BackendError, Result},
    grammar::ItemPath,
    semantic::{
        TypeRegistry,
        types::{
            BitflagsDefinition, EnumDefinition, ItemCategory, ItemDefinition, ItemDefinitionInner,
            ItemStateResolved, Region, TypeAliasDefinition, TypeDefinition, Visibility,
        },
    },
    span::ItemLocation,
};

use quote::{ToTokens, quote};

use super::{
    doc_links::{DocLinkCx, hex_literal},
    helpers::{
        build_extra_derives, build_generic_params, doc_to_tokens, flatten_type_name,
        generate_size_check, sa_type_to_syn_type, str_to_ident, visibility_to_tokens,
    },
    values::{
        build_const, build_extern_value, build_function, build_nested_const_impls,
        build_nested_extern_value_impls,
    },
};

pub(super) fn build_item(
    type_registry: &TypeRegistry,
    definition: &ItemDefinition,
    cfg_ctx: &crate::parser::cfg::CfgContext,
    options: &crate::BuildOptions,
    extern_rust_names: &HashMap<String, String>,
    module_paths: &BTreeSet<ItemPath>,
    doc_cx: &DocLinkCx,
) -> Result<proc_macro2::TokenStream> {
    let resolved = definition
        .resolved()
        .ok_or_else(|| BackendError::TypeCodeGenFailed {
            type_path: definition.path.clone(),
            kind: crate::backends::error::TypeCodeGenFailedKind::TypeNotResolved,
            location: definition.location,
        })?;

    let ItemStateResolved {
        size,
        inner,
        alignment,
    } = resolved;
    let visibility = definition.visibility;
    let path = &definition.path;
    let type_parameters = &definition.type_parameters;

    use ItemDefinitionInner as IDI;
    let location = &definition.location;
    match definition.category() {
        ItemCategory::Defined => match inner {
            IDI::Type(td) => build_type(
                type_registry,
                path,
                *size,
                *alignment,
                visibility,
                td,
                location,
                type_parameters,
                cfg_ctx,
                options,
                module_paths,
                doc_cx,
            ),
            IDI::Enum(ed) => build_enum(
                type_registry,
                path,
                *size,
                visibility,
                ed,
                location,
                cfg_ctx,
                options,
                module_paths,
                doc_cx,
            ),
            IDI::Bitflags(bd) => build_bitflags(
                type_registry,
                path,
                *size,
                visibility,
                bd,
                location,
                options.rust_module_prefix.as_ref(),
                module_paths,
                doc_cx,
            ),
            IDI::TypeAlias(ta) => build_type_alias(
                type_registry,
                path,
                visibility,
                ta,
                location,
                type_parameters,
                options.rust_module_prefix.as_ref(),
                module_paths,
                doc_cx,
            ),
            IDI::Constant(cd) => build_const(path, visibility, cd, location, module_paths, doc_cx),
            IDI::ExternValue(ev) => build_extern_value(
                path,
                visibility,
                ev,
                location,
                options.rust_module_prefix.as_ref(),
                module_paths,
                doc_cx,
            ),
        },
        ItemCategory::Predefined => Ok(quote! {}),
        ItemCategory::Extern => {
            // Emit `pub use <rust_name> as <Name>;` for an extern backed by a
            // real Rust type, so references to it resolve without a
            // hand-written prologue `use`. Externs without a binding emit
            // nothing (the consumer supplies the type some other way).
            let leaf = path.last().map(|s| s.as_str()).unwrap_or_default();
            match extern_rust_names.get(leaf) {
                Some(rust_name) => {
                    let target: syn::Path = syn::parse_str(rust_name).map_err(|e| {
                        BackendError::Formatting(format!(
                            "invalid rust_name `{rust_name}` for extern `{leaf}`: {e}"
                        ))
                    })?;
                    let alias = str_to_ident(leaf);
                    Ok(quote! { pub use #target as #alias; })
                }
                None => Ok(quote! {}),
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn build_type(
    type_registry: &TypeRegistry,
    path: &ItemPath,
    size: usize,
    alignment: usize,
    visibility: Visibility,
    type_definition: &TypeDefinition,
    location: &ItemLocation,
    type_parameters: &[String],
    cfg_ctx: &crate::parser::cfg::CfgContext,
    options: &crate::BuildOptions,
    module_paths: &BTreeSet<ItemPath>,
    doc_cx: &DocLinkCx,
) -> Result<proc_macro2::TokenStream> {
    let name = flatten_type_name(path, module_paths);
    let name = &name;
    let prefix = options.rust_module_prefix.as_ref();

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
        pinned,
        nested_item_paths: _,
    } = type_definition;

    let visibility = visibility_to_tokens(visibility);
    let doc = doc_cx.node(doc, location);
    let mut fields = regions
        .iter()
        .map(|r| {
            let Region {
                visibility,
                name: field,
                doc,
                type_ref,
                is_base: _,
                location,
            } = r;
            let field_name = field
                .as_deref()
                .ok_or_else(|| BackendError::FieldCodeGenFailed {
                    type_path: path.clone(),
                    field_name: "unnamed".to_string(),
                    kind: crate::backends::error::FieldCodeGenFailedKind::FieldNameNotPresent,
                    location: *location,
                })?;
            let field_ident = str_to_ident(field_name);
            let visibility = visibility_to_tokens(*visibility);
            let syn_type = sa_type_to_syn_type(type_ref, prefix, Some(module_paths))?;
            let doc = doc_cx.node(doc, location);
            Ok(quote! {
                #doc
                #visibility #field_ident: #syn_type
            })
        })
        .collect::<Result<Vec<_>>>()?;

    // Pinned types get a PhantomPinned marker field, making them !Unpin and
    // forcing consumers to use Box::pin / Pin<&mut T>.
    if *pinned {
        fields.push(quote! {
            #[doc(hidden)]
            _pin: ::std::marker::PhantomPinned
        });
    }

    let name_ident = str_to_ident(name.as_str());
    let size_check_impl = generate_size_check(name.as_str(), size);

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
        .map(|v| -> Result<proc_macro2::TokenStream> {
            let accessor = if let Some(field) = &v.base_field {
                let field = str_to_ident(field);
                quote! { #field . vftable() }
            } else {
                quote! { vftable }
            };
            let vftable_type = sa_type_to_syn_type(&v.type_, prefix, Some(module_paths))?;
            Ok(quote! {
                pub fn vftable(&self) -> #vftable_type {
                    self. #accessor as #vftable_type
                }
            })
        })
        .transpose()?;

    let cfg_pass = |cfg: &Option<crate::parser::cfg::CfgPredicate>| match cfg {
        Some(p) => p.evaluate(cfg_ctx),
        None => true,
    };
    // Not sure about filtering out internal functions at this level,
    // might be better to do it in semantic?
    let associated_functions_impl = associated_functions
        .iter()
        .filter(|f| !f.is_internal())
        .filter(|f| cfg_pass(&f.cfg))
        .map(|f| build_function(f, options, true, module_paths, doc_cx))
        .collect::<Result<Vec<_>>>()?;

    let vftable_function_impl = vftable
        .as_ref()
        .map(|v| {
            v.functions
                .iter()
                .filter(|f| !f.is_internal())
                .filter(|f| cfg_pass(&f.cfg))
                .map(|f| build_function(f, options, true, module_paths, doc_cx))
                .collect::<Result<Vec<_>>>()
        })
        .transpose()?
        .unwrap_or_default();

    // Pinned types must not be Copy/Clone — that would allow moving out from
    // behind a Pin. Suppress those derives regardless of copyable/cloneable.
    let effective_copyable = *copyable && !*pinned;
    let effective_cloneable = *cloneable && !*pinned;
    let extra_derives = build_extra_derives(effective_copyable, effective_cloneable, *defaultable);

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
                let type_ = sa_type_to_syn_type(&type_, prefix, Some(module_paths))?;

                Ok((type_, field_path))
            })
            .collect::<Result<Vec<_>>>()?;

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
                    let conflicting_impl_doc_lines: Vec<String> = conflicting_impl_message
                        .trim()
                        .lines()
                        .map(|s| s.to_string())
                        .collect();
                    let conflicting_impl_doc = doc_to_tokens(false, &conflicting_impl_doc_lines, None);
                    let conflicting_impl_ident = quote::format_ident!(
                        "_CONFLICTING_{}_{}",
                        &name.to_uppercase(),
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

    // Generate type parameters for generic types
    let generic_params = build_generic_params(type_parameters);

    // For generic types, we can't do compile-time size checks (size depends on T)
    // and we skip some impl blocks that don't make sense for generics
    let size_check_impl = if type_parameters.is_empty() {
        size_check_impl
    } else {
        None
    };

    // Skip as_ref conversions for generic types (they'd need phantom data etc.)
    let as_ref_conversions = if type_parameters.is_empty() {
        as_ref_conversions
    } else {
        vec![]
    };

    // Emit nested constants as associated constants in an impl block
    let nested_const_impls = build_nested_const_impls(type_registry, path, module_paths, doc_cx);
    // Emit nested extern values as associated `get_*` accessors in an impl block
    let nested_extern_value_impls =
        build_nested_extern_value_impls(type_registry, path, module_paths, doc_cx);

    Ok(quote! {
        #derives
        #[repr(C #packed #alignment)]
        #doc
        #visibility struct #name_ident #generic_params {
            #(#fields),*
        }
        #size_check_impl
        #singleton_impl
        impl #generic_params #name_ident #generic_params {
            #vftable_fn_impl
            #(#associated_functions_impl)*
            #(#vftable_function_impl)*
        }
        #nested_const_impls
        #nested_extern_value_impls
        #(#as_ref_conversions)*
    })
}

#[allow(clippy::too_many_arguments)]
fn build_enum(
    type_registry: &TypeRegistry,
    path: &ItemPath,
    size: usize,
    visibility: Visibility,
    enum_definition: &EnumDefinition,
    location: &ItemLocation,
    cfg_ctx: &crate::parser::cfg::CfgContext,
    options: &crate::BuildOptions,
    module_paths: &BTreeSet<ItemPath>,
    doc_cx: &DocLinkCx,
) -> Result<proc_macro2::TokenStream> {
    let name = flatten_type_name(path, module_paths);
    let name = &name;
    let prefix = options.rust_module_prefix.as_ref();

    let EnumDefinition {
        singleton,
        variants,
        doc,
        type_,
        copyable,
        cloneable,
        default,
        associated_functions,
        pinned,
    } = enum_definition;

    let syn_type = sa_type_to_syn_type(type_, prefix, Some(module_paths))?;
    let name_ident = str_to_ident(name.as_str());

    let visibility = visibility_to_tokens(visibility);
    let doc = doc_cx.node(doc, location);

    let size_check_impl = generate_size_check(name.as_str(), size);

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

    // Pinned enums suppress Copy/Clone (pinned types must not be movable).
    // Field-less enums can't have a PhantomPinned field, so this is the only
    // effect of #[pinned] on enums in the Rust backend.
    let effective_copyable = *copyable && !*pinned;
    let effective_cloneable = *cloneable && !*pinned;
    let extra_derives =
        build_extra_derives(effective_copyable, effective_cloneable, default.is_some());

    let syn_fields = variants.iter().enumerate().map(|(idx, variant)| {
        let name_ident = str_to_ident(&variant.name);
        let value = variant.value;
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

    let cfg_pass = |cfg: &Option<crate::parser::cfg::CfgPredicate>| match cfg {
        Some(p) => p.evaluate(cfg_ctx),
        None => true,
    };
    // Build associated functions
    let associated_functions_impl = associated_functions
        .iter()
        .filter(|f| !f.is_internal())
        .filter(|f| cfg_pass(&f.cfg))
        .map(|f| build_function(f, options, true, module_paths, doc_cx))
        .collect::<Result<Vec<_>>>()?;

    let associated_impl = if !associated_functions_impl.is_empty() {
        Some(quote! {
            impl #name_ident {
                #(#associated_functions_impl)*
            }
        })
    } else {
        None
    };

    // Emit nested constants as associated constants in an impl block
    let nested_const_impls = build_nested_const_impls(type_registry, path, module_paths, doc_cx);
    // Emit nested extern values as associated `get_*` accessors in an impl block
    let nested_extern_value_impls =
        build_nested_extern_value_impls(type_registry, path, module_paths, doc_cx);

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
        #nested_const_impls
        #nested_extern_value_impls
    })
}

#[allow(clippy::too_many_arguments)]
fn build_bitflags(
    type_registry: &TypeRegistry,
    path: &ItemPath,
    size: usize,
    visibility: Visibility,
    bitflags_definition: &BitflagsDefinition,
    location: &ItemLocation,
    prefix: Option<&ItemPath>,
    module_paths: &BTreeSet<ItemPath>,
    doc_cx: &DocLinkCx,
) -> Result<proc_macro2::TokenStream> {
    let name = flatten_type_name(path, module_paths);
    let name = &name;

    let BitflagsDefinition {
        singleton,
        flags,
        doc,
        type_,
        default,
        ..
    } = bitflags_definition;

    let syn_type = sa_type_to_syn_type(type_, prefix, Some(module_paths))?;
    let name_ident = str_to_ident(name.as_str());

    let visibility = visibility_to_tokens(visibility);
    let doc = doc_cx.node(doc, location);

    let size_check_impl = generate_size_check(name.as_str(), size);

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
        let field_ident = str_to_ident(&flags[idx].name);
        quote! {
            impl Default for #name_ident {
                fn default() -> Self {
                    Self::#field_ident
                }
            }
        }
    });

    // The `__bitflags!` macro (emitted into the crate root) provides all the
    // derives (`Debug`, `Copy`, `Clone`, comparison, `Hash`, ...) and the
    // bitflags API itself, so we only forward the doc + visibility + flags.
    let syn_fields = flags.iter().map(|flag| {
        let name_ident = str_to_ident(&flag.name);
        let value = flag.value;
        quote! {
            const #name_ident = #value as _;
        }
    });

    // Emit nested constants as associated constants in an impl block
    let nested_const_impls = build_nested_const_impls(type_registry, path, module_paths, doc_cx);
    // Emit nested extern values as associated `get_*` accessors in an impl block
    let nested_extern_value_impls =
        build_nested_extern_value_impls(type_registry, path, module_paths, doc_cx);

    Ok(quote! {
        crate::__bitflags! {
            #doc
            #visibility struct #name_ident: #syn_type {
                #(#syn_fields)*
            }
        }
        #size_check_impl
        #singleton_impl
        #default_impl
        #nested_const_impls
        #nested_extern_value_impls
    })
}

#[allow(clippy::too_many_arguments)]
fn build_type_alias(
    _type_registry: &TypeRegistry,
    path: &ItemPath,
    visibility: Visibility,
    type_alias_definition: &TypeAliasDefinition,
    location: &ItemLocation,
    type_parameters: &[String],
    prefix: Option<&ItemPath>,
    module_paths: &BTreeSet<ItemPath>,
    doc_cx: &DocLinkCx,
) -> Result<proc_macro2::TokenStream> {
    let name = flatten_type_name(path, module_paths);
    let name = &name;

    let TypeAliasDefinition { target, doc } = type_alias_definition;

    let name_ident = str_to_ident(name.as_str());
    let visibility = visibility_to_tokens(visibility);
    let doc = doc_cx.node(doc, location);
    let target_type = sa_type_to_syn_type(target, prefix, Some(module_paths))?;

    let generic_params = build_generic_params(type_parameters);

    Ok(quote! {
        #doc
        #visibility type #name_ident #generic_params = #target_type;
    })
}
