use std::{collections::HashMap, fmt::Write as _, path::Path, str::FromStr, sync::LazyLock};

use crate::{
    backends::{BackendError, Result},
    grammar::ItemPath,
    semantic::{
        Module, SemanticOutput, TypeRegistry,
        types::{
            Argument, BitflagsDefinition, EnumDefinition, ExternValue, Function, FunctionBody,
            ItemCategory, ItemDefinition, ItemDefinitionInner, ItemStateResolved, PredefinedItem,
            Region, Type, TypeAliasDefinition, TypeDefinition, Visibility,
        },
    },
    span::ItemLocation,
};

use quote::{ToTokens, quote};

/// A freestanding reimplementation of the subset of `bitflags::bitflags!` that
/// Pyxis needs, so generated crates don't have to depend on the `bitflags`
/// crate. Emitted once into the crate root (see [`BITFLAGS_MACRO`]) whenever the
/// crate contains any `bitflags` definition, and invoked as `__bitflags!` from
/// every module that defines bitflags.
///
/// The generated type is a `#[repr(transparent)]` newtype over the underlying
/// integer, always `Copy + Clone`, with the usual bitflags API (`contains`,
/// `insert`, `|`, `&`, `^`, `!`, `from_bits`, ...). It mirrors the ergonomics of
/// the `bitflags` crate closely enough for typical consumption without pulling
/// in an external dependency.
const BITFLAGS_MACRO: &str = include_str!("bitflags_impl.rs");

/// Whether any (Rust-cfg-included) bitflags definition exists anywhere in the
/// crate. Used to decide whether to emit [`BITFLAGS_MACRO`] into the root
/// module.
fn crate_uses_bitflags(
    semantic_state: &SemanticOutput,
    cfg_ctx: &crate::parser::cfg::CfgContext,
) -> bool {
    let cfg_pass = |cfg: &Option<crate::parser::cfg::CfgPredicate>| match cfg {
        Some(p) => p.evaluate(cfg_ctx),
        None => true,
    };
    let type_registry = semantic_state.type_registry();
    semantic_state.modules().values().any(|module| {
        module
            .definitions(type_registry)
            .filter(|d| cfg_pass(&d.cfg))
            .any(|d| {
                d.resolved()
                    .is_some_and(|r| matches!(r.inner, ItemDefinitionInner::Bitflags(_)))
            })
    })
}

pub fn write_module(
    out_dir: &Path,
    key: &ItemPath,
    semantic_state: &SemanticOutput,
    module: &Module,
    options: &crate::BuildOptions,
) -> Result<()> {
    const FORMAT_OUTPUT: bool = true;

    // Direct child modules (sorted by name), used both to wire up `pub mod`
    // / `pub use` declarations and to decide this module's file layout.
    let mut children: Vec<(&str, &ItemPath, &Module)> = semantic_state
        .modules()
        .iter()
        .filter(|(p, _)| p.parent().as_ref() == Some(key))
        .filter_map(|(p, m)| p.last().map(|s| (s.as_str(), p, m)))
        .collect();
    children.sort_by_key(|(name, _, _)| *name);
    let has_children = !children.is_empty();

    // Output path:
    // - root module (empty key)  -> <out_dir>/lib.rs
    // - module with children     -> <out_dir>/<segments>/mod.rs
    // - leaf module              -> <out_dir>/<segments>.rs
    let mut path = out_dir.to_path_buf();
    for segment in key.iter() {
        path.push(segment.as_str());
    }
    if key.is_empty() {
        path.push(options.rust_root_file_name.as_deref().unwrap_or("lib.rs"));
    } else if has_children {
        path.push("mod.rs");
    } else {
        path.set_extension("rs");
    }

    let directory_path = path.parent().map(|p| p.to_path_buf()).unwrap_or_default();
    std::fs::create_dir_all(&directory_path).map_err(|e| BackendError::Io {
        error: e,
        context: format!("Failed to create directory {}", directory_path.display()),
    })?;

    let mut raw_output = String::new();

    let cfg_ctx = crate::parser::cfg::CfgContext {
        backend: crate::Backend::Rust,
    };

    // Lint `allow`s cascade to descendant modules, so they only need to live
    // on the root file (lib.rs / the mounted-subtree root); emitting them on
    // the root also overrides a stricter host crate (the innermost level
    // wins), keeping the whole generated subtree quiet.
    if key.is_empty() {
        writeln!(
            raw_output,
            "#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals, clippy::missing_safety_doc, clippy::unnecessary_cast, clippy::module_inception)]"
        )?;
    }
    // Disable rustfmt on generated files to prevent the prettyplease-formatted code being reformatted
    // by a stray project-wide `cargo fmt` invocation. (Per-file: rustfmt runs
    // per-file, so unlike lint levels this can't live only on the root.)
    // <https://stackoverflow.com/questions/59247458/is-there-a-stable-way-to-tell-rustfmt-to-skip-an-entire-file#comment138279076_75910283>
    writeln!(raw_output, "#![cfg_attr(any(), rustfmt::skip)]")?;
    writeln!(raw_output, "{}", doc_to_tokens(true, module.doc()))?;

    // Emit the freestanding `__bitflags!` macro definition exactly once, on the
    // crate root, when the crate contains any bitflags. It is
    // `#[macro_export]`-ed, so it lands at the crate root regardless of any
    // module prefix and is callable from every module as `crate::__bitflags!`.
    // This lets generated crates drop their `bitflags` dependency. It must
    // follow the inner attributes (`#![...]`) above, since those can only
    // precede items in a module body.
    if key.is_empty() && crate_uses_bitflags(semantic_state, &cfg_ctx) {
        raw_output.push_str(BITFLAGS_MACRO);
    }

    let backends = module.backends.get(&crate::Backend::Rust);
    let prologues = backends
        .iter()
        .flat_map(|bs| bs.iter().flat_map(|b| b.prologue.header.as_deref()))
        .collect::<Vec<_>>()
        .join("\n");
    let epilogues = backends
        .iter()
        .flat_map(|bs| bs.iter().flat_map(|b| b.epilogue.header.as_deref()))
        .collect::<Vec<_>>()
        .join("\n");

    writeln!(raw_output, "{prologues}")?;

    // Wire up child modules. Every folder that contains `.pyxis` files has a
    // module (see `synthesize_ancestor_modules`), so this produces a complete
    // module tree without any hand-written `mod.rs`/`lib.rs`. The optional
    // `pub use <child>::*;` re-export is controlled by `rust_reexport_children`,
    // and a child can opt out of it with `#![rust(no_reexport)]`.
    for (child, child_path, child_module) in &children {
        writeln!(raw_output, "pub mod {child};")?;
        // Skip the glob re-export when the module opts out, or when it has
        // nothing public to re-export (a vacuous `pub use` would just trip
        // `unused_imports`).
        if options.rust_reexport_children
            && !child_module.rust_no_reexport()
            && module_has_public_exports(child_path, child_module, semantic_state)
        {
            writeln!(raw_output, "pub use {child}::*;")?;
        }
    }

    // Import every item referenced by an intra-doc link in this module's docs
    // so rustdoc can resolve `[`Type`]` / `[`Type::method`]` style links. The
    // imports exist purely for the links (the names may otherwise be unused),
    // hence the `allow`. Items local to this module or already in scope are
    // skipped, as are paths that aren't plain Rust identifiers.
    let module_scope = module.scope();
    let mut doc_imports = semantic_state.doc_link_resolver().module_imports(
        semantic_state.type_registry(),
        semantic_state.modules(),
        key,
    );
    doc_imports.retain(|p| {
        p.parent().as_ref() != Some(key)
            && !module_scope.contains(p)
            && p.iter().all(|s| is_plain_ident(s.as_str()))
    });
    // De-duplicate by leaf name: two different items sharing a leaf can't both
    // be imported unqualified (and the link would be ambiguous anyway).
    {
        let mut seen_leaves = std::collections::HashSet::new();
        doc_imports.retain(|p| {
            p.last()
                .map(|s| seen_leaves.insert(s.as_str().to_string()))
                .unwrap_or(false)
        });
    }
    if !doc_imports.is_empty() {
        let prefix = options.rust_module_prefix.as_ref();
        let root = match prefix {
            Some(prefix) => format!("crate::{prefix}"),
            None => "crate".to_string(),
        };
        let inner = doc_imports
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(raw_output, "#[allow(unused_imports)]")?;
        writeln!(raw_output, "use {root}::{{{inner}}};")?;
    }

    let cfg_pass = |cfg: &Option<crate::parser::cfg::CfgPredicate>| match cfg {
        Some(p) => p.evaluate(&cfg_ctx),
        None => true,
    };
    // Extern types with a `#[rust_name = "..."]` binding get a `pub use`
    // alias to the real Rust type (keyed by extern name).
    let extern_rust_names: HashMap<String, String> = module
        .extern_rust_names()
        .map(|(name, path)| (name.to_string(), path.to_string()))
        .collect();
    let mut definitions = module
        .definitions(semantic_state.type_registry())
        .filter(|d| cfg_pass(&d.cfg))
        .collect::<Vec<_>>();
    definitions.sort_by_key(|d| &d.path);
    for definition in definitions {
        writeln!(
            raw_output,
            "{}",
            build_item(
                semantic_state.type_registry(),
                definition,
                &cfg_ctx,
                options,
                &extern_rust_names,
            )?
        )?;
    }

    let mut extern_values = module.extern_values.clone();
    extern_values.sort_by_key(|ev| ev.name.clone());
    for ev in &extern_values {
        writeln!(
            raw_output,
            "{}",
            build_extern_value(ev, options.rust_module_prefix.as_ref())?
        )?;
    }

    // Generate freestanding functions
    let freestanding_functions = module
        .functions()
        .iter()
        .filter(|f| !f.is_internal())
        .filter(|f| cfg_pass(&f.cfg))
        .map(|f| build_function(f, options, false))
        .collect::<Result<Vec<_>>>()?;
    for func in freestanding_functions {
        writeln!(raw_output, "{func}")?;
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

    std::fs::write(&path, &output).map_err(|e| BackendError::Io {
        error: e,
        context: format!("Failed to write Rust output to {}", path.display()),
    })?;

    if let Some(error) = error {
        return Err(BackendError::Formatting(error));
    }

    Ok(())
}

/// Whether `pub use <module>::*;` would re-export anything: a public
/// definition, public free function, public extern value, or a child module
/// (whose name the glob re-exports). Extern types emit no Rust item, so they
/// don't count. A module with only private items produces a vacuous glob.
fn module_has_public_exports(
    path: &ItemPath,
    module: &Module,
    semantic_state: &SemanticOutput,
) -> bool {
    let type_registry = semantic_state.type_registry();
    module
        .definitions(type_registry)
        .any(|d| d.visibility == Visibility::Public)
        || module.functions().iter().any(|f| f.is_public())
        || module
            .extern_values
            .iter()
            .any(|ev| ev.visibility == Visibility::Public)
        || semantic_state
            .modules()
            .keys()
            .any(|p| p.parent().as_ref() == Some(path))
}

fn build_item(
    type_registry: &TypeRegistry,
    definition: &ItemDefinition,
    cfg_ctx: &crate::parser::cfg::CfgContext,
    options: &crate::BuildOptions,
    extern_rust_names: &HashMap<String, String>,
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
            ),
            IDI::Enum(ed) => build_enum(path, *size, visibility, ed, location, cfg_ctx, options),
            IDI::Bitflags(bd) => build_bitflags(
                path,
                *size,
                visibility,
                bd,
                location,
                options.rust_module_prefix.as_ref(),
            ),
            IDI::TypeAlias(ta) => build_type_alias(
                type_registry,
                path,
                visibility,
                ta,
                location,
                type_parameters,
                options.rust_module_prefix.as_ref(),
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
) -> Result<proc_macro2::TokenStream> {
    let name = get_type_name(path, location)?;
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
    } = type_definition;

    let visibility = visibility_to_tokens(visibility);
    let doc = doc_to_tokens(false, doc);
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
            let syn_type = sa_type_to_syn_type(type_ref, prefix)?;
            let doc = doc_to_tokens(false, doc);
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
            let vftable_type = sa_type_to_syn_type(&v.type_, prefix)?;
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
        .map(|f| build_function(f, options, true))
        .collect::<Result<Vec<_>>>()?;

    let vftable_function_impl = vftable
        .as_ref()
        .map(|v| {
            v.functions
                .iter()
                .filter(|f| !f.is_internal())
                .filter(|f| cfg_pass(&f.cfg))
                .map(|f| build_function(f, options, true))
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
                let type_ = sa_type_to_syn_type(&type_, prefix)?;

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
                    let conflicting_impl_doc = doc_to_tokens(false, &conflicting_impl_doc_lines);
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
        #(#as_ref_conversions)*
    })
}

fn build_enum(
    path: &ItemPath,
    size: usize,
    visibility: Visibility,
    enum_definition: &EnumDefinition,
    location: &ItemLocation,
    cfg_ctx: &crate::parser::cfg::CfgContext,
    options: &crate::BuildOptions,
) -> Result<proc_macro2::TokenStream> {
    let name = get_type_name(path, location)?;
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

    let syn_type = sa_type_to_syn_type(type_, prefix)?;
    let name_ident = str_to_ident(name.as_str());

    let visibility = visibility_to_tokens(visibility);
    let doc = doc_to_tokens(false, doc);

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
        .map(|f| build_function(f, options, true))
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
    location: &ItemLocation,
    prefix: Option<&ItemPath>,
) -> Result<proc_macro2::TokenStream> {
    let name = get_type_name(path, location)?;

    let BitflagsDefinition {
        singleton,
        flags,
        doc,
        type_,
        default,
        ..
    } = bitflags_definition;

    let syn_type = sa_type_to_syn_type(type_, prefix)?;
    let name_ident = str_to_ident(name.as_str());

    let visibility = visibility_to_tokens(visibility);
    let doc = doc_to_tokens(false, doc);

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
    })
}

fn build_type_alias(
    _type_registry: &TypeRegistry,
    path: &ItemPath,
    visibility: Visibility,
    type_alias_definition: &TypeAliasDefinition,
    location: &ItemLocation,
    type_parameters: &[String],
    prefix: Option<&ItemPath>,
) -> Result<proc_macro2::TokenStream> {
    let name = get_type_name(path, location)?;

    let TypeAliasDefinition { target, doc } = type_alias_definition;

    let name_ident = str_to_ident(name.as_str());
    let visibility = visibility_to_tokens(visibility);
    let doc = doc_to_tokens(false, doc);
    let target_type = sa_type_to_syn_type(target, prefix)?;

    let generic_params = build_generic_params(type_parameters);

    Ok(quote! {
        #doc
        #visibility type #name_ident #generic_params = #target_type;
    })
}

fn build_function(
    function: &Function,
    options: &crate::BuildOptions,
    in_impl: bool,
) -> Result<proc_macro2::TokenStream> {
    let prefix = options.rust_module_prefix.as_ref();
    // External-body methods declare their existence in pyxis but get their
    // body from the user's `backend rust prologue/epilogue` block. Rust
    // permits multiple `impl Foo` blocks, so the user's epilogue can host
    // its own `impl Foo { fn bar(...) { ... } }` without conflict — and
    // the rust backend skips emission entirely.
    if function.body.is_external() {
        return Ok(proc_macro2::TokenStream::new());
    }
    let name = str_to_ident(&function.name);
    let doc = doc_to_tokens(false, &function.doc);

    let arguments = function
        .arguments
        .iter()
        .map(|a| {
            Ok(match a {
                Argument::ConstSelf { .. } => quote! { &self },
                Argument::MutSelf { .. } => quote! { &mut self },
                Argument::Field { name, type_, .. } => {
                    let name = str_to_ident(name);
                    let syn_type = sa_type_to_syn_type(type_, prefix)?;
                    quote! {
                        #name: #syn_type
                    }
                }
            })
        })
        .collect::<Result<Vec<_>>>()?;

    let lambda_arguments = function
        .arguments
        .iter()
        .map(|a| {
            Ok(match a {
                Argument::ConstSelf { .. } => quote! { this: *const Self },
                Argument::MutSelf { .. } => quote! { this: *mut Self },
                Argument::Field { name, type_, .. } => {
                    let name = str_to_ident(name);
                    let syn_type = sa_type_to_syn_type(type_, prefix)?;
                    quote! {
                        #name: #syn_type
                    }
                }
            })
        })
        .collect::<Result<Vec<_>>>()?;

    let is_field_function = function.body.is_field();
    let call_arguments = function
        .arguments
        .iter()
        // Only pass `self` to the function if it's not a field function
        .filter(|a| !is_field_function || !a.is_self())
        .map(|a| match a {
            Argument::ConstSelf { .. } => quote! { self as *const Self as _ },
            Argument::MutSelf { .. } => quote! { self as *mut Self as _ },
            Argument::Field { name, .. } => {
                let name = str_to_ident(name);
                quote! { #name }
            }
        })
        .collect::<Vec<_>>();

    let return_type = function
        .return_type
        .as_ref()
        .map(|type_ref| -> Result<proc_macro2::TokenStream> {
            let syn_type = sa_type_to_syn_type(type_ref, prefix)?;
            Ok(quote! { -> #syn_type })
        })
        .transpose()?;

    let calling_convention = function.calling_convention.as_str();
    // When the `public_addresses` option is set, emit a `pub const <Fn>_ADDRESS: usize`
    // alongside the function so consumers can reference the address (e.g. to hook it)
    // without hardcoding it. The const is always `pub` so it's usable even when the
    // function wrapper itself is private. The function body transmutes the const.
    let mut address_const = proc_macro2::TokenStream::new();
    let function_body = match &function.body {
        FunctionBody::Address { address } => {
            let address_lit = hex_literal(*address);
            let transmute_target = if options.public_addresses {
                let const_ident = quote::format_ident!("{}_ADDRESS", function.name);
                address_const = quote! {
                    pub const #const_ident: usize = #address_lit;
                };
                if in_impl {
                    quote! { Self::#const_ident }
                } else {
                    quote! { #const_ident }
                }
            } else {
                quote! { #address_lit as usize }
            };
            quote! {
                let f:
                    unsafe extern #calling_convention
                    fn(#(#lambda_arguments),*) #return_type
                = ::std::mem::transmute(#transmute_target);
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
        FunctionBody::External => {
            // External-body functions are short-circuited at the top of
            // build_function — we never reach here.
            unreachable!("FunctionBody::External handled above");
        }
    };

    let visibility = visibility_to_tokens(function.visibility);
    Ok(quote! {
        #address_const
        #doc
        #visibility unsafe fn #name(#(#arguments),*) #return_type {
            unsafe {
                #function_body
            }
        }
    })
}

fn build_extern_value(
    ev: &ExternValue,
    prefix: Option<&ItemPath>,
) -> Result<proc_macro2::TokenStream> {
    let visibility = visibility_to_tokens(ev.visibility);
    let function_ident = quote::format_ident!("get_{}", ev.name);
    let type_ = sa_type_to_syn_type(&ev.type_, prefix)?;
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

/// Whether `s` is a plain Rust identifier (no generics, operators, etc.), so it
/// can appear verbatim in a `use` path.
fn is_plain_ident(s: &str) -> bool {
    let mut chars = s.chars();
    chars
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// Extract the type name from an ItemPath, returning an error if the path is empty.
fn get_type_name<'a>(
    path: &'a ItemPath,
    location: &ItemLocation,
) -> Result<&'a crate::grammar::ItemPathSegment> {
    path.last().ok_or_else(|| BackendError::TypeCodeGenFailed {
        type_path: path.clone(),
        kind: crate::backends::error::TypeCodeGenFailedKind::EmptyItemPath,
        location: *location,
    })
}

/// Generate a compile-time size check function for a type.
fn generate_size_check(name: &str, size: usize) -> Option<proc_macro2::TokenStream> {
    (size > 0).then(|| {
        let name_ident = str_to_ident(name);
        let size_check_ident = quote::format_ident!("_{}_size_check", name);
        let size = hex_literal(size);
        quote! {
            fn #size_check_ident() {
                unsafe {
                    ::std::mem::transmute::<[u8; #size], #name_ident>([0u8; #size]);
                }
                unreachable!()
            }
        }
    })
}

/// Build the extra derive attributes based on type properties.
fn build_extra_derives(
    copyable: bool,
    cloneable: bool,
    defaultable: bool,
) -> Vec<proc_macro2::TokenStream> {
    let mut derives = vec![];
    if copyable {
        derives.push(quote! { Copy });
    }
    if cloneable {
        derives.push(quote! { Clone });
    }
    if defaultable {
        derives.push(quote! { Default });
    }
    derives
}

/// Generate type parameter tokens for generic types.
fn build_generic_params(type_parameters: &[String]) -> proc_macro2::TokenStream {
    if type_parameters.is_empty() {
        quote! {}
    } else {
        let type_param_idents: Vec<proc_macro2::Ident> =
            type_parameters.iter().map(|p| str_to_ident(p)).collect();
        quote! { < #(#type_param_idents),* > }
    }
}

fn fully_qualified_type_ref_impl(
    out: &mut String,
    type_ref: &Type,
    prefix: Option<&ItemPath>,
) -> std::result::Result<(), std::fmt::Error> {
    use std::fmt::Write;

    // `crate::` qualifier, including any module prefix that mounts the
    // generated tree as a submodule (e.g. `crate::jc2::`).
    fn write_crate_qualifier(
        out: &mut String,
        prefix: Option<&ItemPath>,
    ) -> std::result::Result<(), std::fmt::Error> {
        write!(out, "crate::")?;
        if let Some(prefix) = prefix {
            write!(out, "{prefix}::")?;
        }
        Ok(())
    }

    /// Maps predefined type paths to their Rust type names.
    ///
    /// Most predefined types map directly to Rust primitives, but `void`
    /// maps to `::std::ffi::c_void`. Future backends (e.g., C#) would use
    /// different mappings (u8→byte, i32→int, etc.).
    ///
    /// Uses exhaustive match to ensure new predefined types are handled.
    static PREDEFINED_TYPE_MAP: LazyLock<HashMap<ItemPath, &'static str>> = LazyLock::new(|| {
        PredefinedItem::ALL
            .iter()
            .map(|p| {
                let rust_type = match p {
                    PredefinedItem::Void => "::std::ffi::c_void",
                    PredefinedItem::Bool => "bool",
                    PredefinedItem::U8 => "u8",
                    PredefinedItem::U16 => "u16",
                    PredefinedItem::U32 => "u32",
                    PredefinedItem::U64 => "u64",
                    PredefinedItem::U128 => "u128",
                    PredefinedItem::I8 => "i8",
                    PredefinedItem::I16 => "i16",
                    PredefinedItem::I32 => "i32",
                    PredefinedItem::I64 => "i64",
                    PredefinedItem::I128 => "i128",
                    PredefinedItem::F32 => "f32",
                    PredefinedItem::F64 => "f64",
                    PredefinedItem::CChar => "::std::ffi::c_char",
                    // Atomic types
                    PredefinedItem::AtomicBool => "::std::sync::atomic::AtomicBool",
                    PredefinedItem::AtomicU8 => "::std::sync::atomic::AtomicU8",
                    PredefinedItem::AtomicU16 => "::std::sync::atomic::AtomicU16",
                    PredefinedItem::AtomicU32 => "::std::sync::atomic::AtomicU32",
                    PredefinedItem::AtomicU64 => "::std::sync::atomic::AtomicU64",
                    PredefinedItem::AtomicI8 => "::std::sync::atomic::AtomicI8",
                    PredefinedItem::AtomicI16 => "::std::sync::atomic::AtomicI16",
                    PredefinedItem::AtomicI32 => "::std::sync::atomic::AtomicI32",
                    PredefinedItem::AtomicI64 => "::std::sync::atomic::AtomicI64",
                };
                (ItemPath::from(p.name()), rust_type)
            })
            .collect()
    });

    match type_ref {
        Type::Unresolved(_) => panic!("received unresolved type {type_ref:?}"),
        Type::Raw(path) => {
            // Check if this is a predefined type
            if let Some(rust_type) = PREDEFINED_TYPE_MAP.get(path) {
                return write!(out, "{rust_type}");
            }
            // Not a predefined type - qualify with crate:: if needed
            if path.len() > 1 {
                write_crate_qualifier(out, prefix)?;
            }
            write!(out, "{path}")
        }
        Type::Generic(base_path, args) => {
            // Generate Rust generic syntax: `Base<Arg1, Arg2>`
            if base_path.len() > 1 {
                write_crate_qualifier(out, prefix)?;
            }
            write!(out, "{base_path}<")?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ")?;
                }
                fully_qualified_type_ref_impl(out, arg, prefix)?;
            }
            write!(out, ">")
        }
        Type::TypeParameter(name) => {
            // Type parameter - just output the name (e.g., `T`)
            write!(out, "{name}")
        }
        Type::ConstPointer(tr) => {
            write!(out, "*const ")?;
            fully_qualified_type_ref_impl(out, tr.as_ref(), prefix)
        }
        Type::MutPointer(tr) => {
            write!(out, "*mut ")?;
            fully_qualified_type_ref_impl(out, tr.as_ref(), prefix)
        }
        Type::Array(tr, size) => {
            write!(out, "[")?;
            fully_qualified_type_ref_impl(out, tr.as_ref(), prefix)?;
            write!(out, "; {size}]")
        }
        Type::Function(calling_convention, args, return_type) => {
            write!(out, r#"unsafe extern "{calling_convention}" fn ("#)?;
            for (field, type_ref) in args.iter() {
                write!(out, "{field}: ")?;
                fully_qualified_type_ref_impl(out, type_ref, prefix)?;
                write!(out, ", ")?;
            }
            write!(out, ")")?;
            if let Some(type_ref) = return_type {
                write!(out, " -> ")?;
                fully_qualified_type_ref_impl(out, type_ref, prefix)?;
            }
            Ok(())
        }
    }
}

fn fully_qualified_type_ref(
    type_ref: &Type,
    prefix: Option<&ItemPath>,
) -> std::result::Result<String, std::fmt::Error> {
    let mut out = String::new();
    fully_qualified_type_ref_impl(&mut out, type_ref, prefix)?;
    Ok(out)
}

fn sa_type_to_syn_type(type_ref: &Type, prefix: Option<&ItemPath>) -> Result<syn::Type> {
    Ok(syn::parse_str(&fully_qualified_type_ref(
        type_ref, prefix,
    )?)?)
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
    };
    let doc_attrs = doc.iter().map(|line| {
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
