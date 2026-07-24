use std::collections::BTreeSet;

use crate::{
    backends::Result,
    grammar::ItemPath,
    semantic::{
        TypeRegistry,
        types::{
            Argument, ConstDefinition as SemanticConstDefinition, ConstValue,
            ExternValueDefinition as SemanticExternValueDefinition, Function, FunctionBody,
            ItemDefinitionInner, Type, Visibility,
        },
    },
    span::ItemLocation,
};

use quote::quote;

use super::{
    doc_links::{DocLinkCx, hex_literal},
    helpers::{flatten_type_name, sa_type_to_syn_type, str_to_ident, visibility_to_tokens},
};

/// Render a `ConstValue` as a Rust expression token stream. `type_` is the
/// const's declared type, consulted to pick the right float literal suffix.
/// `module_paths` is needed to flatten type/const paths for nested items.
fn const_value_to_tokens(
    value: &ConstValue,
    type_: &Type,
    module_paths: &BTreeSet<ItemPath>,
) -> proc_macro2::TokenStream {
    match value {
        ConstValue::Int(v) => {
            let lit = proc_macro2::Literal::i64_unsuffixed(*v as i64);
            quote! { #lit }
        }
        ConstValue::Float(bits) => {
            let f = f64::from_bits(*bits);
            // For f32, narrow before rendering; f64 emits bare.
            if type_.is_f32() {
                let lit = proc_macro2::Literal::f32_unsuffixed(f as f32);
                quote! { #lit }
            } else {
                let lit = proc_macro2::Literal::f64_unsuffixed(f);
                quote! { #lit }
            }
        }
        ConstValue::String(s) => {
            let s = s.as_str();
            quote! { #s }
        }
        ConstValue::CString(s) => {
            // `quote! { c#s }` would tokenize `c` and `#s` as two separate
            // tokens (identifier + string literal), not a single C-string
            // literal. Construct the full literal as a string and parse it
            // into a TokenStream, mirroring how `EnumValue` handles paths.
            // The stored value has escapes resolved, so re-escape before
            // embedding in the literal.
            let escaped = escape_rust_string_contents(s);
            let lit = format!("c\"{escaped}\"");
            lit.parse().unwrap_or_else(|_| quote! { () })
        }
        ConstValue::EnumValue(p) => {
            // Build the path as a Rust path expression. We construct it as a
            // string and parse it to get proper tokenization.
            let path_str = p.to_string();
            path_str.parse().unwrap_or_else(|_| quote! { () })
        }
        ConstValue::Struct { type_path, fields } => {
            // Emit `TypeName { field: value, ... }` using the fully-qualified
            // type name (same rendering as the const's type annotation).
            let type_syn = match sa_type_to_syn_type(
                &Type::Raw(type_path.clone()),
                None,
                Some(module_paths),
            ) {
                Ok(t) => t,
                Err(_) => {
                    // Fallback to flattened name if full qualification fails.
                    let flat = flatten_type_name(type_path, module_paths);
                    let flat_ident = str_to_ident(&flat);
                    let field_tokens: Vec<proc_macro2::TokenStream> = fields
                        .iter()
                        .map(|(name, val)| {
                            let field_ident = str_to_ident(name);
                            let val_tokens = const_value_to_tokens(val, type_, module_paths);
                            quote! { #field_ident: #val_tokens }
                        })
                        .collect();
                    return quote! { #flat_ident { #(#field_tokens),* } };
                }
            };
            let field_tokens: Vec<proc_macro2::TokenStream> = fields
                .iter()
                .map(|(name, val)| {
                    let field_ident = str_to_ident(name);
                    let val_tokens = const_value_to_tokens(val, type_, module_paths);
                    quote! { #field_ident: #val_tokens }
                })
                .collect();
            quote! { #type_syn { #(#field_tokens),* } }
        }
        ConstValue::Array(elements) => {
            let elem_tokens: Vec<proc_macro2::TokenStream> = elements
                .iter()
                .map(|e| const_value_to_tokens(e, type_, module_paths))
                .collect();
            quote! { [ #(#elem_tokens),* ] }
        }
        ConstValue::ConstRef(path) => {
            // Flatten the path the same way type names are flattened, since
            // nested constants are emitted as `ParentName_ConstName` in Rust.
            let flat = flatten_type_name(path, module_paths);
            flat.parse().unwrap_or_else(|_| quote! { () })
        }
    }
}

/// Escape string contents for embedding inside a Rust string literal
/// (between the quotes). Re-escapes `\`, `"`, `\n`, `\r`, `\t` — the set of
/// characters that have special meaning inside a regular string literal.
fn escape_rust_string_contents(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }
    out
}

pub(super) fn build_const(
    path: &ItemPath,
    visibility: Visibility,
    const_definition: &SemanticConstDefinition,
    location: &ItemLocation,
    module_paths: &BTreeSet<ItemPath>,
    doc_cx: &DocLinkCx,
) -> Result<proc_macro2::TokenStream> {
    let name = flatten_type_name(path, module_paths);
    let name_ident = str_to_ident(name.as_str());
    let visibility = visibility_to_tokens(visibility);
    let type_ = sa_type_to_syn_type(&const_definition.type_, None, Some(module_paths))?;
    let doc = doc_cx.node(&const_definition.doc, location);
    let value_tokens = const_value_to_tokens(
        &const_definition.value,
        &const_definition.type_,
        module_paths,
    );

    Ok(quote! {
        #doc
        #visibility const #name_ident: #type_ = #value_tokens;
    })
}

/// Collect nested constants from the type registry for a given parent path
/// and emit them as associated constants inside an `impl` block.
pub(super) fn build_nested_const_impls(
    type_registry: &TypeRegistry,
    parent_path: &ItemPath,
    module_paths: &BTreeSet<ItemPath>,
    doc_cx: &DocLinkCx,
) -> Option<proc_macro2::TokenStream> {
    use ItemDefinitionInner as IDI;

    let parent_name = flatten_type_name(parent_path, module_paths);
    let parent_ident = str_to_ident(parent_name.as_str());

    let mut const_items: Vec<proc_macro2::TokenStream> = Vec::new();

    // Iterate all items in the registry that are direct children of parent_path
    for (item_path, item) in type_registry.iter() {
        if item_path.parent().as_ref() != Some(parent_path) {
            continue;
        }
        let Some(resolved) = item.resolved() else {
            continue;
        };
        if let IDI::Constant(cd) = &resolved.inner {
            let const_name = item_path.last().map(|s| s.as_str()).unwrap_or_default();
            let name_ident = str_to_ident(const_name);
            let type_ = match sa_type_to_syn_type(&cd.type_, None, Some(module_paths)) {
                Ok(t) => t,
                Err(_) => continue,
            };
            let doc = doc_cx.node(&cd.doc, &item.location);
            let value_tokens = const_value_to_tokens(&cd.value, &cd.type_, module_paths);

            const_items.push(quote! {
                #doc
                pub const #name_ident: #type_ = #value_tokens;
            });
        }
    }

    if const_items.is_empty() {
        None
    } else {
        Some(quote! {
            impl #parent_ident {
                #(#const_items)*
            }
        })
    }
}

pub(super) fn build_function(
    function: &Function,
    options: &crate::BuildOptions,
    in_impl: bool,
    module_paths: &BTreeSet<ItemPath>,
    doc_cx: &DocLinkCx,
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
    let doc = doc_cx.node(&function.doc, &function.location);

    let arguments = function
        .arguments
        .iter()
        .map(|a| {
            Ok(match a {
                Argument::ConstSelf { .. } => quote! { &self },
                Argument::MutSelf { .. } => quote! { &mut self },
                Argument::Field { name, type_, .. } => {
                    let name = str_to_ident(name);
                    let syn_type = sa_type_to_syn_type(type_, prefix, Some(module_paths))?;
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
                    let syn_type = sa_type_to_syn_type(type_, prefix, Some(module_paths))?;
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
            let syn_type = sa_type_to_syn_type(type_ref, prefix, Some(module_paths))?;
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

/// The identifier of an extern value's accessor. Both the accessor emission
/// (module-level free fn and nested associated fn) and the doc-link rewriting go
/// through this, so a doc link to an extern value resolves to the exact name the
/// accessor is emitted under.
pub(super) fn extern_value_accessor_name(value_name: &str) -> String {
    format!("get_{value_name}")
}

/// Emit a module-level extern value as a freestanding `get_<name>()` accessor
/// over its fixed address.
pub(super) fn build_extern_value(
    path: &ItemPath,
    visibility: Visibility,
    ev: &SemanticExternValueDefinition,
    location: &ItemLocation,
    prefix: Option<&ItemPath>,
    module_paths: &BTreeSet<ItemPath>,
    doc_cx: &DocLinkCx,
) -> Result<proc_macro2::TokenStream> {
    let name = flatten_type_name(path, module_paths);
    let visibility = visibility_to_tokens(visibility);
    let function_ident = str_to_ident(&extern_value_accessor_name(&name));
    let type_ = sa_type_to_syn_type(&ev.type_, prefix, Some(module_paths))?;
    let address = hex_literal(ev.address);
    let doc = doc_cx.node(&ev.doc, location);

    Ok(quote! {
        #doc
        #visibility unsafe fn #function_ident() -> &'static mut #type_ {
            unsafe { &mut *(#address as *mut #type_) }
        }
    })
}

/// Collect nested extern values from the type registry for a given parent path
/// and emit them as associated `get_<name>()` accessors inside an `impl` block —
/// the value-item analogue of [`build_nested_const_impls`], modelling e.g. a
/// C++ class's static globals as `Parent::get_<name>()`.
pub(super) fn build_nested_extern_value_impls(
    type_registry: &TypeRegistry,
    parent_path: &ItemPath,
    module_paths: &BTreeSet<ItemPath>,
    doc_cx: &DocLinkCx,
) -> Option<proc_macro2::TokenStream> {
    use ItemDefinitionInner as IDI;

    let parent_name = flatten_type_name(parent_path, module_paths);
    let parent_ident = str_to_ident(parent_name.as_str());

    let mut items: Vec<proc_macro2::TokenStream> = Vec::new();

    for (item_path, item) in type_registry.iter() {
        if item_path.parent().as_ref() != Some(parent_path) {
            continue;
        }
        let Some(resolved) = item.resolved() else {
            continue;
        };
        if let IDI::ExternValue(ev) = &resolved.inner {
            let value_name = item_path.last().map(|s| s.as_str()).unwrap_or_default();
            let function_ident = str_to_ident(&extern_value_accessor_name(value_name));
            let type_ = match sa_type_to_syn_type(&ev.type_, None, Some(module_paths)) {
                Ok(t) => t,
                Err(_) => continue,
            };
            let address = hex_literal(ev.address);
            let doc = doc_cx.node(&ev.doc, &item.location);

            items.push(quote! {
                #doc
                pub unsafe fn #function_ident() -> &'static mut #type_ {
                    unsafe { &mut *(#address as *mut #type_) }
                }
            });
        }
    }

    if items.is_empty() {
        None
    } else {
        Some(quote! {
            impl #parent_ident {
                #(#items)*
            }
        })
    }
}
