use std::{
    collections::{BTreeSet, HashMap},
    sync::LazyLock,
};

use crate::{
    backends::Result,
    grammar::ItemPath,
    semantic::{
        doc_links::ResolvedDocLink,
        types::{PredefinedItem, Type, Visibility},
    },
};

use quote::quote;

use super::{doc_links::DocLinkCx, values::extern_value_accessor_name};

pub(super) fn str_to_ident(s: &str) -> syn::Ident {
    quote::format_ident!("{}", s)
}

/// Flatten a nested item path to a Rust-safe identifier by joining type-nesting
/// segments with `_`. Module segments are identified by matching against known
/// module paths; everything after the module prefix is a type segment.
///
/// For `module::Outer`: returns `"Outer"` (same as before).
/// For `module::Outer::Inner`: returns `"Outer_Inner"`.
/// For `module::Outer::Inner::DeeplyNested`: returns `"Outer_Inner_DeeplyNested"`.
pub(super) fn flatten_type_name(path: &ItemPath, module_paths: &BTreeSet<ItemPath>) -> String {
    let module_len = find_module_prefix_len(path, module_paths);
    path.iter()
        .skip(module_len)
        .map(|s| s.as_str())
        .collect::<Vec<_>>()
        .join("_")
}

/// The rustdoc path of an extern value's accessor, for rewriting a doc link that
/// points at the value. `value_path` is the extern value's item path
/// (`module::[Type::…::]name`); the accessor is `get_<name>` — a free fn when
/// module-level, an inherent method when nested. Enclosing type segments are
/// flattened (`Outer::Inner` → `Outer_Inner`) and, when the value lives in
/// another module, the path is absolutized to `crate::…` — mirroring the
/// nested-item rewrites — while the trailing `get_<name>` stays its own segment
/// so rustdoc resolves it as a function / method.
pub(super) fn extern_value_accessor_doc_path(
    value_path: &ItemPath,
    current_module: &ItemPath,
    module_paths: &BTreeSet<ItemPath>,
    prefix: Option<&ItemPath>,
) -> String {
    let module_len = find_module_prefix_len(value_path, module_paths);
    let declaring_module: ItemPath = value_path.iter().take(module_len).cloned().collect();
    let leaf = value_path.last().map(|s| s.as_str()).unwrap_or_default();
    let accessor = extern_value_accessor_name(leaf);
    // Type segments between the module prefix and the value's own leaf (empty for
    // a module-level extern value).
    let type_count = value_path.len().saturating_sub(module_len + 1);
    let type_segments: Vec<&str> = value_path
        .iter()
        .skip(module_len)
        .take(type_count)
        .map(|s| s.as_str())
        .collect();

    let mut segments: Vec<String> = Vec::new();
    if &declaring_module != current_module {
        segments.push(match prefix {
            Some(prefix) => format!("crate::{prefix}"),
            None => "crate".to_string(),
        });
        segments.extend(declaring_module.iter().map(|s| s.as_str().to_string()));
    }
    if !type_segments.is_empty() {
        segments.push(type_segments.join("_"));
    }
    segments.push(accessor);
    segments.join("::")
}

/// Find the length of the longest module path prefix of `path`.
pub(super) fn find_module_prefix_len(path: &ItemPath, module_paths: &BTreeSet<ItemPath>) -> usize {
    let mut module_len = 0;
    for i in 1..=path.len() {
        let prefix: ItemPath = path.iter().take(i).cloned().collect();
        if module_paths.contains(&prefix) {
            module_len = i;
        }
    }
    module_len
}

/// Generate a compile-time size check function for a type.
pub(super) fn generate_size_check(name: &str, size: usize) -> Option<proc_macro2::TokenStream> {
    (size > 0).then(|| {
        let name_ident = str_to_ident(name);
        let size_check_ident = quote::format_ident!("_{}_size_check", name);
        let size = super::doc_links::hex_literal(size);
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
pub(super) fn build_extra_derives(
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
pub(super) fn build_generic_params(type_parameters: &[String]) -> proc_macro2::TokenStream {
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
    module_paths: Option<&BTreeSet<ItemPath>>,
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
                    PredefinedItem::Str => "&str",
                    PredefinedItem::CStr => "&::std::ffi::CStr",
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
            // For nested items, flatten type-nesting segments with `_`.
            if let Some(mp) = module_paths {
                let flat = flatten_type_name(path, mp);
                if path.len() > 1 {
                    // Render the module prefix with `::` and the flattened type name
                    let module_len = find_module_prefix_len(path, mp);
                    let module_part: Vec<&str> =
                        path.iter().take(module_len).map(|s| s.as_str()).collect();
                    if !module_part.is_empty() {
                        write!(out, "{}::", module_part.join("::"))?;
                    }
                    write!(out, "{flat}")
                } else {
                    write!(out, "{flat}")
                }
            } else {
                write!(out, "{path}")
            }
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
                fully_qualified_type_ref_impl(out, arg, prefix, module_paths)?;
            }
            write!(out, ">")
        }
        Type::TypeParameter(name) => {
            // Type parameter - just output the name (e.g., `T`)
            write!(out, "{name}")
        }
        Type::ConstPointer(tr) => {
            write!(out, "*const ")?;
            fully_qualified_type_ref_impl(out, tr.as_ref(), prefix, module_paths)
        }
        Type::MutPointer(tr) => {
            write!(out, "*mut ")?;
            fully_qualified_type_ref_impl(out, tr.as_ref(), prefix, module_paths)
        }
        Type::Array(tr, size) => {
            write!(out, "[")?;
            fully_qualified_type_ref_impl(out, tr.as_ref(), prefix, module_paths)?;
            write!(out, "; {size}]")
        }
        Type::Function(calling_convention, args, return_type) => {
            write!(out, r#"unsafe extern "{calling_convention}" fn ("#)?;
            for (field, type_ref) in args.iter() {
                write!(out, "{field}: ")?;
                fully_qualified_type_ref_impl(out, type_ref, prefix, module_paths)?;
                write!(out, ", ")?;
            }
            write!(out, ")")?;
            if let Some(type_ref) = return_type {
                write!(out, " -> ")?;
                fully_qualified_type_ref_impl(out, type_ref, prefix, module_paths)?;
            }
            Ok(())
        }
    }
}

fn fully_qualified_type_ref(
    type_ref: &Type,
    prefix: Option<&ItemPath>,
    module_paths: Option<&BTreeSet<ItemPath>>,
) -> std::result::Result<String, std::fmt::Error> {
    let mut out = String::new();
    fully_qualified_type_ref_impl(&mut out, type_ref, prefix, module_paths)?;
    Ok(out)
}

pub(super) fn sa_type_to_syn_type(
    type_ref: &Type,
    prefix: Option<&ItemPath>,
    module_paths: Option<&BTreeSet<ItemPath>>,
) -> Result<syn::Type> {
    Ok(syn::parse_str(&fully_qualified_type_ref(
        type_ref,
        prefix,
        module_paths,
    )?)?)
}

pub(super) fn visibility_to_tokens(visibility: Visibility) -> proc_macro2::TokenStream {
    match visibility {
        Visibility::Public => quote! { pub },
        Visibility::Private => quote! {},
    }
}

pub(super) fn doc_to_tokens(
    is_module_doc: bool,
    doc: &[String],
    links: Option<(&DocLinkCx, &[ResolvedDocLink])>,
) -> proc_macro2::TokenStream {
    if doc.is_empty() {
        return proc_macro2::TokenStream::new();
    };
    let doc_attrs = doc.iter().map(|line| {
        let rewritten = match links {
            Some((cx, block)) => cx.rewrite_line(line, block),
            None => line.clone(),
        };
        if is_module_doc {
            quote! { #![doc = #rewritten] }
        } else {
            quote! { #[doc = #rewritten] }
        }
    });
    quote! {
        #(#doc_attrs)*
    }
}
