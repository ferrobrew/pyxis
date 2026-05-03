//! C++-specific bindings extracted from `#[cpp_header = "..."]` /
//! `#[cpp_name = "..."]` attributes on `extern type` declarations.
//!
//! The pyxis IR keeps these as opaque grammar-level attributes; the C++
//! backend walks them once up front to build a per-item map, then consults
//! it from `deps.rs` (to add `#include <header>` lines) and `render.rs`
//! (to emit `using LocalName = std::external::Name;` aliases).

use std::collections::BTreeMap;

use crate::{
    grammar::{self, ItemPath},
    semantic::ResolvedSemanticState,
};

/// Bindings extracted from a single `extern type` declaration.
#[derive(Debug, Clone, Default)]
pub struct CppExternBinding {
    /// The header to `#include` so the binding's name is in scope.
    /// Stored verbatim — the value should already include angle brackets
    /// (`"<atomic>"`) or quotes (`"\"my_header.h\""`) as desired in the
    /// emitted `#include` directive.
    pub header: Option<String>,
    /// The fully-qualified C++ name to substitute for the extern type
    /// (e.g. `"std::atomic<int32_t>"`). When absent, the extern type's
    /// pyxis name is reused as the C++ name.
    pub name: Option<String>,
}

/// Walk every module in the resolved semantic state and collect any
/// `#[cpp_header = "..."]` / `#[cpp_name = "..."]` attributes attached to
/// `extern type` declarations.
pub fn build_cpp_extern_bindings(
    semantic_state: &ResolvedSemanticState,
) -> BTreeMap<ItemPath, CppExternBinding> {
    let mut out = BTreeMap::new();
    for (module_path, module) in semantic_state.modules() {
        for item in module.ast.extern_types() {
            let grammar::ModuleItem::ExternType {
                name, attributes, ..
            } = item
            else {
                continue;
            };
            let mut binding = CppExternBinding::default();
            for attribute in attributes {
                if let grammar::Attribute::Assign {
                    name: attr_name,
                    items,
                    ..
                } = attribute
                {
                    let key = attr_name.as_str();
                    if key != "cpp_header" && key != "cpp_name" {
                        continue;
                    }
                    let Some(string_value) = items
                        .exprs_vec()
                        .first()
                        .and_then(|e| e.string_literal().map(str::to_string))
                    else {
                        continue;
                    };
                    match key {
                        "cpp_header" => binding.header = Some(string_value),
                        "cpp_name" => binding.name = Some(string_value),
                        _ => {}
                    }
                }
            }
            if binding.header.is_none() && binding.name.is_none() {
                continue;
            }
            let item_path = module_path.join(name.as_str().into());
            out.insert(item_path, binding);
        }
    }
    out
}
