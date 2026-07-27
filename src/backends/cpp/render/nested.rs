//! Rendering for items nested inside a struct-shaped item: in-class
//! declarations (enums, structs, bitflags, type aliases, constants, extern
//! values) and the out-of-class definitions for nested extern-value accessors.

use std::fmt::Write;

use super::{RenderCtx, items::format_const_value};
use crate::{
    backends::Result,
    grammar::ItemPath,
    semantic::types::{ConstValue, ItemDefinitionInner},
    span::ItemLocation,
};

/// Render nested item declarations (enums, types, bitflags, type aliases,
/// constants, extern values) inside the struct body. Struct/array/const-ref
/// constants that need an out-of-class definition are pushed into
/// `deferred_consts` for the caller to emit after the class body.
pub(super) fn render_declarations(
    body: &mut String,
    nested_item_paths: &[ItemPath],
    ctx: RenderCtx,
    deferred_consts: &mut Vec<(String, String, String)>,
) -> Result<()> {
    if nested_item_paths.is_empty() {
        return Ok(());
    }
    let mut prev_was_constant = false;
    for nested_path in nested_item_paths {
        if let Ok(nested_item) = ctx.registry.get(nested_path, &ItemLocation::internal()) {
            if let Some(nested_resolved) = nested_item.resolved() {
                let curr_is_constant =
                    matches!(&nested_resolved.inner, ItemDefinitionInner::Constant(_));
                // Add blank line before nested item if body has content,
                // unless both the previous and current items are constants
                // (consecutive constants form a contiguous block).
                if !body.trim().is_empty() && !(prev_was_constant && curr_is_constant) {
                    writeln!(body)?;
                }
                prev_was_constant = curr_is_constant;
                let nested_name = nested_path
                    .last()
                    .map(|s| s.as_str().to_string())
                    .unwrap_or_default();
                let nested_name = super::cpp_ident(&nested_name);
                match &nested_resolved.inner {
                    ItemDefinitionInner::Type(nested_td) => {
                        super::types::render_doc(
                            body,
                            &nested_td.doc,
                            1,
                            ctx,
                            &nested_item.location,
                        )?;
                        writeln!(body, "    struct {nested_name} {{")?;
                        let nested_had_fields = !nested_td.regions.is_empty();
                        for region in &nested_td.regions {
                            super::items::render_field_indented(body, region, ctx, false, 2)?;
                        }
                        // Render nested constants inside the nested struct
                        let nested_has_consts = nested_td.nested_item_paths.iter().any(|p| {
                            ctx.registry
                                .get(p, &ItemLocation::internal())
                                .ok()
                                .and_then(|i| i.resolved())
                                .is_some_and(|r| {
                                    matches!(r.inner, ItemDefinitionInner::Constant(_))
                                })
                        });
                        // Add blank line between fields and constants
                        if nested_had_fields && nested_has_consts {
                            writeln!(body)?;
                        }
                        for nested_nested_path in &nested_td.nested_item_paths {
                            if let Ok(nested_nested_item) = ctx
                                .registry
                                .get(nested_nested_path, &ItemLocation::internal())
                                && let Some(nested_nested_resolved) = nested_nested_item.resolved()
                                && let ItemDefinitionInner::Constant(nested_cd) =
                                    &nested_nested_resolved.inner
                            {
                                let nested_const_name = nested_nested_path
                                    .last()
                                    .map(|s| s.as_str().to_string())
                                    .unwrap_or_default();
                                let nested_const_name = super::cpp_ident(&nested_const_name);
                                super::types::render_doc(
                                    body,
                                    &nested_cd.doc,
                                    2,
                                    ctx,
                                    &nested_nested_item.location,
                                )?;
                                let bf_type = super::render_type(&nested_cd.type_, ctx)?;
                                let value_str =
                                    format_const_value(&nested_cd.value, &nested_cd.type_);
                                writeln!(
                                    body,
                                    "        static constexpr {bf_type} {nested_const_name} = {value_str};"
                                )?;
                            }
                        }
                        writeln!(body, "    }};")?;
                    }
                    ItemDefinitionInner::Union(nested_ud) => {
                        super::types::render_doc(
                            body,
                            &nested_ud.doc,
                            1,
                            ctx,
                            &nested_item.location,
                        )?;
                        writeln!(body, "    union {nested_name} {{")?;
                        for region in &nested_ud.regions {
                            super::items::render_field_indented(body, region, ctx, false, 2)?;
                        }
                        writeln!(body, "    }};")?;
                    }
                    ItemDefinitionInner::Enum(nested_ed) => {
                        super::types::render_doc(
                            body,
                            &nested_ed.doc,
                            1,
                            ctx,
                            &nested_item.location,
                        )?;
                        writeln!(
                            body,
                            "    enum class {nested_name} : {} {{",
                            super::render_type(&nested_ed.type_, ctx)?
                        )?;
                        for variant in &nested_ed.variants {
                            writeln!(
                                body,
                                "        {} = {},",
                                super::cpp_ident(&variant.name),
                                variant.value
                            )?;
                        }
                        writeln!(body, "    }};")?;
                    }
                    ItemDefinitionInner::Bitflags(nested_bd) => {
                        super::types::render_doc(
                            body,
                            &nested_bd.doc,
                            1,
                            ctx,
                            &nested_item.location,
                        )?;
                        writeln!(body, "    struct {nested_name} {{")?;
                        let bf_type = super::render_type(&nested_bd.type_, ctx)?;
                        for flag in &nested_bd.flags {
                            writeln!(
                                body,
                                "        static constexpr {bf_type} {} = {};",
                                super::cpp_ident(&flag.name),
                                flag.value
                            )?;
                        }
                        writeln!(body, "    }};")?;
                    }
                    ItemDefinitionInner::TypeAlias(nested_ta) => {
                        super::types::render_doc(
                            body,
                            &nested_ta.doc,
                            1,
                            ctx,
                            &nested_item.location,
                        )?;
                        writeln!(
                            body,
                            "    using {nested_name} = {};",
                            super::render_type(&nested_ta.target, ctx)?
                        )?;
                    }
                    ItemDefinitionInner::Constant(nested_cd) => {
                        super::types::render_doc(
                            body,
                            &nested_cd.doc,
                            1,
                            ctx,
                            &nested_item.location,
                        )?;
                        let bf_type = super::render_type(&nested_cd.type_, ctx)?;
                        let value_str = format_const_value(&nested_cd.value, &nested_cd.type_);
                        // For scalar/POD types, use `static constexpr` with
                        // in-class initialization. For struct/array/const-ref
                        // types, the type may not be a C++ literal type (trivial
                        // ctor/dtor), and if the type is the enclosing class
                        // itself, it's incomplete inside the body. So declare
                        // `static const T name;` here (no initializer) and
                        // define `inline const T Class::name = ...;` after
                        // the class body (in `post_header`).
                        let needs_out_of_class = matches!(
                            &nested_cd.value,
                            ConstValue::Struct { .. }
                                | ConstValue::Array(_)
                                | ConstValue::ConstRef(_)
                        );
                        if needs_out_of_class {
                            writeln!(body, "    static const {bf_type} {nested_name};")?;
                            deferred_consts.push((nested_name.to_string(), bf_type, value_str));
                        } else {
                            writeln!(
                                body,
                                "    static constexpr {bf_type} {nested_name} = {value_str};"
                            )?;
                        }
                    }
                    ItemDefinitionInner::ExternValue(nested_ev) => {
                        // A nested extern value (e.g. a C++ class's static
                        // global) becomes a static accessor over its address.
                        // Declared here; defined out-of-class below (in the
                        // `.cpp` for non-templates, like the singleton
                        // accessor and member functions).
                        super::types::render_doc(
                            body,
                            &nested_ev.doc,
                            1,
                            ctx,
                            &nested_item.location,
                        )?;
                        let decl = super::render_declaration(
                            &nested_ev.type_,
                            &format!("&get_{nested_name}()"),
                            ctx,
                        )?;
                        writeln!(body, "    static {decl};")?;
                    }
                }
            }
        }
    }
    Ok(())
}

/// Out-of-class definitions for nested extern-value accessors, declared as
/// `static T& get_<name>();` in the struct body. Non-template parents put
/// these in the `.cpp`; templates keep them header-visible — the caller
/// picks `out` accordingly.
pub(super) fn render_extern_value_definitions(
    out: &mut String,
    nested_item_paths: &[ItemPath],
    parent_name: &str,
    ctx: RenderCtx,
) -> Result<()> {
    for nested_path in nested_item_paths {
        let Ok(nested_item) = ctx.registry.get(nested_path, &ItemLocation::internal()) else {
            continue;
        };
        let Some(nested_resolved) = nested_item.resolved() else {
            continue;
        };
        if let ItemDefinitionInner::ExternValue(nested_ev) = &nested_resolved.inner {
            let value_name = nested_path.last().map(|s| s.as_str()).unwrap_or_default();
            let value_name = super::cpp_ident(value_name);
            let decl = super::render_declaration(
                &nested_ev.type_,
                &format!("&{parent_name}::get_{value_name}()"),
                ctx,
            )?;
            let target = super::render_declaration(&nested_ev.type_, "*", ctx)?;
            writeln!(out, "{decl} {{")?;
            writeln!(
                out,
                "    return *reinterpret_cast<{target}>(0x{addr:X});",
                addr = nested_ev.address
            )?;
            writeln!(out, "}}")?;
            writeln!(out)?;
        }
    }
    Ok(())
}
