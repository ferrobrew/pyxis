//! Union rendering. A pyxis union maps straight onto a C++ `union` — the one
//! place where the C++ backend needs no encoding tricks, since C++ has the
//! construct natively.

use std::fmt::Write;

use super::{RenderCtx, RenderedItem};
use crate::{backends::Result, semantic::types::UnionDefinition, span::ItemLocation};

pub(super) fn render_union(
    name: &str,
    ud: &UnionDefinition,
    size: usize,
    alignment: usize,
    ctx: RenderCtx,
    location: &ItemLocation,
) -> Result<RenderedItem> {
    let name = &*super::cpp_ident(name);

    let mut out = String::new();
    super::types::render_doc(&mut out, &ud.doc, 0, ctx, location)?;
    if ud.packed {
        writeln!(out, "#pragma pack(push, 1)")?;
    }

    let mut body = String::new();

    // Type-declaring nested items come before fields so a member may reference
    // one (`Outer::Inner`).
    super::nested::render_type_declarations(&mut body, &ud.nested_item_paths, ctx)?;
    for region in &ud.regions {
        super::items::render_field(&mut body, region, ctx, false)?;
    }

    // Nested items declared inside the union body are rendered in-class, the
    // same way a struct's are. Unions have no place for out-of-class constant
    // definitions to attach to any differently, so `deferred_consts` is handled
    // identically.
    let mut deferred_consts: Vec<(String, String, String)> = Vec::new();
    super::structs::render_deleted_special_members_if(&mut body, name, ud.pinned)?;
    super::nested::render_value_declarations(
        &mut body,
        &ud.nested_item_paths,
        ctx,
        &mut deferred_consts,
    )?;

    if body.trim().is_empty() {
        writeln!(out, "union alignas({alignment}) {name} {{}};")?;
    } else {
        writeln!(out, "union alignas({alignment}) {name} {{")?;
        out.push_str(&body);
        writeln!(out, "}};")?;
    }
    if ud.packed {
        writeln!(out, "#pragma pack(pop)")?;
    }

    if size > 0 {
        writeln!(out, "static_assert(sizeof({name}) == 0x{size:X});")?;
    }
    writeln!(out, "static_assert(alignof({name}) == {alignment});")?;

    let mut post_header = String::new();
    for (const_name, const_type, const_value) in &deferred_consts {
        writeln!(
            post_header,
            "inline const {const_type} {name}::{const_name} = {const_value};"
        )?;
    }

    let mut post_cpp = String::new();
    super::nested::render_extern_value_definitions(
        &mut post_cpp,
        &ud.nested_item_paths,
        name,
        ctx,
    )?;

    Ok(RenderedItem {
        decl: out,
        post_header,
        post_cpp,
    })
}
