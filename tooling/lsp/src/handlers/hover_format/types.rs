use super::*;

use pyxis::{
    grammar::{Expr, Ident, ItemDefinitionInner},
    semantic::types::{ItemDefinitionInner as ResolvedInner, ItemStateResolved},
};

/// Format a type definition for hover display with size and alignment
pub(crate) fn format_type_hover_with_size(
    definition: &ItemDefinition,
    size: usize,
    alignment: usize,
) -> String {
    let mut md = format_type_hover(definition);
    push_facts(
        &mut md,
        &[("size", fmt_bytes(size)), ("align", fmt_bytes(alignment))],
    );
    md
}

/// Format a type definition for hover display
pub(crate) fn format_type_hover(definition: &ItemDefinition) -> String {
    let name = definition.name.as_str();
    let kind = match &definition.inner {
        ItemDefinitionInner::Type(_) => "type",
        ItemDefinitionInner::Enum(_) => "enum",
        ItemDefinitionInner::Bitflags(_) => "bitflags",
        ItemDefinitionInner::TypeAlias(_) => "type alias",
        ItemDefinitionInner::Constant(_) => "const",
        ItemDefinitionInner::ExternValue(_) => "extern",
    };
    let mut md = format!("**{}** `{}`\n\n", kind, name);

    if !definition.doc_comments.is_empty() {
        md.push_str(&definition.doc_comments.join("\n"));
        md.push_str("\n\n");
    }

    if let ItemDefinitionInner::Type(td) = &definition.inner {
        md.push_str("**Fields:**\n");
        for statement in td.statements() {
            if let TypeField::Field(vis, name, type_) = &statement.field {
                let vis_str = if matches!(vis, Visibility::Public) {
                    "pub "
                } else {
                    ""
                };
                md.push_str(&format!("- `{}{}: {}`\n", vis_str, name, type_));
            }
        }
    }

    if let ItemDefinitionInner::Constant(cd) = &definition.inner {
        md.push_str(&format!(
            "`{}: {} = {}`\n",
            name,
            cd.type_,
            render_const_expr(&cd.expr)
        ));
        // For an integer constant, also show the value in the other common bases.
        if let Expr::IntLiteral { value, .. } = &cd.expr {
            md.push_str(&format!("\n{}\n", format_int_reprs(*value as i128)));
        }
    }

    if let ItemDefinitionInner::ExternValue(ev) = &definition.inner {
        md.push_str(&format!("`{}: {}`\n", name, ev.type_));
    }

    md
}

/// Hover markdown for a struct field.
pub(crate) fn format_field_hover(
    vis: &Visibility,
    name: &Ident,
    type_: &Type,
    attributes: &Attributes,
    type_size: Option<usize>,
    offset: Option<usize>,
) -> String {
    let vis_str = if matches!(vis, Visibility::Public) {
        "pub "
    } else {
        ""
    };
    // The signature line already names the field, so no separate header.
    let mut md = format!("```pyxis\n{}{}: {}\n```\n", vis_str, name.as_str(), type_);
    let attrs = render_attributes(attributes);
    if !attrs.is_empty() {
        md.push_str(&format!("\n**Attributes:** {attrs}\n"));
    }
    let mut facts = Vec::new();
    if let Some(offset) = offset {
        facts.push(("offset", format!("`0x{offset:X}` ({offset})")));
    }
    if let Some(size) = type_size {
        facts.push(("type size", fmt_bytes(size)));
    }
    push_facts(&mut md, &facts);
    md
}

/// Compute a field's byte offset within its resolved parent type by summing the
/// sizes of preceding layout regions. The resolver inserts explicit padding
/// regions, so the running total is the true offset.
pub(crate) fn field_offset(
    parent_resolved: &ItemStateResolved,
    field_name: &str,
    type_registry: &TypeRegistry,
) -> Option<usize> {
    let ResolvedInner::Type(td) = &parent_resolved.inner else {
        return None;
    };
    let mut offset = 0usize;
    for region in &td.regions {
        if region.name.as_deref() == Some(field_name) {
            return Some(offset);
        }
        offset += region.size(type_registry)?;
    }
    None
}

/// Render hover markdown for a pointer/array/unknown *shell* type. The caller
/// (via `type_hit_at`) has already determined the cursor is on this exact type's
/// shell, not its inner pointee/element. The output is intentionally identical
/// to the legacy `type_shell_at` so snapshots/tests don't move.
pub(crate) fn shell_hover_md(
    type_: &Type,
    type_registry: &TypeRegistry,
    scope: &[ItemPath],
    decl_registry: &DeclarationRegistry,
    pointer_size: usize,
) -> String {
    match type_ {
        Type::ConstPointer { pointee, .. } => {
            let mut md = format!("**pointer** `{type_}`\n\npoints to `{pointee}` (const)\n");
            push_facts(&mut md, &[("size", fmt_bytes(pointer_size))]);
            md
        }
        Type::MutPointer { pointee, .. } => {
            let mut md = format!("**pointer** `{type_}`\n\npoints to `{pointee}` (mut)\n");
            push_facts(&mut md, &[("size", fmt_bytes(pointer_size))]);
            md
        }
        Type::Array { element, size, .. } => {
            let mut md = format!("**array** `{type_}`\n\n`{size}` × `{element}`\n");
            let mut facts = Vec::new();
            if let Some(s) = type_size_of(type_, type_registry, scope, decl_registry, pointer_size)
            {
                facts.push(("size", fmt_bytes(s)));
            }
            if let Some(a) = type_align_of(type_, type_registry, scope, decl_registry, pointer_size)
            {
                facts.push(("align", fmt_bytes(a)));
            }
            push_facts(&mut md, &facts);
            md
        }
        Type::Unknown { size, .. } => {
            let mut md = format!("**unknown** `{type_}`\n");
            push_facts(&mut md, &[("size", fmt_bytes(*size))]);
            md
        }
        // type_hit_at only yields Shell for pointer/array/unknown.
        Type::Ident { .. } => String::new(),
    }
}

/// Best-effort alignment of a type: pointer → pointer size, array → element
/// alignment, named type → its resolved alignment.
pub(crate) fn type_align_of(
    type_: &Type,
    type_registry: &TypeRegistry,
    scope: &[ItemPath],
    decl_registry: &DeclarationRegistry,
    pointer_size: usize,
) -> Option<usize> {
    match type_ {
        Type::ConstPointer { .. } | Type::MutPointer { .. } => Some(pointer_size),
        Type::Array { element, .. } => {
            type_align_of(element, type_registry, scope, decl_registry, pointer_size)
        }
        Type::Unknown { .. } => None,
        Type::Ident { path, .. } => {
            let resolved = resolve_type_path(path, scope, decl_registry)?;
            type_registry
                .get(&resolved, &ItemLocation::internal())
                .ok()?
                .resolved()
                .map(|r| r.alignment)
        }
    }
}

/// Best-effort size of a field type: pointer → pointer size, array →
/// element × count, `unknown<N>` → N, named type → its resolved size.
pub(crate) fn type_size_of(
    type_: &Type,
    type_registry: &TypeRegistry,
    scope: &[ItemPath],
    decl_registry: &DeclarationRegistry,
    pointer_size: usize,
) -> Option<usize> {
    match type_ {
        Type::ConstPointer { .. } | Type::MutPointer { .. } => Some(pointer_size),
        Type::Array { element, size, .. } => {
            type_size_of(element, type_registry, scope, decl_registry, pointer_size)
                .map(|s| s * size)
        }
        Type::Unknown { size, .. } => Some(*size),
        Type::Ident { path, .. } => {
            let resolved = resolve_type_path(path, scope, decl_registry)?;
            type_registry
                .get(&resolved, &ItemLocation::internal())
                .ok()?
                .resolved()
                .map(|r| r.size)
        }
    }
}
