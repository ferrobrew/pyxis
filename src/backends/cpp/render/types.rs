//! `Type` → C++ type-expression rendering, doc-comment rendering, and
//! intra-doc link rewriting to doxygen `@ref`s.

use super::RenderCtx;
use crate::{backends::Result, grammar::ItemPath, semantic::types::Type};

/// Render a `Type` as a C++ type expression. For arrays the caller is
/// responsible for placing the `[N]` suffix after the field name.
pub fn render_type(ty: &Type, ctx: RenderCtx) -> Result<String> {
    Ok(match ty {
        Type::Unresolved(_) => "/* unresolved */ void".to_string(),
        Type::TypeParameter(name) => name.clone(),
        Type::Raw(path) => render_path(path, ctx),
        Type::Generic(base, args) => {
            let base_str = render_path(base, ctx);
            let args_str = args
                .iter()
                .map(|a| render_type(a, ctx))
                .collect::<Result<Vec<_>>>()?
                .join(", ");
            format!("{base_str}<{args_str}>")
        }
        Type::ConstPointer(inner) => format!("const {}*", render_type(inner, ctx)?),
        Type::MutPointer(inner) => format!("{}*", render_type(inner, ctx)?),
        Type::Array(inner, _n) => {
            // Fields handle the `[N]` suffix themselves; for nested contexts
            // (like template args) emit the bare element type.
            render_type(inner, ctx)?
        }
        Type::Function(cc, args, ret) => {
            // Bare function-pointer expression (no name) for use in template
            // arguments / type aliases. `render_function_pointer_decl`
            // handles the with-name field/parameter case.
            let cc_macro = super::structs::calling_conv_macro(*cc);
            let ret_text = match ret.as_deref() {
                Some(t) => render_type(t, ctx)?,
                None => "void".to_string(),
            };
            let arg_types = args
                .iter()
                .map(|(_, t)| render_type(t, ctx))
                .collect::<Result<Vec<_>>>()?
                .join(", ");
            format!("{ret_text} ({cc_macro}*)({arg_types})")
        }
    })
}

fn render_path(path: &ItemPath, ctx: RenderCtx) -> String {
    // Predefined items map to C++ primitives directly (no namespace).
    if let Ok(item) = ctx
        .registry
        .get(path, &crate::span::ItemLocation::internal())
    {
        if let Some(predef) = item.predefined {
            return super::idents::predefined_to_cpp(predef).to_string();
        }
        // Externs with a #[cpp_name] binding: substitute the C++ name
        // verbatim (the pyxis leaf is allowed to contain generic syntax
        // that can't be a C++ identifier).
        if matches!(item.category, crate::semantic::types::ItemCategory::Extern)
            && let Some(binding) = ctx.bindings.get(path)
            && let Some(name) = &binding.name
        {
            return name.clone();
        }
    }
    // Same-module: bare name, unless it collides with a member of the class
    // currently being rendered. A bare leaf resolves in class scope first,
    // so a struct with a member whose name matches a type -- legal in
    // pyxis/Rust, where fields and types occupy separate namespaces --
    // poisons that name for the rest of the class: `Viewport Viewport;`
    // makes a later `const Viewport*` resolve to the member, not the type
    // (MSVC C2327 + cascade). When that happens we fall through to the
    // fully-qualified form, which is looked up in namespace scope and
    // bypasses the member. References that don't collide keep the bare name.
    let target_module = path.parent().unwrap_or_else(ItemPath::empty);
    let leaf = path.last().map(|s| s.as_str()).unwrap_or("");
    if &target_module == ctx.module_path {
        let leaf_ident = super::cpp_ident(leaf);
        let collides = ctx
            .shadowed_members
            .is_some_and(|m| m.contains(leaf_ident.as_ref()));
        if !collides {
            return leaf_ident.into_owned();
        }
    }
    // Cross-module (or a same-module collision): fully qualified. Module
    // segments are namespaces (escaped against C-runtime globals too); the
    // leaf is the type name.
    let mut out = String::new();
    out.push_str("::");
    let last = path.len().saturating_sub(1);
    for (i, seg) in path.iter().enumerate() {
        if i > 0 {
            out.push_str("::");
        }
        let escaped = if i == last {
            super::cpp_ident(seg.as_str())
        } else {
            super::cpp_namespace_ident(seg.as_str())
        };
        out.push_str(&escaped);
    }
    out
}

pub(super) fn render_doc(
    out: &mut String,
    doc: &[String],
    indent_levels: usize,
    ctx: RenderCtx,
    location: &crate::span::ItemLocation,
) -> Result<()> {
    use std::fmt::Write;

    let links = ctx.doc_links.at(location);
    let pad = "    ".repeat(indent_levels);
    for line in doc {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            // Blank doc line - emit just `///` with no trailing space.
            writeln!(out, "{pad}///")?;
        } else {
            let rewritten = rewrite_doc_links(trimmed, links, ctx);
            writeln!(out, "{pad}/// {rewritten}")?;
        }
    }
    Ok(())
}

/// Rewrite each resolved intra-doc link in `line` into a doxygen-resolvable
/// form: the markdown destination becomes `@ref <qualified C++ name>`
/// (`[`field`](Self::field)` → `[`field`](@ref ns::Container::field)`), and a
/// code shortcut becomes an inline link so its written label survives.
/// Doxygen's markdown support resolves `[label](@ref target)` to the target's
/// documentation.
///
/// Links whose target has no documented C++ entity (predefined primitives,
/// externs bound to out-of-tree types) are flattened to their bare label —
/// leaving the raw path as a markdown destination would render a dead
/// `href="Path::To"` link.
fn rewrite_doc_links(
    line: &str,
    links: &[crate::semantic::doc_links::ResolvedDocLink],
    ctx: RenderCtx,
) -> String {
    use crate::semantic::doc_links::{DocLinkSyntax, scan_links};
    if links.is_empty() {
        return line.to_string();
    }
    let mut result = line.to_string();
    let mut scanned = scan_links(line);
    scanned.retain(|l| l.syntax != DocLinkSyntax::PlainShortcut);
    for link in scanned.into_iter().rev() {
        let Some(resolved) = links.iter().find(|r| r.text == link.path) else {
            continue;
        };
        let label = &line[link.label_region.0..link.label_region.1];
        let replacement = match doxygen_ref(&resolved.target, ctx) {
            Some(target) => format!("[{label}](@ref {target})"),
            None => label.to_string(),
        };
        result.replace_range(link.link.0..link.link.1, &replacement);
    }
    result
}

/// The fully-qualified C++ name of a resolved link target, for a doxygen
/// `@ref` — or `None` when no documented C++ entity exists for it.
///
/// Module segments are namespaces and nested types stay genuinely nested in
/// C++, so the item path maps segment-for-segment (with identifier escaping).
/// Extern values map to their `get_<name>` accessor.
fn doxygen_ref(
    target: &crate::semantic::doc_links::DocLinkTarget,
    ctx: RenderCtx,
) -> Option<String> {
    use crate::semantic::{
        doc_links::{DocLinkMemberKind, DocLinkTarget},
        types::ItemDefinitionInner,
    };

    let qualify = |path: &ItemPath| -> Option<String> {
        // Predefined items are C++ primitives; externs may be bound to
        // out-of-tree types. Neither has documentation to reference.
        if let Ok(item) = ctx
            .registry
            .get(path, &crate::span::ItemLocation::internal())
        {
            if item.predefined.is_some()
                || matches!(item.category, crate::semantic::types::ItemCategory::Extern)
            {
                return None;
            }
        }
        let last = path.len().saturating_sub(1);
        Some(
            path.iter()
                .enumerate()
                .map(|(i, seg)| {
                    if i == last {
                        super::cpp_ident(seg.as_str()).into_owned()
                    } else {
                        super::cpp_namespace_ident(seg.as_str()).into_owned()
                    }
                })
                .collect::<Vec<_>>()
                .join("::"),
        )
    };

    match target {
        DocLinkTarget::Item(path) => qualify(path),
        DocLinkTarget::Member { item, name, kind } => {
            let base = qualify(item)?;
            // Nested constants/extern values under an enum or bitflags parent
            // have no struct body to live in; the emitter flattens them to
            // module scope as `Parent_NAME` / `Parent_get_name()` (see
            // `render_nested_values_cpp_flat`). Mirror that here.
            let parent_is_bodyless = ctx
                .registry
                .get(item, &crate::span::ItemLocation::internal())
                .ok()
                .and_then(|i| i.resolved())
                .is_some_and(|r| {
                    matches!(
                        r.inner,
                        ItemDefinitionInner::Enum(_) | ItemDefinitionInner::Bitflags(_)
                    )
                });
            let value_member = matches!(
                kind,
                DocLinkMemberKind::Constant | DocLinkMemberKind::ExternValue
            );
            let accessor = |name: &str| match kind {
                DocLinkMemberKind::ExternValue => format!("get_{}", super::cpp_ident(name)),
                _ => super::cpp_ident(name).into_owned(),
            };
            Some(if parent_is_bodyless && value_member {
                format!("{base}_{}", accessor(name))
            } else {
                format!("{base}::{}", accessor(name))
            })
        }
        DocLinkTarget::Function { module, name } => {
            let ns = module
                .iter()
                .map(|s| super::cpp_namespace_ident(s.as_str()).into_owned())
                .collect::<Vec<_>>()
                .join("::");
            Some(if ns.is_empty() {
                super::cpp_ident(name).into_owned()
            } else {
                format!("{ns}::{}", super::cpp_ident(name))
            })
        }
        DocLinkTarget::ExternValue { module, name } => {
            let ns = module
                .iter()
                .map(|s| super::cpp_namespace_ident(s.as_str()).into_owned())
                .collect::<Vec<_>>()
                .join("::");
            let accessor = format!("get_{}", super::cpp_ident(name));
            Some(if ns.is_empty() {
                accessor
            } else {
                format!("{ns}::{accessor}")
            })
        }
    }
}
