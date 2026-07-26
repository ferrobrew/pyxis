//! `Type` → C++ type-expression rendering, doc-comment rendering, and
//! intra-doc link rewriting to doxygen `@ref`s.

use super::RenderCtx;
use crate::{backends::Result, grammar::ItemPath, semantic::types::Type};

/// Render a `Type` as a C++ type expression. For arrays the caller is
/// responsible for placing the `[N]` suffix after the field name; use
/// [`render_declaration`] when a declarator name is being attached.
pub fn render_type(ty: &Type, ctx: RenderCtx) -> Result<String> {
    Ok(match ty {
        Type::Array(inner, _n) => {
            // Fields handle the `[N]` suffix themselves; for nested contexts
            // (like template args) emit the bare element type.
            render_type(inner, ctx)?
        }
        _ => render_declaration(ty, "", ctx)?,
    })
}

/// Render a full C++ declaration of `ty` around the given declarator core -
/// a field name, a parameter name, `name(params)` for a function, or `""`
/// for a bare type-id.
///
/// C declaration syntax nests inside-out, so this can't be done by gluing a
/// name onto a rendered type: an array of function pointers is
/// `R (*name[N])(args)`, not `R (*)(args) name[N]`.
pub fn render_declaration(ty: &Type, declarator: &str, ctx: RenderCtx) -> Result<String> {
    render_declaration_with(ty, declarator, ctx, false)
}

/// A parameter type-id. Unlike [`render_type`] this keeps an array's extent,
/// so a declaration (`void f(uint32_t x[4])`) and the function-pointer alias
/// used to call it (`void (*)(uint32_t[4])`) agree on the parameter type
/// instead of one of them silently decaying and the other not.
pub fn render_parameter_type(ty: &Type, ctx: RenderCtx) -> Result<String> {
    render_declaration(ty, "", ctx)
}

/// As [`render_declaration`], but when `rewrite_self_arg_to_void_ptr` the
/// outermost function type's first parameter is rendered as `void*` (or
/// `const void*`, preserving const-ness). Vftable struct slots use this so
/// derived types can pass their `this` without explicit base-chain casts.
pub fn render_declaration_with(
    ty: &Type,
    declarator: &str,
    ctx: RenderCtx,
    rewrite_self_arg_to_void_ptr: bool,
) -> Result<String> {
    let (base, declarator) = build_declarator(
        ty,
        declarator.to_string(),
        ctx,
        rewrite_self_arg_to_void_ptr,
        false,
    )?;

    // Leading `*`s bind to the base type in this codebase's house style
    // (`T* name`, not `T *name`); anything else is separated by a space.
    let stars = declarator.len() - declarator.trim_start_matches('*').len();
    let (stars, rest) = declarator.split_at(stars);
    Ok(if rest.is_empty() {
        format!("{base}{stars}")
    } else {
        format!("{base}{stars} {rest}")
    })
}

/// Peel `ty` outside-in, wrapping the declarator as we go, and return the
/// base type text plus the finished declarator.
///
/// `is_const` means "the type at this level is const-qualified", propagated
/// down from an enclosing `*const`. It cannot simply be prepended to the base
/// type: for `*const fn()` the base is the *return* type, and `const void
/// (**f)()` would qualify the wrong thing. Instead each level that produces a
/// `*` of its own consumes the flag as a trailing `const` on that star.
fn build_declarator(
    ty: &Type,
    declarator: String,
    ctx: RenderCtx,
    rewrite_self_arg_to_void_ptr: bool,
    is_const: bool,
) -> Result<(String, String)> {
    // The `*` this level contributes, const-qualified if an enclosing
    // `*const` says the thing it points at is immutable.
    let star = if is_const { "*const " } else { "*" };
    // Applied to a base type (`const uint32_t`), where there is no star to
    // hang the qualifier off.
    let qualify = |base: String| {
        if is_const {
            format!("const {base}")
        } else {
            base
        }
    };

    Ok(match ty {
        Type::ConstPointer(inner) => build_declarator(
            inner,
            format!("{star}{declarator}"),
            ctx,
            false,
            // What we point at is what `*const` makes immutable.
            true,
        )?,
        Type::MutPointer(inner) => {
            build_declarator(inner, format!("{star}{declarator}"), ctx, false, false)?
        }
        Type::Array(inner, n) => {
            // `[]` binds tighter than `*`, so a pointer to an array needs
            // parens; an array of pointers does not.
            let declarator = if declarator.starts_with('*') {
                format!("({declarator})[{n}]")
            } else {
                format!("{declarator}[{n}]")
            };
            // A const array is an array of const elements.
            build_declarator(inner, declarator, ctx, false, is_const)?
        }
        Type::Function(cc, args, ret) => {
            let cc_macro = super::structs::calling_conv_macro(*cc);
            let arg_types = args
                .iter()
                .enumerate()
                .map(|(i, arg)| {
                    if rewrite_self_arg_to_void_ptr && i == 0 {
                        Ok(match arg.type_.as_ref() {
                            Type::ConstPointer(_) => "const void*".to_string(),
                            Type::MutPointer(_) => "void*".to_string(),
                            other => render_parameter_type(other, ctx)?,
                        })
                    } else {
                        render_parameter_type(&arg.type_, ctx)
                    }
                })
                .collect::<Result<Vec<_>>>()?
                .join(", ");
            // A function type is never itself const-qualified; `*const fn()`
            // makes the *pointer* immutable, and this level owns that star.
            let declarator = format!("({cc_macro}{star}{declarator})({arg_types})");
            match ret.as_deref() {
                Some(ret) => build_declarator(ret, declarator, ctx, false, false)?,
                None => ("void".to_string(), declarator),
            }
        }
        Type::Unresolved(_) => (qualify("/* unresolved */ void".to_string()), declarator),
        Type::TypeParameter(name) => (qualify(name.clone()), declarator),
        Type::Raw(path) => (qualify(render_path(path, ctx)), declarator),
        Type::Generic(base, args) => {
            let base_str = render_path(base, ctx);
            let args_str = args
                .iter()
                .map(|a| render_type(a, ctx))
                .collect::<Result<Vec<_>>>()?
                .join(", ");
            (qualify(format!("{base_str}<{args_str}>")), declarator)
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
