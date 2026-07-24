use super::*;

use pyxis::grammar::{Argument, Expr, Splice};

/// Describe a splice keyword (`prologue`/`epilogue`/`definition`/`for`) under
/// the cursor. The grammar AST carries no spans for these, so we tokenize and
/// match the token directly. A `r#"…"#` splice lexes as a single token, so
/// keywords inside the spliced code can never match. Only tokens within the
/// splice statement's own span are considered.
pub(crate) fn splice_term_at(
    tokens: &[Token],
    splice: &Splice,
    loc: &Location,
) -> Option<(String, Span)> {
    use pyxis::tokenizer::TokenKind;
    if !splice.location.span.contains(loc) {
        return None;
    }
    let token = tokens.iter().find(|t| {
        t.location.span.contains(loc) && splice.location.span.contains(&t.location.span.start)
    })?;
    let desc: &str = match &token.kind {
        TokenKind::Prologue => "Splice emitted *before* the module's generated output.",
        TokenKind::Epilogue => "Splice emitted *after* the module's generated output.",
        TokenKind::Ident(s) if s == "for" => "Attributes this splice to a specific type.",
        TokenKind::Ident(s) if s == "definition" => {
            "Targets the source/definition file rather than the header (cpp only)."
        }
        _ => return None,
    };
    Some((format!("**splice**\n\n{desc}"), token.location.span))
}

/// Render a function signature as Pyxis source (e.g. `pub fn foo(&mut self, x: u32) -> bool`).
pub(crate) fn render_fn_signature(f: &Function) -> String {
    let mut s = String::new();
    if matches!(f.visibility, Visibility::Public) {
        s.push_str("pub ");
    }
    s.push_str("fn ");
    s.push_str(f.name.as_str());
    s.push('(');
    let args: Vec<String> = f
        .arguments
        .iter()
        .map(|arg| match arg {
            Argument::ConstSelf { .. } => "&self".to_string(),
            Argument::MutSelf { .. } => "&mut self".to_string(),
            Argument::Named { ident, type_, .. } => format!("{}: {}", ident.as_str(), type_),
        })
        .collect();
    s.push_str(&args.join(", "));
    s.push(')');
    if let Some(ret) = &f.return_type {
        s.push_str(&format!(" -> {ret}"));
    }
    s
}

/// If the cursor is on a named argument of `f`, produce its hover (name, type,
/// type size) scoped to the argument name. The argument *type* is handled by
/// `find_reference_at`, so this only fires on the name.
pub(crate) fn named_arg_hover(
    f: &Function,
    loc: &Location,
    tokens: &[Token],
    type_registry: &TypeRegistry,
    scope: &[ItemPath],
    decl_registry: &DeclarationRegistry,
    pointer_size: usize,
) -> Option<(String, Span)> {
    for arg in &f.arguments {
        let Argument::Named {
            ident,
            type_,
            location,
        } = arg
        else {
            continue;
        };
        if !location.span.contains(loc) {
            continue;
        }
        let span =
            name_token_span(tokens, &location.span.start, ident.as_str()).unwrap_or(location.span);
        let mut md = format!("**arg** `{}`\n\n", ident.as_str());
        md.push_str(&format!("```pyxis\n{}: {}\n```\n", ident.as_str(), type_));
        if let Some(size) = type_size_of(type_, type_registry, scope, decl_registry, pointer_size) {
            push_facts(&mut md, &[("type size", fmt_bytes(size))]);
        }
        return Some((md, span));
    }
    None
}

/// The span of a `self`/`&self`/`&mut self` receiver of `f` if the cursor is on
/// it (so a `self` hover can show the containing type, scoped to `self`).
pub(crate) fn self_arg_span(f: &Function, loc: &Location) -> Option<Span> {
    for arg in &f.arguments {
        match arg {
            Argument::ConstSelf { location } | Argument::MutSelf { location }
                if location.span.contains(loc) =>
            {
                return Some(location.span);
            }
            _ => {}
        }
    }
    None
}

/// Hover markdown for an enum variant / bitflags flag, including its value.
pub(crate) fn format_variant_hover(
    kind: &str,
    name: &str,
    value: Option<i128>,
    attributes: &Attributes,
    doc: &[String],
) -> String {
    let mut md = format!("**{kind}** `{name}`\n");
    if !doc.is_empty() {
        md.push_str(&format!("\n{}\n", doc.join("\n")));
    }
    let attrs = render_attributes(attributes);
    if !attrs.is_empty() {
        md.push_str(&format!("\n**Attributes:** {attrs}\n"));
    }
    if let Some(v) = value {
        let value = if v >= 0 {
            format!("`{v}` (`0x{v:X}`)")
        } else {
            format!("`{v}`")
        };
        push_facts(&mut md, &[("value", value)]);
    }
    md
}

/// Hover markdown for a function (vftable entry or impl method).
pub(crate) fn format_function_hover(f: &Function) -> String {
    let mut md = format!("**fn** `{}`\n\n", f.name.as_str());
    md.push_str(&format!("```pyxis\n{}\n```\n", render_fn_signature(f)));
    let attrs = render_attributes(&f.attributes);
    if !attrs.is_empty() {
        md.push_str(&format!("\n**Attributes:** {attrs}\n"));
    }
    if !f.doc_comments.is_empty() {
        md.push_str(&format!("\n{}\n", f.doc_comments.join("\n")));
    }
    md
}

/// The `#[address(N)]` value on an extern value's attributes, if present.
fn address_attribute_value(attributes: &Attributes) -> Option<isize> {
    attributes.0.iter().find_map(|attr| {
        let (name, items) = attr.function()?;
        if name.as_str() != "address" {
            return None;
        }
        match items.exprs().next()? {
            Expr::IntLiteral { value, .. } => Some(*value),
            _ => None,
        }
    })
}

/// The base hover markdown for an `extern value`: its `name: type` signature and
/// its fixed `#[address]`. Callers with a resolved layout append size facts.
pub(crate) fn format_extern_value_hover(
    name: &str,
    type_: &Type,
    attributes: &Attributes,
) -> String {
    let mut md = format!("**extern value** `{name}`\n\n```pyxis\n{name}: {type_}\n```\n");
    if let Some(address) = address_attribute_value(attributes) {
        push_facts(&mut md, &[("address", format!("`0x{address:X}`"))]);
    }
    md
}

/// An explicit `#[index(N)]` on a vftable function, if present.
pub(crate) fn explicit_vftable_index(f: &Function) -> Option<usize> {
    f.attributes.iter().find_map(|attr| match attr {
        Attribute::Function { name, items, .. } if name.as_str() == "index" => items
            .exprs()
            .next()
            .and_then(|e| e.int_literal())
            .map(|v| v as usize),
        _ => None,
    })
}

/// The slot index of `target` within a vftable's function list. Indices run
/// sequentially but an `#[index(N)]` resets the running counter (the compiler
/// pads the gap with `_vfunc_*` entries), so this mirrors that assignment.
pub(crate) fn vftable_index_of(fns: &[Function], target: &Function) -> usize {
    let mut idx = 0;
    for func in fns {
        if let Some(n) = explicit_vftable_index(func) {
            idx = n;
        }
        if std::ptr::eq(func, target) {
            return idx;
        }
        idx += 1;
    }
    idx
}

/// Hover for a vftable entry: the function signature plus its slot index and
/// byte offset from the base of the generated vftable struct.
pub(crate) fn format_vftable_fn_hover(f: &Function, index: usize, pointer_size: usize) -> String {
    let mut md = format_function_hover(f);
    push_facts(
        &mut md,
        &[
            ("index", format!("`{index}`")),
            ("vftable offset", fmt_bytes(index * pointer_size)),
        ],
    );
    md
}
