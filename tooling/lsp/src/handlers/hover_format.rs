use super::*;

use pyxis::{
    grammar::{Argument, Backend, Expr, Ident, IntFormat, ItemDefinitionInner, StringFormat},
    semantic::types::{ItemDefinitionInner as ResolvedInner, ItemStateResolved},
    tokenizer::TokenKind,
};

/// Hover for a predefined (`bool`, `u32`, …) or extern type — these have no
/// source definition but a known size/alignment.
pub(crate) fn builtin_hover(
    path: &ItemPath,
    decl_registry: &DeclarationRegistry,
) -> Option<String> {
    let (kind, size, alignment) = if let Some(p) = decl_registry.get_predefined(path) {
        ("builtin", p.size, p.alignment)
    } else if let Some(e) = decl_registry.get_extern_type(path) {
        ("extern type", e.size, e.alignment)
    } else {
        return None;
    };
    let mut md = format!("**{kind}** `{path}`\n");
    push_facts(
        &mut md,
        &[("size", fmt_bytes(size)), ("align", fmt_bytes(alignment))],
    );
    Some(md)
}

/// Render a byte quantity as `` `0x10` (16) `` for hover facts.
pub(crate) fn fmt_bytes(n: usize) -> String {
    format!("`0x{n:X}` ({n})")
}

/// Join labelled facts into a single markdown line so they don't collapse onto
/// each other (a lone `\n` is not a line break in markdown). rust-analyzer
/// renders the same kind of `size = …, align = …` brief; we use ` | ` so the
/// facts read as a compact, scannable strip.
pub(crate) fn facts_line(facts: &[(&str, String)]) -> String {
    facts
        .iter()
        .map(|(label, value)| format!("{label} {value}"))
        .collect::<Vec<_>>()
        .join("  |  ")
}

/// Append a facts line as its own paragraph (blank line before).
pub(crate) fn push_facts(md: &mut String, facts: &[(&str, String)]) {
    if !facts.is_empty() {
        md.push_str(&format!("\n{}\n", facts_line(facts)));
    }
}

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

/// Render a `const`'s value expression back to source-like text, preserving the
/// base an integer was written in.
fn render_const_expr(expr: &Expr) -> String {
    match expr {
        Expr::IntLiteral { value, format, .. } => int_in_base(*value as i128, *format),
        Expr::FloatLiteral { raw_text, .. } => raw_text.clone(),
        Expr::StringLiteral { value, format, .. } => match format {
            StringFormat::Regular => format!("{value:?}"),
            StringFormat::Raw => format!("r#\"{value}\"#"),
        },
        Expr::Ident { ident, .. } => ident.to_string(),
        Expr::Path { path, .. } => path.to_string(),
    }
}

/// Format an integer in a given base with a `0x`/`0b`/`0o` prefix, using a
/// sign-magnitude form (`-0x3`) rather than a fixed-width two's-complement one.
fn int_in_base(value: i128, base: IntFormat) -> String {
    let (sign, mag) = if value < 0 {
        ("-", value.unsigned_abs())
    } else {
        ("", value as u128)
    };
    match base {
        IntFormat::Decimal => format!("{value}"),
        IntFormat::Hex => format!("{sign}0x{mag:X}"),
        IntFormat::Binary => format!("{sign}0b{mag:b}"),
        IntFormat::Octal => format!("{sign}0o{mag:o}"),
    }
}

/// A compact multi-base view of an integer for hover:
/// `` decimal `31`  |  hex `0x1F`  |  binary `0b11111` ``.
pub(crate) fn format_int_reprs(value: i128) -> String {
    facts_line(&[
        ("decimal", format!("`{value}`")),
        ("hex", format!("`{}`", int_in_base(value, IntFormat::Hex))),
        (
            "binary",
            format!("`{}`", int_in_base(value, IntFormat::Binary)),
        ),
    ])
}

/// Parse an integer literal token's source text (`42`, `0x1F`, `-0b1010`, with
/// optional `_` separators) into its value.
fn parse_int_literal_str(s: &str) -> Option<i128> {
    let s = s.replace('_', "");
    let (neg, body) = match s.strip_prefix('-') {
        Some(rest) => (true, rest),
        None => (false, s.as_str()),
    };
    let mag = if let Some(h) = body.strip_prefix("0x") {
        i128::from_str_radix(h, 16).ok()?
    } else if let Some(b) = body.strip_prefix("0b") {
        i128::from_str_radix(b, 2).ok()?
    } else if let Some(o) = body.strip_prefix("0o") {
        i128::from_str_radix(o, 8).ok()?
    } else {
        body.parse::<i128>().ok()?
    };
    Some(if neg { -mag } else { mag })
}

/// Hover for a numeric or character literal token: its value in several forms,
/// so hovering e.g. an enum variant's value describes the literal itself rather
/// than resolving to the enclosing variant. String literals are intentionally
/// excluded — backend splices (`r#"…"#`) lex as string literals, and their
/// contents are not meaningfully "a literal value" to a reader.
pub(crate) fn literal_hover(kind: &TokenKind) -> Option<String> {
    match kind {
        TokenKind::IntLiteral(s) => {
            let value = parse_int_literal_str(s)?;
            Some(format!(
                "**integer literal**\n\n{}\n",
                format_int_reprs(value)
            ))
        }
        TokenKind::FloatLiteral(s) => {
            let v: f64 = s.replace('_', "").parse().ok()?;
            let facts = facts_line(&[
                ("value", format!("`{v}`")),
                ("bits", format!("`0x{:016X}`", v.to_bits())),
            ]);
            Some(format!("**float literal**\n\n{facts}\n"))
        }
        TokenKind::CharLiteral(c) => {
            let facts = facts_line(&[
                ("codepoint", format!("`U+{:04X}`", *c as u32)),
                ("decimal", format!("`{}`", *c as u32)),
            ]);
            Some(format!(
                "**character literal** `'{}'`\n\n{facts}\n",
                c.escape_default()
            ))
        }
        _ => None,
    }
}

/// Describe a backend keyword (`cpp`/`rust`/`prologue`/`epilogue`/`definition`/
/// `uses`/`for`) under the cursor. The grammar AST carries no spans for these,
/// so we tokenize and match the token directly. A `r#"…"#` splice lexes as a
/// single token, so keywords inside the spliced code can never match.
pub(crate) fn backend_term_at(
    tokens: &[Token],
    backend: &Backend,
    loc: &Location,
) -> Option<(String, Span)> {
    use pyxis::tokenizer::TokenKind;
    let name = backend.name.name();
    let token = tokens.iter().find(|t| t.location.span.contains(loc))?;
    let desc: String = match &token.kind {
        TokenKind::Backend => format!("The `{name}` backend block."),
        TokenKind::Prologue => "Splice emitted *before* this backend's generated output.".into(),
        TokenKind::Epilogue => "Splice emitted *after* this backend's generated output.".into(),
        TokenKind::Ident(s) if s == name => {
            format!("The `{name}` backend — code emitted by the {name} generator.")
        }
        TokenKind::Ident(s) if s == "for" => "Attributes this splice to a specific type.".into(),
        TokenKind::Ident(s) if s == "definition" => {
            "Targets the source/definition file rather than the header.".into()
        }
        TokenKind::Ident(s) if s == "uses" => {
            "Declares other-module items this backend block depends on.".into()
        }
        _ => return None,
    };
    Some((format!("**backend**\n\n{desc}"), token.location.span))
}

/// Render a definition/field's attributes compactly (e.g. `#[base] #[cfg(...)]`).
pub(crate) fn render_attributes(attributes: &Attributes) -> String {
    attributes
        .0
        .iter()
        .map(|a| format!("`{}`", render_attribute(a)))
        .collect::<Vec<_>>()
        .join(" ")
}

/// The attribute name (`size`, `cfg`, `base`, …).
pub(crate) fn attribute_name(attribute: &Attribute) -> &str {
    match attribute {
        Attribute::Ident { ident, .. } => ident.as_str(),
        Attribute::Function { name, .. } => name.as_str(),
        Attribute::Assign { name, .. } => name.as_str(),
        Attribute::Cfg { .. } => "cfg",
    }
}

/// A one-line description of a known Pyxis attribute, derived from its
/// structured form: `#[cfg(...)]` is recognised by its own variant; the rest
/// are keyed by their attribute name (the only data distinguishing them).
pub(crate) fn attribute_description(attribute: &Attribute) -> Option<&'static str> {
    if let Attribute::Cfg { .. } = attribute {
        return Some("Conditional-compilation predicate; each backend evaluates it independently.");
    }
    Some(match attribute_name(attribute) {
        "size" => "Asserts/overrides the type's total size in bytes.",
        "align" => "Overrides the type's alignment in bytes.",
        "packed" => "Removes inter-field padding (alignment 1).",
        "base" => "Marks the field as a base class, inlined at the start of the type.",
        "index" => "Pins a vftable entry to a specific slot index.",
        "address" => "Pins the item to a fixed absolute address.",
        "singleton" => "Marks the type as a singleton living at a fixed address.",
        "copyable" => "Marks the type as trivially copyable.",
        "cloneable" => "Marks the type as cloneable.",
        "defaultable" | "default" => "Marks the type/variant as the default.",
        "pinned" => "Marks the type as non-relocatable (must not be moved in memory).",
        "calling_convention" => "Sets the function's calling convention.",
        _ => return None,
    })
}

/// The source text covered by `span` (single line only).
pub(crate) fn span_text(content: &str, span: &Span) -> Option<String> {
    if span.start.line != span.end.line {
        return None;
    }
    let line = content.lines().nth(span.start.line.saturating_sub(1))?;
    let lo = span.start.column.saturating_sub(1);
    let hi = span.end.column.saturating_sub(1).min(line.len());
    line.get(lo..hi).map(str::to_string)
}

/// Find an attribute whose span contains `loc`, anywhere in the module (type /
/// field / vftable / enum-variant / impl / function attributes).
pub(crate) fn attribute_at<'a>(
    module: &'a Module,
    loc: &Location,
) -> Option<(&'a Attribute, Span)> {
    use pyxis::grammar::ImplItem;
    let find = |attrs: &'a Attributes| {
        attrs
            .0
            .iter()
            .find(|a| a.location().span.contains(loc))
            .map(|a| (a, a.location().span))
    };
    for item in &module.items {
        match item {
            ModuleItem::Definition { definition } => {
                let inner_attrs = match &definition.inner {
                    ItemDefinitionInner::Type(td) => &td.attributes,
                    ItemDefinitionInner::Enum(e) => &e.attributes,
                    ItemDefinitionInner::Bitflags(b) => &b.attributes,
                    ItemDefinitionInner::TypeAlias(ta) => &ta.attributes,
                    ItemDefinitionInner::Constant(c) => &c.attributes,
                    ItemDefinitionInner::ExternValue(ev) => &ev.attributes,
                };
                if let Some(hit) = find(inner_attrs) {
                    return Some(hit);
                }
                match &definition.inner {
                    ItemDefinitionInner::Type(td) => {
                        for s in td.statements() {
                            if let Some(hit) = find(&s.attributes) {
                                return Some(hit);
                            }
                            if let TypeField::Vftable(fns) = &s.field {
                                for f in fns {
                                    if let Some(hit) = find(&f.attributes) {
                                        return Some(hit);
                                    }
                                }
                            }
                        }
                    }
                    ItemDefinitionInner::Enum(e) => {
                        for s in e.statements() {
                            if let Some(hit) = find(&s.attributes) {
                                return Some(hit);
                            }
                        }
                    }
                    ItemDefinitionInner::Bitflags(b) => {
                        for s in b.statements() {
                            if let Some(hit) = find(&s.attributes) {
                                return Some(hit);
                            }
                        }
                    }
                    ItemDefinitionInner::TypeAlias(_) => {}
                    ItemDefinitionInner::Constant(_) => {}
                    ItemDefinitionInner::ExternValue(_) => {}
                }
            }
            ModuleItem::Impl { impl_block } => {
                if let Some(hit) = find(&impl_block.attributes) {
                    return Some(hit);
                }
                for it in &impl_block.items {
                    if let ImplItem::Function(f) = it
                        && let Some(hit) = find(&f.attributes)
                    {
                        return Some(hit);
                    }
                }
            }
            ModuleItem::Function { function } => {
                if let Some(hit) = find(&function.attributes) {
                    return Some(hit);
                }
            }
            ModuleItem::ExternType { attributes, .. } => {
                if let Some(hit) = find(attributes) {
                    return Some(hit);
                }
            }
            _ => {}
        }
    }
    None
}

/// Hover markdown for an attribute under the cursor.
pub(crate) fn format_attribute_hover(attribute: &Attribute, span: &Span, content: &str) -> String {
    // The attribute span covers the inner content (`size(0x10)`); re-wrap it as
    // `#[…]` so the hover shows the attribute as written.
    let src = span_text(content, span)
        .map(|s| format!("#[{s}]"))
        .unwrap_or_else(|| render_attribute(attribute));
    let mut md = format!("**attribute**\n\n```pyxis\n{src}\n```\n");
    if let Some(desc) = attribute_description(attribute) {
        md.push_str(&format!("\n{desc}\n"));
    }
    md
}

/// Render a single attribute as Pyxis source (without code fencing).
pub(crate) fn render_attribute(attribute: &Attribute) -> String {
    match attribute {
        Attribute::Ident { ident, .. } => format!("#[{}]", ident.as_str()),
        Attribute::Function { name, .. } => format!("#[{}(…)]", name.as_str()),
        Attribute::Assign { name, .. } => format!("#[{} = …]", name.as_str()),
        Attribute::Cfg { .. } => "#[cfg(…)]".to_string(),
    }
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

#[cfg(test)]
mod tests {
    use super::{format_int_reprs, literal_hover};
    use pyxis::tokenizer::TokenKind;

    #[test]
    fn int_reprs_cover_bases_and_signs() {
        assert_eq!(
            format_int_reprs(31),
            "decimal `31`  |  hex `0x1F`  |  binary `0b11111`"
        );
        assert_eq!(
            format_int_reprs(-3),
            "decimal `-3`  |  hex `-0x3`  |  binary `-0b11`"
        );
    }

    #[test]
    fn literal_hover_handles_numeric_and_char() {
        // Hex source still shows all bases.
        let hex = literal_hover(&TokenKind::IntLiteral("0x1F".into())).unwrap();
        assert!(hex.contains("integer literal"));
        assert!(hex.contains("decimal `31`"));
        assert!(hex.contains("hex `0x1F`"));

        // Underscores are ignored.
        let sep = literal_hover(&TokenKind::IntLiteral("1_000".into())).unwrap();
        assert!(sep.contains("decimal `1000`"));

        let ch = literal_hover(&TokenKind::CharLiteral('A')).unwrap();
        assert!(ch.contains("U+0041"));
        assert!(ch.contains("decimal `65`"));
    }

    #[test]
    fn literal_hover_ignores_strings_and_idents() {
        // Backend splices lex as string literals; they must not hover as values.
        assert!(literal_hover(&TokenKind::StringLiteral("hi".into())).is_none());
        assert!(literal_hover(&TokenKind::Ident("Foo".into())).is_none());
    }
}
