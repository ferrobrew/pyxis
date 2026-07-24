use super::*;

use pyxis::grammar::{Expr, IntFormat, StringFormat};

/// Hover for a predefined (`bool`, `u32`, …) or extern type — these have no
/// source definition but a known size/alignment.
pub(crate) fn builtin_hover(
    path: &ItemPath,
    decl_registry: &DeclarationRegistry,
) -> Option<String> {
    let (kind, size, alignment) = if let Some(p) = decl_registry.get_predefined(path) {
        ("builtin", p.size, p.alignment)
    } else {
        let e = decl_registry.get_extern_type(path)?;
        ("extern type", e.size, e.alignment)
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

/// Render a `const`'s value expression back to source-like text, preserving the
/// base an integer was written in.
pub(crate) fn render_const_expr(expr: &Expr) -> String {
    match expr {
        Expr::IntLiteral { value, format, .. } => int_in_base(*value as i128, *format),
        Expr::FloatLiteral { raw_text, .. } => raw_text.clone(),
        Expr::StringLiteral { value, format, .. } => match format {
            StringFormat::Regular => format!("{value:?}"),
            StringFormat::Raw => format!("r#\"{value}\"#"),
        },
        Expr::CStringLiteral { value, format, .. } => match format {
            StringFormat::Regular => format!("c{value:?}"),
            StringFormat::Raw => format!("cr#\"{value}\"#"),
        },
        Expr::Ident { ident, .. } => ident.to_string(),
        Expr::Path { path, .. } => path.to_string(),
        Expr::StructLiteral {
            type_name, fields, ..
        } => {
            let mut s = format!("{type_name} {{ ");
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&format!(
                    "{}: {}",
                    field.ident(),
                    render_const_expr(&field.1)
                ));
            }
            s.push_str(" }");
            s
        }
        Expr::ArrayLiteral { elements, .. } => {
            let parts: Vec<String> = elements.iter().map(render_const_expr).collect();
            format!("[{}]", parts.join(", "))
        }
    }
}

/// Format an integer in a given base with a `0x`/`0b`/`0o` prefix, using a
/// sign-magnitude form (`-0x3`) rather than a fixed-width two's-complement one.
pub(crate) fn int_in_base(value: i128, base: IntFormat) -> String {
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
