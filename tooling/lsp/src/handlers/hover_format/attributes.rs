use super::*;

use pyxis::grammar::ItemDefinitionInner;

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
            // A cfg-gated `use` (`#[cfg(backend = "cpp")] use ...;`) carries its
            // gate in `attributes`; hovering it should describe the attribute.
            ModuleItem::Use { attributes, .. } => {
                if let Some(hit) = find(attributes) {
                    return Some(hit);
                }
            }
            // Likewise a cfg-gated splice statement.
            ModuleItem::Splice { splice } => {
                if let Some(hit) = find(&splice.attributes) {
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
