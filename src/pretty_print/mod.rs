/// Basic pretty printer for Pyxis AST
///
/// This module provides functionality to convert a parsed AST back into
/// formatted Pyxis source code. This is useful for:
/// - Validating the AST design
/// - Formatting/normalizing code
/// - Testing round-trip parsing
use crate::grammar::{ItemDefinitionInner, *};
use std::fmt::Write;

mod definitions;
mod expr_and_format;
#[cfg(test)]
mod tests;

pub struct PrettyPrinter {
    output: String,
    indent_level: usize,
    indent_string: String,
    /// Context for determining how to format expressions
    in_vftable_index: bool,
    /// Width in bits for binary literal formatting (None means default to 32)
    binary_literal_width: Option<usize>,
}

impl PrettyPrinter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            indent_string: "    ".to_string(), // 4 spaces
            in_vftable_index: false,
            binary_literal_width: None,
        }
    }

    pub fn with_indent(indent: &str) -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            indent_string: indent.to_string(),
            in_vftable_index: false,
            binary_literal_width: None,
        }
    }

    pub(super) fn indent(&mut self) {
        self.indent_level += 1;
    }

    pub(super) fn dedent(&mut self) {
        self.indent_level = self.indent_level.saturating_sub(1);
    }

    pub(super) fn write_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.output.push_str(&self.indent_string);
        }
    }

    pub(super) fn writeln(&mut self, s: &str) {
        self.output.push_str(s);
        self.output.push('\n');
    }

    pub fn print_module(&mut self, module: &Module) -> String {
        // Print module-level doc comments
        for doc in &module.doc_comments {
            writeln!(&mut self.output, "//!{doc}").unwrap();
        }

        // Add blank line after module doc comments if there are any
        if !module.doc_comments.is_empty() {
            self.writeln("");
        }

        // Print items with lookahead for proper spacing
        for (i, item) in module.items.iter().enumerate() {
            let next_item = module.items.get(i + 1);
            self.print_module_item(item, next_item);

            // Preserve a single blank line between a comment and the following
            // item if the source had one. Comments otherwise group with the
            // item directly beneath them, so we must not invent a blank line.
            if let (ModuleItem::Comment { .. }, Some(next)) = (item, next_item) {
                use crate::span::HasLocation;
                let comment_end = item.location().span.end.line;
                let next_start = next.location().span.start.line;
                if next_start > comment_end + 1 {
                    self.writeln("");
                }
            }
        }

        self.output.trim().to_string()
    }

    fn print_module_item(&mut self, item: &ModuleItem, next_item: Option<&ModuleItem>) {
        match item {
            ModuleItem::Comment { comment } => {
                self.print_comment(comment);
                // Don't add blank line after comments - they group with the following item
            }
            ModuleItem::InnerAttributes { attributes, .. } => {
                self.print_inner_attributes(attributes);
                // Separate from the following item with a blank line, unless
                // it's another inner-attribute group.
                if !matches!(next_item, Some(ModuleItem::InnerAttributes { .. })) {
                    self.writeln("");
                }
            }
            ModuleItem::Use {
                tree,
                visibility,
                attributes,
                ..
            } => {
                self.print_attributes(attributes);
                self.write_indent();
                let tree_str = self.format_use_tree(tree);
                let vis = match visibility {
                    Visibility::Public => "pub ",
                    Visibility::Private => "",
                };
                writeln!(&mut self.output, "{vis}use {tree_str};").unwrap();
                // Only add blank line if next item is not a use statement
                if !matches!(next_item, Some(ModuleItem::Use { .. })) {
                    self.writeln("");
                }
            }
            ModuleItem::ExternType {
                name,
                attributes,
                doc_comments,
                ..
            } => {
                // Print doc comments
                for doc in doc_comments {
                    self.write_indent();
                    writeln!(&mut self.output, "///{doc}").unwrap();
                }
                self.print_attributes(attributes);
                self.write_indent();
                writeln!(&mut self.output, "extern type {name};").unwrap();
                self.writeln("");
            }
            ModuleItem::Splice { splice } => {
                self.print_splice(splice);
                self.writeln("");
            }
            ModuleItem::Definition { definition } => {
                self.print_item_definition(definition, false);
                // Add blank line after this item, unless the next item is
                // another value item (group consts / extern values together
                // without blank lines) or an impl block.
                let is_value = is_value_item(&definition.inner);
                let next_is_value = matches!(
                    next_item,
                    Some(ModuleItem::Definition { definition }) if is_value_item(&definition.inner)
                );
                if !(matches!(next_item, Some(ModuleItem::Impl { .. }))
                    || (is_value && next_is_value))
                {
                    self.writeln("");
                }
            }
            ModuleItem::Impl { impl_block } => {
                self.print_impl_block(impl_block);
                self.writeln("");
            }
            ModuleItem::Function { function } => {
                self.print_function(function);
                self.writeln("");
            }
        }
    }

    pub(super) fn print_comment(&mut self, comment: &Comment) {
        match comment {
            Comment::DocOuter { lines, .. } => {
                for line in lines {
                    self.write_indent();
                    writeln!(&mut self.output, "/// {line}").unwrap();
                }
            }
            Comment::DocInner { lines, .. } => {
                for line in lines {
                    self.write_indent();
                    writeln!(&mut self.output, "//! {line}").unwrap();
                }
            }
            Comment::Regular { text, .. } => {
                // Regular comments include the // prefix
                self.write_indent();
                writeln!(&mut self.output, "{text}").unwrap();
            }
            Comment::MultiLine { lines, .. } => {
                // Multiline comments include /* and */ in the text
                for line in lines {
                    self.write_indent();
                    writeln!(&mut self.output, "{line}").unwrap();
                }
            }
        }
    }

    pub(super) fn print_comment_inline(&mut self, comment: &Comment) {
        match comment {
            Comment::Regular { text, .. } => {
                // Regular comments include the // prefix
                write!(&mut self.output, "{text}").unwrap();
            }
            Comment::MultiLine { lines, .. } => {
                // Multiline comments - just print first line inline for now
                if let Some(first) = lines.first() {
                    write!(&mut self.output, "{first}").unwrap();
                }
                // If there are more lines, print them on separate lines
                for line in lines.iter().skip(1) {
                    writeln!(&mut self.output).unwrap();
                    self.write_indent();
                    write!(&mut self.output, "{line}").unwrap();
                }
            }
            _ => {
                // For doc comments, shouldn't appear as trailing comments
                // but handle gracefully
                self.print_comment(comment);
            }
        }
    }

    pub(super) fn print_attributes(&mut self, attrs: &Attributes) {
        self.print_attributes_inner(attrs, false);
    }

    /// Print inner attributes (`#![...]`), used at the top of a module.
    fn print_inner_attributes(&mut self, attrs: &Attributes) {
        self.print_attributes_inner(attrs, true);
    }

    fn print_attributes_inner(&mut self, attrs: &Attributes, inner: bool) {
        if attrs.0.is_empty() {
            return;
        }

        self.write_indent();
        write!(&mut self.output, "{}[", if inner { "#!" } else { "#" }).unwrap();
        for (i, attr) in attrs.0.iter().enumerate() {
            if i > 0 {
                write!(&mut self.output, ", ").unwrap();
            }
            self.print_attribute(attr);
        }
        writeln!(&mut self.output, "]").unwrap();
    }

    pub(super) fn print_attribute(&mut self, attr: &Attribute) {
        match attr {
            Attribute::Ident { ident, .. } => {
                write!(&mut self.output, "{ident}").unwrap();
            }
            Attribute::Function { name, items, .. } => {
                // Check special formatting requirements
                let is_index = name.as_str() == "index";
                let needs_underscore = matches!(name.as_str(), "address" | "singleton");

                if is_index {
                    self.in_vftable_index = true;
                }

                write!(&mut self.output, "{name}(").unwrap();
                let mut first_expr = true;
                for item in items {
                    match item {
                        AttributeItem::Expr { expr, .. } => {
                            if !first_expr {
                                write!(&mut self.output, ", ").unwrap();
                            }
                            first_expr = false;

                            // Format with underscores for address/singleton
                            if needs_underscore {
                                if let Expr::IntLiteral { value, .. } = expr {
                                    let formatted = self.format_hex_with_underscores(*value);
                                    write!(&mut self.output, "{formatted}").unwrap();
                                } else {
                                    self.print_expr(expr);
                                }
                            } else {
                                self.print_expr(expr);
                            }
                        }
                        AttributeItem::Comment { text, .. } => {
                            write!(&mut self.output, " {text}").unwrap();
                        }
                    }
                }
                write!(&mut self.output, ")").unwrap();

                if is_index {
                    self.in_vftable_index = false;
                }
            }
            Attribute::Assign { name, items, .. } => {
                write!(&mut self.output, "{name} = ").unwrap();
                for item in items {
                    match item {
                        AttributeItem::Expr { expr, .. } => {
                            self.print_expr(expr);
                        }
                        AttributeItem::Comment { text, .. } => {
                            write!(&mut self.output, " {text}").unwrap();
                        }
                    }
                }
            }
            Attribute::Cfg { predicate, .. } => {
                write!(&mut self.output, "cfg(").unwrap();
                self.print_cfg_predicate(predicate);
                write!(&mut self.output, ")").unwrap();
            }
        }
    }

    fn print_cfg_predicate(&mut self, p: &crate::parser::cfg::CfgPredicate) {
        use crate::parser::cfg::{CfgAtom, CfgPredicate};
        match p {
            CfgPredicate::Atom { atom, .. } => match atom {
                CfgAtom::Ident { name, .. } => {
                    write!(&mut self.output, "{name}").unwrap();
                }
                CfgAtom::KeyValue { key, value, .. } => {
                    write!(&mut self.output, "{key} = \"{value}\"").unwrap();
                }
            },
            CfgPredicate::Any { predicates, .. } => {
                write!(&mut self.output, "any(").unwrap();
                for (i, child) in predicates.iter().enumerate() {
                    if i > 0 {
                        write!(&mut self.output, ", ").unwrap();
                    }
                    self.print_cfg_predicate(child);
                }
                write!(&mut self.output, ")").unwrap();
            }
            CfgPredicate::All { predicates, .. } => {
                write!(&mut self.output, "all(").unwrap();
                for (i, child) in predicates.iter().enumerate() {
                    if i > 0 {
                        write!(&mut self.output, ", ").unwrap();
                    }
                    self.print_cfg_predicate(child);
                }
                write!(&mut self.output, ")").unwrap();
            }
            CfgPredicate::Not { predicate, .. } => {
                write!(&mut self.output, "not(").unwrap();
                self.print_cfg_predicate(predicate);
                write!(&mut self.output, ")").unwrap();
            }
        }
    }
}

impl Default for PrettyPrinter {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to pretty print a module
pub fn pretty_print(module: &Module) -> String {
    let mut printer = PrettyPrinter::new();
    printer.print_module(module)
}

/// Whether an item is a compact "value" item — a constant or an extern value.
/// These print as grouped one-liners (no blank lines between them) and carry
/// their own statement terminator when nested, unlike block items (type / enum
/// / bitflags) which end in `}` and need a trailing comma appended.
pub(super) fn is_value_item(inner: &ItemDefinitionInner) -> bool {
    matches!(
        inner,
        ItemDefinitionInner::Constant(_) | ItemDefinitionInner::ExternValue(_)
    )
}
