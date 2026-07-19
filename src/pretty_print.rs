/// Basic pretty printer for Pyxis AST
///
/// This module provides functionality to convert a parsed AST back into
/// formatted Pyxis source code. This is useful for:
/// - Validating the AST design
/// - Formatting/normalizing code
/// - Testing round-trip parsing
use crate::grammar::{ItemDefinitionInner, *};
use std::fmt::Write;

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

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        self.indent_level = self.indent_level.saturating_sub(1);
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.output.push_str(&self.indent_string);
        }
    }

    fn writeln(&mut self, s: &str) {
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

    fn print_comment(&mut self, comment: &Comment) {
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

    fn print_comment_inline(&mut self, comment: &Comment) {
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

    fn print_attributes(&mut self, attrs: &Attributes) {
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

    /// Format a hex number with underscores every 3 digits from the right
    /// e.g., 0x142ED0E78 -> 0x142_ED0_E78
    fn format_hex_with_underscores(&self, val: isize) -> String {
        if val < 0 {
            return format!("{val}");
        }

        let hex_str = format!("{val:X}");
        let mut result = String::from("0x");
        let len = hex_str.len();

        for (i, ch) in hex_str.chars().enumerate() {
            if i > 0 && (len - i) % 3 == 0 {
                result.push('_');
            }
            result.push(ch);
        }

        result
    }

    /// Format a binary number with padding and underscores every 4 bits
    /// e.g., for u8: 1 -> 0b0000_0001
    /// e.g., for u32: 1 -> 0b0000_0000_0000_0000_0000_0000_0000_0001
    fn format_binary_with_padding(&self, val: isize) -> String {
        if val < 0 {
            return format!("0b{val:b}");
        }

        // Determine width based on context or default to 32
        let width = self.binary_literal_width.unwrap_or(32);

        // Format as binary and pad to width
        let bin_str = format!("{val:b}");
        let padding = width.saturating_sub(bin_str.len());
        let padded = "0".repeat(padding) + &bin_str;

        // Add underscores every 4 bits from the right
        let mut result = String::from("0b");
        for (i, ch) in padded.chars().enumerate() {
            if i > 0 && (padded.len() - i) % 4 == 0 {
                result.push('_');
            }
            result.push(ch);
        }

        result
    }

    /// Get the bit width from a type (e.g., u8 -> 8, u32 -> 32)
    fn get_type_bit_width(&self, type_: &Type) -> Option<usize> {
        if let Type::Ident { path, .. } = type_ {
            // For bit width, we only care about single-segment primitive types
            if path.len() == 1 {
                if let Some(segment) = path.last() {
                    return match segment.as_str() {
                        "u8" | "i8" => Some(8),
                        "u16" | "i16" => Some(16),
                        "u32" | "i32" => Some(32),
                        "u64" | "i64" => Some(64),
                        "u128" | "i128" => Some(128),
                        _ => None,
                    };
                }
            }
        }
        None
    }

    fn print_attribute(&mut self, attr: &Attribute) {
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

    fn print_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntLiteral { value, format, .. } => match format {
                IntFormat::Hex => write!(&mut self.output, "0x{value:X}").unwrap(),
                IntFormat::Binary => {
                    let formatted = self.format_binary_with_padding(*value);
                    write!(&mut self.output, "{formatted}").unwrap();
                }
                IntFormat::Octal => write!(&mut self.output, "0o{value:o}").unwrap(),
                IntFormat::Decimal => write!(&mut self.output, "{value}").unwrap(),
            },
            Expr::StringLiteral { value, format, .. } => {
                match format {
                    StringFormat::Raw => {
                        // Determine the number of # needed
                        let hash_count = self.count_hashes_needed(value);
                        let hashes = "#".repeat(hash_count);
                        write!(&mut self.output, "r{hashes}\"{value}\"{hashes}").unwrap();
                    }
                    StringFormat::Regular => {
                        // Escape special characters for regular strings
                        write!(&mut self.output, "\"").unwrap();
                        for ch in value.chars() {
                            match ch {
                                '"' => write!(&mut self.output, "\\\"").unwrap(),
                                '\\' => write!(&mut self.output, "\\\\").unwrap(),
                                '\n' => write!(&mut self.output, "\\n").unwrap(),
                                '\r' => write!(&mut self.output, "\\r").unwrap(),
                                '\t' => write!(&mut self.output, "\\t").unwrap(),
                                _ => write!(&mut self.output, "{ch}").unwrap(),
                            }
                        }
                        write!(&mut self.output, "\"").unwrap();
                    }
                }
            }
            Expr::CStringLiteral { value, format, .. } => match format {
                StringFormat::Raw => {
                    let hash_count = self.count_hashes_needed(value);
                    let hashes = "#".repeat(hash_count);
                    write!(&mut self.output, "cr{hashes}\"{value}\"{hashes}").unwrap();
                }
                StringFormat::Regular => {
                    write!(&mut self.output, "c\"").unwrap();
                    for ch in value.chars() {
                        match ch {
                            '"' => write!(&mut self.output, "\\\"").unwrap(),
                            '\\' => write!(&mut self.output, "\\\\").unwrap(),
                            '\n' => write!(&mut self.output, "\\n").unwrap(),
                            '\r' => write!(&mut self.output, "\\r").unwrap(),
                            '\t' => write!(&mut self.output, "\\t").unwrap(),
                            _ => write!(&mut self.output, "{ch}").unwrap(),
                        }
                    }
                    write!(&mut self.output, "\"").unwrap();
                }
            },
            Expr::Ident { ident, .. } => write!(&mut self.output, "{ident}").unwrap(),
            Expr::FloatLiteral { raw_text, .. } => {
                write!(&mut self.output, "{raw_text}").unwrap();
            }
            Expr::Path { path, .. } => {
                write!(&mut self.output, "{path}").unwrap();
            }
            Expr::StructLiteral {
                type_name, fields, ..
            } => {
                write!(&mut self.output, "{type_name} {{ ").unwrap();
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(&mut self.output, ", ").unwrap();
                    }
                    write!(&mut self.output, "{}: ", field.ident()).unwrap();
                    self.print_expr(&field.1);
                }
                write!(&mut self.output, " }}").unwrap();
            }
            Expr::ArrayLiteral { elements, .. } => {
                write!(&mut self.output, "[").unwrap();
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(&mut self.output, ", ").unwrap();
                    }
                    self.print_expr(elem);
                }
                write!(&mut self.output, "]").unwrap();
            }
        }
    }

    /// Count how many # characters are needed for a raw string
    fn count_hashes_needed(&self, s: &str) -> usize {
        let mut max_consecutive = 0;
        let mut current_consecutive = 0;
        let mut after_quote = false;

        for ch in s.chars() {
            if ch == '"' {
                after_quote = true;
                current_consecutive = 0;
            } else if after_quote && ch == '#' {
                current_consecutive += 1;
                max_consecutive = max_consecutive.max(current_consecutive);
            } else {
                after_quote = false;
                current_consecutive = 0;
            }
        }

        max_consecutive + 1
    }

    fn format_item_path(&self, path: &ItemPath) -> String {
        path.iter()
            .map(|seg| seg.as_str())
            .collect::<Vec<_>>()
            .join("::")
    }

    fn format_type_parameters(&self, type_parameters: &[TypeParameter]) -> String {
        if type_parameters.is_empty() {
            String::new()
        } else {
            let params: Vec<&str> = type_parameters.iter().map(|p| p.name.as_str()).collect();
            format!("<{}>", params.join(", "))
        }
    }

    /// Format a UseTree for pretty printing
    fn format_use_tree(&self, tree: &UseTree) -> String {
        match tree {
            UseTree::Path { path, .. } => self.format_item_path(path),
            UseTree::Group { prefix, items, .. } => {
                let prefix_str = self.format_item_path(prefix);
                let items_str = items
                    .iter()
                    .map(|item| self.format_use_tree(item))
                    .collect::<Vec<_>>()
                    .join(", ");
                if prefix_str.is_empty() {
                    format!("{{{items_str}}}")
                } else {
                    format!("{prefix_str}::{{{items_str}}}")
                }
            }
        }
    }

    /// Format a string literal with the specified format
    fn format_string_with_format(&self, s: &str, format: StringFormat) -> String {
        match format {
            StringFormat::Raw => {
                // Determine the number of # needed
                let hash_count = self.count_hashes_needed(s);
                let hashes = "#".repeat(hash_count);
                format!("r{hashes}\"{s}\"{hashes}")
            }
            StringFormat::Regular => {
                // Escape special characters for regular strings
                let mut result = String::from("\"");
                for ch in s.chars() {
                    match ch {
                        '"' => result.push_str("\\\""),
                        '\\' => result.push_str("\\\\"),
                        '\n' => result.push_str("\\n"),
                        '\r' => result.push_str("\\r"),
                        '\t' => result.push_str("\\t"),
                        _ => result.push(ch),
                    }
                }
                result.push('"');
                result
            }
        }
    }

    fn print_splice(&mut self, splice: &Splice) {
        // Leading `#[cfg(...)]` (or any) attributes, one per line.
        self.print_attributes(&splice.attributes);
        self.write_indent();
        // Splices are code blocks: render any multi-line body as a raw string
        // so it lays out across real lines instead of a single `"\n...\n"`
        // escape soup. Single-line bodies keep their original format.
        let format = if splice.text.contains('\n') {
            StringFormat::Raw
        } else {
            splice.format
        };
        let s = self.format_string_with_format(&splice.text, format);
        let m = self.splice_modifiers(splice.definition, splice.for_type.as_ref());
        let kw = splice.kind.keyword();
        writeln!(&mut self.output, "{kw}{m} {s};").unwrap();
    }

    /// Format the modifier suffix for a splice slot: an optional `definition`
    /// followed by an optional `for <ItemPath>`, each preceded by a space.
    /// Returns an empty string when neither modifier is present so it can be
    /// interpolated into `"... prologue{m} {s};"` without trimming.
    fn splice_modifiers(&self, is_definition: bool, for_type: Option<&ItemPath>) -> String {
        let mut out = String::new();
        if is_definition {
            out.push_str(" definition");
        }
        if let Some(path) = for_type {
            out.push_str(" for ");
            out.push_str(&self.format_item_path(path));
        }
        out
    }

    fn print_item_definition(&mut self, def: &ItemDefinition, nested: bool) {
        // Print doc comments (they already include the space after ///)
        for doc in &def.doc_comments {
            self.write_indent();
            writeln!(&mut self.output, "///{doc}").unwrap();
        }

        // Print attributes and comments from the inner definition
        let (attributes, inline_trailing_comments, following_comments) = match &def.inner {
            ItemDefinitionInner::Type(td) => (
                &td.attributes,
                &td.inline_trailing_comments,
                &td.following_comments,
            ),
            ItemDefinitionInner::Enum(ed) => (
                &ed.attributes,
                &ed.inline_trailing_comments,
                &ed.following_comments,
            ),
            ItemDefinitionInner::Bitflags(bf) => (
                &bf.attributes,
                &bf.inline_trailing_comments,
                &bf.following_comments,
            ),
            ItemDefinitionInner::TypeAlias(ta) => (&ta.attributes, &Vec::new(), &Vec::new()),
            ItemDefinitionInner::Constant(cd) => (&cd.attributes, &Vec::new(), &Vec::new()),
            ItemDefinitionInner::ExternValue(ev) => (&ev.attributes, &Vec::new(), &Vec::new()),
        };

        // Print attributes with inline trailing comments
        if !attributes.0.is_empty() {
            self.write_indent();
            write!(&mut self.output, "#[").unwrap();
            for (i, attr) in attributes.0.iter().enumerate() {
                if i > 0 {
                    write!(&mut self.output, ", ").unwrap();
                }
                self.print_attribute(attr);
            }
            write!(&mut self.output, "]").unwrap();

            // Print inline trailing comments (comments on the same line as attributes)
            for comment in inline_trailing_comments {
                write!(&mut self.output, " ").unwrap();
                self.print_comment_inline(comment);
            }

            writeln!(&mut self.output).unwrap();
        }

        // Print following comments (comments on lines after attributes)
        for comment in following_comments {
            self.print_comment(comment);
        }

        self.write_indent();
        if def.visibility == Visibility::Public {
            write!(&mut self.output, "pub ").unwrap();
        }

        let type_params = self.format_type_parameters(&def.type_parameters);

        match &def.inner {
            ItemDefinitionInner::Type(td) => {
                // Opaque types (`type Name`, no body) have no braces, so they take
                // a caller-supplied terminator: `,` when nested, `;` at module
                // level. A braced body — even an empty one — is self-terminating.
                if td.is_opaque {
                    let terminator = if nested { ',' } else { ';' };
                    writeln!(
                        &mut self.output,
                        "type {}{}{terminator}",
                        def.name, type_params
                    )
                    .unwrap();
                } else {
                    writeln!(&mut self.output, "type {}{} {{", def.name, type_params).unwrap();
                    self.indent();

                    // Partition items into groups: (comments, statement) pairs.
                    // Comments attach to the NEXT statement in source order.
                    // Then split into nested-item groups and other groups.
                    let mut groups: Vec<(Vec<&Comment>, &TypeDefItem)> = Vec::new();
                    let mut pending_comments: Vec<&Comment> = Vec::new();
                    for item in &td.items {
                        match item {
                            TypeDefItem::Comment(c) => {
                                pending_comments.push(c);
                            }
                            TypeDefItem::Statement(_) => {
                                groups.push((std::mem::take(&mut pending_comments), item));
                            }
                        }
                    }

                    // Partition into three groups: value items (constants and
                    // extern values), nested types, and other items. Value items
                    // are compact one-liners grouped together without blank lines.
                    let const_groups: Vec<_> = groups
                        .iter()
                        .filter(|(_, item)| {
                            if let TypeDefItem::Statement(stmt) = item {
                                if let TypeField::Item(inner) = &stmt.field {
                                    return is_value_item(&inner.inner);
                                }
                            }
                            false
                        })
                        .collect();
                    let nested_type_groups: Vec<_> = groups
                        .iter()
                        .filter(|(_, item)| {
                            if let TypeDefItem::Statement(stmt) = item {
                                if let TypeField::Item(inner) = &stmt.field {
                                    return !is_value_item(&inner.inner);
                                }
                            }
                            false
                        })
                        .collect();
                    let other_groups: Vec<_> = groups
                        .iter()
                        .filter(|(_, item)| {
                            if let TypeDefItem::Statement(stmt) = item {
                                return !matches!(stmt.field, TypeField::Item(_));
                            }
                            true
                        })
                        .collect();

                    // Emit constants first (no blank lines between them)
                    for (comments, item) in &const_groups {
                        for c in comments {
                            self.print_comment(c);
                        }
                        if let TypeDefItem::Statement(stmt) = item {
                            self.print_type_statement(stmt, None);
                        }
                    }

                    // Blank line between constants and nested types
                    if !const_groups.is_empty() && !nested_type_groups.is_empty() {
                        self.writeln("");
                    }

                    // Emit nested types
                    for (comments, item) in &nested_type_groups {
                        for c in comments {
                            self.print_comment(c);
                        }
                        if let TypeDefItem::Statement(stmt) = item {
                            self.print_type_statement(stmt, None);
                        }
                    }

                    // Blank line between nested items and other items
                    if (!const_groups.is_empty() || !nested_type_groups.is_empty())
                        && !other_groups.is_empty()
                    {
                        self.writeln("");
                    }

                    // Emit other items (fields, vftables)
                    for (idx, (comments, item)) in other_groups.iter().enumerate() {
                        for c in comments {
                            self.print_comment(c);
                        }
                        if let TypeDefItem::Statement(stmt) = item {
                            // Pass the next item for vftable blank-line logic
                            let next_item = other_groups.get(idx + 1).map(|(_, it)| *it);
                            self.print_type_statement(stmt, next_item);
                        }
                    }

                    // Emit any trailing comments (comments after the last statement)
                    for c in &pending_comments {
                        self.print_comment(c);
                    }

                    self.dedent();
                    self.write_indent();
                    writeln!(&mut self.output, "}}").unwrap();
                }
            }
            ItemDefinitionInner::Enum(ed) => {
                write!(&mut self.output, "enum {}: ", def.name).unwrap();
                self.print_type(&ed.type_);
                writeln!(&mut self.output, " {{").unwrap();
                self.indent();
                // Set binary literal width based on enum type
                let old_width = self.binary_literal_width;
                self.binary_literal_width = self.get_type_bit_width(&ed.type_);

                // Partition: const items first, then other items (variants, comments)
                let const_items: Vec<&EnumDefItem> = ed
                    .items
                    .iter()
                    .filter(|item| matches!(item, EnumDefItem::Item(inner) if is_value_item(&inner.inner)))
                    .collect();
                let other_items: Vec<&EnumDefItem> = ed
                    .items
                    .iter()
                    .filter(|item| !matches!(item, EnumDefItem::Item(inner) if is_value_item(&inner.inner)))
                    .collect();

                // Emit constants first
                for item in &const_items {
                    if let EnumDefItem::Item(inner) = item {
                        self.print_item_definition(inner, true);
                    }
                }

                // Blank line between constants and other items
                if !const_items.is_empty() && !other_items.is_empty() {
                    self.writeln("");
                }

                // Emit other items (variants, comments, non-const nested items)
                for item in &other_items {
                    match item {
                        EnumDefItem::Comment(comment) => {
                            self.print_comment(comment);
                        }
                        EnumDefItem::Statement(stmt) => {
                            self.print_enum_statement(stmt, None);
                        }
                        EnumDefItem::Item(inner) => {
                            self.print_item_definition(inner, true);
                        }
                    }
                }

                self.binary_literal_width = old_width;
                self.dedent();
                self.write_indent();
                writeln!(&mut self.output, "}}").unwrap();
            }
            ItemDefinitionInner::Bitflags(bf) => {
                write!(&mut self.output, "bitflags {}: ", def.name).unwrap();
                self.print_type(&bf.type_);
                writeln!(&mut self.output, " {{").unwrap();
                self.indent();
                // Set binary literal width based on bitflags type
                let old_width = self.binary_literal_width;
                self.binary_literal_width = self.get_type_bit_width(&bf.type_);

                // Partition: const items first, then other items (flags, comments)
                let const_items: Vec<&BitflagsDefItem> = bf
                    .items
                    .iter()
                    .filter(|item| matches!(item, BitflagsDefItem::Item(inner) if is_value_item(&inner.inner)))
                    .collect();
                let other_items: Vec<&BitflagsDefItem> = bf
                    .items
                    .iter()
                    .filter(|item| !matches!(item, BitflagsDefItem::Item(inner) if is_value_item(&inner.inner)))
                    .collect();

                // Emit constants first
                for item in &const_items {
                    if let BitflagsDefItem::Item(inner) = item {
                        self.print_item_definition(inner, true);
                    }
                }

                // Blank line between constants and other items
                if !const_items.is_empty() && !other_items.is_empty() {
                    self.writeln("");
                }

                // Emit other items (flags, comments, non-const nested items)
                for item in &other_items {
                    match item {
                        BitflagsDefItem::Comment(comment) => {
                            self.print_comment(comment);
                        }
                        BitflagsDefItem::Statement(stmt) => {
                            self.print_bitflags_statement(stmt, None);
                        }
                        BitflagsDefItem::Item(inner) => {
                            self.print_item_definition(inner, true);
                        }
                    }
                }

                self.binary_literal_width = old_width;
                self.dedent();
                self.write_indent();
                writeln!(&mut self.output, "}}").unwrap();
            }
            ItemDefinitionInner::TypeAlias(ta) => {
                write!(&mut self.output, "type {}{} = ", def.name, type_params).unwrap();
                self.print_type(&ta.target);
                let terminator = if nested { ',' } else { ';' };
                writeln!(&mut self.output, "{terminator}").unwrap();
            }
            ItemDefinitionInner::Constant(cd) => {
                write!(&mut self.output, "const {}: ", def.name).unwrap();
                self.print_type(&cd.type_);
                write!(&mut self.output, " = ").unwrap();
                self.print_expr(&cd.expr);
                let terminator = if nested { ',' } else { ';' };
                writeln!(&mut self.output, "{terminator}").unwrap();
            }
            ItemDefinitionInner::ExternValue(ev) => {
                write!(&mut self.output, "extern {}: ", def.name).unwrap();
                self.print_type(&ev.type_);
                let terminator = if nested { ',' } else { ';' };
                writeln!(&mut self.output, "{terminator}").unwrap();
            }
        }
    }

    fn print_type_statement(&mut self, stmt: &TypeStatement, next_item: Option<&TypeDefItem>) {
        // Add blank line before this statement if it has index/address attribute and it's not the first item
        // But don't add if we already have a blank line (e.g., from vftable)
        let has_index_or_address = stmt.attributes.0.iter().any(|attr| {
            matches!(attr, Attribute::Function { name, .. } if name.as_str() == "index" || name.as_str() == "address")
        });

        if has_index_or_address && !self.output.ends_with("{\n") && !self.output.ends_with("\n\n") {
            self.writeln("");
        }

        // Print doc comments (they already include the space after ///)
        for doc in &stmt.doc_comments {
            self.write_indent();
            writeln!(&mut self.output, "///{doc}").unwrap();
        }

        self.print_attributes(&stmt.attributes);

        match &stmt.field {
            TypeField::Field(vis, name, type_) => {
                self.write_indent();
                if *vis == Visibility::Public {
                    write!(&mut self.output, "pub ").unwrap();
                }
                write!(&mut self.output, "{name}: ").unwrap();
                self.print_type(type_);
                write!(&mut self.output, ",").unwrap();

                // Print inline trailing comments
                for comment in &stmt.inline_trailing_comments {
                    write!(&mut self.output, " ").unwrap();
                    self.print_comment_inline(comment);
                }

                writeln!(&mut self.output).unwrap();

                // Print following comments (comments on lines after the field)
                for comment in &stmt.following_comments {
                    self.print_comment(comment);
                }
            }
            TypeField::Vftable(funcs) => {
                self.write_indent();
                if funcs.is_empty() {
                    write!(&mut self.output, "vftable {{}},").unwrap();
                } else {
                    writeln!(&mut self.output, "vftable {{").unwrap();
                    self.indent();
                    for (i, func) in funcs.iter().enumerate() {
                        // Add blank line before function if it has index attribute and it's not the first
                        let has_index = func.attributes.0.iter().any(|attr| {
                            matches!(attr, Attribute::Function { name, .. } if name.as_str() == "index")
                        });
                        if has_index && i > 0 {
                            self.writeln("");
                        }
                        self.print_function(func);
                    }
                    self.dedent();
                    self.write_indent();
                    write!(&mut self.output, "}},").unwrap();
                }

                // Print inline trailing comments for vftable too
                for comment in &stmt.inline_trailing_comments {
                    write!(&mut self.output, " ").unwrap();
                    self.print_comment_inline(comment);
                }

                writeln!(&mut self.output).unwrap();

                // Print following comments (comments on lines after vftable)
                for comment in &stmt.following_comments {
                    self.print_comment(comment);
                }

                // Add blank line after vftable if there's a field following
                if let Some(TypeDefItem::Statement(_)) = next_item {
                    self.writeln("");
                }
            }
            TypeField::Item(inner_def) => {
                // print_item_definition does its own write_indent()
                self.print_item_definition(inner_def, true);
                // Add trailing comma after nested type/enum/bitflags (value items
                // like consts and extern values already include their own
                // terminator from print_item_definition)
                if !is_value_item(&inner_def.inner) {
                    // Replace the trailing newline after `}` with `},\n`
                    if self.output.ends_with("}\n") {
                        self.output.pop(); // remove \n
                        writeln!(&mut self.output, ",").unwrap();
                    }
                }
            }
        }
    }

    fn print_enum_statement(&mut self, stmt: &EnumStatement, _next_item: Option<&EnumDefItem>) {
        // Print doc comments (they already include the space after ///)
        for doc in &stmt.doc_comments {
            self.write_indent();
            writeln!(&mut self.output, "///{doc}").unwrap();
        }

        self.print_attributes(&stmt.attributes);
        self.write_indent();
        write!(&mut self.output, "{}", stmt.name).unwrap();
        if let Some(expr) = &stmt.expr {
            write!(&mut self.output, " = ").unwrap();
            self.print_expr(expr);
        }
        write!(&mut self.output, ",").unwrap();

        // Print inline trailing comments
        for comment in &stmt.inline_trailing_comments {
            write!(&mut self.output, " ").unwrap();
            self.print_comment_inline(comment);
        }

        writeln!(&mut self.output).unwrap();

        // Print following comments (comments on lines after the enum variant)
        for comment in &stmt.following_comments {
            self.print_comment(comment);
        }
    }

    fn print_bitflags_statement(
        &mut self,
        stmt: &BitflagsStatement,
        _next_item: Option<&BitflagsDefItem>,
    ) {
        // Print doc comments (they already include the space after ///)
        for doc in &stmt.doc_comments {
            self.write_indent();
            writeln!(&mut self.output, "///{doc}").unwrap();
        }

        self.print_attributes(&stmt.attributes);
        self.write_indent();
        write!(&mut self.output, "{} = ", stmt.name).unwrap();
        self.print_expr(&stmt.expr);
        write!(&mut self.output, ",").unwrap();

        // Print inline trailing comments
        for comment in &stmt.inline_trailing_comments {
            write!(&mut self.output, " ").unwrap();
            self.print_comment_inline(comment);
        }

        writeln!(&mut self.output).unwrap();

        // Print following comments (comments on lines after the bitflag)
        for comment in &stmt.following_comments {
            self.print_comment(comment);
        }
    }

    fn print_type(&mut self, type_: &Type) {
        match type_ {
            Type::Ident {
                path, generic_args, ..
            } => {
                write!(&mut self.output, "{path}").unwrap();
                if !generic_args.is_empty() {
                    write!(&mut self.output, "<").unwrap();
                    for (i, arg) in generic_args.iter().enumerate() {
                        if i > 0 {
                            write!(&mut self.output, ", ").unwrap();
                        }
                        self.print_type(arg);
                    }
                    write!(&mut self.output, ">").unwrap();
                }
            }
            Type::ConstPointer { pointee, .. } => {
                write!(&mut self.output, "*const ").unwrap();
                self.print_type(pointee);
            }
            Type::MutPointer { pointee, .. } => {
                write!(&mut self.output, "*mut ").unwrap();
                self.print_type(pointee);
            }
            Type::Array { element, size, .. } => {
                write!(&mut self.output, "[").unwrap();
                self.print_type(element);
                write!(&mut self.output, "; {size}]").unwrap();
            }
            Type::Unknown { size, .. } => {
                // Format unknown sizes as hex
                write!(&mut self.output, "unknown<0x{size:X}>").unwrap();
            }
        }
    }

    fn print_impl_block(&mut self, impl_block: &FunctionBlock) {
        self.print_attributes(&impl_block.attributes);
        self.write_indent();
        // Build the qualified name string: "Outer::Inner" for qualified impls,
        // or just "Foo" for simple impls.
        let name_str = if let Some(np) = &impl_block.name_path {
            let mut s = impl_block.name.as_str().to_string();
            for seg in np.iter() {
                s.push_str("::");
                s.push_str(seg.as_str());
            }
            s
        } else {
            impl_block.name.as_str().to_string()
        };
        if impl_block.type_parameters.is_empty() {
            writeln!(&mut self.output, "impl {name_str} {{").unwrap();
        } else {
            let params = impl_block
                .type_parameters
                .iter()
                .map(|tp| tp.name.as_str())
                .collect::<Vec<_>>()
                .join(", ");
            let args = impl_block
                .type_arguments
                .iter()
                .map(|tp| tp.name.as_str())
                .collect::<Vec<_>>()
                .join(", ");
            if args.is_empty() {
                writeln!(&mut self.output, "impl<{params}> {name_str} {{").unwrap();
            } else {
                writeln!(&mut self.output, "impl<{params}> {name_str}<{args}> {{",).unwrap();
            }
        }
        self.indent();

        for (i, item) in impl_block.items.iter().enumerate() {
            match item {
                ImplItem::Comment(comment) => {
                    self.print_comment(comment);
                }
                ImplItem::Function(func) => {
                    // Add blank line before function if it has address attribute and it's not the first
                    let has_address = func.attributes.0.iter().any(|attr| {
                        matches!(attr, Attribute::Function { name, .. } if name.as_str() == "address")
                    });
                    if has_address && i > 0 {
                        self.writeln("");
                    }
                    self.print_function(func);
                }
            }
        }

        self.dedent();
        self.write_indent();
        writeln!(&mut self.output, "}}").unwrap();
    }

    fn print_function(&mut self, func: &Function) {
        // Print doc comments (they already include the space after ///)
        for doc in &func.doc_comments {
            self.write_indent();
            writeln!(&mut self.output, "///{doc}").unwrap();
        }

        self.print_attributes(&func.attributes);
        self.write_indent();
        if func.visibility == Visibility::Public {
            write!(&mut self.output, "pub ").unwrap();
        }
        write!(&mut self.output, "fn {}(", func.name).unwrap();

        for (i, arg) in func.arguments.iter().enumerate() {
            if i > 0 {
                write!(&mut self.output, ", ").unwrap();
            }
            self.print_argument(arg);
        }

        write!(&mut self.output, ")").unwrap();

        if let Some(ret_type) = &func.return_type {
            write!(&mut self.output, " -> ").unwrap();
            self.print_type(ret_type);
        }

        writeln!(&mut self.output, ";").unwrap();
    }

    fn print_argument(&mut self, arg: &Argument) {
        match arg {
            Argument::Named { ident, type_, .. } => {
                write!(&mut self.output, "{ident}: ").unwrap();
                self.print_type(type_);
            }
            Argument::ConstSelf { .. } => write!(&mut self.output, "&self").unwrap(),
            Argument::MutSelf { .. } => write!(&mut self.output, "&mut self").unwrap(),
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
fn is_value_item(inner: &ItemDefinitionInner) -> bool {
    matches!(
        inner,
        ItemDefinitionInner::Constant(_) | ItemDefinitionInner::ExternValue(_)
    )
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;
    use crate::parser::parse_str_for_tests;

    #[test]
    fn test_pretty_print_basic() {
        let text = r#"
        pub type Test {
            field: i32,
        }
        "#;

        let expected = r#"
pub type Test {
    field: i32,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);
    }

    #[test]
    fn opaque_and_braced_empty_types_are_distinct() {
        // An opaque type keeps its `;`; an empty braced body keeps its braces.
        // Both must round-trip to themselves rather than collapsing together.
        for text in ["pub type Marker;", "pub type Marker {\n}"] {
            let module = parse_str_for_tests(text).unwrap();
            assert_eq!(pretty_print(&module), text);
        }
    }

    #[test]
    fn pub_use_reexport_round_trips() {
        // `pub use` (an explicit re-export) must survive a round-trip and stay
        // distinct from a plain `use`.
        let text = "pub use math::Vector3;\nuse math::Matrix4;";
        let module = parse_str_for_tests(text).unwrap();
        assert_eq!(pretty_print(&module), text);
    }

    #[test]
    fn test_pretty_print_module_inner_attributes() {
        // Module-level inner attributes (`#![...]`) must survive a round-trip
        // so `pyxis fmt` doesn't strip them.
        let text = r#"
        // a module-level inner attribute
        #![rust(example_flag)]

        pub type Foo {
            field: i32,
        }
        "#;

        let expected = r#"
// a module-level inner attribute
#![rust(example_flag)]

pub type Foo {
    field: i32,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);
    }

    #[test]
    fn test_pretty_print_with_comments() {
        let text = r#"
// This is a regular comment
/// This is a doc comment
pub type Test {
    // Field comment
    field1: i32,
    /// Doc comment for field2
    field2: bool,
}

#[singleton(0x1_18F_B64), size(0x40), align(16)] // 0x3C
pub type InputDeviceManager {
    #[address(0x18)]
    pub enabled: bool,
}
        "#;

        let output = r#"
// This is a regular comment
/// This is a doc comment
pub type Test {
    // Field comment
    field1: i32,
    /// Doc comment for field2
    field2: bool,
}

#[singleton(0x1_18F_B64), size(0x40), align(16)] // 0x3C
pub type InputDeviceManager {
    #[address(0x18)]
    pub enabled: bool,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        dbg!(&module);
        dbg!(&printed);

        assert_eq!(printed, output);
    }

    #[test]
    fn test_pretty_print_comments_in_attributes() {
        let text = r#"
#[singleton(0x1_18F_C20), size(0x620 /* actually 0x61C */), align(16)]
pub type AnarkGui {
    vftable {},

    #[address(0x1A0)]
    pub next_state: AnarkState,
    pub active_state: AnarkState,
}
        "#;

        let output = r#"
#[singleton(0x1_18F_C20), size(0x620 /* actually 0x61C */), align(16)]
pub type AnarkGui {
    vftable {},

    #[address(0x1A0)]
    pub next_state: AnarkState,
    pub active_state: AnarkState,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, output);
    }

    #[test]
    fn test_pretty_print_multiple_trailing_comments() {
        let text = r#"
#[size(0x10)] // size comment
// another comment
pub type MultiCommentTest {
    field: i32,
}

#[align(8)] /* block comment */
pub type BlockCommentTest {
    value: u64,
}
        "#;

        let expected = r#"
#[size(0x10)] // size comment
// another comment
pub type MultiCommentTest {
    field: i32,
}

#[align(8)] /* block comment */
pub type BlockCommentTest {
    value: u64,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str_for_tests(&printed).unwrap();
        let printed2 = pretty_print(&module2);

        assert_eq!(printed, printed2);
    }

    #[test]
    fn test_pretty_print_multiple_non_inline_trailing_comments() {
        let text = r#"
#[size(0x10)]
// size comment
// another comment
pub type MultiCommentTest {
    field: i32,
}

#[align(8)]
/* block comment */
pub type BlockCommentTest {
    value: u64,
}
        "#;

        let expected = r#"
#[size(0x10)]
// size comment
// another comment
pub type MultiCommentTest {
    field: i32,
}

#[align(8)]
/* block comment */
pub type BlockCommentTest {
    value: u64,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str_for_tests(&printed).unwrap();
        let printed2 = pretty_print(&module2);

        assert_eq!(printed, printed2);
    }

    #[test]
    fn test_pretty_print_enum_with_trailing_comments() {
        let text = r#"
#[repr(u32)] // enum representation
pub enum State: u32 {
    Idle = 0,
    // State comment
    Active = 1,
    Done = 2,
}
        "#;

        let expected = r#"
#[repr(u32)] // enum representation
pub enum State: u32 {
    Idle = 0,
    // State comment
    Active = 1,
    Done = 2,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str_for_tests(&printed).unwrap();
        let printed2 = pretty_print(&module2);

        assert_eq!(printed, printed2);
    }

    #[test]
    fn test_pretty_print_bitflags_with_trailing_comments() {
        let text = r#"
#[repr(u32)] // flags representation
pub bitflags Flags: u32 {
    // Flag comment
    READ = 0x1,
    WRITE = 0x2,
    EXECUTE = 0x4,
}
        "#;

        let expected = r#"
#[repr(u32)] // flags representation
pub bitflags Flags: u32 {
    // Flag comment
    READ = 0x1,
    WRITE = 0x2,
    EXECUTE = 0x4,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str_for_tests(&printed).unwrap();
        let printed2 = pretty_print(&module2);

        assert_eq!(printed, printed2);
    }

    #[test]
    fn test_pretty_print_mixed_comments() {
        let text = r#"
// Module level comment

/// Documentation for Foo
#[size(0x20)] // Foo size
pub type Foo {
    // Field comment
    /// Field documentation
    field1: i32,
    field2: bool, // inline field comment
}

// Separator comment

/// Documentation for Bar
#[align(16)] /* alignment */
pub type Bar {
    value: u64,
}
        "#;

        let expected = r#"
// Module level comment

/// Documentation for Foo
#[size(0x20)] // Foo size
pub type Foo {
    // Field comment
    /// Field documentation
    field1: i32,
    field2: bool, // inline field comment
}

// Separator comment

/// Documentation for Bar
#[align(16)] /* alignment */
pub type Bar {
    value: u64,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str_for_tests(&printed).unwrap();
        let printed2 = pretty_print(&module2);

        assert_eq!(printed, printed2);
    }

    #[test]
    fn test_pretty_print_no_attributes_with_comments() {
        let text = r#"
// Comment before definition
pub type SimpleType {
    field: i32,
}
        "#;

        let expected = r#"
// Comment before definition
pub type SimpleType {
    field: i32,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str_for_tests(&printed).unwrap();
        let printed2 = pretty_print(&module2);

        assert_eq!(printed, printed2);
    }

    #[test]
    fn test_pretty_print_comment_before_definition_with_doc_and_attributes() {
        // A plain comment immediately preceding a definition should stay
        // attached to it; the formatter must not insert a blank line between
        // them, even when the definition has both doc comments and attributes.
        let text = r#"
// Plain comment
/// Doc comment
#[size(0x4)]
pub type Foo {
    field: i32,
}
        "#;

        let expected = r#"
// Plain comment
/// Doc comment
#[size(0x4)]
pub type Foo {
    field: i32,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str_for_tests(&printed).unwrap();
        let printed2 = pretty_print(&module2);

        assert_eq!(printed, printed2);
    }

    #[test]
    fn test_pretty_print_doc_comment_on_freestanding_function() {
        let text = r#"
/// Doc comment for a freestanding function
#[address(0x123)]
pub fn test();
        "#;

        let expected = r#"
/// Doc comment for a freestanding function
#[address(0x123)]
pub fn test();
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str_for_tests(&printed).unwrap();
        let printed2 = pretty_print(&module2);

        assert_eq!(printed, printed2);
    }

    #[test]
    fn test_pretty_print_comment_on_separate_line_after_attributes() {
        let text = r#"
#[size(0x10)]
// First comment on separate line
// Second comment on separate line
pub type Foo {
    field: i32,
}
        "#;

        let expected = r#"
#[size(0x10)]
// First comment on separate line
// Second comment on separate line
pub type Foo {
    field: i32,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str_for_tests(&printed).unwrap();
        let printed2 = pretty_print(&module2);

        assert_eq!(printed, printed2);
    }

    #[test]
    fn test_pretty_print_multiple_inline_comments() {
        let text = r#"
#[size(0x10)] // comment1 // comment2
pub type Foo {
    field: i32,
}
        "#;

        let expected = r#"
#[size(0x10)] // comment1 // comment2
pub type Foo {
    field: i32,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str_for_tests(&printed).unwrap();
        let printed2 = pretty_print(&module2);

        assert_eq!(printed, printed2);
    }

    #[test]
    fn test_pretty_print_complex_comment_layout() {
        let text = r#"
#[size(0x20)] // inline comment
// separate line comment 1
// separate line comment 2
pub type ComplexLayout {
    field1: i32,
    field2: bool, // field comment
}

#[align(8)] /* block */ /* another block */
pub type MultipleBlocks {
    value: u64,
}
        "#;

        let expected = r#"
#[size(0x20)] // inline comment
// separate line comment 1
// separate line comment 2
pub type ComplexLayout {
    field1: i32,
    field2: bool, // field comment
}

#[align(8)] /* block */ /* another block */
pub type MultipleBlocks {
    value: u64,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str_for_tests(&printed).unwrap();
        let printed2 = pretty_print(&module2);

        assert_eq!(printed, printed2);
    }

    #[test]
    fn test_multiline_regular_splice_becomes_raw() {
        // A multi-line splice body is a code block: it renders as a raw string
        // across real lines rather than a single-line `"\n...\n"` escape soup,
        // regardless of whether the source used a regular or raw literal.
        let text = r#"
#[cfg(backend = "rust")]
prologue "\n    use crate::shared_ptr::*;\n    use std::mem::ManuallyDrop;\n";
        "#;

        let expected = r##"#[cfg(backend = "rust")]
prologue r#"
    use crate::shared_ptr::*;
    use std::mem::ManuallyDrop;
"#;"##;

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);
    }

    #[test]
    fn test_single_line_regular_splice_stays_regular() {
        // A single-line body has no newline to lay out, so it keeps its
        // regular-string form.
        let text = r#"
#[cfg(backend = "rust")]
epilogue "pub const K: u32 = 1;";
        "#;

        let expected = r#"#[cfg(backend = "rust")]
epilogue "pub const K: u32 = 1;";"#;

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);
    }

    #[test]
    fn test_preserve_raw_string_format_in_splice() {
        let text = r##"
#[cfg(backend = "rust")]
prologue r#"
    use crate::shared_ptr::*;
    use std::mem::ManuallyDrop;
"#;
        "##;

        let expected = r##"
#[cfg(backend = "rust")]
prologue r#"
    use crate::shared_ptr::*;
    use std::mem::ManuallyDrop;
"#;
        "##
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);
    }

    #[test]
    fn test_preserve_module_doc_comments() {
        let text = r#"
//! This is a render block.

#[size(8), align(4)]
pub type RenderBlock {
    field: u32,
}
        "#;

        let expected = r#"
//! This is a render block.

#[size(8), align(4)]
pub type RenderBlock {
    field: u32,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);
    }

    #[test]
    fn test_preserve_extern_type_doc_comments() {
        let text = r#"
/// `ManuallyDrop<SharedPtr<u32>>` is used instead of `SharedPtr<u32>` to avoid
/// the `Drop` implementation of `SharedPtr<u32>` being called when the `RenderBlock`
/// is dropped. The destructor, which we call in `drop`, will decrement the refcount
/// for us.
#[size(8), align(4)]
extern type ManuallyDrop<SharedPtr<u32>>;
        "#;

        let expected = r#"
/// `ManuallyDrop<SharedPtr<u32>>` is used instead of `SharedPtr<u32>` to avoid
/// the `Drop` implementation of `SharedPtr<u32>` being called when the `RenderBlock`
/// is dropped. The destructor, which we call in `drop`, will decrement the refcount
/// for us.
#[size(8), align(4)]
extern type ManuallyDrop<SharedPtr<u32>>;
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);
    }

    #[test]
    fn test_binary_literal_formatting_u8() {
        let text = r#"
#[copyable]
pub bitflags CameraState: u8 {
    m_UseOffCenter = 0b0000_0001,
    m_ScreenshotSeriesRunning = 0b0000_0010,
    m_Ortho = 0b0000_0100,
    m_ComputeView = 0b0000_1000,
    m_DirtyProjection = 0b0001_0000,
    m_IsRenderCamera = 0b0010_0000,
}
        "#;

        let expected = r#"
#[copyable]
pub bitflags CameraState: u8 {
    m_UseOffCenter = 0b0000_0001,
    m_ScreenshotSeriesRunning = 0b0000_0010,
    m_Ortho = 0b0000_0100,
    m_ComputeView = 0b0000_1000,
    m_DirtyProjection = 0b0001_0000,
    m_IsRenderCamera = 0b0010_0000,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);
    }

    #[test]
    fn test_binary_literal_formatting_u32() {
        let text = r#"
pub bitflags TestFlags: u32 {
    FLAG_1 = 0b0000_0000_0000_0000_0000_0000_0000_0001,
    FLAG_2 = 0b0000_0000_0000_0000_0000_0000_0000_0010,
    FLAG_BIG = 0b1000_0000_0000_0000_0000_0000_0000_0000,
}
        "#;

        let expected = r#"
pub bitflags TestFlags: u32 {
    FLAG_1 = 0b0000_0000_0000_0000_0000_0000_0000_0001,
    FLAG_2 = 0b0000_0000_0000_0000_0000_0000_0000_0010,
    FLAG_BIG = 0b1000_0000_0000_0000_0000_0000_0000_0000,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);
    }

    #[test]
    fn test_pinned_attribute_round_trips() {
        let text = r#"
#[pinned]
pub type PinnedType {
    pub value: u32,
}
        "#;

        let expected = r#"
#[pinned]
pub type PinnedType {
    pub value: u32,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);
    }

    #[test]
    fn test_nested_enum_only_no_blank() {
        let text = r#"
        pub type Outer {
            pub enum InnerEnum: u8 {
                A,
                B,
            }
        }
        "#;

        let expected = r#"
pub type Outer {
    pub enum InnerEnum: u8 {
        A,
        B,
    },
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);
    }

    #[test]
    fn test_field_only_no_blank() {
        let text = r#"
        pub type Outer {
            pub field: u32,
        }
        "#;

        let expected = r#"
pub type Outer {
    pub field: u32,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);
    }

    #[test]
    fn test_nested_and_field_one_blank() {
        let text = r#"
        pub type Outer {
            pub field: u32,
            pub enum InnerEnum: u8 {
                A,
                B,
            }
        }
        "#;

        // Nested items should be reordered first, with a blank line between
        // the nested item group and the field group.
        let expected = r#"
pub type Outer {
    pub enum InnerEnum: u8 {
        A,
        B,
    },

    pub field: u32,
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);
    }

    #[test]
    fn test_nested_round_trip() {
        let text = r#"
pub type Outer {
    pub field: u32,
    pub enum InnerEnum: u8 {
        A,
        B,
    }
    pub type InnerType {
        pub inner_field: u16,
    }
}
        "#
        .trim();

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);
        // Round-trip: parse the printed output and print again
        let module2 = parse_str_for_tests(&printed).unwrap();
        let printed2 = pretty_print(&module2);

        assert_eq!(printed, printed2);
    }

    #[test]
    fn new_const_forms_round_trip() {
        // C-string literals, array literals, and const aliases must all
        // survive a round-trip through pretty-print.
        let text = "pub const DLL_NAME: cstr = c\"kernel32.dll\";\n\
pub const ARR: [i32; 3] = [1, 2, 3];\n\
pub const ALIAS: i32 = ARR;";

        let module = parse_str_for_tests(text).unwrap();
        let printed = pretty_print(&module);
        let module2 = parse_str_for_tests(&printed).unwrap();
        let printed2 = pretty_print(&module2);

        assert_eq!(printed, printed2);
    }
}
