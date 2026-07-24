use super::{PrettyPrinter, is_value_item};
use crate::grammar::{ItemDefinitionInner, *};
use std::fmt::Write;

impl PrettyPrinter {
    pub(super) fn print_item_definition(&mut self, def: &ItemDefinition, nested: bool) {
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
}
