use super::PrettyPrinter;
use crate::{grammar::*, infallible_write, infallible_writeln};

impl PrettyPrinter {
    /// Format a hex number with underscores every 3 digits from the right
    /// e.g., 0x142ED0E78 -> 0x142_ED0_E78
    pub(super) fn format_hex_with_underscores(&self, val: isize) -> String {
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
    pub(super) fn get_type_bit_width(&self, type_: &Type) -> Option<usize> {
        if let TypeKind::Ident { path, .. } = &type_.kind {
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

    pub(super) fn print_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntLiteral { value, format, .. } => match format {
                IntFormat::Hex => infallible_write!(&mut self.output, "0x{value:X}"),
                IntFormat::Binary => {
                    let formatted = self.format_binary_with_padding(*value);
                    infallible_write!(&mut self.output, "{formatted}");
                }
                IntFormat::Octal => infallible_write!(&mut self.output, "0o{value:o}"),
                IntFormat::Decimal => infallible_write!(&mut self.output, "{value}"),
            },
            Expr::StringLiteral { value, format, .. } => {
                match format {
                    StringFormat::Raw => {
                        // Determine the number of # needed
                        let hash_count = self.count_hashes_needed(value);
                        let hashes = "#".repeat(hash_count);
                        infallible_write!(&mut self.output, "r{hashes}\"{value}\"{hashes}");
                    }
                    StringFormat::Regular => {
                        // Escape special characters for regular strings
                        infallible_write!(&mut self.output, "\"");
                        for ch in value.chars() {
                            match ch {
                                '"' => infallible_write!(&mut self.output, "\\\""),
                                '\\' => infallible_write!(&mut self.output, "\\\\"),
                                '\n' => infallible_write!(&mut self.output, "\\n"),
                                '\r' => infallible_write!(&mut self.output, "\\r"),
                                '\t' => infallible_write!(&mut self.output, "\\t"),
                                _ => infallible_write!(&mut self.output, "{ch}"),
                            }
                        }
                        infallible_write!(&mut self.output, "\"");
                    }
                }
            }
            Expr::CStringLiteral { value, format, .. } => match format {
                StringFormat::Raw => {
                    let hash_count = self.count_hashes_needed(value);
                    let hashes = "#".repeat(hash_count);
                    infallible_write!(&mut self.output, "cr{hashes}\"{value}\"{hashes}");
                }
                StringFormat::Regular => {
                    infallible_write!(&mut self.output, "c\"");
                    for ch in value.chars() {
                        match ch {
                            '"' => infallible_write!(&mut self.output, "\\\""),
                            '\\' => infallible_write!(&mut self.output, "\\\\"),
                            '\n' => infallible_write!(&mut self.output, "\\n"),
                            '\r' => infallible_write!(&mut self.output, "\\r"),
                            '\t' => infallible_write!(&mut self.output, "\\t"),
                            _ => infallible_write!(&mut self.output, "{ch}"),
                        }
                    }
                    infallible_write!(&mut self.output, "\"");
                }
            },
            Expr::Ident { ident, .. } => infallible_write!(&mut self.output, "{ident}"),
            Expr::FloatLiteral { raw_text, .. } => {
                infallible_write!(&mut self.output, "{raw_text}");
            }
            Expr::Path { path, .. } => {
                infallible_write!(&mut self.output, "{path}");
            }
            Expr::StructLiteral {
                type_name, fields, ..
            } => {
                infallible_write!(&mut self.output, "{type_name} {{ ");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        infallible_write!(&mut self.output, ", ");
                    }
                    infallible_write!(&mut self.output, "{}: ", field.ident());
                    self.print_expr(&field.1);
                }
                infallible_write!(&mut self.output, " }}");
            }
            Expr::ArrayLiteral { elements, .. } => {
                infallible_write!(&mut self.output, "[");
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        infallible_write!(&mut self.output, ", ");
                    }
                    self.print_expr(elem);
                }
                infallible_write!(&mut self.output, "]");
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

    pub(super) fn format_type_parameters(&self, type_parameters: &[TypeParameter]) -> String {
        if type_parameters.is_empty() {
            String::new()
        } else {
            let params: Vec<&str> = type_parameters.iter().map(|p| p.name.as_str()).collect();
            format!("<{}>", params.join(", "))
        }
    }

    /// Format a UseTree for pretty printing
    pub(super) fn format_use_tree(&self, tree: &UseTree) -> String {
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

    pub(super) fn print_splice(&mut self, splice: &Splice) {
        // Leading `#[cfg(...)]` (or any) attributes, one per line.
        self.print_attributes(&splice.attributes);
        self.write_indent();
        // Splices are code blocks: render any multi-line body as a raw string
        // so it lays out across real lines instead of a single-line `"\n...\n"`
        // escape soup. Single-line bodies keep their original format.
        let format = if splice.text.contains('\n') {
            StringFormat::Raw
        } else {
            splice.format
        };
        let s = self.format_string_with_format(&splice.text, format);
        let m = self.splice_modifiers(splice.definition, splice.for_type.as_ref());
        let kw = splice.kind.keyword();
        infallible_writeln!(&mut self.output, "{kw}{m} {s};");
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

    pub(super) fn print_type(&mut self, type_: &Type) {
        // Type-position attributes print inline, ahead of the type they
        // annotate: `#[calling_convention(cdecl)] fn()`.
        if !type_.attributes.0.is_empty() {
            infallible_write!(&mut self.output, "#[");
            for (i, attr) in type_.attributes.0.iter().enumerate() {
                if i > 0 {
                    infallible_write!(&mut self.output, ", ");
                }
                self.print_attribute(attr);
            }
            infallible_write!(&mut self.output, "] ");
        }

        match &type_.kind {
            TypeKind::Ident {
                path, generic_args, ..
            } => {
                infallible_write!(&mut self.output, "{path}");
                if !generic_args.is_empty() {
                    infallible_write!(&mut self.output, "<");
                    for (i, arg) in generic_args.iter().enumerate() {
                        if i > 0 {
                            infallible_write!(&mut self.output, ", ");
                        }
                        self.print_type(arg);
                    }
                    infallible_write!(&mut self.output, ">");
                }
            }
            TypeKind::ConstPointer { pointee, .. } => {
                infallible_write!(&mut self.output, "*const ");
                self.print_type(pointee);
            }
            TypeKind::MutPointer { pointee, .. } => {
                infallible_write!(&mut self.output, "*mut ");
                self.print_type(pointee);
            }
            TypeKind::Array { element, size, .. } => {
                infallible_write!(&mut self.output, "[");
                self.print_type(element);
                infallible_write!(&mut self.output, "; {size}]");
            }
            TypeKind::Unknown { size, .. } => {
                // Format unknown sizes as hex
                infallible_write!(&mut self.output, "unknown<0x{size:X}>");
            }
            TypeKind::Function {
                arguments,
                return_type,
            } => {
                infallible_write!(&mut self.output, "fn(");
                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        infallible_write!(&mut self.output, ", ");
                    }
                    if let Some(name) = &arg.name {
                        infallible_write!(&mut self.output, "{name}: ");
                    }
                    self.print_type(&arg.type_);
                }
                infallible_write!(&mut self.output, ")");
                if let Some(return_type) = return_type {
                    infallible_write!(&mut self.output, " -> ");
                    self.print_type(return_type);
                }
            }
        }
    }

    pub(super) fn print_impl_block(&mut self, impl_block: &FunctionBlock) {
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
            infallible_writeln!(&mut self.output, "impl {name_str} {{");
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
                infallible_writeln!(&mut self.output, "impl<{params}> {name_str} {{");
            } else {
                infallible_writeln!(&mut self.output, "impl<{params}> {name_str}<{args}> {{",);
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
        infallible_writeln!(&mut self.output, "}}");
    }

    pub(super) fn print_function(&mut self, func: &Function) {
        // Print doc comments (they already include the space after ///)
        for doc in &func.doc_comments {
            self.write_indent();
            infallible_writeln!(&mut self.output, "///{doc}");
        }

        self.print_attributes(&func.attributes);
        self.write_indent();
        if func.visibility == Visibility::Public {
            infallible_write!(&mut self.output, "pub ");
        }
        infallible_write!(&mut self.output, "fn {}(", func.name);

        for (i, arg) in func.arguments.iter().enumerate() {
            if i > 0 {
                infallible_write!(&mut self.output, ", ");
            }
            self.print_argument(arg);
        }

        infallible_write!(&mut self.output, ")");

        if let Some(ret_type) = &func.return_type {
            infallible_write!(&mut self.output, " -> ");
            self.print_type(ret_type);
        }

        infallible_writeln!(&mut self.output, ";");
    }

    fn print_argument(&mut self, arg: &Argument) {
        match arg {
            Argument::Named { ident, type_, .. } => {
                infallible_write!(&mut self.output, "{ident}: ");
                self.print_type(type_);
            }
            Argument::ConstSelf { .. } => infallible_write!(&mut self.output, "&self"),
            Argument::MutSelf { .. } => infallible_write!(&mut self.output, "&mut self"),
        }
    }
}
