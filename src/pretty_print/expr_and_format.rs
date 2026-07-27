use super::PrettyPrinter;
use crate::grammar::*;
use std::fmt::Write;

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

    pub(super) fn print_type(&mut self, type_: &Type) {
        // Type-position attributes print inline, ahead of the type they
        // annotate: `#[calling_convention(cdecl)] fn()`.
        if !type_.attributes.0.is_empty() {
            write!(&mut self.output, "#[").unwrap();
            for (i, attr) in type_.attributes.0.iter().enumerate() {
                if i > 0 {
                    write!(&mut self.output, ", ").unwrap();
                }
                self.print_attribute(attr);
            }
            write!(&mut self.output, "] ").unwrap();
        }

        match &type_.kind {
            TypeKind::Ident {
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
            TypeKind::ConstPointer { pointee, .. } => {
                write!(&mut self.output, "*const ").unwrap();
                self.print_type(pointee);
            }
            TypeKind::MutPointer { pointee, .. } => {
                write!(&mut self.output, "*mut ").unwrap();
                self.print_type(pointee);
            }
            TypeKind::Array { element, size, .. } => {
                write!(&mut self.output, "[").unwrap();
                self.print_type(element);
                write!(&mut self.output, "; {size}]").unwrap();
            }
            TypeKind::Unknown { size, .. } => {
                // Format unknown sizes as hex
                write!(&mut self.output, "unknown<0x{size:X}>").unwrap();
            }
            TypeKind::Function {
                arguments,
                return_type,
            } => {
                write!(&mut self.output, "fn(").unwrap();
                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        write!(&mut self.output, ", ").unwrap();
                    }
                    if let Some(name) = &arg.name {
                        write!(&mut self.output, "{name}: ").unwrap();
                    }
                    self.print_type(&arg.type_);
                }
                write!(&mut self.output, ")").unwrap();
                if let Some(return_type) = return_type {
                    write!(&mut self.output, " -> ").unwrap();
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

    pub(super) fn print_function(&mut self, func: &Function) {
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
