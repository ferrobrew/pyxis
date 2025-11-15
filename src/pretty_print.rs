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
}

impl PrettyPrinter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            indent_string: "    ".to_string(), // 4 spaces
        }
    }

    pub fn with_indent(indent: &str) -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            indent_string: indent.to_string(),
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
        // Print module-level attributes
        self.print_attributes(&module.attributes);

        // Print items with lookahead for proper spacing
        for (i, item) in module.items.iter().enumerate() {
            self.print_module_item(item);

            // Add blank line between regular comment and definition with both doc comments and attributes
            if let ModuleItem::Comment(_comment) = item {
                if let Some(next_item) = module.items.get(i + 1) {
                    if let ModuleItem::Definition(def) = next_item {
                        // Add blank line if definition has both doc comments and attributes
                        let has_attrs = match &def.inner {
                            ItemDefinitionInner::Type(td) => !td.attributes.0.is_empty(),
                            ItemDefinitionInner::Enum(ed) => !ed.attributes.0.is_empty(),
                            ItemDefinitionInner::Bitflags(bf) => !bf.attributes.0.is_empty(),
                        };

                        if !def.doc_comments.is_empty() && has_attrs {
                            self.writeln("");
                        }
                    }
                }
            }
        }

        self.output.trim().to_string()
    }

    fn print_module_item(&mut self, item: &ModuleItem) {
        match item {
            ModuleItem::Comment(comment) => {
                self.print_comment(&comment.value);
                // Don't add blank line after comments - they group with the following item
            }
            ModuleItem::Use(path) => {
                self.write_indent();
                let path_str = self.format_item_path(path);
                writeln!(&mut self.output, "use {path_str};").unwrap();
                self.writeln("");
            }
            ModuleItem::ExternType(name, attrs, _doc) => {
                self.print_attributes(attrs);
                self.write_indent();
                writeln!(&mut self.output, "extern type {name};").unwrap();
                self.writeln("");
            }
            ModuleItem::Backend(backend) => {
                self.print_backend(backend);
                self.writeln("");
            }
            ModuleItem::Definition(def) => {
                self.print_item_definition(def);
                self.writeln("");
            }
            ModuleItem::Impl(impl_block) => {
                self.print_impl_block(impl_block);
                self.writeln("");
            }
            ModuleItem::ExternValue(extern_val) => {
                self.print_extern_value(extern_val);
                self.writeln("");
            }
            ModuleItem::Function(func) => {
                self.print_function(func);
                self.writeln("");
            }
        }
    }

    fn print_comment(&mut self, comment: &Comment) {
        match comment {
            Comment::DocOuter(lines) => {
                for line in lines {
                    self.write_indent();
                    writeln!(&mut self.output, "/// {line}").unwrap();
                }
            }
            Comment::DocInner(lines) => {
                for line in lines {
                    self.write_indent();
                    writeln!(&mut self.output, "//! {line}").unwrap();
                }
            }
            Comment::Regular(text) => {
                // Regular comments include the // prefix
                self.write_indent();
                writeln!(&mut self.output, "{text}").unwrap();
            }
            Comment::MultiLine(lines) => {
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
            Comment::Regular(text) => {
                // Regular comments include the // prefix
                write!(&mut self.output, "{text}").unwrap();
            }
            Comment::MultiLine(lines) => {
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
        if attrs.0.is_empty() {
            return;
        }

        self.write_indent();
        write!(&mut self.output, "#[").unwrap();
        for (i, attr) in attrs.0.iter().enumerate() {
            if i > 0 {
                write!(&mut self.output, ", ").unwrap();
            }
            self.print_attribute(attr);
        }
        writeln!(&mut self.output, "]").unwrap();
    }

    fn print_attribute(&mut self, attr: &Attribute) {
        match attr {
            Attribute::Ident(name) => {
                write!(&mut self.output, "{}", name).unwrap();
            }
            Attribute::Function(name, items) => {
                write!(&mut self.output, "{}(", name).unwrap();
                let mut first_expr = true;
                for item in items {
                    match item {
                        AttributeItem::Expr(expr) => {
                            if !first_expr {
                                write!(&mut self.output, ", ").unwrap();
                            }
                            first_expr = false;
                            self.print_expr(expr);
                        }
                        AttributeItem::Comment(comment) => {
                            write!(&mut self.output, " {}", comment).unwrap();
                        }
                    }
                }
                write!(&mut self.output, ")").unwrap();
            }
            Attribute::Assign(name, items) => {
                write!(&mut self.output, "{} = ", name).unwrap();
                for item in items {
                    match item {
                        AttributeItem::Expr(expr) => {
                            self.print_expr(expr);
                        }
                        AttributeItem::Comment(comment) => {
                            write!(&mut self.output, " {}", comment).unwrap();
                        }
                    }
                }
            }
        }
    }

    fn print_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntLiteral(val) => write!(&mut self.output, "0x{val:X}").unwrap(),
            Expr::StringLiteral(s) => write!(&mut self.output, "\"{s}\"").unwrap(),
            Expr::Ident(name) => write!(&mut self.output, "{name}").unwrap(),
        }
    }

    fn format_item_path(&self, path: &ItemPath) -> String {
        path.iter()
            .map(|seg| seg.as_str())
            .collect::<Vec<_>>()
            .join("::")
    }

    fn print_backend(&mut self, backend: &Backend) {
        self.write_indent();
        writeln!(&mut self.output, "backend {} {{", backend.name).unwrap();
        self.indent();

        if let Some(prologue) = &backend.prologue {
            self.write_indent();
            writeln!(&mut self.output, "prologue r#\"{prologue}\"#;").unwrap();
        }

        if let Some(epilogue) = &backend.epilogue {
            self.write_indent();
            writeln!(&mut self.output, "epilogue r#\"{epilogue}\"#;").unwrap();
        }

        self.dedent();
        self.write_indent();
        writeln!(&mut self.output, "}}").unwrap();
    }

    fn print_item_definition(&mut self, def: &ItemDefinition) {
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
                self.print_comment_inline(&comment.value);
            }

            writeln!(&mut self.output).unwrap();
        }

        // Print following comments (comments on lines after attributes)
        for comment in following_comments {
            self.print_comment(&comment.value);
        }

        self.write_indent();
        if def.visibility == Visibility::Public {
            write!(&mut self.output, "pub ").unwrap();
        }

        match &def.inner {
            ItemDefinitionInner::Type(td) => {
                writeln!(&mut self.output, "type {} {{", def.name).unwrap();
                self.indent();
                for item in &td.items {
                    match item {
                        TypeDefItem::Comment(comment) => {
                            self.print_comment(&comment.value);
                        }
                        TypeDefItem::Statement(stmt) => {
                            self.print_type_statement(stmt);
                        }
                    }
                }
                self.dedent();
                self.write_indent();
                writeln!(&mut self.output, "}}").unwrap();
            }
            ItemDefinitionInner::Enum(ed) => {
                write!(&mut self.output, "enum {}: ", def.name).unwrap();
                self.print_type(&ed.type_);
                writeln!(&mut self.output, " {{").unwrap();
                self.indent();
                for item in &ed.items {
                    match item {
                        EnumDefItem::Comment(comment) => {
                            self.print_comment(&comment.value);
                        }
                        EnumDefItem::Statement(stmt) => {
                            self.print_enum_statement(stmt);
                        }
                    }
                }
                self.dedent();
                self.write_indent();
                writeln!(&mut self.output, "}}").unwrap();
            }
            ItemDefinitionInner::Bitflags(bf) => {
                write!(&mut self.output, "bitflags {}: ", def.name).unwrap();
                self.print_type(&bf.type_);
                writeln!(&mut self.output, " {{").unwrap();
                self.indent();
                for item in &bf.items {
                    match item {
                        BitflagsDefItem::Comment(comment) => {
                            self.print_comment(&comment.value);
                        }
                        BitflagsDefItem::Statement(stmt) => {
                            self.print_bitflags_statement(stmt);
                        }
                    }
                }
                self.dedent();
                self.write_indent();
                writeln!(&mut self.output, "}}").unwrap();
            }
        }
    }

    fn print_type_statement(&mut self, stmt: &TypeStatement) {
        // Print doc comments (they already include the space after ///)
        for doc in &stmt.doc_comments {
            self.write_indent();
            writeln!(&mut self.output, "///{doc}").unwrap();
        }

        self.print_attributes(&stmt.attributes);
        self.write_indent();

        match &stmt.field {
            TypeField::Field(vis, name, type_) => {
                if *vis == Visibility::Public {
                    write!(&mut self.output, "pub ").unwrap();
                }
                write!(&mut self.output, "{name}: ").unwrap();
                self.print_type(type_);
                write!(&mut self.output, ",").unwrap();

                // Print inline trailing comments
                for comment in &stmt.inline_trailing_comments {
                    write!(&mut self.output, " ").unwrap();
                    self.print_comment_inline(&comment.value);
                }

                writeln!(&mut self.output).unwrap();

                // Print following comments (comments on lines after the field)
                for comment in &stmt.following_comments {
                    self.print_comment(&comment.value);
                }
            }
            TypeField::Vftable(funcs) => {
                if funcs.is_empty() {
                    write!(&mut self.output, "vftable {{}},").unwrap();
                } else {
                    writeln!(&mut self.output, "vftable {{").unwrap();
                    self.indent();
                    for func in funcs {
                        self.print_function(func);
                    }
                    self.dedent();
                    self.write_indent();
                    write!(&mut self.output, "}},").unwrap();
                }

                // Print inline trailing comments for vftable too
                for comment in &stmt.inline_trailing_comments {
                    write!(&mut self.output, " ").unwrap();
                    self.print_comment_inline(&comment.value);
                }

                writeln!(&mut self.output).unwrap();

                // Print following comments (comments on lines after vftable)
                for comment in &stmt.following_comments {
                    self.print_comment(&comment.value);
                }
            }
        }
    }

    fn print_enum_statement(&mut self, stmt: &EnumStatement) {
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
            self.print_comment_inline(&comment.value);
        }

        writeln!(&mut self.output).unwrap();

        // Print following comments (comments on lines after the enum variant)
        for comment in &stmt.following_comments {
            self.print_comment(&comment.value);
        }
    }

    fn print_bitflags_statement(&mut self, stmt: &BitflagsStatement) {
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
            self.print_comment_inline(&comment.value);
        }

        writeln!(&mut self.output).unwrap();

        // Print following comments (comments on lines after the bitflag)
        for comment in &stmt.following_comments {
            self.print_comment(&comment.value);
        }
    }

    fn print_type(&mut self, type_: &Type) {
        match type_ {
            Type::Ident(name, _) => write!(&mut self.output, "{name}").unwrap(),
            Type::ConstPointer(inner) => {
                write!(&mut self.output, "*const ").unwrap();
                self.print_type(inner);
            }
            Type::MutPointer(inner) => {
                write!(&mut self.output, "*mut ").unwrap();
                self.print_type(inner);
            }
            Type::Array(inner, size) => {
                write!(&mut self.output, "[").unwrap();
                self.print_type(inner);
                write!(&mut self.output, "; {size}]").unwrap();
            }
            Type::Unknown(size) => {
                write!(&mut self.output, "unknown<{size}>").unwrap();
            }
        }
    }

    fn print_impl_block(&mut self, impl_block: &FunctionBlock) {
        self.print_attributes(&impl_block.attributes);
        self.write_indent();
        writeln!(&mut self.output, "impl {} {{", impl_block.name).unwrap();
        self.indent();

        for item in &impl_block.items {
            match item {
                ImplItem::Comment(comment) => {
                    self.print_comment(&comment.value);
                }
                ImplItem::Function(func) => {
                    self.print_function(func);
                }
            }
        }

        self.dedent();
        self.write_indent();
        writeln!(&mut self.output, "}}").unwrap();
    }

    fn print_extern_value(&mut self, extern_val: &ExternValue) {
        self.print_attributes(&extern_val.attributes);
        self.write_indent();
        if extern_val.visibility == Visibility::Public {
            write!(&mut self.output, "pub ").unwrap();
        }
        write!(&mut self.output, "extern {}: ", extern_val.name).unwrap();
        self.print_type(&extern_val.type_);
        writeln!(&mut self.output, ";").unwrap();
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
            Argument::Named(name, type_) => {
                write!(&mut self.output, "{name}: ").unwrap();
                self.print_type(type_);
            }
            Argument::ConstSelf => write!(&mut self.output, "&self").unwrap(),
            Argument::MutSelf => write!(&mut self.output, "&mut self").unwrap(),
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

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;
    use crate::parser::parse_str;

    #[test]
    fn test_pretty_print_basic() {
        let text = r#"
        pub type Test {
            field: i32,
        }
        "#;

        let module = parse_str(text).unwrap();
        let printed = pretty_print(&module);

        assert!(printed.contains("pub type Test"));
        assert!(printed.contains("field: i32"));
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

#[singleton(0x118FB64), size(0x40), align(0x10)] // 0x3C
pub type InputDeviceManager {
    #[address(0x18)]
    pub enabled: bool,
}
        "#
        .trim();

        let module = parse_str(text).unwrap();
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
#[singleton(0x118FC20), size(0x620 /* actually 0x61C */), align(0x10)]
pub type AnarkGui {
    vftable {},
    #[address(0x1A0)]
    pub next_state: AnarkState,
    pub active_state: AnarkState,
}
        "#
        .trim();

        let module = parse_str(text).unwrap();
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

#[align(0x8)] /* block comment */
pub type BlockCommentTest {
    value: u64,
}
        "#
        .trim();

        let module = parse_str(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str(&printed).unwrap();
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

#[align(0x8)]
/* block comment */
pub type BlockCommentTest {
    value: u64,
}
        "#
        .trim();

        let module = parse_str(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str(&printed).unwrap();
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
    Idle = 0x0,
    // State comment
    Active = 0x1,
    Done = 0x2,
}
        "#
        .trim();

        let module = parse_str(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str(&printed).unwrap();
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

        let module = parse_str(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str(&printed).unwrap();
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
#[align(0x10)] /* alignment */
pub type Bar {
    value: u64,
}
        "#
        .trim();

        let module = parse_str(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str(&printed).unwrap();
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

        let module = parse_str(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str(&printed).unwrap();
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

        let module = parse_str(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str(&printed).unwrap();
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

        let module = parse_str(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str(&printed).unwrap();
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

#[align(0x8)] /* block */ /* another block */
pub type MultipleBlocks {
    value: u64,
}
        "#
        .trim();

        let module = parse_str(text).unwrap();
        let printed = pretty_print(&module);

        assert_eq!(printed, expected);

        // Parse again to verify round-trip
        let module2 = parse_str(&printed).unwrap();
        let printed2 = pretty_print(&module2);

        assert_eq!(printed, printed2);
    }
}
