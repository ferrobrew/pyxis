use crate::{
    grammar::ItemPath,
    span::{EqualsIgnoringLocations, HasLocation, ItemLocation},
    tokenizer::TokenKind,
};

#[cfg(test)]
use crate::span::StripLocations;

use super::{
    ParseError,
    attributes::{Attributes, Visibility},
    core::Parser,
    expressions::{Expr, StringFormat},
    module::ModuleItem,
    types::{Ident, Type},
};

#[cfg(test)]
use super::attributes::Attribute;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Backend {
    pub name: Ident,
    pub prologue: Option<String>,
    pub prologue_format: StringFormat,
    pub epilogue: Option<String>,
    pub epilogue_format: StringFormat,
    pub location: ItemLocation,
}
impl HasLocation for Backend {
    fn location(&self) -> &ItemLocation {
        &self.location
    }
}
#[cfg(test)]
impl StripLocations for Backend {
    fn strip_locations(&self) -> Self {
        Backend {
            name: self.name.strip_locations(),
            prologue: self.prologue.strip_locations(),
            prologue_format: self.prologue_format.strip_locations(),
            epilogue: self.epilogue.strip_locations(),
            epilogue_format: self.epilogue_format.strip_locations(),
            location: ItemLocation::test(),
        }
    }
}
#[cfg(test)]
impl Backend {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.into(),
            prologue: None,
            prologue_format: StringFormat::Regular,
            epilogue: None,
            epilogue_format: StringFormat::Regular,
            location: ItemLocation::test(),
        }
    }
    pub fn with_prologue(mut self, prologue: impl Into<String>) -> Self {
        self.prologue = Some(prologue.into());
        self
    }
    pub fn with_prologue_format(
        mut self,
        prologue: impl Into<String>,
        format: StringFormat,
    ) -> Self {
        self.prologue = Some(prologue.into());
        self.prologue_format = format;
        self
    }
    pub fn with_epilogue(mut self, epilogue: impl Into<String>) -> Self {
        self.epilogue = Some(epilogue.into());
        self
    }
    pub fn with_epilogue_format(
        mut self,
        epilogue: impl Into<String>,
        format: StringFormat,
    ) -> Self {
        self.epilogue = Some(epilogue.into());
        self.epilogue_format = format;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternValue {
    pub visibility: Visibility,
    pub name: Ident,
    pub type_: Type,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    pub location: ItemLocation,
}
impl HasLocation for ExternValue {
    fn location(&self) -> &ItemLocation {
        &self.location
    }
}
#[cfg(test)]
impl StripLocations for ExternValue {
    fn strip_locations(&self) -> Self {
        ExternValue {
            visibility: self.visibility.strip_locations(),
            name: self.name.strip_locations(),
            type_: self.type_.strip_locations(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.strip_locations(),
            location: ItemLocation::test(),
        }
    }
}
#[cfg(test)]
impl ExternValue {
    pub fn new(
        visibility: Visibility,
        name: &str,
        type_: Type,
        attributes: impl IntoIterator<Item = Attribute>,
    ) -> Self {
        Self {
            visibility,
            name: name.into(),
            type_,
            attributes: Attributes::from_iter(attributes),
            doc_comments: vec![],
            location: ItemLocation::test(),
        }
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
}

/// Represents a use tree for braced imports.
/// Examples:
/// - `Vector3` → `UseTree::Path { path: ["Vector3"], location }`
/// - `math::{Vector3, Matrix4}` → `UseTree::Group { prefix: ["math"], items: [Path(...), Path(...)] }`
/// - `types::{math::{V3, V4}, Game}` → nested groups
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UseTree {
    /// A leaf path like `Vector3` or `math::Vector3`
    Path {
        path: ItemPath,
        location: ItemLocation,
    },
    /// A group like `{A, B}` or `path::{A, B}`
    Group {
        prefix: ItemPath,
        items: Vec<UseTree>,
        location: ItemLocation,
    },
}
impl UseTree {
    /// Flatten this UseTree into a list of complete ItemPaths.
    /// For example, `math::{Vector3, Matrix4}` becomes `["math::Vector3", "math::Matrix4"]`
    pub fn flatten(&self) -> Vec<ItemPath> {
        self.flatten_with_locations()
            .into_iter()
            .map(|(path, _)| path)
            .collect()
    }

    /// Flatten this UseTree into a list of (ItemPath, ItemLocation) pairs.
    /// The location points to the original use tree item for error reporting.
    pub fn flatten_with_locations(&self) -> Vec<(ItemPath, ItemLocation)> {
        match self {
            UseTree::Path { path, location } => vec![(path.clone(), *location)],
            UseTree::Group { prefix, items, .. } => items
                .iter()
                .flat_map(|item| {
                    item.flatten_with_locations()
                        .into_iter()
                        .map(|(item_path, location)| {
                            (
                                prefix
                                    .iter()
                                    .chain(item_path.iter())
                                    .cloned()
                                    .collect::<ItemPath>(),
                                location,
                            )
                        })
                })
                .collect(),
        }
    }
}
impl HasLocation for UseTree {
    fn location(&self) -> &ItemLocation {
        match self {
            UseTree::Path { location, .. } => location,
            UseTree::Group { location, .. } => location,
        }
    }
}
#[cfg(test)]
impl UseTree {
    /// Create a Path variant with a test location (for use in tests)
    pub fn path(path: impl Into<ItemPath>) -> Self {
        UseTree::Path {
            path: path.into(),
            location: ItemLocation::test(),
        }
    }

    /// Create a Group variant with a test location (for use in tests)
    pub fn group(prefix: impl Into<ItemPath>, items: impl IntoIterator<Item = UseTree>) -> Self {
        UseTree::Group {
            prefix: prefix.into(),
            items: items.into_iter().collect(),
            location: ItemLocation::test(),
        }
    }
}
#[cfg(test)]
impl StripLocations for UseTree {
    fn strip_locations(&self) -> Self {
        match self {
            UseTree::Path { path, .. } => UseTree::Path {
                path: path.strip_locations(),
                location: ItemLocation::test(),
            },
            UseTree::Group { prefix, items, .. } => UseTree::Group {
                prefix: prefix.strip_locations(),
                items: items.iter().map(|i| i.strip_locations()).collect(),
                location: ItemLocation::test(),
            },
        }
    }
}
impl EqualsIgnoringLocations for UseTree {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        match (self, other) {
            (UseTree::Path { path: a, .. }, UseTree::Path { path: b, .. }) => {
                a.equals_ignoring_locations(b)
            }
            (
                UseTree::Group {
                    prefix: p1,
                    items: i1,
                    ..
                },
                UseTree::Group {
                    prefix: p2,
                    items: i2,
                    ..
                },
            ) => p1.equals_ignoring_locations(p2) && i1.equals_ignoring_locations(i2),
            _ => false,
        }
    }
}

impl Parser {
    /// Parse a use statement, potentially with braced imports (including nested).
    /// Returns a single ModuleItem containing a UseTree.
    pub(crate) fn parse_use(&mut self) -> Result<ModuleItem, ParseError> {
        let first_token = self.expect(TokenKind::Use)?;

        // Check for super keyword (not supported yet)
        if let TokenKind::Ident(name) = self.peek()
            && name == "super"
        {
            return Err(ParseError::SuperNotSupported {
                location: self.current().location,
            });
        }

        let tree = self.parse_use_tree()?;
        let last_token = self.expect(TokenKind::Semi)?;
        let location = self.item_location_from_token_range(&first_token, &last_token);

        Ok(ModuleItem::Use { tree, location })
    }

    /// Parse a use tree (recursive for nested braced imports).
    /// Handles: `path::Item`, `path::{A, B}`, `path::{a::{X, Y}, B}`
    fn parse_use_tree(&mut self) -> Result<UseTree, ParseError> {
        // Parse the path prefix (may be empty for top-level braces, or full path)
        let (prefix, path_location) = self.parse_item_path()?;

        // Check for braced imports: path::{...}
        // Note: parse_item_path already consumed the :: before {, so we just check for {
        if matches!(self.peek(), TokenKind::LBrace) {
            let brace_start = self.current().location.span.start;
            self.advance(); // consume {

            let mut items = Vec::new();

            // Parse comma-separated use trees
            loop {
                if matches!(self.peek(), TokenKind::RBrace) {
                    break;
                }

                // Recursively parse each item as a use tree (supports nesting)
                let item = self.parse_use_tree()?;
                items.push(item);

                // Check for comma or end of list
                if matches!(self.peek(), TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }

            let rbrace = self.expect(TokenKind::RBrace)?;
            let end_pos = rbrace.location.span.end;
            let location = self.item_location_from_locations(brace_start, end_pos);

            Ok(UseTree::Group {
                prefix,
                items,
                location,
            })
        } else {
            // Simple path: just return it
            Ok(UseTree::Path {
                path: prefix,
                location: path_location,
            })
        }
    }

    pub(crate) fn parse_extern_type(&mut self) -> Result<ModuleItem, ParseError> {
        let start_pos = self.current().location.span.start;
        let mut doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        // Also collect doc comments that appear after attributes
        let after_attr_doc_comments = self.collect_doc_comments();
        doc_comments.extend(after_attr_doc_comments);

        self.expect(TokenKind::Extern)?;
        self.expect(TokenKind::Type)?;
        let (mut name, _) = self.expect_ident()?;

        // Handle generics - concatenate them into the type name string
        if matches!(self.peek(), TokenKind::Lt) {
            let mut type_str = name.0;
            type_str.push('<');
            self.advance(); // consume <

            let mut depth = 1;
            while depth > 0 && !matches!(self.peek(), TokenKind::Eof) {
                match self.peek().clone() {
                    TokenKind::Lt => {
                        type_str.push('<');
                        depth += 1;
                        self.advance();
                    }
                    TokenKind::Gt => {
                        type_str.push('>');
                        depth -= 1;
                        self.advance();
                    }
                    TokenKind::Comma => {
                        type_str.push_str(", ");
                        self.advance();
                    }
                    TokenKind::Ident(n) => {
                        type_str.push_str(&n);
                        self.advance();
                    }
                    TokenKind::ColonColon => {
                        type_str.push_str("::");
                        self.advance();
                    }
                    _ => {
                        self.advance();
                    }
                }
            }
            name = Ident(type_str);
        }

        self.expect(TokenKind::Semi)?;
        let end_pos = if self.pos > 0 {
            self.tokens[self.pos - 1].location.span.end
        } else {
            self.current().location.span.end
        };
        let location = self.item_location_from_locations(start_pos, end_pos);
        Ok(ModuleItem::ExternType {
            name,
            attributes,
            doc_comments,
            location,
        })
    }

    pub(crate) fn parse_extern_value(&mut self) -> Result<ExternValue, ParseError> {
        let start_pos = self.current().location.span.start;
        let mut doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        // Also collect doc comments that appear after attributes
        let after_attr_doc_comments = self.collect_doc_comments();
        doc_comments.extend(after_attr_doc_comments);

        let visibility = self.parse_visibility()?;
        self.expect(TokenKind::Extern)?;
        let (name, _) = self.expect_ident()?;
        self.expect(TokenKind::Colon)?;
        let type_ = self.parse_type()?;
        self.expect(TokenKind::Semi)?;
        let end_pos = if self.pos > 0 {
            self.tokens[self.pos - 1].location.span.end
        } else {
            self.current().location.span.end
        };
        let location = self.item_location_from_locations(start_pos, end_pos);

        Ok(ExternValue {
            visibility,
            name,
            type_,
            attributes,
            doc_comments,
            location,
        })
    }

    pub(crate) fn parse_backend(&mut self) -> Result<Backend, ParseError> {
        let first_token = self.expect(TokenKind::Backend)?;
        let (name, _) = self.expect_ident()?;

        let mut prologue = None;
        let mut prologue_format = StringFormat::Regular;
        let mut epilogue = None;
        let mut epilogue_format = StringFormat::Regular;

        // Check if we have braces or direct prologue/epilogue
        let last_token = if matches!(self.peek(), TokenKind::LBrace) {
            // Form: backend name { prologue ...; epilogue ...; }
            self.advance(); // consume {

            while !matches!(self.peek(), TokenKind::RBrace) {
                match self.peek() {
                    TokenKind::Prologue => {
                        self.advance();
                        let expr = self.parse_expr()?;
                        if let Expr::StringLiteral { value, format, .. } = expr {
                            prologue = Some(value);
                            prologue_format = format;
                        } else {
                            return Err(ParseError::ExpectedStringLiteral {
                                found: self.peek().clone(),
                                location: *expr.location(),
                            });
                        }
                        self.expect(TokenKind::Semi)?;
                    }
                    TokenKind::Epilogue => {
                        self.advance();
                        let expr = self.parse_expr()?;
                        if let Expr::StringLiteral { value, format, .. } = expr {
                            epilogue = Some(value);
                            epilogue_format = format;
                        } else {
                            return Err(ParseError::ExpectedStringLiteral {
                                found: self.peek().clone(),
                                location: *expr.location(),
                            });
                        }
                        self.expect(TokenKind::Semi)?;
                    }
                    _ => {
                        return Err(ParseError::ExpectedPrologueOrEpilogue {
                            found: self.peek().clone(),
                            location: self.current().location,
                        });
                    }
                }
            }

            self.expect(TokenKind::RBrace)?
        } else {
            // Form: backend name prologue ... or backend name epilogue ...
            match self.peek() {
                TokenKind::Prologue => {
                    self.advance();
                    let expr = self.parse_expr()?;
                    if let Expr::StringLiteral { value, format, .. } = expr {
                        prologue = Some(value);
                        prologue_format = format;
                    } else {
                        return Err(ParseError::ExpectedStringLiteral {
                            found: self.peek().clone(),
                            location: *expr.location(),
                        });
                    }
                    self.expect(TokenKind::Semi)?
                }
                TokenKind::Epilogue => {
                    self.advance();
                    let expr = self.parse_expr()?;
                    if let Expr::StringLiteral { value, format, .. } = expr {
                        epilogue = Some(value);
                        epilogue_format = format;
                    } else {
                        return Err(ParseError::ExpectedStringLiteral {
                            found: self.peek().clone(),
                            location: *expr.location(),
                        });
                    }
                    self.expect(TokenKind::Semi)?
                }
                _ => {
                    return Err(ParseError::ExpectedBackendContent {
                        found: self.peek().clone(),
                        location: self.current().location,
                    });
                }
            }
        };

        let location = self.item_location_from_token_range(&first_token, &last_token);
        Ok(Backend {
            name,
            prologue,
            prologue_format,
            epilogue,
            epilogue_format,
            location,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        grammar::{ModuleItem, StringFormat, test_aliases::*},
        parser::{ParseError, parse_str_for_tests},
        span::StripLocations,
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn can_parse_use() {
        let text = r#"
        use hello::TestType<Hey>;
        type Test {
            test: TestType<Hey>,
        }
        "#;

        // Use paths preserve generic syntax in the path (as "TestType<Hey>")
        // Type references parse generics properly
        let ast = M::new()
            .with_uses([IP::from("hello::TestType<Hey>")])
            .with_definitions([ID::new(
                (V::Private, "Test"),
                TD::new([TS::field(
                    (V::Private, "test"),
                    T::generic("TestType", [T::ident("Hey")]),
                )]),
            )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_braced_imports() {
        let text = r#"
        use math::{Matrix4, Vector3};
        type Test {
            matrix: Matrix4,
            vector: Vector3,
        }
        "#;

        let ast = M::new()
            .with_use_trees([UT::group(
                "math",
                [UT::path("Matrix4"), UT::path("Vector3")],
            )])
            .with_definitions([ID::new(
                (V::Private, "Test"),
                TD::new([
                    TS::field((V::Private, "matrix"), T::ident("Matrix4")),
                    TS::field((V::Private, "vector"), T::ident("Vector3")),
                ]),
            )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_braced_imports_with_generics() {
        let text = r#"
        use types::{SharedPtr<T>, Vec<u32>};
        "#;

        let ast = M::new().with_use_trees([UT::group(
            "types",
            [UT::path("SharedPtr<T>"), UT::path("Vec<u32>")],
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_braced_imports_single_item() {
        let text = r#"
        use math::{Matrix4};
        "#;

        let ast = M::new().with_use_trees([UT::group("math", [UT::path("Matrix4")])]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_braced_imports_with_trailing_comma() {
        let text = r#"
        use math::{Matrix4, Vector3,};
        "#;

        let ast = M::new().with_use_trees([UT::group(
            "math",
            [UT::path("Matrix4"), UT::path("Vector3")],
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_braced_imports_nested_path() {
        let text = r#"
        use graphics::math::{Matrix4, Vector3};
        "#;

        let ast = M::new().with_use_trees([UT::group(
            "graphics::math",
            [UT::path("Matrix4"), UT::path("Vector3")],
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_empty_braced_imports() {
        let text = r#"
        use math::{};
        "#;

        let ast = M::new().with_use_trees([UT::group("math", [])]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_nested_braced_imports() {
        let text = r#"
        use types::{math::{Vector3, Matrix4}, Game};
        "#;

        let ast = M::new().with_use_trees([UT::group(
            "types",
            [
                UT::group("math", [UT::path("Vector3"), UT::path("Matrix4")]),
                UT::path("Game"),
            ],
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_flatten_nested_braced_imports() {
        // Test that UseTree::flatten() works correctly for nested imports
        let tree = UT::group(
            "types",
            [
                UT::group("math", [UT::path("Vector3"), UT::path("Matrix4")]),
                UT::path("Game"),
            ],
        );

        let flattened = tree.flatten();
        assert_eq!(flattened.len(), 3);
        assert_eq!(flattened[0], IP::from("types::math::Vector3"));
        assert_eq!(flattened[1], IP::from("types::math::Matrix4"));
        assert_eq!(flattened[2], IP::from("types::Game"));
    }

    #[test]
    fn will_die_on_super_for_now() {
        let text = r#"
        use super::TestType<Hey>;
        "#;

        let error = parse_str_for_tests(text).err().unwrap();
        assert!(
            matches!(error, ParseError::SuperNotSupported { .. }),
            "Expected SuperNotSupported error, got: {error:?}"
        );
    }

    #[test]
    fn can_parse_extern() {
        let text = r#"
        #[size(12)]
        extern type TestType<Hey>;
        type Test {
            test: TestType<Hey>,
        }
        "#;

        // Extern type names preserve the generic syntax literally in the name
        // But type references now parse generics properly
        let ast = M::new()
            .with_extern_types([("TestType<Hey>".into(), As::from_iter([A::size_decimal(12)]))])
            .with_definitions([ID::new(
                (V::Private, "Test"),
                TD::new([TS::field(
                    (V::Private, "test"),
                    T::generic("TestType", [T::ident("Hey")]),
                )]),
            )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_extern_with_multiline_doc_comment() {
        let text = r#"
#[size(8), align(4)]
/// `ManuallyDrop<SharedPtr<u32>>` is used instead of `SharedPtr<u32>` to avoid
/// the `Drop` implementation of `SharedPtr<u32>` being called when the `RenderBlock`
/// is dropped. The destructor, which we call in `drop`, will decrement the refcount
/// for us.
extern type ManuallyDrop<SharedPtr<u32>>;
    "#;

        let module = parse_str_for_tests(text).unwrap().strip_locations();

        // Verify we have one extern type item
        assert_eq!(module.items.len(), 1);

        // Verify it's an ExternType with the correct attributes and doc comments
        match &module.items[0] {
            ModuleItem::ExternType {
                name,
                attributes,
                doc_comments,
                ..
            } => {
                assert_eq!(name.0, "ManuallyDrop<SharedPtr<u32>>");
                assert_eq!(attributes.0.len(), 2);
                assert_eq!(doc_comments.len(), 4); // 4 lines of doc comment
                assert!(doc_comments[0].contains("ManuallyDrop<SharedPtr<u32>>"));
                assert!(doc_comments[1].contains("Drop` implementation"));
                assert!(doc_comments[2].contains("dropped"));
                assert!(doc_comments[3].contains("for us"));
            }
            _ => panic!("Expected ExternType"),
        }
    }

    #[test]
    fn can_parse_extern_value() {
        let text = r#"
        #[size(4)]
        extern type SomeType;
        #[address(0x1337)]
        pub extern some_value: *mut SomeType;
        #[address(0x1338)]
        extern some_private_value: *mut SomeType;
        "#;

        let ast = M::new()
            .with_extern_types([("SomeType".into(), As::from_iter([A::size_decimal(4)]))])
            .with_extern_values([
                EV::new(
                    V::Public,
                    "some_value",
                    T::ident("SomeType").mut_pointer(),
                    [A::address(0x1337)],
                ),
                EV::new(
                    V::Private,
                    "some_private_value",
                    T::ident("SomeType").mut_pointer(),
                    [A::address(0x1338)],
                ),
            ]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_backends() {
        let text = r##"
backend rust {
    prologue r#"
        use std::ffi::CString;
        use std::os::raw::c_char;
    "#;

    epilogue r#"
        fn main() {
            println!("Hello, world!");
        }
    "#;
}


backend rust prologue r#"
    use std::ffi::CString;
    use std::os::raw::c_char;
"#;

backend rust epilogue r#"
    fn main() {
        println!("Hello, world!");
    }
"#;
"##;

        let ast = M::new().with_backends([
            B::new("rust")
                .with_prologue_format(
                    r#"
        use std::ffi::CString;
        use std::os::raw::c_char;
    "#,
                    StringFormat::Raw,
                )
                .with_epilogue_format(
                    r#"
        fn main() {
            println!("Hello, world!");
        }
    "#,
                    StringFormat::Raw,
                ),
            B::new("rust").with_prologue_format(
                r#"
    use std::ffi::CString;
    use std::os::raw::c_char;
"#,
                StringFormat::Raw,
            ),
            B::new("rust").with_epilogue_format(
                r#"
    fn main() {
        println!("Hello, world!");
    }
"#,
                StringFormat::Raw,
            ),
        ]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_backend_with_multiline_prologue() {
        let text = r#"
backend rust prologue "
    use crate::shared_ptr::*;
    use std::mem::ManuallyDrop;
";
    "#;

        let ast = M::new().with_backends([B::new("rust")
            .with_prologue("\n    use crate::shared_ptr::*;\n    use std::mem::ManuallyDrop;\n")]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }
}
