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

/// A `prologue`/`epilogue` slot on a backend block. The plain form
/// (`prologue r#"..."#;`) targets the header (or whatever the backend
/// considers its primary output); the `definition` modifier
/// (`prologue definition r#"..."#;`) targets the source file - currently
/// only meaningful for `backend cpp` where header / source are distinct.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(StripLocations))]
pub struct BackendSplice {
    pub header: Option<String>,
    pub header_format: StringFormat,
    pub definition: Option<String>,
    pub definition_format: StringFormat,
}
impl Default for BackendSplice {
    fn default() -> Self {
        Self {
            header: None,
            header_format: StringFormat::Regular,
            definition: None,
            definition_format: StringFormat::Regular,
        }
    }
}
impl BackendSplice {
    pub fn is_empty(&self) -> bool {
        self.header.is_none() && self.definition.is_none()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct Backend {
    /// Typed backend identity. Validated at parse time - an unknown
    /// backend name raises `ParseError::UnknownBackend` rather than
    /// surviving as a string in the IR.
    pub name: crate::Backend,
    pub prologue: BackendSplice,
    pub epilogue: BackendSplice,
    /// Per-backend explicit working set: which other-module items this
    /// backend block references. Drives cpp `#include` selection and
    /// (in principle) any other backend that wants to know what its
    /// epilogue/prologue depends on. `use`-style with the same syntax as
    /// module-level imports - braced groups, nested groups, etc.
    pub uses: Vec<UseTree>,
    pub location: ItemLocation,
}
#[cfg(test)]
impl Backend {
    /// Test helper: panics if the name isn't a valid backend. Tests
    /// that want to exercise the unknown-backend error path should
    /// construct a `ParseError::UnknownBackend` directly instead.
    pub fn new(name: &str) -> Self {
        let name = crate::Backend::from_name(name)
            .unwrap_or_else(|| panic!("test used unknown backend `{name}`"));
        Self {
            name,
            prologue: BackendSplice::default(),
            epilogue: BackendSplice::default(),
            uses: Vec::new(),
            location: ItemLocation::test(),
        }
    }
    pub fn with_prologue(mut self, prologue: impl Into<String>) -> Self {
        self.prologue.header = Some(prologue.into());
        self
    }
    pub fn with_prologue_format(
        mut self,
        prologue: impl Into<String>,
        format: StringFormat,
    ) -> Self {
        self.prologue.header = Some(prologue.into());
        self.prologue.header_format = format;
        self
    }
    pub fn with_prologue_definition(mut self, prologue: impl Into<String>) -> Self {
        self.prologue.definition = Some(prologue.into());
        self
    }
    pub fn with_prologue_definition_format(
        mut self,
        prologue: impl Into<String>,
        format: StringFormat,
    ) -> Self {
        self.prologue.definition = Some(prologue.into());
        self.prologue.definition_format = format;
        self
    }
    pub fn with_epilogue(mut self, epilogue: impl Into<String>) -> Self {
        self.epilogue.header = Some(epilogue.into());
        self
    }
    pub fn with_epilogue_format(
        mut self,
        epilogue: impl Into<String>,
        format: StringFormat,
    ) -> Self {
        self.epilogue.header = Some(epilogue.into());
        self.epilogue.header_format = format;
        self
    }
    pub fn with_epilogue_definition(mut self, epilogue: impl Into<String>) -> Self {
        self.epilogue.definition = Some(epilogue.into());
        self
    }
    pub fn with_epilogue_definition_format(
        mut self,
        epilogue: impl Into<String>,
        format: StringFormat,
    ) -> Self {
        self.epilogue.definition = Some(epilogue.into());
        self.epilogue.definition_format = format;
        self
    }
    pub fn with_uses(mut self, uses: impl IntoIterator<Item = UseTree>) -> Self {
        self.uses = uses.into_iter().collect();
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct ExternValue {
    pub visibility: Visibility,
    pub name: Ident,
    pub type_: Type,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    pub location: ItemLocation,
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
#[derive(Debug, Clone, PartialEq, Eq, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
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

    /// Parse the body of a single `prologue`/`epilogue` slot, after the
    /// keyword has been consumed. Optionally accepts a `definition`
    /// modifier (only meaningful for `backend cpp`; rejected at the
    /// semantic layer for other backends). Then expects a string literal
    /// and a trailing semicolon.
    fn parse_backend_splice_slot(&mut self, slot: &mut BackendSplice) -> Result<(), ParseError> {
        // Optional `definition` modifier.
        let is_definition = matches!(
            self.peek(),
            TokenKind::Ident(s) if s == "definition"
        );
        if is_definition {
            self.advance();
        }
        let expr = self.parse_expr()?;
        let Expr::StringLiteral { value, format, .. } = expr else {
            return Err(ParseError::ExpectedStringLiteral {
                found: self.peek().clone(),
                location: *expr.location(),
            });
        };
        if is_definition {
            slot.definition = Some(value);
            slot.definition_format = format;
        } else {
            slot.header = Some(value);
            slot.header_format = format;
        }
        self.expect(TokenKind::Semi)?;
        Ok(())
    }

    /// Returns the most recently consumed token (typically a closing
    /// `;` or `}`), used to compute span ranges.
    fn last_token(&self) -> &crate::tokenizer::Token {
        let idx = self.pos.saturating_sub(1);
        &self.tokens[idx.min(self.tokens.len() - 1)]
    }

    pub(crate) fn parse_backend(&mut self) -> Result<Backend, ParseError> {
        let first_token = self.expect(TokenKind::Backend)?;
        let (name_ident, name_span) = self.expect_ident()?;
        let name = match crate::Backend::from_name(name_ident.0.as_str()) {
            Some(b) => b,
            None => {
                return Err(ParseError::UnknownBackend {
                    found: name_ident.0,
                    location: ItemLocation {
                        file_id: first_token.location.file_id,
                        span: name_span,
                    },
                });
            }
        };

        let mut prologue = BackendSplice::default();
        let mut epilogue = BackendSplice::default();
        let mut uses: Vec<UseTree> = Vec::new();

        // Check if we have braces or direct prologue/epilogue
        let last_token = if matches!(self.peek(), TokenKind::LBrace) {
            // Form: backend name { use ...; prologue ...; epilogue ...; }
            self.advance(); // consume {

            while !matches!(self.peek(), TokenKind::RBrace) {
                match self.peek() {
                    TokenKind::Use => {
                        // `use foo::bar;` or `use foo::{a, b};` - same
                        // grammar as module-level use statements.
                        self.advance();
                        let tree = self.parse_use_tree()?;
                        self.expect(TokenKind::Semi)?;
                        uses.push(tree);
                    }
                    TokenKind::Prologue => {
                        self.advance();
                        self.parse_backend_splice_slot(&mut prologue)?;
                    }
                    TokenKind::Epilogue => {
                        self.advance();
                        self.parse_backend_splice_slot(&mut epilogue)?;
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
            // Form: `backend name prologue ...` or `backend name epilogue ...`
            // (with optional `definition` modifier).
            match self.peek() {
                TokenKind::Prologue => {
                    self.advance();
                    self.parse_backend_splice_slot(&mut prologue)?;
                    self.last_token().clone()
                }
                TokenKind::Epilogue => {
                    self.advance();
                    self.parse_backend_splice_slot(&mut epilogue)?;
                    self.last_token().clone()
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
            epilogue,
            uses,
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
                assert_eq!(
                    doc_comments,
                    &[
                        " `ManuallyDrop<SharedPtr<u32>>` is used instead of `SharedPtr<u32>` to avoid",
                        " the `Drop` implementation of `SharedPtr<u32>` being called when the `RenderBlock`",
                        " is dropped. The destructor, which we call in `drop`, will decrement the refcount",
                        " for us.",
                    ]
                );
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

    #[cfg(feature = "cpp")]
    #[test]
    fn can_parse_backend_with_uses() {
        let text = r##"
backend cpp {
    use types::math::Matrix4;
    use types::shared_ptr::{SharedPtr, WeakPtr};

    epilogue r#"
        inline auto test() { return 1; }
    "#;
}
"##;

        let ast = M::new().with_backends([B::new("cpp")
            .with_uses([
                UT::path("types::math::Matrix4"),
                UT::group(
                    "types::shared_ptr",
                    [UT::path("SharedPtr"), UT::path("WeakPtr")],
                ),
            ])
            .with_epilogue_format(
                r#"
        inline auto test() { return 1; }
    "#,
                StringFormat::Raw,
            )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn unknown_backend_name_is_a_parse_error() {
        // `backend foobar { ... }` should be rejected at parse time
        // before any semantic analysis runs - typos shouldn't escape
        // into the IR.
        let text = r##"backend foobar prologue r#""#;"##;
        let err = parse_str_for_tests(text).unwrap_err();
        match err {
            ParseError::UnknownBackend { found, .. } => {
                assert_eq!(found, "foobar");
            }
            other => panic!("expected UnknownBackend, got {other:?}"),
        }
    }

    #[test]
    fn known_backend_names_parse() {
        // Every enabled backend name should round-trip cleanly through
        // the parser. `cpp` and `json` are feature-gated, so only assert
        // on them when their feature is on.
        let mut names: Vec<&'static str> = vec!["rust"];
        #[cfg(feature = "cpp")]
        names.push("cpp");
        #[cfg(feature = "json")]
        names.push("json");
        for name in names {
            let text = format!(r##"backend {name} prologue r#""#;"##);
            let module = parse_str_for_tests(&text).unwrap();
            let backends: Vec<_> = module.backends().collect();
            assert_eq!(backends.len(), 1);
            assert_eq!(backends[0].name.name(), name);
        }
    }

    #[cfg(feature = "cpp")]
    #[test]
    fn can_parse_backend_with_definition_modifier() {
        // The `definition` modifier on an epilogue/prologue lands in the
        // .cpp source file rather than the .hpp header. Both shorthand
        // and block forms should accept it.
        let text = r##"
backend cpp epilogue definition r#"
    bool Probe::read() const { return value; }
"#;

backend cpp {
    prologue definition r#"
        #include <windows.h>
    "#;
    epilogue r#"
        bool header_only_helper();
    "#;
    epilogue definition r#"
        bool source_only_helper() { return true; }
    "#;
}
"##;

        let ast = M::new().with_backends([
            B::new("cpp").with_epilogue_definition_format(
                r#"
    bool Probe::read() const { return value; }
"#,
                StringFormat::Raw,
            ),
            B::new("cpp")
                .with_prologue_definition_format(
                    r#"
        #include <windows.h>
    "#,
                    StringFormat::Raw,
                )
                .with_epilogue_format(
                    r#"
        bool header_only_helper();
    "#,
                    StringFormat::Raw,
                )
                .with_epilogue_definition_format(
                    r#"
        bool source_only_helper() { return true; }
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

    // ========================================================================
    // Use statement error tests
    // ========================================================================

    use crate::span::ItemLocation;
    use crate::tokenizer::TokenKind;

    #[test]
    fn use_empty_path_parses_ok() {
        // Parser accepts `use;` - semantic layer catches it
        let text = r#"
        use;
        "#;
        // Empty use path parses OK, semantic validation would catch
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn use_missing_semicolon() {
        let text = r#"
        use foo::bar
        type Test {}
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Semi],
                found: TokenKind::Type,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn use_braced_missing_closing_brace() {
        let text = r#"
        use foo::{bar, baz;
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RBrace],
                found: TokenKind::Semi,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn use_braced_empty_parses_ok() {
        // Parser accepts `use foo::{};` - empty brace group
        let text = r#"
        use foo::{};
        "#;
        // Empty braced import parses OK
        assert!(parse_str_for_tests(text).is_ok());
    }

    // ========================================================================
    // Extern type/value error tests
    // ========================================================================

    #[test]
    fn extern_type_missing_name() {
        let text = r#"
        extern type;
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIdentifier {
                found: TokenKind::Semi,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn extern_type_missing_semicolon() {
        let text = r#"
        extern type Foo
        type Bar {}
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Semi],
                found: TokenKind::Type,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }
}
