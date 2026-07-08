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
    types::Ident,
};

/// Which end of a module's generated output a splice attaches to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
#[cfg_attr(test, strip_locations(copy))]
pub enum SpliceKind {
    /// `prologue ...;` — emitted *before* the module's generated output.
    Prologue,
    /// `epilogue ...;` — emitted *after* the module's generated output.
    Epilogue,
}
impl SpliceKind {
    pub fn keyword(self) -> &'static str {
        match self {
            SpliceKind::Prologue => "prologue",
            SpliceKind::Epilogue => "epilogue",
        }
    }
}

/// A standalone `prologue`/`epilogue` splice statement: raw backend code
/// spliced into a module's generated output.
///
/// A leading `#[cfg(...)]` is optional. Gated → emitted only for the
/// backends the predicate selects; ungated → emitted unconditionally
/// across every backend (e.g. a licence header). The predicate lives in
/// `attributes` and is read via [`Attributes::cfg`].
///
/// The `definition` modifier (`epilogue definition r#"..."#;`) targets the
/// source file rather than the header — only meaningful for `cpp`, where
/// header and source are distinct. It is rejected at the semantic layer
/// unless the splice's cfg resolves cpp-only.
///
/// An optional `for <ItemPath>` clause (`epilogue for SharedPtr r#"..."#;`)
/// attributes the splice to a type defined in the same module, so the
/// viewer renders it on that type's page instead of the module page. The
/// path is stored as-written and resolved to an absolute item path during
/// semantic analysis.
#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct Splice {
    pub kind: SpliceKind,
    /// Leading outer attributes. Only `#[cfg(...)]` is meaningful; other
    /// attributes are rejected during semantic analysis.
    pub attributes: Attributes,
    /// `definition` modifier: splice into the source file (cpp `.cpp`).
    pub definition: bool,
    /// `for <Type>` attribution target, as-written (module-relative or
    /// absolute). Resolved to an absolute item path at semantic-analysis
    /// time; `None` means "module-level".
    pub for_type: Option<ItemPath>,
    pub text: String,
    pub format: StringFormat,
    pub location: ItemLocation,
}
#[cfg(test)]
impl Splice {
    fn new(kind: SpliceKind, text: impl Into<String>) -> Self {
        Self {
            kind,
            attributes: Attributes::default(),
            definition: false,
            for_type: None,
            text: text.into(),
            format: StringFormat::Raw,
            location: ItemLocation::test(),
        }
    }
    pub fn prologue(text: impl Into<String>) -> Self {
        Self::new(SpliceKind::Prologue, text)
    }
    pub fn epilogue(text: impl Into<String>) -> Self {
        Self::new(SpliceKind::Epilogue, text)
    }
    pub fn with_format(mut self, format: StringFormat) -> Self {
        self.format = format;
        self
    }
    pub fn definition(mut self) -> Self {
        self.definition = true;
        self
    }
    pub fn for_type(mut self, for_type: impl Into<ItemPath>) -> Self {
        self.for_type = Some(for_type.into());
        self
    }
    /// Attach a `#[cfg(backend = "<name>")]` gate.
    pub fn cfg_backend(mut self, name: &str) -> Self {
        self.attributes
            .0
            .push(super::attributes::Attribute::cfg_backend(name));
        self
    }
}

/// Represents a use tree for braced imports.
/// Examples:
/// - `Vector3` → `UseTree::Path { path: ["Vector3"], location }`
/// - `math::{Vector3, Matrix4}` → `UseTree::Group { prefix: ["math"], items: [Path(...), Path(...)] }`
/// - `types::{math::{V3, V4}, Game}` → nested groups
#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
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
    ///
    /// `start_pos` anchors the item's span start: the leading attribute (if
    /// any), else the `use`/`pub` keyword, so the span covers the whole
    /// statement. `attributes` carries an optional leading `#[cfg(...)]`.
    pub(crate) fn parse_use(
        &mut self,
        visibility: Visibility,
        attributes: Attributes,
        start_pos: crate::span::Location,
    ) -> Result<ModuleItem, ParseError> {
        self.expect(TokenKind::Use)?;

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
        let location = self.item_location_from_locations(start_pos, last_token.location.span.end);

        Ok(ModuleItem::Use {
            tree,
            visibility,
            attributes,
            location,
        })
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
        // Full span (incl. leading doc comments / attributes), used by the
        // formatter to associate preceding comments and spacing.
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

        // Declaration position, used for documentation source links.
        let declaration_start = self.current().location.span.start;
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
        let declaration_location = self.item_location_from_locations(declaration_start, end_pos);
        Ok(ModuleItem::ExternType {
            name,
            attributes,
            doc_comments,
            location,
            declaration_location,
        })
    }

    /// Parse a standalone `prologue`/`epilogue` splice statement, including
    /// any leading `#[cfg(...)]` attributes. Accepts, in any order, an
    /// optional `definition` modifier (only meaningful for cpp; rejected at
    /// the semantic layer unless the cfg resolves cpp-only) and an optional
    /// `for <ItemPath>` attribution target, then a string literal and a
    /// trailing semicolon.
    pub(crate) fn parse_splice(&mut self) -> Result<Splice, ParseError> {
        // Full span (incl. leading attributes), used by the formatter to
        // associate preceding comments and spacing.
        let start_pos = self.current().location.span.start;
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        let kind = match self.peek() {
            TokenKind::Prologue => SpliceKind::Prologue,
            TokenKind::Epilogue => SpliceKind::Epilogue,
            _ => {
                return Err(ParseError::ExpectedPrologueOrEpilogue {
                    found: self.peek().clone(),
                    location: self.current().location,
                });
            }
        };
        self.advance(); // consume `prologue` / `epilogue`

        // Optional modifiers: `definition` and `for <ItemPath>`, in any
        // order. Each may appear at most once; duplicates fall through to the
        // string-literal parse and surface as a clear "expected string
        // literal" error.
        let mut definition = false;
        let mut has_for = false;
        let mut for_type = None;
        loop {
            match self.peek() {
                TokenKind::Ident(s) if s == "definition" && !definition => {
                    definition = true;
                    self.advance();
                }
                TokenKind::Ident(s) if s == "for" && !has_for => {
                    has_for = true;
                    self.advance(); // consume `for`
                    if !matches!(self.peek(), TokenKind::Ident(_)) {
                        return Err(ParseError::ExpectedIdentifier {
                            found: self.peek().clone(),
                            location: self.current().location,
                        });
                    }
                    let (path, _loc) = self.parse_item_path()?;
                    for_type = Some(path);
                }
                _ => break,
            }
        }

        let expr = self.parse_expr()?;
        let Expr::StringLiteral { value, format, .. } = expr else {
            return Err(ParseError::ExpectedStringLiteral {
                found: self.peek().clone(),
                location: *expr.location(),
            });
        };
        let semi = self.expect(TokenKind::Semi)?;
        let location = self.item_location_from_locations(start_pos, semi.location.span.end);
        Ok(Splice {
            kind,
            attributes,
            definition,
            for_type,
            text: value,
            format,
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
    fn can_parse_pub_use_reexport() {
        // `pub use` is an explicit re-export; plain `use` is private.
        let text = r#"
        pub use math::Vector3;
        use math::Matrix4;
        "#;

        let module = parse_str_for_tests(text).unwrap();
        let uses: Vec<_> = module
            .items
            .iter()
            .filter_map(|i| match i {
                ModuleItem::Use {
                    tree, visibility, ..
                } => Some((tree.flatten(), *visibility)),
                _ => None,
            })
            .collect();
        assert_eq!(uses.len(), 2);
        assert_eq!(uses[0].0, vec![IP::from("math::Vector3")]);
        assert_eq!(uses[0].1, V::Public);
        assert_eq!(uses[1].0, vec![IP::from("math::Matrix4")]);
        assert_eq!(uses[1].1, V::Private);
    }

    #[test]
    fn can_parse_pub_use_braced() {
        let text = r#"
        pub use math::{Matrix4, Vector3};
        "#;

        let ast = M::new().with_pub_use_trees([UT::group(
            "math",
            [UT::path("Matrix4"), UT::path("Vector3")],
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
            .with_definitions([
                ID::new(
                    (V::Public, "some_value"),
                    EVD::new(T::ident("SomeType").mut_pointer())
                        .with_attributes([A::address(0x1337)]),
                ),
                ID::new(
                    (V::Private, "some_private_value"),
                    EVD::new(T::ident("SomeType").mut_pointer())
                        .with_attributes([A::address(0x1338)]),
                ),
            ]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_splices() {
        // Standalone splice statements: an ungated one (every backend) and
        // cfg-gated ones. Each is its own module item, in source order.
        let text = r##"
prologue r#"// licence header"#;

#[cfg(backend = "rust")]
prologue r#"
    use std::ffi::CString;
    use std::os::raw::c_char;
"#;

#[cfg(backend = "rust")]
epilogue r#"
    fn main() {
        println!("Hello, world!");
    }
"#;
"##;

        let ast = M::new().with_splices([
            SP::prologue("// licence header"),
            SP::prologue("\n    use std::ffi::CString;\n    use std::os::raw::c_char;\n")
                .cfg_backend("rust"),
            SP::epilogue("\n    fn main() {\n        println!(\"Hello, world!\");\n    }\n")
                .cfg_backend("rust"),
        ]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_cfg_gated_use() {
        // A leading `#[cfg(...)]` on a `use` parses into the use's attributes.
        let text = r#"
        #[cfg(backend = "cpp")]
        use types::math::Matrix4;
        "#;

        let module = parse_str_for_tests(text).unwrap();
        let uses: Vec<_> = module
            .items
            .iter()
            .filter_map(|i| match i {
                ModuleItem::Use {
                    tree, attributes, ..
                } => Some((tree.flatten(), attributes.cfg())),
                _ => None,
            })
            .collect();
        assert_eq!(uses.len(), 1);
        assert_eq!(uses[0].0, vec![IP::from("types::math::Matrix4")]);
        assert!(uses[0].1.is_some(), "cfg-gated use should carry a cfg");
    }

    #[test]
    fn can_parse_splice_with_definition_modifier() {
        // The `definition` modifier lands the splice in the cpp `.cpp` source
        // file rather than the `.hpp` header.
        let text = r##"
#[cfg(backend = "cpp")]
epilogue definition r#"
    bool Probe::read() const { return value; }
"#;
"##;

        let ast = M::new().with_splices([SP::epilogue(
            "\n    bool Probe::read() const { return value; }\n",
        )
        .definition()
        .cfg_backend("cpp")]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_splice_with_for_type() {
        // `for <Type>` tags a splice as belonging to that type's page rather
        // than the module page.
        let text = r##"
#[cfg(backend = "rust")]
epilogue for Widget r#"
    impl Widget { pub fn new() -> Widget { Widget { id: 0 } } }
"#;
"##;

        let ast = M::new().with_splices([SP::epilogue(
            "\n    impl Widget { pub fn new() -> Widget { Widget { id: 0 } } }\n",
        )
        .for_type("Widget")
        .cfg_backend("rust")]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_splice_for_type_with_definition_any_order() {
        // `definition` and `for` are independent modifiers — either order works.
        let text = r##"
#[cfg(backend = "cpp")]
epilogue definition for Probe r#"
    bool Probe::read() const { return value; }
"#;
#[cfg(backend = "cpp")]
epilogue for Probe definition r#"
    bool Probe::init() { value = 0; }
"#;
#[cfg(backend = "cpp")]
epilogue for Probe r#"
    bool Probe::is_ready() const { return value != 0; }
"#;
"##;

        let ast = M::new().with_splices([
            SP::epilogue("\n    bool Probe::read() const { return value; }\n")
                .definition()
                .for_type("Probe")
                .cfg_backend("cpp"),
            SP::epilogue("\n    bool Probe::init() { value = 0; }\n")
                .definition()
                .for_type("Probe")
                .cfg_backend("cpp"),
            SP::epilogue("\n    bool Probe::is_ready() const { return value != 0; }\n")
                .for_type("Probe")
                .cfg_backend("cpp"),
        ]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_ungated_splice_with_plain_string() {
        // A plain (non-raw) string literal splice, emitted for every backend.
        let text = r#"
prologue "
    use crate::shared_ptr::*;
    use std::mem::ManuallyDrop;
";
    "#;

        let ast = M::new().with_splices([SP::prologue(
            "\n    use crate::shared_ptr::*;\n    use std::mem::ManuallyDrop;\n",
        )
        .with_format(StringFormat::Regular)]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    // ========================================================================
    // Use statement error tests
    // ========================================================================

    use crate::{span::ItemLocation, tokenizer::TokenKind};

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
