use crate::{
    span::{HasLocation, ItemLocation},
    tokenizer::TokenKind,
};

#[cfg(test)]
use crate::span::StripLocations;

use super::{
    ParseError,
    attributes::{Attributes, Visibility},
    core::Parser,
    types::{Ident, TypeParameter},
};

mod bitflags;
mod enums;
mod misc;
mod types;

pub use bitflags::{BitflagsDefItem, BitflagsDefinition, BitflagsStatement};
pub use enums::{EnumDefItem, EnumDefinition, EnumStatement};
pub use misc::{ConstDefinition, ExternValueDefinition, TypeAliasDefinition};
pub use types::{TypeDefItem, TypeDefinition, TypeField, TypeStatement};

/// Comment node types
#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub enum Comment {
    /// Doc comment for outer items (///)
    DocOuter {
        lines: Vec<String>,
        location: ItemLocation,
    },
    /// Doc comment for inner items (//!)
    DocInner {
        lines: Vec<String>,
        location: ItemLocation,
    },
    /// Regular comment (//)
    Regular {
        text: String,
        location: ItemLocation,
    },
    /// Multiline comment (/* */)
    MultiLine {
        lines: Vec<String>,
        location: ItemLocation,
    },
}

// items
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub enum ItemDefinitionInner {
    Type(TypeDefinition),
    Enum(EnumDefinition),
    Bitflags(BitflagsDefinition),
    TypeAlias(TypeAliasDefinition),
    Constant(ConstDefinition),
    ExternValue(ExternValueDefinition),
}
impl From<TypeDefinition> for ItemDefinitionInner {
    fn from(item: TypeDefinition) -> Self {
        ItemDefinitionInner::Type(item)
    }
}
impl From<EnumDefinition> for ItemDefinitionInner {
    fn from(item: EnumDefinition) -> Self {
        ItemDefinitionInner::Enum(item)
    }
}
impl From<BitflagsDefinition> for ItemDefinitionInner {
    fn from(item: BitflagsDefinition) -> Self {
        ItemDefinitionInner::Bitflags(item)
    }
}
impl From<TypeAliasDefinition> for ItemDefinitionInner {
    fn from(item: TypeAliasDefinition) -> Self {
        ItemDefinitionInner::TypeAlias(item)
    }
}
impl From<ConstDefinition> for ItemDefinitionInner {
    fn from(item: ConstDefinition) -> Self {
        ItemDefinitionInner::Constant(item)
    }
}
impl From<ExternValueDefinition> for ItemDefinitionInner {
    fn from(item: ExternValueDefinition) -> Self {
        ItemDefinitionInner::ExternValue(item)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct ItemDefinition {
    pub visibility: Visibility,
    pub name: Ident,
    /// Type parameters for generic types (e.g., `[T, U]` in `type Map<T, U>`)
    pub type_parameters: Vec<TypeParameter>,
    pub doc_comments: Vec<String>,
    pub inner: ItemDefinitionInner,
    /// Span of the whole item, including leading doc comments and attributes.
    /// Used by the formatter (to reconstruct blank-line spacing) and for
    /// diagnostics.
    pub location: ItemLocation,
    /// Position of the declaration itself (the `pub`/keyword), excluding leading
    /// doc comments and attributes. Used for documentation source links so they
    /// point at the definition line rather than its first attribute.
    pub declaration_location: ItemLocation,
}
impl Default for ItemDefinition {
    fn default() -> Self {
        Self {
            visibility: Visibility::Private,
            name: Ident::from(""),
            type_parameters: vec![],
            doc_comments: vec![],
            inner: ItemDefinitionInner::Type(TypeDefinition::default()),
            location: ItemLocation::internal(),
            declaration_location: ItemLocation::internal(),
        }
    }
}
#[cfg(test)]
impl ItemDefinition {
    pub fn new(
        (visibility, name): (Visibility, &str),
        inner: impl Into<ItemDefinitionInner>,
    ) -> Self {
        Self {
            visibility,
            name: name.into(),
            type_parameters: vec![],
            doc_comments: vec![],
            inner: inner.into(),
            location: ItemLocation::test(),
            declaration_location: ItemLocation::test(),
        }
    }
    pub fn generic(
        (visibility, name): (Visibility, &str),
        type_parameters: impl IntoIterator<Item = TypeParameter>,
        inner: impl Into<ItemDefinitionInner>,
    ) -> Self {
        Self {
            visibility,
            name: name.into(),
            type_parameters: type_parameters.into_iter().collect(),
            doc_comments: vec![],
            inner: inner.into(),
            location: ItemLocation::test(),
            declaration_location: ItemLocation::test(),
        }
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
    pub fn with_type_parameters(
        mut self,
        type_parameters: impl IntoIterator<Item = TypeParameter>,
    ) -> Self {
        self.type_parameters = type_parameters.into_iter().collect();
        self
    }
}

/// Whether an item carries its own closing token or expects a terminator from
/// whatever context it appears in.
///
/// Brace-delimited items (`type Name { .. }`, `enum`, `bitflags`) close with `}`
/// and need nothing after them. Value-like items (`const`, `extern`, a type
/// alias, or an opaque `type Name`) are terminated by a separator supplied by
/// the context: `;` at module level, an optional `,` inside a
/// type/enum/bitflags body. Consuming that separator is the caller's job so each
/// context can enforce its own rule. Derive this from a parsed item with
/// [`ItemDefinition::terminator`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ItemTerminator {
    SelfTerminating,
    Separated,
}

impl ItemDefinition {
    /// How this item is terminated. Brace-delimited definitions are
    /// self-terminating; value-like items (including opaque `type Name`) expect
    /// a caller-supplied separator.
    pub(crate) fn terminator(&self) -> ItemTerminator {
        match &self.inner {
            ItemDefinitionInner::Type(td) if !td.is_opaque => ItemTerminator::SelfTerminating,
            ItemDefinitionInner::Enum(_) | ItemDefinitionInner::Bitflags(_) => {
                ItemTerminator::SelfTerminating
            }
            _ => ItemTerminator::Separated,
        }
    }
}

impl Parser {
    pub(crate) fn parse_item_definition(&mut self) -> Result<ItemDefinition, ParseError> {
        // Capture the start position
        let start_pos = self.current().location.span.start;

        let mut doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        // Remember the line where attributes ended (or where we currently are if no attributes)
        // We need to check this before we start collecting comments
        let attributes_end_line = if !attributes.0.is_empty() && self.pos > 0 {
            // Get the line from the previous token (the ] that closed the attributes)
            self.tokens[self.pos - 1].location.span.end.line
        } else {
            // No attributes, so comments can't be inline with them
            0 // Use 0 as a sentinel value that won't match any real line
        };

        // Collect comments after attributes, separating inline from following
        let mut inline_trailing_comments = Vec::new();
        let mut following_comments = Vec::new();
        while matches!(
            self.peek(),
            TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
        ) {
            let comment_line = self.current().location.span.start.line;
            if let Some(comment) = self.collect_comment() {
                if comment_line == attributes_end_line {
                    // Comment is on the same line as the attributes
                    inline_trailing_comments.push(comment);
                } else {
                    // Comment is on a following line
                    following_comments.push(comment);
                }
            }
        }

        // Also collect doc comments that appear after attributes
        let after_attr_doc_comments = self.collect_doc_comments();
        doc_comments.extend(after_attr_doc_comments);

        // The declaration starts here, at the `pub`/keyword — after any doc
        // comments and attributes. Source links use this so they point at the
        // definition line, not its first attribute.
        let declaration_start = self.current().location.span.start;
        let declaration_location =
            self.item_location_from_locations(declaration_start, declaration_start);

        let visibility = self.parse_visibility()?;

        match self.peek() {
            TokenKind::Type => {
                self.advance();
                let (name, _) = self.expect_ident()?;

                // Parse optional type parameters: type Name<T, U> { ... }
                let type_parameters = self.parse_type_parameters()?;

                // Check if this is a type alias (= Type), a type definition
                // ({ ... }), or an opaque type (bare `type Name`).
                if matches!(self.peek(), TokenKind::Eq) {
                    // Type alias: type Name = TargetType
                    // Type aliases don't support type parameters (yet). The
                    // terminator is left to the caller.
                    self.advance(); // Consume '='
                    let target = self.parse_type()?;

                    // Capture the end position
                    let end_pos = if self.pos > 0 {
                        self.tokens[self.pos - 1].location.span.end
                    } else {
                        self.current().location.span.end
                    };

                    let location = self.item_location_from_locations(start_pos, end_pos);
                    Ok(ItemDefinition {
                        visibility,
                        name,
                        type_parameters,
                        doc_comments,
                        inner: ItemDefinitionInner::TypeAlias(TypeAliasDefinition {
                            target,
                            attributes,
                            location,
                        }),
                        location,
                        declaration_location,
                    })
                } else {
                    // Type definition: type Name { ... } or opaque type Name.
                    let mut def = TypeDefinition {
                        items: Vec::new(),
                        attributes,
                        is_opaque: false,
                        inline_trailing_comments: inline_trailing_comments.clone(),
                        following_comments: following_comments.clone(),
                    };

                    // A braced body is self-terminating; an opaque `type Name`
                    // (no body) is terminated by the caller (`;` at module level,
                    // optional `,` in a body). The distinction is recorded on the
                    // definition via `is_opaque` — see `ItemDefinition::terminator`.
                    if matches!(self.peek(), TokenKind::LBrace) {
                        self.advance(); // Consume '{'
                        def.items = self.parse_type_def_items()?;
                        self.expect(TokenKind::RBrace)?;
                    } else {
                        def.is_opaque = true;
                    }

                    // Capture the end position
                    let end_pos = if self.pos > 0 {
                        self.tokens[self.pos - 1].location.span.end
                    } else {
                        self.current().location.span.end
                    };

                    let location = self.item_location_from_locations(start_pos, end_pos);
                    Ok(ItemDefinition {
                        visibility,
                        name,
                        type_parameters,
                        doc_comments,
                        inner: ItemDefinitionInner::Type(def),
                        location,
                        declaration_location,
                    })
                }
            }
            TokenKind::Enum => {
                self.advance();
                let (name, _) = self.expect_ident()?;
                self.expect(TokenKind::Colon)?;
                let type_ = self.parse_type()?;
                self.expect(TokenKind::LBrace)?;
                let items = self.parse_enum_def_items()?;
                self.expect(TokenKind::RBrace)?;

                // Capture the end position
                let end_pos = if self.pos > 0 {
                    self.tokens[self.pos - 1].location.span.end
                } else {
                    self.current().location.span.end
                };

                let location = self.item_location_from_locations(start_pos, end_pos);
                Ok(ItemDefinition {
                    visibility,
                    name,
                    type_parameters: vec![], // Enums don't support type parameters
                    doc_comments,
                    inner: ItemDefinitionInner::Enum(EnumDefinition {
                        type_,
                        items,
                        attributes,
                        inline_trailing_comments: inline_trailing_comments.clone(),
                        following_comments: following_comments.clone(),
                    }),
                    location,
                    declaration_location,
                })
            }
            TokenKind::Bitflags => {
                self.advance();
                let (name, _) = self.expect_ident()?;
                self.expect(TokenKind::Colon)?;
                let type_ = self.parse_type()?;
                self.expect(TokenKind::LBrace)?;
                let items = self.parse_bitflags_def_items()?;
                self.expect(TokenKind::RBrace)?;

                // Capture the end position
                let end_pos = if self.pos > 0 {
                    self.tokens[self.pos - 1].location.span.end
                } else {
                    self.current().location.span.end
                };

                let location = self.item_location_from_locations(start_pos, end_pos);
                Ok(ItemDefinition {
                    visibility,
                    name,
                    type_parameters: vec![], // Bitflags don't support type parameters
                    doc_comments,
                    inner: ItemDefinitionInner::Bitflags(BitflagsDefinition {
                        type_,
                        items,
                        attributes,
                        inline_trailing_comments,
                        following_comments,
                    }),
                    location,
                    declaration_location,
                })
            }
            TokenKind::Const => {
                self.advance(); // consume `const`
                let (name, _) = self.expect_ident()?;
                self.expect(TokenKind::Colon)?;
                let type_ = self.parse_type()?;
                self.expect(TokenKind::Eq)?;
                let expr = self.parse_expr()?;

                let end_pos = if self.pos > 0 {
                    self.tokens[self.pos - 1].location.span.end
                } else {
                    self.current().location.span.end
                };

                let location = self.item_location_from_locations(start_pos, end_pos);
                Ok(ItemDefinition {
                    visibility,
                    name,
                    type_parameters: vec![], // Constants don't support type parameters
                    doc_comments,
                    inner: ItemDefinitionInner::Constant(ConstDefinition {
                        type_,
                        expr,
                        attributes,
                        location,
                    }),
                    location,
                    declaration_location,
                })
            }
            TokenKind::Extern => {
                self.advance(); // consume `extern`
                let (name, _) = self.expect_ident()?;
                self.expect(TokenKind::Colon)?;
                let type_ = self.parse_type()?;

                let end_pos = if self.pos > 0 {
                    self.tokens[self.pos - 1].location.span.end
                } else {
                    self.current().location.span.end
                };

                let location = self.item_location_from_locations(start_pos, end_pos);
                Ok(ItemDefinition {
                    visibility,
                    name,
                    type_parameters: vec![], // Extern values don't support type parameters
                    doc_comments,
                    inner: ItemDefinitionInner::ExternValue(ExternValueDefinition {
                        type_,
                        attributes,
                        location,
                    }),
                    location,
                    declaration_location,
                })
            }
            _ => Err(ParseError::ExpectedItemDefinition {
                found: self.peek().clone(),
                location: self.current().location,
            }),
        }
    }

    /// Parse optional type parameters: `<T, U, V>`
    pub(crate) fn parse_type_parameters(&mut self) -> Result<Vec<TypeParameter>, ParseError> {
        if !matches!(self.peek(), TokenKind::Lt) {
            return Ok(vec![]);
        }

        self.advance(); // consume <
        let mut params = Vec::new();

        // Parse first type parameter (if any)
        if !matches!(self.peek(), TokenKind::Gt) {
            let (ident, span) = self.expect_ident()?;
            let location = self.item_location_from_locations(span.start, span.end);
            params.push(TypeParameter {
                name: ident.0,
                location,
            });

            // Parse remaining comma-separated type parameters
            while matches!(self.peek(), TokenKind::Comma) {
                self.advance(); // consume ,
                let (ident, span) = self.expect_ident()?;
                let location = self.item_location_from_locations(span.start, span.end);
                params.push(TypeParameter {
                    name: ident.0,
                    location,
                });
            }
        }

        self.expect(TokenKind::Gt)?;
        Ok(params)
    }

    /// Whether the tokens at `pos` (already advanced past any leading doc
    /// comments, attributes, and comments) begin a nested item declaration:
    /// `type`/`enum`/`bitflags`/`const`, or an `extern <name>: T` value — each
    /// optionally `pub`. Note `extern type ...` is deliberately excluded: extern
    /// types are module-level only, so `extern` counts as a nested item only
    /// when it is *not* immediately followed by `type`.
    pub(super) fn peek_is_nested_item(&self, pos: usize) -> bool {
        fn is_item_kw(kind: Option<&TokenKind>) -> bool {
            matches!(
                kind,
                Some(TokenKind::Type | TokenKind::Enum | TokenKind::Bitflags | TokenKind::Const)
            )
        }
        let is_extern_value = |pos: usize| {
            matches!(self.peek_at(pos), Some(TokenKind::Extern))
                && !matches!(self.peek_at(pos + 1), Some(TokenKind::Type))
        };
        if is_item_kw(self.peek_at(pos)) || is_extern_value(pos) {
            return true;
        }
        if matches!(self.peek_at(pos), Some(TokenKind::Pub)) {
            return is_item_kw(self.peek_at(pos + 1)) || is_extern_value(pos + 1);
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_str_for_tests;

    #[test]
    fn can_parse_nested_enum() {
        let text = r#"
        pub type Outer {
            pub field: u32,
            pub enum InnerEnum: u8 {
                A,
                B,
                C,
            }
        }
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn can_parse_nested_type() {
        let text = r#"
        pub type Outer {
            pub field: u32,
            pub type InnerType {
                pub inner_field: u16,
            }
        }
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn can_parse_nested_type_alias() {
        let text = r#"
        pub type Outer {
            pub field: u32,
            pub type InnerAlias = u32,
        }
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn can_parse_nested_items_and_fields() {
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
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }
}
