use crate::{
    span::{HasLocation, ItemLocation},
    tokenizer::TokenKind,
};

#[cfg(test)]
use crate::span::StripLocations;

use super::{Comment, ItemDefinition};
use crate::parser::{
    ParseError,
    attributes::{Attributes, Visibility},
    core::Parser,
    functions::Function,
    types::{Ident, Type},
};

#[cfg(test)]
use crate::parser::attributes::Attribute;

// types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub enum TypeField {
    Field(Visibility, Ident, Type),
    Vftable(Vec<Function>),
    /// A nested item declaration (enum, type, bitflags, type alias) inside a `type` body.
    Item(Box<ItemDefinition>),
}
#[cfg(test)]
impl TypeField {
    pub fn field(
        visibility: Visibility,
        name: impl Into<Ident>,
        type_: impl Into<Type>,
    ) -> TypeField {
        TypeField::Field(visibility, name.into(), type_.into())
    }

    pub fn vftable(functions: impl IntoIterator<Item = Function>) -> TypeField {
        TypeField::Vftable(functions.into_iter().collect())
    }

    pub fn item(item: ItemDefinition) -> TypeField {
        TypeField::Item(Box::new(item))
    }
}
impl TypeField {
    pub fn is_vftable(&self) -> bool {
        matches!(self, TypeField::Vftable(_))
    }
}

/// Items in a type definition body (preserves ordering and comments)
#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub enum TypeDefItem {
    Comment(Comment),
    Statement(TypeStatement),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct TypeStatement {
    pub field: TypeField,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    #[cfg_attr(test, strip_locations(skip))]
    pub inline_trailing_comments: Vec<Comment>, // Comments on same line as field
    #[cfg_attr(test, strip_locations(skip))]
    pub following_comments: Vec<Comment>, // Comments on lines after field
    pub location: ItemLocation,
}
#[cfg(test)]
impl TypeStatement {
    pub fn field((visibility, name): (Visibility, &str), type_: Type) -> TypeStatement {
        TypeStatement {
            field: TypeField::Field(visibility, name.into(), type_),
            attributes: Default::default(),
            doc_comments: vec![],
            inline_trailing_comments: Vec::new(),
            following_comments: Vec::new(),
            location: ItemLocation::test(),
        }
    }
    pub fn vftable(functions: impl IntoIterator<Item = Function>) -> TypeStatement {
        TypeStatement {
            field: TypeField::vftable(functions),
            attributes: Default::default(),
            doc_comments: vec![],
            inline_trailing_comments: Vec::new(),
            following_comments: Vec::new(),
            location: ItemLocation::test(),
        }
    }
    pub fn item(item: ItemDefinition) -> TypeStatement {
        TypeStatement {
            field: TypeField::item(item),
            attributes: Default::default(),
            doc_comments: vec![],
            inline_trailing_comments: Vec::new(),
            following_comments: Vec::new(),
            location: ItemLocation::test(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl IntoIterator<Item = Attribute>) -> Self {
        self.attributes = Attributes::from_iter(attributes);
        self
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
    pub fn with_inline_trailing_comments(mut self, inline_trailing_comments: Vec<Comment>) -> Self {
        self.inline_trailing_comments = inline_trailing_comments;
        self
    }
    pub fn with_following_comments(mut self, following_comments: Vec<Comment>) -> Self {
        self.following_comments = following_comments;
        self
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct TypeDefinition {
    pub items: Vec<TypeDefItem>,
    pub attributes: Attributes,
    /// Whether this was written in opaque form (`type Name`, no braces) rather
    /// than with a `{ … }` body. An opaque type and an empty-bodied type both
    /// have no `items`, but they differ in terminator handling: an opaque type
    /// is separator-terminated (caller supplies `;`/`,`) while a braced body is
    /// self-terminating. See [`ItemDefinition::terminator`].
    pub is_opaque: bool,
    pub inline_trailing_comments: Vec<Comment>, // Comments on same line as attributes
    pub following_comments: Vec<Comment>,       // Comments on lines after attributes
}
#[cfg(test)]
impl StripLocations for TypeDefinition {
    fn strip_locations(&self) -> Self {
        TypeDefinition {
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    TypeDefItem::Comment(_) => None, // Filter out comments
                    TypeDefItem::Statement(s) => Some(TypeDefItem::Statement(s.strip_locations())),
                })
                .collect(),
            attributes: self.attributes.strip_locations(),
            is_opaque: self.is_opaque,
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
        }
    }
}
#[cfg(test)]
impl TypeDefinition {
    pub fn new(statements: impl IntoIterator<Item = TypeStatement>) -> Self {
        Self {
            items: statements.into_iter().map(TypeDefItem::Statement).collect(),
            attributes: Default::default(),
            is_opaque: false,
            inline_trailing_comments: Vec::new(),
            following_comments: Vec::new(),
        }
    }

    /// Build an opaque type definition (`type Name`, no body).
    pub fn opaque() -> Self {
        Self {
            is_opaque: true,
            ..Self::new([])
        }
    }
    pub fn with_attributes(mut self, attributes: impl IntoIterator<Item = Attribute>) -> Self {
        self.attributes = Attributes::from_iter(attributes);
        self
    }
    pub fn with_inline_trailing_comments(mut self, inline_trailing_comments: Vec<Comment>) -> Self {
        self.inline_trailing_comments = inline_trailing_comments;
        self
    }
    pub fn with_following_comments(mut self, following_comments: Vec<Comment>) -> Self {
        self.following_comments = following_comments;
        self
    }
}
impl TypeDefinition {
    pub fn statements(&self) -> impl Iterator<Item = &TypeStatement> {
        self.items.iter().filter_map(|item| match item {
            TypeDefItem::Statement(stmt) => Some(stmt),
            _ => None,
        })
    }
}

impl Parser {
    pub(crate) fn parse_type_def_items(&mut self) -> Result<Vec<TypeDefItem>, ParseError> {
        let mut items = Vec::new();

        while !matches!(self.peek(), TokenKind::RBrace) {
            // Collect non-doc comments (doc comments will be collected by parse_type_statement)
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                if let Some(comment) = self.collect_comment() {
                    items.push(TypeDefItem::Comment(comment));
                }
            }

            if matches!(self.peek(), TokenKind::RBrace) {
                break;
            }

            let mut stmt = self.parse_type_statement()?;
            let statement_line = self.current().location.span.end.line;

            // Optional trailing comma
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            }

            // Collect trailing comments after the comma, separating inline from following
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                let comment_line = self.current().location.span.start.line;
                if let Some(comment) = self.collect_comment() {
                    if comment_line == statement_line {
                        // Comment is on the same line as the field
                        stmt.inline_trailing_comments.push(comment);
                    } else {
                        // Comment is on a following line
                        stmt.following_comments.push(comment);
                    }
                }
            }

            items.push(TypeDefItem::Statement(stmt));
        }

        Ok(items)
    }

    pub(crate) fn parse_type_statement(&mut self) -> Result<TypeStatement, ParseError> {
        // Peek ahead past doc comments and attributes to detect nested item
        // declarations (type, enum, bitflags). If found, delegate to
        // parse_item_definition which collects its own doc comments/attributes.
        {
            let mut pos = self.pos;
            // Skip doc comments
            while matches!(self.peek_at(pos), Some(TokenKind::DocOuter(_))) {
                pos += 1;
            }
            // Skip attributes
            if matches!(self.peek_at(pos), Some(TokenKind::Hash)) {
                pos = self.skip_attributes_lookahead(pos);
            }
            // Skip any comments after attributes
            while matches!(
                self.peek_at(pos),
                Some(
                    TokenKind::Comment(_) | TokenKind::MultiLineComment(_) | TokenKind::DocOuter(_)
                )
            ) {
                pos += 1;
            }
            // Check for nested item keywords: Type, Enum, Bitflags, Const, or Pub followed by one of those
            let is_nested_item = self.peek_is_nested_item(pos);
            if is_nested_item {
                // Nested-item terminators inside a body are the optional trailing
                // comma consumed by `parse_type_def_items`, so the item's own
                // terminator kind is irrelevant here.
                let inner_def = self.parse_item_definition()?;
                let location = inner_def.location;
                return Ok(TypeStatement {
                    field: TypeField::Item(Box::new(inner_def)),
                    attributes: Attributes::default(),
                    doc_comments: Vec::new(),
                    inline_trailing_comments: Vec::new(),
                    following_comments: Vec::new(),
                    location,
                });
            }
        }

        let doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        // Span starts at the declaration (vftable / field), not its doc comment
        // / attributes.
        let start_pos = self.current().location.span.start;
        if matches!(self.peek(), TokenKind::Vftable) {
            self.advance();
            self.expect(TokenKind::LBrace)?;
            let functions = self.parse_functions_in_block()?;
            self.expect(TokenKind::RBrace)?;

            let end_pos = if self.pos > 0 {
                self.tokens[self.pos - 1].location.span.end
            } else {
                self.current().location.span.end
            };

            let location = self.item_location_from_locations(start_pos, end_pos);
            Ok(TypeStatement {
                field: TypeField::Vftable(functions),
                attributes,
                doc_comments,
                inline_trailing_comments: Vec::new(), // Will be populated by parse_type_def_items
                following_comments: Vec::new(),
                location,
            })
        } else {
            let visibility = self.parse_visibility()?;
            let (name, _) = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let type_ = self.parse_type()?;

            let end_pos = if self.pos > 0 {
                self.tokens[self.pos - 1].location.span.end
            } else {
                self.current().location.span.end
            };

            let location = self.item_location_from_locations(start_pos, end_pos);
            Ok(TypeStatement {
                field: TypeField::Field(visibility, name, type_),
                attributes,
                doc_comments,
                inline_trailing_comments: Vec::new(), // Will be populated by parse_type_def_items
                following_comments: Vec::new(),
                location,
            })
        }
    }
}

#[cfg(test)]
mod tests;
