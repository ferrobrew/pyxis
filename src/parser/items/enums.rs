use crate::{
    span::{HasLocation, ItemLocation},
    tokenizer::TokenKind,
};

#[cfg(test)]
use crate::span::StripLocations;

use super::{Comment, ItemDefinition};
use crate::parser::{
    ParseError,
    attributes::Attributes,
    core::Parser,
    expressions::Expr,
    types::{Ident, Type},
};

#[cfg(test)]
use crate::parser::attributes::Attribute;

// enums
/// Items in an enum definition (preserves ordering and comments)
#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub enum EnumDefItem {
    Comment(Comment),
    Statement(EnumStatement),
    /// A nested item declaration (const, type, enum, etc.) inside an enum body.
    Item(Box<ItemDefinition>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct EnumStatement {
    pub name: Ident,
    pub expr: Option<Expr>,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    #[cfg_attr(test, strip_locations(skip))]
    pub inline_trailing_comments: Vec<Comment>, // Comments on same line as enum variant
    #[cfg_attr(test, strip_locations(skip))]
    pub following_comments: Vec<Comment>, // Comments on lines after enum variant
    pub location: ItemLocation,
}
#[cfg(test)]
impl EnumStatement {
    pub fn new(name: Ident, expr: Option<Expr>) -> EnumStatement {
        EnumStatement {
            name,
            expr,
            attributes: Default::default(),
            doc_comments: vec![],
            inline_trailing_comments: Vec::new(),
            following_comments: Vec::new(),
            location: ItemLocation::test(),
        }
    }
    pub fn field(name: &str) -> EnumStatement {
        Self::new(name.into(), None)
    }
    pub fn field_with_expr(name: &str, expr: Expr) -> EnumStatement {
        Self::new(name.into(), Some(expr))
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDefinition {
    pub type_: Type,
    pub items: Vec<EnumDefItem>,
    pub attributes: Attributes,
    pub inline_trailing_comments: Vec<Comment>, // Comments on same line as attributes
    pub following_comments: Vec<Comment>,       // Comments on lines after attributes
}
#[cfg(test)]
impl StripLocations for EnumDefinition {
    fn strip_locations(&self) -> Self {
        EnumDefinition {
            type_: self.type_.strip_locations(),
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    EnumDefItem::Comment(_) => None, // Filter out comments
                    EnumDefItem::Statement(s) => Some(EnumDefItem::Statement(s.strip_locations())),
                    EnumDefItem::Item(inner) => {
                        Some(EnumDefItem::Item(Box::new((**inner).strip_locations())))
                    }
                })
                .collect(),
            attributes: self.attributes.strip_locations(),
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
        }
    }
}
#[cfg(test)]
impl EnumDefinition {
    pub fn new(
        type_: Type,
        statements: impl IntoIterator<Item = EnumStatement>,
        attributes: impl IntoIterator<Item = Attribute>,
    ) -> Self {
        Self {
            type_,
            items: statements.into_iter().map(EnumDefItem::Statement).collect(),
            attributes: Attributes::from_iter(attributes),
            inline_trailing_comments: Vec::new(),
            following_comments: Vec::new(),
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
impl EnumDefinition {
    pub fn statements(&self) -> impl Iterator<Item = &EnumStatement> {
        self.items.iter().filter_map(|item| match item {
            EnumDefItem::Statement(stmt) => Some(stmt),
            _ => None,
        })
    }
}

impl Parser {
    pub(crate) fn parse_enum_def_items(&mut self) -> Result<Vec<EnumDefItem>, ParseError> {
        let mut items = Vec::new();

        while !matches!(self.peek(), TokenKind::RBrace) {
            // Collect non-doc comments (doc comments will be collected by parse_enum_statement)
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                if let Some(comment) = self.collect_comment() {
                    items.push(EnumDefItem::Comment(comment));
                }
            }

            if matches!(self.peek(), TokenKind::RBrace) {
                break;
            }

            // Check for nested item declarations (type, enum, bitflags, const)
            // by peeking ahead past doc comments and attributes.
            {
                let mut pos = self.pos;
                while matches!(self.peek_at(pos), Some(TokenKind::DocOuter(_))) {
                    pos += 1;
                }
                if matches!(self.peek_at(pos), Some(TokenKind::Hash)) {
                    pos = self.skip_attributes_lookahead(pos);
                }
                while matches!(
                    self.peek_at(pos),
                    Some(
                        TokenKind::Comment(_)
                            | TokenKind::MultiLineComment(_)
                            | TokenKind::DocOuter(_)
                    )
                ) {
                    pos += 1;
                }
                let is_nested_item = self.peek_is_nested_item(pos);
                if is_nested_item {
                    let inner_def = self.parse_item_definition()?;
                    // Optional trailing comma after the nested item
                    if matches!(self.peek(), TokenKind::Comma) {
                        self.advance();
                    }
                    items.push(EnumDefItem::Item(Box::new(inner_def)));
                    continue;
                }
            }

            let mut stmt = self.parse_enum_statement()?;
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
                        // Comment is on the same line as the enum variant
                        stmt.inline_trailing_comments.push(comment);
                    } else {
                        // Comment is on a following line
                        stmt.following_comments.push(comment);
                    }
                }
            }

            items.push(EnumDefItem::Statement(stmt));
        }

        Ok(items)
    }

    pub(crate) fn parse_enum_statement(&mut self) -> Result<EnumStatement, ParseError> {
        let doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        // Span starts at the declaration, not its doc comment / attributes.
        let start_pos = self.current().location.span.start;
        let (name, _) = self.expect_ident()?;
        let expr = if matches!(self.peek(), TokenKind::Eq) {
            self.advance();
            Some(self.parse_expr()?)
        } else {
            None
        };

        let end_pos = if self.pos > 0 {
            self.tokens[self.pos - 1].location.span.end
        } else {
            self.current().location.span.end
        };

        let location = self.item_location_from_locations(start_pos, end_pos);
        Ok(EnumStatement {
            name,
            expr,
            attributes,
            doc_comments,
            inline_trailing_comments: Vec::new(), // Will be populated by parse_enum_def_items
            following_comments: Vec::new(),
            location,
        })
    }
}

#[cfg(test)]
mod tests;
