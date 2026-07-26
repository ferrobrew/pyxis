use crate::tokenizer::TokenKind;

#[cfg(test)]
use crate::span::StripLocations;

use super::{Comment, TypeDefItem, TypeStatement};
use crate::parser::{ParseError, attributes::Attributes, core::Parser};

#[cfg(test)]
use crate::parser::attributes::Attribute;

/// A `union` body: a set of competing readings of the same bytes, all starting
/// at the same offset.
///
/// The body reuses [`TypeDefItem`]/[`TypeStatement`] rather than defining a
/// parallel statement type, so comment attribution, attributes, and nested item
/// declarations work exactly as they do in a `type` body. Constructs that make
/// no sense in a union — `vftable` blocks, `#[base]` fields — parse fine here
/// and are rejected in the semantic layer, where the error can carry a span
/// pointing at the offending statement.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct UnionDefinition {
    pub items: Vec<TypeDefItem>,
    pub attributes: Attributes,
    pub inline_trailing_comments: Vec<Comment>, // Comments on same line as attributes
    pub following_comments: Vec<Comment>,       // Comments on lines after attributes
}

#[cfg(test)]
impl StripLocations for UnionDefinition {
    fn strip_locations(&self) -> Self {
        UnionDefinition {
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    TypeDefItem::Comment(_) => None, // Filter out comments
                    TypeDefItem::Statement(s) => Some(TypeDefItem::Statement(s.strip_locations())),
                })
                .collect(),
            attributes: self.attributes.strip_locations(),
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
        }
    }
}

#[cfg(test)]
impl UnionDefinition {
    pub fn new(statements: impl IntoIterator<Item = TypeStatement>) -> Self {
        Self {
            items: statements.into_iter().map(TypeDefItem::Statement).collect(),
            attributes: Default::default(),
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

impl UnionDefinition {
    pub fn statements(&self) -> impl Iterator<Item = &TypeStatement> {
        self.items.iter().filter_map(|item| match item {
            TypeDefItem::Statement(stmt) => Some(stmt),
            _ => None,
        })
    }
}

impl Parser {
    /// Parse a `{ … }` union body. The caller has already consumed the `union`
    /// keyword (and, for a named union, the name).
    pub(crate) fn parse_union_body(&mut self) -> Result<Vec<TypeDefItem>, ParseError> {
        self.expect(TokenKind::LBrace)?;
        let items = self.parse_type_def_items()?;
        self.expect(TokenKind::RBrace)?;
        Ok(items)
    }
}
