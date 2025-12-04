use crate::{
    span::{HasLocation, ItemLocation, Located},
    tokenizer::TokenKind,
};

#[cfg(test)]
use crate::span::StripLocations;

use super::{
    ParseError,
    attributes::{Attributes, Visibility},
    core::Parser,
    expressions::Expr,
    functions::Function,
    types::{Ident, Type},
};

#[cfg(test)]
use super::attributes::Attribute;

/// Comment node types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
impl HasLocation for Comment {
    fn location(&self) -> &ItemLocation {
        match self {
            Comment::DocOuter { location, .. } => location,
            Comment::DocInner { location, .. } => location,
            Comment::Regular { location, .. } => location,
            Comment::MultiLine { location, .. } => location,
        }
    }
}
#[cfg(test)]
impl StripLocations for Comment {
    fn strip_locations(&self) -> Self {
        match self {
            Comment::DocOuter { lines, .. } => Comment::DocOuter {
                lines: lines.strip_locations(),
                location: ItemLocation::test(),
            },
            Comment::DocInner { lines, .. } => Comment::DocInner {
                lines: lines.strip_locations(),
                location: ItemLocation::test(),
            },
            Comment::Regular { text, .. } => Comment::Regular {
                text: text.strip_locations(),
                location: ItemLocation::test(),
            },
            Comment::MultiLine { lines, .. } => Comment::MultiLine {
                lines: lines.strip_locations(),
                location: ItemLocation::test(),
            },
        }
    }
}

// types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeField {
    Field(Visibility, Ident, Type),
    Vftable(Vec<Function>),
}
#[cfg(test)]
impl StripLocations for TypeField {
    fn strip_locations(&self) -> Self {
        match self {
            TypeField::Field(v, n, t) => TypeField::Field(
                v.strip_locations(),
                n.strip_locations(),
                t.strip_locations(),
            ),
            TypeField::Vftable(funcs) => TypeField::Vftable(funcs.strip_locations()),
        }
    }
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
}
impl TypeField {
    pub fn is_vftable(&self) -> bool {
        matches!(self, TypeField::Vftable(_))
    }
}

/// Items in a type definition body (preserves ordering and comments)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeDefItem {
    Comment(Comment),
    Statement(TypeStatement),
}
impl HasLocation for TypeDefItem {
    fn location(&self) -> &ItemLocation {
        match self {
            TypeDefItem::Comment(c) => c.location(),
            TypeDefItem::Statement(s) => s.location(),
        }
    }
}
#[cfg(test)]
impl StripLocations for TypeDefItem {
    fn strip_locations(&self) -> Self {
        match self {
            TypeDefItem::Comment(comment) => TypeDefItem::Comment(comment.strip_locations()),
            TypeDefItem::Statement(statement) => {
                TypeDefItem::Statement(statement.strip_locations())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeStatement {
    pub field: TypeField,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    pub inline_trailing_comments: Vec<Comment>, // Comments on same line as field
    pub following_comments: Vec<Comment>,       // Comments on lines after field
    pub location: ItemLocation,
}
impl HasLocation for TypeStatement {
    fn location(&self) -> &ItemLocation {
        &self.location
    }
}
#[cfg(test)]
impl StripLocations for TypeStatement {
    fn strip_locations(&self) -> Self {
        TypeStatement {
            field: self.field.strip_locations(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.strip_locations(),
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
            location: ItemLocation::test(),
        }
    }
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
pub struct TypeDefinition {
    pub items: Vec<TypeDefItem>,
    pub attributes: Attributes,
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
impl TypeDefinition {
    pub fn statements(&self) -> impl Iterator<Item = &TypeStatement> {
        self.items.iter().filter_map(|item| match item {
            TypeDefItem::Statement(stmt) => Some(stmt),
            _ => None,
        })
    }
}

// enums
/// Items in an enum definition (preserves ordering and comments)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EnumDefItem {
    Comment(Comment),
    Statement(EnumStatement),
}
impl HasLocation for EnumDefItem {
    fn location(&self) -> &ItemLocation {
        match self {
            EnumDefItem::Comment(c) => c.location(),
            EnumDefItem::Statement(s) => s.location(),
        }
    }
}
#[cfg(test)]
impl StripLocations for EnumDefItem {
    fn strip_locations(&self) -> Self {
        match self {
            EnumDefItem::Comment(comment) => EnumDefItem::Comment(comment.strip_locations()),
            EnumDefItem::Statement(statement) => {
                EnumDefItem::Statement(statement.strip_locations())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumStatement {
    pub name: Ident,
    pub expr: Option<Expr>,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    pub inline_trailing_comments: Vec<Comment>, // Comments on same line as enum variant
    pub following_comments: Vec<Comment>,       // Comments on lines after enum variant
    pub location: ItemLocation,
}
impl HasLocation for EnumStatement {
    fn location(&self) -> &ItemLocation {
        &self.location
    }
}
#[cfg(test)]
impl StripLocations for EnumStatement {
    fn strip_locations(&self) -> Self {
        EnumStatement {
            name: self.name.strip_locations(),
            expr: self.expr.strip_locations(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.strip_locations(),
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
            location: ItemLocation::test(),
        }
    }
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

// bitflags
/// Items in a bitflags definition (preserves ordering and comments)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BitflagsDefItem {
    Comment(Comment),
    Statement(BitflagsStatement),
}
impl HasLocation for BitflagsDefItem {
    fn location(&self) -> &ItemLocation {
        match self {
            BitflagsDefItem::Comment(c) => c.location(),
            BitflagsDefItem::Statement(s) => s.location(),
        }
    }
}
#[cfg(test)]
impl StripLocations for BitflagsDefItem {
    fn strip_locations(&self) -> Self {
        match self {
            BitflagsDefItem::Comment(comment) => {
                BitflagsDefItem::Comment(comment.strip_locations())
            }
            BitflagsDefItem::Statement(statement) => {
                BitflagsDefItem::Statement(statement.strip_locations())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BitflagsStatement {
    pub name: Ident,
    pub expr: Expr,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    pub inline_trailing_comments: Vec<Comment>, // Comments on same line as bitflag
    pub following_comments: Vec<Comment>,       // Comments on lines after bitflag
    pub location: ItemLocation,
}
impl HasLocation for BitflagsStatement {
    fn location(&self) -> &ItemLocation {
        &self.location
    }
}
#[cfg(test)]
impl StripLocations for BitflagsStatement {
    fn strip_locations(&self) -> Self {
        BitflagsStatement {
            name: self.name.strip_locations(),
            expr: self.expr.strip_locations(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.strip_locations(),
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
            location: ItemLocation::test(),
        }
    }
}
#[cfg(test)]
impl BitflagsStatement {
    pub fn new(name: Ident, expr: Expr) -> BitflagsStatement {
        BitflagsStatement {
            name,
            expr,
            attributes: Default::default(),
            doc_comments: vec![],
            inline_trailing_comments: Vec::new(),
            following_comments: Vec::new(),
            location: ItemLocation::test(),
        }
    }
    pub fn field(name: &str, expr: Expr) -> BitflagsStatement {
        Self::new(name.into(), expr)
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
pub struct BitflagsDefinition {
    pub type_: Type,
    pub items: Vec<BitflagsDefItem>,
    pub attributes: Attributes,
    pub inline_trailing_comments: Vec<Comment>, // Comments on same line as attributes
    pub following_comments: Vec<Comment>,       // Comments on lines after attributes
}
#[cfg(test)]
impl StripLocations for BitflagsDefinition {
    fn strip_locations(&self) -> Self {
        BitflagsDefinition {
            type_: self.type_.strip_locations(),
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    BitflagsDefItem::Comment(_) => None, // Filter out comments
                    BitflagsDefItem::Statement(s) => {
                        Some(BitflagsDefItem::Statement(s.strip_locations()))
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
impl BitflagsDefinition {
    pub fn new(
        type_: Type,
        statements: impl IntoIterator<Item = BitflagsStatement>,
        attributes: impl IntoIterator<Item = Attribute>,
    ) -> Self {
        Self {
            type_,
            items: statements
                .into_iter()
                .map(BitflagsDefItem::Statement)
                .collect(),
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
impl BitflagsDefinition {
    pub fn statements(&self) -> impl Iterator<Item = &BitflagsStatement> {
        self.items.iter().filter_map(|item| match item {
            BitflagsDefItem::Statement(stmt) => Some(stmt),
            _ => None,
        })
    }
}

// items
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ItemDefinitionInner {
    Type(TypeDefinition),
    Enum(EnumDefinition),
    Bitflags(BitflagsDefinition),
}
#[cfg(test)]
impl StripLocations for ItemDefinitionInner {
    fn strip_locations(&self) -> Self {
        match self {
            ItemDefinitionInner::Type(t) => ItemDefinitionInner::Type(t.strip_locations()),
            ItemDefinitionInner::Enum(e) => ItemDefinitionInner::Enum(e.strip_locations()),
            ItemDefinitionInner::Bitflags(b) => ItemDefinitionInner::Bitflags(b.strip_locations()),
        }
    }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemDefinition {
    pub visibility: Visibility,
    pub name: Ident,
    pub doc_comments: Vec<String>,
    pub inner: ItemDefinitionInner,
}
#[cfg(test)]
impl StripLocations for ItemDefinition {
    fn strip_locations(&self) -> Self {
        ItemDefinition {
            visibility: self.visibility.strip_locations(),
            name: self.name.strip_locations(),
            doc_comments: self.doc_comments.strip_locations(),
            inner: self.inner.strip_locations(),
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
            doc_comments: vec![],
            inner: inner.into(),
        }
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
}

impl Parser {
    pub(crate) fn parse_item_definition(&mut self) -> Result<Located<ItemDefinition>, ParseError> {
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

        let visibility = self.parse_visibility()?;

        match self.peek() {
            TokenKind::Type => {
                self.advance();
                let (name, _) = self.expect_ident()?;
                let mut def = TypeDefinition {
                    items: Vec::new(),
                    attributes,
                    inline_trailing_comments: inline_trailing_comments.clone(),
                    following_comments: following_comments.clone(),
                };

                // Support both "type Name;" and "type Name { ... }"
                if matches!(self.peek(), TokenKind::Semi) {
                    self.advance(); // Consume semicolon
                } else {
                    self.expect(TokenKind::LBrace)?;
                    def.items = self.parse_type_def_items()?;
                    self.expect(TokenKind::RBrace)?;
                }

                // Capture the end position
                let end_pos = if self.pos > 0 {
                    self.tokens[self.pos - 1].location.span.end
                } else {
                    self.current().location.span.end
                };

                let location = self.item_location_from_locations(start_pos, end_pos);
                Ok(Located::new(
                    ItemDefinition {
                        visibility,
                        name,
                        doc_comments,
                        inner: ItemDefinitionInner::Type(def),
                    },
                    location,
                ))
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
                Ok(Located::new(
                    ItemDefinition {
                        visibility,
                        name,
                        doc_comments,
                        inner: ItemDefinitionInner::Enum(EnumDefinition {
                            type_,
                            items,
                            attributes,
                            inline_trailing_comments: inline_trailing_comments.clone(),
                            following_comments: following_comments.clone(),
                        }),
                    },
                    location,
                ))
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
                Ok(Located::new(
                    ItemDefinition {
                        visibility,
                        name,
                        doc_comments,
                        inner: ItemDefinitionInner::Bitflags(BitflagsDefinition {
                            type_,
                            items,
                            attributes,
                            inline_trailing_comments,
                            following_comments,
                        }),
                    },
                    location,
                ))
            }
            _ => Err(ParseError::ExpectedItemDefinition {
                found: self.peek().clone(),
                location: self.current().location.clone(),
            }),
        }
    }

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
        let start_pos = self.current().location.span.start;

        let doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

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
        let start_pos = self.current().location.span.start;

        let doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

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

    pub(crate) fn parse_bitflags_def_items(&mut self) -> Result<Vec<BitflagsDefItem>, ParseError> {
        let mut items = Vec::new();

        while !matches!(self.peek(), TokenKind::RBrace) {
            // Collect non-doc comments (doc comments will be collected by parse_bitflags_statement)
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                if let Some(comment) = self.collect_comment() {
                    items.push(BitflagsDefItem::Comment(comment));
                }
            }

            if matches!(self.peek(), TokenKind::RBrace) {
                break;
            }

            let mut stmt = self.parse_bitflags_statement()?;
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
                        // Comment is on the same line as the bitflag
                        stmt.inline_trailing_comments.push(comment);
                    } else {
                        // Comment is on a following line
                        stmt.following_comments.push(comment);
                    }
                }
            }

            items.push(BitflagsDefItem::Statement(stmt));
        }

        Ok(items)
    }

    pub(crate) fn parse_bitflags_statement(&mut self) -> Result<BitflagsStatement, ParseError> {
        let start_pos = self.current().location.span.start;

        let doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        let (name, _) = self.expect_ident()?;
        self.expect(TokenKind::Eq)?;
        let expr = self.parse_expr()?;

        let end_pos = if self.pos > 0 {
            self.tokens[self.pos - 1].location.span.end
        } else {
            self.current().location.span.end
        };

        let location = self.item_location_from_locations(start_pos, end_pos);
        Ok(BitflagsStatement {
            name,
            expr,
            attributes,
            doc_comments,
            inline_trailing_comments: Vec::new(), // Will be populated by parse_bitflags_def_items
            following_comments: Vec::new(),
            location,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        grammar::{
            IntFormat,
            test_aliases::{int_literal, int_literal_with_format, *},
        },
        parser::parse_str_for_tests,
        span::StripLocations,
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn can_parse_enum() {
        let text = r#"
        #[singleton(0x1234)]
        pub enum TestType: u32 {
            Item0 = -5,
            #[default]
            Item1,
            Item2,
            Item3 = 10,
            Item4
        }
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            ED::new(
                T::ident("u32"),
                [
                    ES::field_with_expr("Item0", int_literal(-5)),
                    ES::field("Item1").with_attributes([A::default()]),
                    ES::field("Item2"),
                    ES::field_with_expr("Item3", int_literal(10)),
                    ES::field("Item4"),
                ],
                [A::singleton(0x1234)],
            ),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_bitflags() {
        let text = r#"
        #[singleton(0x1234)]
        pub bitflags TestType: u32 {
            #[default]
            Item1 = 0b0001,
            Item2 = 0b0010,
            Item3 = 0b0100,
            Item4 = 0b1000,
        }
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal_with_format(1, IntFormat::Binary))
                        .with_attributes([A::default()]),
                    BFS::field("Item2", int_literal_with_format(2, IntFormat::Binary)),
                    BFS::field("Item3", int_literal_with_format(4, IntFormat::Binary)),
                    BFS::field("Item4", int_literal_with_format(8, IntFormat::Binary)),
                ],
                [A::singleton(0x1234)],
            ),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }
}
