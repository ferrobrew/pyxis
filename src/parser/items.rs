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
    expressions::Expr,
    functions::Function,
    types::{Ident, Type, TypeParameter},
};

#[cfg(test)]
use super::attributes::Attribute;

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

// bitflags
/// Items in a bitflags definition (preserves ordering and comments)
#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub enum BitflagsDefItem {
    Comment(Comment),
    Statement(BitflagsStatement),
    /// A nested item declaration (const, type, enum, etc.) inside a bitflags body.
    Item(Box<ItemDefinition>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct BitflagsStatement {
    pub name: Ident,
    pub expr: Expr,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    #[cfg_attr(test, strip_locations(skip))]
    pub inline_trailing_comments: Vec<Comment>, // Comments on same line as bitflag
    #[cfg_attr(test, strip_locations(skip))]
    pub following_comments: Vec<Comment>, // Comments on lines after bitflag
    pub location: ItemLocation,
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
                    BitflagsDefItem::Item(inner) => {
                        Some(BitflagsDefItem::Item(Box::new((**inner).strip_locations())))
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

// type aliases
#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct TypeAliasDefinition {
    pub target: Type,
    pub attributes: Attributes,
    pub location: ItemLocation,
}
#[cfg(test)]
impl TypeAliasDefinition {
    pub fn new(target: Type) -> Self {
        Self {
            target,
            attributes: Default::default(),
            location: ItemLocation::test(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl IntoIterator<Item = Attribute>) -> Self {
        self.attributes = Attributes::from_iter(attributes);
        self
    }
}

// items
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct ConstDefinition {
    pub type_: Type,
    pub expr: Expr,
    pub attributes: Attributes,
    pub location: ItemLocation,
}
#[cfg(test)]
impl ConstDefinition {
    pub fn new(type_: Type, expr: Expr) -> Self {
        Self {
            type_,
            expr,
            attributes: Default::default(),
            location: ItemLocation::test(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl IntoIterator<Item = Attribute>) -> Self {
        self.attributes = Attributes::from_iter(attributes);
        self
    }
}

// items
/// A `pub extern some_value: *mut T;` declaration. The `#[address(...)]`
/// attribute (required at the semantic layer) lives in `attributes`. Like
/// [`ConstDefinition`], this is a value item, not a type; `visibility`,
/// `name`, and `doc_comments` live on the enclosing [`ItemDefinition`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct ExternValueDefinition {
    pub type_: Type,
    pub attributes: Attributes,
    pub location: ItemLocation,
}
#[cfg(test)]
impl ExternValueDefinition {
    pub fn new(type_: Type) -> Self {
        Self {
            type_,
            attributes: Default::default(),
            location: ItemLocation::test(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl IntoIterator<Item = Attribute>) -> Self {
        self.attributes = Attributes::from_iter(attributes);
        self
    }
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

                // Check if this is a type alias (= Type;) or a type definition ({ ... } or ;)
                if matches!(self.peek(), TokenKind::Eq) {
                    // Type alias: type Name = TargetType;
                    // Type aliases don't support type parameters (yet)
                    self.advance(); // Consume '='
                    let target = self.parse_type()?;
                    self.expect(TokenKind::Semi)?;

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
                    // Type definition: type Name { ... } or type Name;
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
                // Module-level consts end with `;`; nested consts (inside
                // type/enum/bitflags bodies) end with `,`. Accept either so
                // the same parser works in both contexts.
                if matches!(self.peek(), TokenKind::Semi | TokenKind::Comma) {
                    self.advance();
                }

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
                // Module-level extern values end with `;`; nested ones (inside
                // type/enum/bitflags bodies) end with `,`. Accept either so the
                // same parser works in both contexts.
                if matches!(self.peek(), TokenKind::Semi | TokenKind::Comma) {
                    self.advance();
                }

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
    fn peek_is_nested_item(&self, pos: usize) -> bool {
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
                    items.push(BitflagsDefItem::Item(Box::new(inner_def)));
                    continue;
                }
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
        let doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        // Span starts at the declaration, not its doc comment / attributes.
        let start_pos = self.current().location.span.start;
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
        parser::{error::ParseError, parse_str_for_tests},
        span::{ItemLocation, StripLocations},
        tokenizer::TokenKind,
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

    #[test]
    fn can_parse_type_alias() {
        let text = r#"
        pub type IntPtr = *const i32;
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Public, "IntPtr"),
            TAD::new(T::const_pointer(T::ident("i32"))),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_type_alias_with_complex_type() {
        let text = r#"
        pub type ArrayPtr = *mut [u32; 16];
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Public, "ArrayPtr"),
            TAD::new(T::mut_pointer(T::array(T::ident("u32"), 16))),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_type_alias_with_path() {
        let text = r#"
        pub type TexturePtr = *const module::Texture;
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Public, "TexturePtr"),
            TAD::new(T::const_pointer(T::ident("module::Texture"))),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_generic_type_definition_single_param() {
        // Generic type with single type parameter
        let text = r#"
        #[size(0x8)]
        pub type Shared<T> {
            pub ptr: *mut T,
        }
        "#;

        let ast = M::new().with_definitions([ID::generic(
            (V::Public, "Shared"),
            [TP::new("T")],
            TD::new([TS::field((V::Public, "ptr"), T::ident("T").mut_pointer())])
                .with_attributes([A::size(8)]),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_generic_type_definition_multiple_params() {
        // Generic type with multiple type parameters
        let text = r#"
        #[size(0x10)]
        pub type Map<K, V> {
            pub key: *mut K,
            pub value: *mut V,
        }
        "#;

        let ast = M::new().with_definitions([ID::generic(
            (V::Public, "Map"),
            [TP::new("K"), TP::new("V")],
            TD::new([
                TS::field((V::Public, "key"), T::ident("K").mut_pointer()),
                TS::field((V::Public, "value"), T::ident("V").mut_pointer()),
            ])
            .with_attributes([A::size(16)]),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_field_with_generic_type_reference() {
        // Type with a field that uses a generic type
        let text = r#"
        #[size(0x8)]
        pub type Container {
            pub shared: Shared<GameObject>,
        }
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Public, "Container"),
            TD::new([TS::field(
                (V::Public, "shared"),
                T::generic("Shared", [T::ident("GameObject")]),
            )])
            .with_attributes([A::size(8)]),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_field_with_nested_generic_type() {
        // Type with a nested generic type (e.g., Shared<Map<K, V>>)
        let text = r#"
        #[size(0x8)]
        pub type Container {
            pub shared_map: Shared<Map<u32, Entity>>,
        }
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Public, "Container"),
            TD::new([TS::field(
                (V::Public, "shared_map"),
                T::generic(
                    "Shared",
                    [T::generic("Map", [T::ident("u32"), T::ident("Entity")])],
                ),
            )])
            .with_attributes([A::size(8)]),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_pointer_to_generic_type() {
        // Pointer to a generic type
        let text = r#"
        #[size(0x8)]
        pub type Container {
            pub ptr: *mut Shared<GameObject>,
        }
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Public, "Container"),
            TD::new([TS::field(
                (V::Public, "ptr"),
                T::generic("Shared", [T::ident("GameObject")]).mut_pointer(),
            )])
            .with_attributes([A::size(8)]),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_array_of_generic_type() {
        // Array of generic type
        let text = r#"
        #[size(0x20)]
        pub type Container {
            pub items: [Shared<Entity>; 4],
        }
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Public, "Container"),
            TD::new([TS::field(
                (V::Public, "items"),
                T::generic("Shared", [T::ident("Entity")]).array(4),
            )])
            .with_attributes([A::size(32)]),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_generic_type_alias_single_param() {
        // Generic type alias with single type parameter
        let text = r#"
        pub type SharedPtr<T> = *mut T;
        "#;

        let ast = M::new().with_definitions([ID::generic(
            (V::Public, "SharedPtr"),
            [TP::new("T")],
            TAD::new(T::ident("T").mut_pointer()),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_generic_type_alias_multiple_params() {
        // Generic type alias with multiple type parameters
        let text = r#"
        pub type MapEntry<K, V> = Pair<K, V>;
        "#;

        let ast = M::new().with_definitions([ID::generic(
            (V::Public, "MapEntry"),
            [TP::new("K"), TP::new("V")],
            TAD::new(T::generic("Pair", [T::ident("K"), T::ident("V")])),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_generic_type_alias_with_generic_target() {
        // Type alias that wraps a generic type
        let text = r#"
        pub type EntityPtr<T> = Shared<T>;
        "#;

        let ast = M::new().with_definitions([ID::generic(
            (V::Public, "EntityPtr"),
            [TP::new("T")],
            TAD::new(T::generic("Shared", [T::ident("T")])),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_generic_type_alias_with_pointer_to_generic() {
        // Generic type alias to pointer of generic type
        let text = r#"
        pub type WeakRef<T> = *const Weak<T>;
        "#;

        let ast = M::new().with_definitions([ID::generic(
            (V::Public, "WeakRef"),
            [TP::new("T")],
            TAD::new(T::generic("Weak", [T::ident("T")]).const_pointer()),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    // ========================================================================
    // Type definition error tests
    // ========================================================================

    #[test]
    fn type_missing_closing_brace() {
        let text = r#"
        pub type TestType {
            field1: i32
        "#;

        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIdentifier {
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn type_field_missing_type() {
        let text = r#"
        pub type TestType {
            field1:,
        }
        "#;

        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedType {
                found: TokenKind::Comma,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn type_missing_name() {
        let text = r#"
        pub type {
            field: i32,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIdentifier {
                found: TokenKind::LBrace,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn type_field_missing_colon() {
        let text = r#"
        type TestType {
            field i32,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Colon],
                found: TokenKind::Ident("i32".to_string()),
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn type_alias_missing_target() {
        let text = r#"
        type IntPtr =;
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedType {
                found: TokenKind::Semi,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn type_alias_missing_semicolon() {
        let text = r#"
        type IntPtr = i32
        type Another {}
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
    fn type_multiple_vftables_parses_ok() {
        // Valid syntax but semantically wrong - parser should accept it
        let text = r#"
        type TestType {
            vftable {},
            vftable {},
        }
        "#;
        // Parses fine - semantic check would catch it
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn empty_type_body_is_valid() {
        let text = r#"
        type Test {}
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn type_with_only_unknown_field() {
        let text = r#"
        type Test {
            _: unknown<16>,
        }
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn generic_type_def_missing_closing_angle() {
        let text = r#"
        type Shared<T {
            field: *mut T,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Gt],
                found: TokenKind::LBrace,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn generic_type_def_empty_params_parses_ok() {
        // Empty type params parse OK - `Shared<>` is just non-generic
        let text = r#"
        type Shared<> {
            field: i32,
        }
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    // ========================================================================
    // Vftable error tests
    // ========================================================================

    #[test]
    fn vftable_missing_opening_brace() {
        let text = r#"
        type TestType {
            vftable
                pub fn test(&self);
            }
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::LBrace],
                found: TokenKind::Pub,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn vftable_missing_closing_brace() {
        let text = r#"
        type TestType {
            vftable {
                pub fn test(&self);
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        // Runs into EOF trying to parse after the unclosed vftable
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIdentifier {
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn vftable_function_missing_semicolon() {
        let text = r#"
        type TestType {
            vftable {
                pub fn test(&self)
            }
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Semi],
                found: TokenKind::RBrace,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn vftable_functions_using_comma_instead_of_semicolon() {
        let text = r#"
        type TestType {
            vftable {
                pub fn test1(&self),
                pub fn test2(&self);
            }
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Semi],
                found: TokenKind::Comma,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn vftable_functions_missing_separator_entirely() {
        let text = r#"
        type TestType {
            vftable {
                pub fn test1(&self)
                pub fn test2(&self);
            }
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Semi],
                found: TokenKind::Pub,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn vftable_functions_missing_separator_private() {
        let text = r#"
        type TestType {
            vftable {
                fn test1(&self)
                fn test2(&self);
            }
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Semi],
                found: TokenKind::Fn,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn vftable_function_missing_fn_keyword() {
        let text = r#"
        type TestType {
            vftable {
                pub test(&self);
            }
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Fn],
                found: TokenKind::Ident("test".to_string()),
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn vftable_function_missing_parentheses() {
        let text = r#"
        type TestType {
            vftable {
                pub fn test;
            }
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::LParen],
                found: TokenKind::Semi,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn vftable_function_missing_closing_paren() {
        let text = r#"
        type TestType {
            vftable {
                pub fn test(&self;
            }
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RParen],
                found: TokenKind::Semi,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn vftable_function_missing_return_type_after_arrow() {
        let text = r#"
        type TestType {
            vftable {
                pub fn test(&self) ->;
            }
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedType {
                found: TokenKind::Semi,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn vftable_function_invalid_self_parameter() {
        let text = r#"
        type TestType {
            vftable {
                pub fn test(self);
            }
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIdentifier {
                found: TokenKind::SelfValue,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn empty_vftable_is_valid() {
        let text = r#"
        type Test {
            vftable {},
        }
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    // ========================================================================
    // Enum error tests
    // ========================================================================

    #[test]
    fn enum_missing_type_annotation() {
        let text = r#"
        pub enum State {
            Idle = 0,
        }
        "#;

        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Colon],
                found: TokenKind::LBrace,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn enum_missing_name() {
        let text = r#"
        pub enum : u32 {
            Item = 0,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIdentifier {
                found: TokenKind::Colon,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn enum_missing_type() {
        let text = r#"
        pub enum State: {
            Item = 0,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedType {
                found: TokenKind::LBrace,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn enum_missing_opening_brace() {
        let text = r#"
        pub enum State: u32
            Item = 0,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::LBrace],
                found: TokenKind::Ident("Item".to_string()),
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn enum_missing_closing_brace() {
        let text = r#"
        pub enum State: u32 {
            Item = 0,
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIdentifier {
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn enum_variant_invalid_expression() {
        let text = r#"
        pub enum State: u32 {
            Item = ,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedExpression {
                found: TokenKind::Comma,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn empty_enum_is_valid() {
        let text = r#"
        enum Test: u32 {}
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    // ========================================================================
    // Bitflags error tests
    // ========================================================================

    #[test]
    fn bitflags_missing_equals() {
        let text = r#"
        pub bitflags Flags: u32 {
            READ 0x1,
        }
        "#;

        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Eq],
                found: TokenKind::IntLiteral("0x1".to_string()),
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn bitflags_missing_name() {
        let text = r#"
        pub bitflags : u32 {
            FLAG = 1,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIdentifier {
                found: TokenKind::Colon,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn bitflags_missing_type() {
        let text = r#"
        pub bitflags Flags: {
            FLAG = 1,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedType {
                found: TokenKind::LBrace,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn bitflags_missing_value() {
        let text = r#"
        pub bitflags Flags: u32 {
            FLAG =,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedExpression {
                found: TokenKind::Comma,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn bitflags_missing_opening_brace() {
        let text = r#"
        pub bitflags Flags: u32
            FLAG = 1,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::LBrace],
                found: TokenKind::Ident("FLAG".to_string()),
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn empty_bitflags_is_valid() {
        let text = r#"
        bitflags Test: u32 {}
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

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
    fn can_parse_nested_bitflags() {
        let text = r#"
        pub type Outer {
            pub field: u32,
            pub bitflags InnerFlags: u32 {
                FLAG_A = 1,
                FLAG_B = 2,
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
            pub type InnerAlias = u32;
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

    #[test]
    fn can_parse_module_level_const() {
        let text = r#"
        pub const MAX_HEALTH: i32 = 100;
        "#;
        let ast = parse_str_for_tests(text).unwrap().strip_locations();
        let module = &ast.items;
        assert_eq!(module.len(), 1);
        match &module[0] {
            crate::grammar::ModuleItem::Definition { definition } => {
                assert_eq!(definition.name.as_str(), "MAX_HEALTH");
                assert!(matches!(
                    &definition.inner,
                    crate::grammar::ItemDefinitionInner::Constant(_)
                ));
            }
            _ => panic!("Expected Definition, got {:?}", module[0]),
        }
    }

    #[test]
    fn can_parse_const_float_value() {
        let text = r#"
        pub const PI: f32 = 3.14159;
        "#;
        let ast = parse_str_for_tests(text).unwrap().strip_locations();
        match &ast.items[0] {
            crate::grammar::ModuleItem::Definition { definition } => {
                let crate::grammar::ItemDefinitionInner::Constant(cd) = &definition.inner else {
                    panic!("Expected Constant");
                };
                assert!(cd.expr.float_literal().is_some());
                assert_eq!(cd.expr.float_literal().unwrap(), "3.14159");
            }
            _ => panic!("Expected Definition"),
        }
    }

    #[test]
    fn can_parse_const_string_value() {
        let text = r#"
        pub const NAME: str = "Pyxis";
        "#;
        let ast = parse_str_for_tests(text).unwrap().strip_locations();
        match &ast.items[0] {
            crate::grammar::ModuleItem::Definition { definition } => {
                let crate::grammar::ItemDefinitionInner::Constant(cd) = &definition.inner else {
                    panic!("Expected Constant");
                };
                assert_eq!(cd.expr.string_literal(), Some("Pyxis"));
            }
            _ => panic!("Expected Definition"),
        }
    }

    #[test]
    fn can_parse_const_enum_value() {
        let text = r#"
        pub const DEFAULT: Color = Color::Red;
        "#;
        let ast = parse_str_for_tests(text).unwrap().strip_locations();
        match &ast.items[0] {
            crate::grammar::ModuleItem::Definition { definition } => {
                let crate::grammar::ItemDefinitionInner::Constant(cd) = &definition.inner else {
                    panic!("Expected Constant");
                };
                assert!(cd.expr.path().is_some());
                assert_eq!(cd.expr.path().unwrap().to_string(), "Color::Red");
            }
            _ => panic!("Expected Definition"),
        }
    }

    #[test]
    fn can_parse_nested_const_in_type() {
        let text = r#"
        pub type Player {
            pub const STARTING_GOLD: u32 = 500,
            pub health: i32,
        }
        "#;
        let ast = parse_str_for_tests(text).unwrap().strip_locations();
        match &ast.items[0] {
            crate::grammar::ModuleItem::Definition { definition } => {
                let crate::grammar::ItemDefinitionInner::Type(td) = &definition.inner else {
                    panic!("Expected Type");
                };
                let items: Vec<_> = td.items.iter().collect();
                assert_eq!(items.len(), 2); // const + field
            }
            _ => panic!("Expected Definition"),
        }
    }

    #[test]
    fn can_parse_nested_const_in_enum() {
        let text = r#"
        pub enum Color: u8 {
            Red,
            Green,
            pub const DEFAULT: Color = Color::Red,
        }
        "#;
        let ast = parse_str_for_tests(text).unwrap().strip_locations();
        match &ast.items[0] {
            crate::grammar::ModuleItem::Definition { definition } => {
                let crate::grammar::ItemDefinitionInner::Enum(ed) = &definition.inner else {
                    panic!("Expected Enum");
                };
                let items: Vec<_> = ed.items.iter().collect();
                assert_eq!(items.len(), 3); // Red + Green + DEFAULT const
            }
            _ => panic!("Expected Definition"),
        }
    }

    #[test]
    fn can_parse_nested_const_in_bitflags() {
        let text = r#"
        pub bitflags Flags: u32 {
            READ = 1,
            WRITE = 2,
            pub const DEFAULT_MASK: u32 = 3,
        }
        "#;
        let ast = parse_str_for_tests(text).unwrap().strip_locations();
        match &ast.items[0] {
            crate::grammar::ModuleItem::Definition { definition } => {
                let crate::grammar::ItemDefinitionInner::Bitflags(bd) = &definition.inner else {
                    panic!("Expected Bitflags");
                };
                let items: Vec<_> = bd.items.iter().collect();
                assert_eq!(items.len(), 3); // READ + WRITE + DEFAULT_MASK const
            }
            _ => panic!("Expected Definition"),
        }
    }
}
