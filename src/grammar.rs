use std::{fmt, path::Path};

use crate::span::{ItemLocation, Located};

/// Format information for integer literals
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntFormat {
    Decimal,
    Hex,
    Binary,
    Octal,
}

/// Format information for string literals
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StringFormat {
    Regular,
    Raw,
}

#[cfg(test)]
pub mod test_aliases {
    pub type M = super::Module;
    pub type ID = super::ItemDefinition;
    pub type TS = super::TypeStatement;
    pub type TD = super::TypeDefinition;
    pub type ES = super::EnumStatement;
    pub type ED = super::EnumDefinition;
    pub type BFS = super::BitflagsStatement;
    pub type BFD = super::BitflagsDefinition;
    pub type T = super::Type;
    pub type A = super::Attribute;
    pub type As = super::Attributes;
    pub type Ar = super::Argument;
    pub type TF = super::TypeField;
    pub type E = super::Expr;
    pub type F = super::Function;
    pub type FB = super::FunctionBlock;
    pub type IP = super::ItemPath;
    pub type B = super::Backend;
    pub type V = super::Visibility;
    pub type EV = super::ExternValue;

    /// Helper to create an IntLiteral expression (defaults to decimal format)
    pub fn int_literal(value: isize) -> E {
        E::IntLiteral {
            value,
            format: super::IntFormat::Decimal,
        }
    }

    /// Helper to create an IntLiteral expression with a specific format
    pub fn int_literal_with_format(value: isize, format: super::IntFormat) -> E {
        E::IntLiteral { value, format }
    }

    /// Helper to create a StringLiteral expression (defaults to regular format)
    pub fn string_literal(value: impl Into<String>) -> E {
        E::StringLiteral {
            value: value.into(),
            format: super::StringFormat::Regular,
        }
    }

    /// Helper to create a StringLiteral expression with a specific format
    pub fn string_literal_with_format(value: impl Into<String>, format: super::StringFormat) -> E {
        E::StringLiteral {
            value: value.into(),
            format,
        }
    }
}

/// Comment node types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Comment {
    /// Doc comment for outer items (///)
    DocOuter(Vec<String>),
    /// Doc comment for inner items (//!)
    DocInner(Vec<String>),
    /// Regular comment (//)
    Regular(String),
    /// Multiline comment (/* */)
    MultiLine(Vec<String>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub String);
impl From<&str> for Ident {
    fn from(item: &str) -> Self {
        Ident(item.to_string())
    }
}
impl Ident {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}
impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}
impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    ConstPointer(Box<Located<Type>>),
    MutPointer(Box<Located<Type>>),
    Array(Box<Located<Type>>, usize),
    Ident(Ident),
    Unknown(usize),
}
#[cfg(test)]
impl Type {
    pub fn ident(ident: &str) -> Type {
        Type::Ident(ident.into())
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Type::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn const_pointer(self) -> Type {
        Type::ConstPointer(Box::new(Located::test(self)))
    }

    pub fn mut_pointer(self) -> Type {
        Type::MutPointer(Box::new(Located::test(self)))
    }

    pub fn array(self, size: usize) -> Type {
        Type::Array(Box::new(Located::test(self)), size)
    }

    pub fn unknown(size: usize) -> Type {
        Type::Unknown(size)
    }
}
impl From<&str> for Type {
    fn from(item: &str) -> Self {
        Type::Ident(item.into())
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::ConstPointer(inner) => write!(f, "*const {inner}"),
            Type::MutPointer(inner) => write!(f, "*mut {inner}"),
            Type::Array(inner, size) => write!(f, "[{inner}; {size}]"),
            Type::Ident(ident) => write!(f, "{ident}"),
            Type::Unknown(size) => write!(f, "unknown({size})"),
        }
    }
}

#[derive(PartialEq, Hash, Eq, Clone, Debug, PartialOrd, Ord)]
pub struct ItemPathSegment(String);
impl ItemPathSegment {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}
impl From<&str> for ItemPathSegment {
    fn from(value: &str) -> Self {
        ItemPathSegment(value.to_string())
    }
}
impl From<String> for ItemPathSegment {
    fn from(value: String) -> Self {
        ItemPathSegment(value)
    }
}
impl fmt::Display for ItemPathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Hash, Eq, Clone, Debug, PartialOrd, Ord)]
pub struct ItemPath(Vec<ItemPathSegment>);
impl ItemPath {
    pub fn empty() -> ItemPath {
        ItemPath(vec![])
    }

    pub fn from_path(path: &Path) -> ItemPath {
        // consider making this a result
        assert!(path.is_relative());

        ItemPath(
            path.with_extension("")
                .iter()
                .map(|s| s.to_string_lossy().as_ref().into())
                .collect(),
        )
    }

    pub fn iter(&self) -> impl Iterator<Item = &ItemPathSegment> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn parent(&self) -> Option<ItemPath> {
        (!self.0.is_empty()).then(|| ItemPath(self.0[..self.0.len() - 1].to_vec()))
    }

    pub fn push(&mut self, segment: ItemPathSegment) {
        self.0.push(segment);
    }

    pub fn join(&self, segment: ItemPathSegment) -> ItemPath {
        let mut path = self.0.clone();
        path.push(segment);
        ItemPath(path)
    }

    pub fn last(&self) -> Option<&ItemPathSegment> {
        self.0.last()
    }
}
impl fmt::Display for ItemPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (index, segment) in self.0.iter().enumerate() {
            if index > 0 {
                write!(f, "::")?;
            }
            write!(f, "{segment}")?;
        }
        Ok(())
    }
}
impl FromIterator<ItemPathSegment> for ItemPath {
    fn from_iter<I: IntoIterator<Item = ItemPathSegment>>(iter: I) -> Self {
        ItemPath(Vec::from_iter(iter))
    }
}
impl From<&str> for ItemPath {
    fn from(value: &str) -> Self {
        ItemPath(value.split("::").map(|s| s.into()).collect())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    IntLiteral { value: isize, format: IntFormat },
    StringLiteral { value: String, format: StringFormat },
    Ident(Ident),
}
impl Expr {
    pub fn int_literal(&self) -> Option<isize> {
        match self {
            Expr::IntLiteral { value, .. } => Some(*value),
            _ => None,
        }
    }
    pub fn string_literal(&self) -> Option<&str> {
        match self {
            Expr::StringLiteral { value, .. } => Some(value.as_str()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttributeItem {
    Expr(Expr),
    Comment(String),
}

impl AttributeItem {
    /// Helper to extract just the expressions from a list of attribute items
    pub fn extract_exprs(items: &[AttributeItem]) -> Vec<&Expr> {
        items
            .iter()
            .filter_map(|item| match item {
                AttributeItem::Expr(expr) => Some(expr),
                AttributeItem::Comment(_) => None,
            })
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Attribute {
    Ident(Ident),
    /// Function attribute with expressions and comments
    /// Example: size(0x620 /* actually 0x61C */)
    Function(Ident, Vec<AttributeItem>),
    /// Assign attribute with expression and optional comments
    /// Example: foo = bar /* comment */
    Assign(Ident, Vec<AttributeItem>),
}
impl Attribute {
    // Ident attributes
    pub fn copyable() -> Self {
        Attribute::Ident("copyable".into())
    }
    pub fn cloneable() -> Self {
        Attribute::Ident("cloneable".into())
    }
    pub fn defaultable() -> Self {
        Attribute::Ident("defaultable".into())
    }
    #[allow(clippy::should_implement_trait)]
    pub fn default() -> Self {
        Attribute::Ident("default".into())
    }
    pub fn base() -> Self {
        Attribute::Ident("base".into())
    }
    pub fn packed() -> Self {
        Attribute::Ident("packed".into())
    }

    // Function attributes
    pub fn function(&self) -> Option<(&Ident, &Vec<AttributeItem>)> {
        match self {
            Attribute::Function(ident, items) => Some((ident, items)),
            _ => None,
        }
    }
    pub fn integer_fn(name: &str, value: isize) -> Self {
        Attribute::Function(
            name.into(),
            vec![AttributeItem::Expr(Expr::IntLiteral {
                value,
                format: IntFormat::Decimal,
            })],
        )
    }
    fn integer_fn_hex(name: &str, value: isize) -> Self {
        Attribute::Function(
            name.into(),
            vec![AttributeItem::Expr(Expr::IntLiteral {
                value,
                format: IntFormat::Hex,
            })],
        )
    }
    pub fn address(address: usize) -> Self {
        Self::integer_fn_hex("address", address as isize)
    }
    pub fn size(size: usize) -> Self {
        Self::integer_fn("size", size as isize)
    }
    pub fn min_size(min_size: usize) -> Self {
        Self::integer_fn("min_size", min_size as isize)
    }
    pub fn align(align: usize) -> Self {
        Self::integer_fn("align", align as isize)
    }
    pub fn singleton(address: usize) -> Self {
        Self::integer_fn_hex("singleton", address as isize)
    }
    pub fn index(index: usize) -> Self {
        Self::integer_fn_hex("index", index as isize)
    }
    pub fn calling_convention(name: &str) -> Self {
        Attribute::Function(
            "calling_convention".into(),
            vec![AttributeItem::Expr(Expr::StringLiteral {
                value: name.into(),
                format: StringFormat::Regular,
            })],
        )
    }

    // Assign attributes
    pub fn assign(&self) -> Option<(&Ident, &Vec<AttributeItem>)> {
        match self {
            Attribute::Assign(ident, items) => Some((ident, items)),
            _ => None,
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Attributes(pub Vec<Attribute>);
impl<const N: usize> From<[Attribute; N]> for Attributes {
    fn from(s: [Attribute; N]) -> Self {
        Attributes(s.to_vec())
    }
}
impl From<Vec<Attribute>> for Attributes {
    fn from(s: Vec<Attribute>) -> Self {
        Attributes(s)
    }
}
impl IntoIterator for Attributes {
    type Item = Attribute;
    type IntoIter = std::vec::IntoIter<Attribute>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<'a> IntoIterator for &'a Attributes {
    type Item = &'a Attribute;
    type IntoIter = std::slice::Iter<'a, Attribute>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
impl FromIterator<Attribute> for Attributes {
    fn from_iter<I: IntoIterator<Item = Attribute>>(iter: I) -> Self {
        Attributes(iter.into_iter().collect())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Argument {
    ConstSelf(ItemLocation),
    MutSelf(ItemLocation),
    Named(Ident, Located<Type>, ItemLocation),
}
#[cfg(test)]
impl Argument {
    pub fn const_self() -> Argument {
        Argument::ConstSelf(ItemLocation::test())
    }
    pub fn mut_self() -> Argument {
        Argument::MutSelf(ItemLocation::test())
    }
    pub fn named(ident: impl Into<Ident>, type_: impl Into<Type>) -> Argument {
        Argument::Named(
            ident.into(),
            Located::test(type_.into()),
            ItemLocation::test(),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub visibility: Visibility,
    pub name: Ident,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    pub arguments: Vec<Argument>,
    pub return_type: Option<Located<Type>>,
    pub location: ItemLocation,
}
#[cfg(test)]
impl Function {
    pub fn new(
        (visibility, name): (Visibility, &str),
        arguments: impl Into<Vec<Argument>>,
    ) -> Self {
        Self {
            visibility,
            name: name.into(),
            attributes: Default::default(),
            doc_comments: vec![],
            arguments: arguments.into(),
            return_type: None,
            location: ItemLocation::test(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
    pub fn with_return_type(mut self, return_type: impl Into<Type>) -> Self {
        self.return_type = Some(Located::test(return_type.into()));
        self
    }
    pub fn with_location(mut self, location: ItemLocation) -> Self {
        self.location = location;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprField(pub Ident, pub Expr);
impl ExprField {
    pub fn ident(&self) -> &Ident {
        &self.0
    }

    pub fn ident_as_str(&self) -> &str {
        self.0.as_str()
    }
}
impl From<(Ident, Expr)> for ExprField {
    fn from(item: (Ident, Expr)) -> Self {
        ExprField(item.0, item.1)
    }
}

// types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeField {
    Field(Visibility, Ident, Located<Type>),
    Vftable(Vec<Function>),
}
#[cfg(test)]
impl TypeField {
    pub fn field(
        visibility: Visibility,
        name: impl Into<Ident>,
        type_: impl Into<Type>,
    ) -> TypeField {
        TypeField::Field(visibility, name.into(), Located::test(type_.into()))
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
    Comment(Located<Comment>),
    Statement(TypeStatement),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeStatement {
    pub field: TypeField,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    pub inline_trailing_comments: Vec<Located<Comment>>, // Comments on same line as field
    pub following_comments: Vec<Located<Comment>>,       // Comments on lines after field
    pub location: ItemLocation,
}
#[cfg(test)]
impl TypeStatement {
    pub fn field((visibility, name): (Visibility, &str), type_: Type) -> TypeStatement {
        TypeStatement {
            field: TypeField::Field(visibility, name.into(), Located::test(type_)),
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
    pub fn with_location(mut self, location: ItemLocation) -> Self {
        self.location = location;
        self
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
    pub fn with_inline_trailing_comments(
        mut self,
        inline_trailing_comments: Vec<Located<Comment>>,
    ) -> Self {
        self.inline_trailing_comments = inline_trailing_comments;
        self
    }
    pub fn with_following_comments(mut self, following_comments: Vec<Located<Comment>>) -> Self {
        self.following_comments = following_comments;
        self
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDefinition {
    pub items: Vec<TypeDefItem>,
    pub attributes: Attributes,
    pub inline_trailing_comments: Vec<Located<Comment>>, // Comments on same line as attributes
    pub following_comments: Vec<Located<Comment>>,       // Comments on lines after attributes
}
impl TypeDefinition {
    pub fn new(statements: impl Into<Vec<TypeStatement>>) -> Self {
        Self {
            items: statements
                .into()
                .into_iter()
                .map(TypeDefItem::Statement)
                .collect(),
            attributes: Default::default(),
            inline_trailing_comments: Vec::new(),
            following_comments: Vec::new(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
    pub fn with_inline_trailing_comments(
        mut self,
        inline_trailing_comments: Vec<Located<Comment>>,
    ) -> Self {
        self.inline_trailing_comments = inline_trailing_comments;
        self
    }
    pub fn with_following_comments(mut self, following_comments: Vec<Located<Comment>>) -> Self {
        self.following_comments = following_comments;
        self
    }

    /// Helper to extract just the statements (for compatibility)
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
    Comment(Located<Comment>),
    Statement(EnumStatement),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumStatement {
    pub name: Ident,
    pub expr: Option<Expr>,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    pub inline_trailing_comments: Vec<Located<Comment>>, // Comments on same line as enum variant
    pub following_comments: Vec<Located<Comment>>,       // Comments on lines after enum variant
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
    pub fn with_location(mut self, location: ItemLocation) -> Self {
        self.location = location;
        self
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
    pub fn with_inline_trailing_comments(
        mut self,
        inline_trailing_comments: Vec<Located<Comment>>,
    ) -> Self {
        self.inline_trailing_comments = inline_trailing_comments;
        self
    }
    pub fn with_following_comments(mut self, following_comments: Vec<Located<Comment>>) -> Self {
        self.following_comments = following_comments;
        self
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDefinition {
    pub type_: Located<Type>,
    pub items: Vec<EnumDefItem>,
    pub attributes: Attributes,
    pub inline_trailing_comments: Vec<Located<Comment>>, // Comments on same line as attributes
    pub following_comments: Vec<Located<Comment>>,       // Comments on lines after attributes
}
#[cfg(test)]
impl EnumDefinition {
    pub fn new(
        type_: Type,
        statements: impl Into<Vec<EnumStatement>>,
        attributes: impl Into<Attributes>,
    ) -> Self {
        Self {
            type_: Located::test(type_),
            items: statements
                .into()
                .into_iter()
                .map(EnumDefItem::Statement)
                .collect(),
            attributes: attributes.into(),
            inline_trailing_comments: Vec::new(),
            following_comments: Vec::new(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
    pub fn with_inline_trailing_comments(
        mut self,
        inline_trailing_comments: Vec<Located<Comment>>,
    ) -> Self {
        self.inline_trailing_comments = inline_trailing_comments;
        self
    }
    pub fn with_following_comments(mut self, following_comments: Vec<Located<Comment>>) -> Self {
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
    Comment(Located<Comment>),
    Statement(BitflagsStatement),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BitflagsStatement {
    pub name: Ident,
    pub expr: Expr,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    pub inline_trailing_comments: Vec<Located<Comment>>, // Comments on same line as bitflag
    pub following_comments: Vec<Located<Comment>>,       // Comments on lines after bitflag
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
    pub fn with_location(mut self, location: ItemLocation) -> Self {
        self.location = location;
        self
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
    pub fn with_inline_trailing_comments(
        mut self,
        inline_trailing_comments: Vec<Located<Comment>>,
    ) -> Self {
        self.inline_trailing_comments = inline_trailing_comments;
        self
    }
    pub fn with_following_comments(mut self, following_comments: Vec<Located<Comment>>) -> Self {
        self.following_comments = following_comments;
        self
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BitflagsDefinition {
    pub type_: Located<Type>,
    pub items: Vec<BitflagsDefItem>,
    pub attributes: Attributes,
    pub inline_trailing_comments: Vec<Located<Comment>>, // Comments on same line as attributes
    pub following_comments: Vec<Located<Comment>>,       // Comments on lines after attributes
}
#[cfg(test)]
impl BitflagsDefinition {
    pub fn new(
        type_: Type,
        statements: impl Into<Vec<BitflagsStatement>>,
        attributes: impl Into<Attributes>,
    ) -> Self {
        Self {
            type_: Located::test(type_),
            items: statements
                .into()
                .into_iter()
                .map(BitflagsDefItem::Statement)
                .collect(),
            attributes: attributes.into(),
            inline_trailing_comments: Vec::new(),
            following_comments: Vec::new(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
    pub fn with_inline_trailing_comments(
        mut self,
        inline_trailing_comments: Vec<Located<Comment>>,
    ) -> Self {
        self.inline_trailing_comments = inline_trailing_comments;
        self
    }
    pub fn with_following_comments(mut self, following_comments: Vec<Located<Comment>>) -> Self {
        self.following_comments = following_comments;
        self
    }
}
impl BitflagsDefinition {
    /// Helper to extract just the statements (for compatibility)
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
    pub location: ItemLocation,
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
            location: ItemLocation::test(),
        }
    }

    pub fn with_location(mut self, location: ItemLocation) -> Self {
        self.location = location;
        self
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
}

/// Items in an impl block (preserves ordering and comments)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImplItem {
    Comment(Located<Comment>),
    Function(Function),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionBlock {
    pub name: Ident,
    pub items: Vec<ImplItem>,
    pub attributes: Attributes,
}
impl FunctionBlock {
    pub fn new(name: impl Into<Ident>, functions: impl Into<Vec<Function>>) -> Self {
        Self {
            name: name.into(),
            items: functions
                .into()
                .into_iter()
                .map(ImplItem::Function)
                .collect(),
            attributes: Default::default(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }

    /// Helper to extract just the functions (for compatibility)
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.items.iter().filter_map(|item| match item {
            ImplItem::Function(func) => Some(func),
            _ => None,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Backend {
    pub name: Ident,
    pub prologue: Option<String>,
    pub prologue_format: StringFormat,
    pub epilogue: Option<String>,
    pub epilogue_format: StringFormat,
}
impl Backend {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.into(),
            prologue: None,
            prologue_format: StringFormat::Regular,
            epilogue: None,
            epilogue_format: StringFormat::Regular,
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
    pub type_: Located<Type>,
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
        attributes: impl Into<Attributes>,
    ) -> Self {
        Self {
            visibility,
            name: name.into(),
            type_: Located::test(type_),
            attributes: attributes.into(),
            doc_comments: vec![],
            location: ItemLocation::test(),
        }
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
    pub fn with_location(mut self, location: ItemLocation) -> Self {
        self.location = location;
        self
    }
}

/// Module-level items (preserves ordering and comments)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleItem {
    Comment(Located<Comment>),
    Use(ItemPath),
    ExternType(Ident, Attributes, Vec<String>, ItemLocation), // name, attributes, doc_comments, location
    Backend(Backend),
    Definition(ItemDefinition),
    Impl(FunctionBlock),
    ExternValue(ExternValue),
    Function(Function),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Module {
    pub items: Vec<ModuleItem>,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
}
impl Module {
    pub fn new() -> Self {
        Self::default()
    }
}
#[cfg(test)]
impl Module {
    pub fn with_uses(mut self, uses: impl Into<Vec<ItemPath>>) -> Self {
        for use_path in uses.into() {
            self.items.push(ModuleItem::Use(use_path));
        }
        self
    }
    pub fn with_extern_types(mut self, extern_types: impl Into<Vec<(Ident, Attributes)>>) -> Self {
        for (name, attrs) in extern_types.into() {
            self.items.push(ModuleItem::ExternType(
                name,
                attrs,
                vec![],
                ItemLocation::test(),
            ));
        }
        self
    }
    pub fn with_extern_values(mut self, extern_values: impl Into<Vec<ExternValue>>) -> Self {
        for extern_value in extern_values.into() {
            self.items.push(ModuleItem::ExternValue(extern_value));
        }
        self
    }
    pub fn with_functions(mut self, functions: impl Into<Vec<Function>>) -> Self {
        for function in functions.into() {
            self.items.push(ModuleItem::Function(function));
        }
        self
    }
    pub fn with_definitions(mut self, definitions: impl Into<Vec<ItemDefinition>>) -> Self {
        for definition in definitions.into() {
            self.items.push(ModuleItem::Definition(definition));
        }
        self
    }
    pub fn with_impls(mut self, impls: impl Into<Vec<FunctionBlock>>) -> Self {
        for impl_block in impls.into() {
            self.items.push(ModuleItem::Impl(impl_block));
        }
        self
    }
    pub fn with_backends(mut self, backends: impl Into<Vec<Backend>>) -> Self {
        for backend in backends.into() {
            self.items.push(ModuleItem::Backend(backend));
        }
        self
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
}
impl Module {
    /// Helper to extract uses (for compatibility)
    pub fn uses(&self) -> impl Iterator<Item = &ItemPath> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Use(path) => Some(path),
            _ => None,
        })
    }

    /// Helper to extract extern_types (for compatibility)
    pub fn extern_types(&self) -> impl Iterator<Item = (&Ident, &Attributes, &ItemLocation)> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::ExternType(name, attrs, _, location) => Some((name, attrs, location)),
            _ => None,
        })
    }

    /// Helper to extract extern_values (for compatibility)
    pub fn extern_values(&self) -> impl Iterator<Item = &ExternValue> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::ExternValue(ev) => Some(ev),
            _ => None,
        })
    }

    /// Helper to extract functions (for compatibility)
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Function(func) => Some(func),
            _ => None,
        })
    }

    /// Helper to extract definitions (for compatibility)
    pub fn definitions(&self) -> impl Iterator<Item = &ItemDefinition> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Definition(def) => Some(def),
            _ => None,
        })
    }

    /// Helper to extract impls (for compatibility)
    pub fn impls(&self) -> impl Iterator<Item = &FunctionBlock> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Impl(impl_block) => Some(impl_block),
            _ => None,
        })
    }

    /// Helper to extract backends (for compatibility)
    pub fn backends(&self) -> impl Iterator<Item = &Backend> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Backend(backend) => Some(backend),
            _ => None,
        })
    }
}
