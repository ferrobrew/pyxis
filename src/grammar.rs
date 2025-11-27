use std::{fmt, path::Path};

use crate::span::{EqualsIgnoringLocations, ItemLocation, Located};

#[cfg(test)]
use crate::span::StripLocations;

/// Format information for integer literals
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntFormat {
    Decimal,
    Hex,
    Binary,
    Octal,
}
#[cfg(test)]
impl StripLocations for IntFormat {
    fn strip_locations(&self) -> Self {
        *self
    }
}

/// Format information for string literals
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StringFormat {
    Regular,
    Raw,
}
#[cfg(test)]
impl StripLocations for StringFormat {
    fn strip_locations(&self) -> Self {
        *self
    }
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
    pub fn int_literal(value: isize) -> E {
        E::IntLiteral {
            value,
            format: super::IntFormat::Decimal,
        }
    }
    pub fn int_literal_with_format(value: isize, format: super::IntFormat) -> E {
        E::IntLiteral { value, format }
    }
    pub fn string_literal(value: impl Into<String>) -> E {
        E::StringLiteral {
            value: value.into(),
            format: super::StringFormat::Regular,
        }
    }
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
#[cfg(test)]
impl StripLocations for Comment {
    fn strip_locations(&self) -> Self {
        match self {
            Comment::DocOuter(comments) => Comment::DocOuter(comments.strip_locations()),
            Comment::DocInner(comments) => Comment::DocInner(comments.strip_locations()),
            Comment::Regular(comment) => Comment::Regular(comment.strip_locations()),
            Comment::MultiLine(comments) => Comment::MultiLine(comments.strip_locations()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub String);
#[cfg(test)]
impl StripLocations for Ident {
    fn strip_locations(&self) -> Self {
        Ident(self.0.strip_locations())
    }
}
impl EqualsIgnoringLocations for Ident {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
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
impl EqualsIgnoringLocations for Type {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::ConstPointer(t), Type::ConstPointer(t2)) => t.equals_ignoring_locations(t2),
            (Type::MutPointer(t), Type::MutPointer(t2)) => t.equals_ignoring_locations(t2),
            (Type::Array(t, n), Type::Array(t2, n2)) => {
                t.equals_ignoring_locations(t2) && n.equals_ignoring_locations(n2)
            }
            (Type::Ident(ident), Type::Ident(ident2)) => ident.equals_ignoring_locations(ident2),
            (Type::Unknown(size), Type::Unknown(size2)) => size.equals_ignoring_locations(size2),
            _ => false,
        }
    }
}
#[cfg(test)]
impl StripLocations for Type {
    fn strip_locations(&self) -> Self {
        match self {
            Type::ConstPointer(located) => Type::ConstPointer(located.strip_locations()),
            Type::MutPointer(located) => Type::MutPointer(located.strip_locations()),
            Type::Array(located, len) => {
                Type::Array(located.strip_locations(), len.strip_locations())
            }
            Type::Ident(ident) => Type::Ident(ident.strip_locations()),
            Type::Unknown(size) => Type::Unknown(size.strip_locations()),
        }
    }
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
#[cfg(test)]
impl StripLocations for ItemPathSegment {
    fn strip_locations(&self) -> Self {
        ItemPathSegment(self.0.strip_locations())
    }
}
impl EqualsIgnoringLocations for ItemPathSegment {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
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
#[cfg(test)]
impl StripLocations for ItemPath {
    fn strip_locations(&self) -> Self {
        ItemPath(self.0.strip_locations())
    }
}
impl EqualsIgnoringLocations for ItemPath {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self.0.equals_ignoring_locations(&other.0)
    }
}
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
#[cfg(test)]
impl StripLocations for Expr {
    fn strip_locations(&self) -> Self {
        match self {
            Expr::IntLiteral { value, format } => Expr::IntLiteral {
                value: value.strip_locations(),
                format: format.strip_locations(),
            },
            Expr::StringLiteral { value, format } => Expr::StringLiteral {
                value: value.strip_locations(),
                format: format.strip_locations(),
            },
            Expr::Ident(ident) => Expr::Ident(ident.strip_locations()),
        }
    }
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
#[cfg(test)]
impl StripLocations for AttributeItem {
    fn strip_locations(&self) -> Self {
        match self {
            AttributeItem::Expr(expr) => AttributeItem::Expr(expr.strip_locations()),
            AttributeItem::Comment(comment) => AttributeItem::Comment(comment.strip_locations()),
        }
    }
}

impl AttributeItem {
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
#[cfg(test)]
impl StripLocations for Attribute {
    fn strip_locations(&self) -> Self {
        match self {
            Attribute::Ident(i) => Attribute::Ident(i.strip_locations()),
            Attribute::Function(name, items) => {
                Attribute::Function(name.clone(), items.strip_locations())
            }
            Attribute::Assign(name, items) => {
                Attribute::Assign(name.clone(), items.strip_locations())
            }
        }
    }
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
#[cfg(test)]
impl StripLocations for Attributes {
    fn strip_locations(&self) -> Self {
        Attributes(self.0.strip_locations())
    }
}
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
#[cfg(test)]
impl StripLocations for Visibility {
    fn strip_locations(&self) -> Self {
        *self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Argument {
    ConstSelf,
    MutSelf,
    Named(Ident, Located<Type>),
}
#[cfg(test)]
impl StripLocations for Argument {
    fn strip_locations(&self) -> Self {
        match self {
            Argument::ConstSelf => Argument::ConstSelf,
            Argument::MutSelf => Argument::MutSelf,
            Argument::Named(ident, ty) => {
                Argument::Named(ident.strip_locations(), ty.strip_locations())
            }
        }
    }
}
#[cfg(test)]
impl Argument {
    pub fn const_self() -> Argument {
        Argument::ConstSelf
    }
    pub fn mut_self() -> Argument {
        Argument::MutSelf
    }
    pub fn named(ident: impl Into<Ident>, type_: impl Into<Type>) -> Argument {
        Argument::Named(ident.into(), Located::test(type_.into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub visibility: Visibility,
    pub name: Ident,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    pub arguments: Vec<Located<Argument>>,
    pub return_type: Option<Located<Type>>,
}
#[cfg(test)]
impl StripLocations for Function {
    fn strip_locations(&self) -> Self {
        Function {
            visibility: self.visibility.strip_locations(),
            name: self.name.strip_locations(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.strip_locations(),
            arguments: self.arguments.strip_locations(),
            return_type: self.return_type.strip_locations(),
        }
    }
}
#[cfg(test)]
impl Function {
    pub fn new(
        (visibility, name): (Visibility, &str),
        arguments: impl IntoIterator<Item = Argument>,
    ) -> Self {
        Self {
            visibility,
            name: name.into(),
            attributes: Default::default(),
            doc_comments: vec![],
            arguments: arguments.into_iter().map(Located::test).collect(),
            return_type: None,
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
    Vftable(Vec<Located<Function>>),
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
        TypeField::Field(visibility, name.into(), Located::test(type_.into()))
    }

    pub fn vftable(functions: impl IntoIterator<Item = Function>) -> TypeField {
        TypeField::Vftable(functions.into_iter().map(Located::test).collect())
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
impl StripLocations for TypeStatement {
    fn strip_locations(&self) -> Self {
        TypeStatement {
            field: self.field.strip_locations(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.strip_locations(),
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
            location: self.location.strip_locations(),
        }
    }
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
impl StripLocations for EnumStatement {
    fn strip_locations(&self) -> Self {
        EnumStatement {
            name: self.name.strip_locations(),
            expr: self.expr.strip_locations(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.strip_locations(),
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
            location: self.location.strip_locations(),
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
impl StripLocations for BitflagsStatement {
    fn strip_locations(&self) -> Self {
        BitflagsStatement {
            name: self.name.strip_locations(),
            expr: self.expr.strip_locations(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.strip_locations(),
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
            location: self.location.strip_locations(),
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

/// Items in an impl block (preserves ordering and comments)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImplItem {
    Comment(Comment),
    Function(Function),
}
#[cfg(test)]
impl StripLocations for ImplItem {
    fn strip_locations(&self) -> Self {
        match self {
            ImplItem::Comment(c) => ImplItem::Comment(c.strip_locations()),
            ImplItem::Function(f) => ImplItem::Function(f.strip_locations()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionBlock {
    pub name: Ident,
    pub items: Vec<Located<ImplItem>>,
    pub attributes: Attributes,
}
#[cfg(test)]
impl StripLocations for FunctionBlock {
    fn strip_locations(&self) -> Self {
        FunctionBlock {
            name: self.name.strip_locations(),
            items: self
                .items
                .iter()
                .filter_map(|item| {
                    item.as_ref()
                        .map(|item| match item {
                            ImplItem::Comment(_) => None, // Filter out comments
                            ImplItem::Function(f) => Some(ImplItem::Function(f.strip_locations())),
                        })
                        .strip_locations()
                        .transpose()
                })
                .collect(),
            attributes: self.attributes.strip_locations(),
        }
    }
}
#[cfg(test)]
impl FunctionBlock {
    pub fn new(name: impl Into<Ident>, functions: impl IntoIterator<Item = Function>) -> Self {
        Self {
            name: name.into(),
            items: functions
                .into_iter()
                .map(ImplItem::Function)
                .map(Located::test)
                .collect(),
            attributes: Default::default(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}
impl FunctionBlock {
    pub fn functions(&self) -> impl Iterator<Item = Located<&Function>> {
        self.items.iter().filter_map(|item| {
            item.as_ref()
                .map(|item| match item {
                    ImplItem::Function(func) => Some(func),
                    _ => None,
                })
                .transpose()
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
#[cfg(test)]
impl StripLocations for Backend {
    fn strip_locations(&self) -> Self {
        Backend {
            name: self.name.strip_locations(),
            prologue: self.prologue.strip_locations(),
            prologue_format: self.prologue_format.strip_locations(),
            epilogue: self.epilogue.strip_locations(),
            epilogue_format: self.epilogue_format.strip_locations(),
        }
    }
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
        }
    }
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
        }
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
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
    Definition(Located<ItemDefinition>),
    Impl(FunctionBlock),
    ExternValue(Located<ExternValue>),
    Function(Located<Function>),
}
#[cfg(test)]
impl StripLocations for ModuleItem {
    fn strip_locations(&self) -> Self {
        match self {
            ModuleItem::Comment(c) => ModuleItem::Comment(c.strip_locations()),
            ModuleItem::Use(p) => ModuleItem::Use(p.strip_locations()),
            ModuleItem::ExternType(n, a, d, l) => ModuleItem::ExternType(
                n.strip_locations(),
                a.strip_locations(),
                d.strip_locations(),
                l.strip_locations(),
            ),
            ModuleItem::Backend(b) => ModuleItem::Backend(b.strip_locations()),
            ModuleItem::Definition(d) => ModuleItem::Definition(d.strip_locations()),
            ModuleItem::Impl(i) => ModuleItem::Impl(i.strip_locations()),
            ModuleItem::ExternValue(e) => ModuleItem::ExternValue(e.strip_locations()),
            ModuleItem::Function(f) => ModuleItem::Function(f.strip_locations()),
        }
    }
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
impl StripLocations for Module {
    fn strip_locations(&self) -> Self {
        Module {
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    ModuleItem::Comment(_) => None, // Filter out comments
                    _ => Some(item.strip_locations()),
                })
                .collect(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.strip_locations(),
        }
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
    pub fn with_extern_values(
        mut self,
        extern_values: impl IntoIterator<Item = ExternValue>,
    ) -> Self {
        for extern_value in extern_values.into_iter().map(Located::test) {
            self.items.push(ModuleItem::ExternValue(extern_value));
        }
        self
    }
    pub fn with_functions(mut self, functions: impl IntoIterator<Item = Function>) -> Self {
        for function in functions.into_iter().map(Located::test) {
            self.items.push(ModuleItem::Function(function));
        }
        self
    }
    pub fn with_definitions(
        mut self,
        definitions: impl IntoIterator<Item = ItemDefinition>,
    ) -> Self {
        for definition in definitions.into_iter().map(Located::test) {
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
    pub fn uses(&self) -> impl Iterator<Item = &ItemPath> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Use(path) => Some(path),
            _ => None,
        })
    }
    pub fn extern_types(&self) -> impl Iterator<Item = (&Ident, &Attributes, &ItemLocation)> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::ExternType(name, attrs, _, location) => Some((name, attrs, location)),
            _ => None,
        })
    }
    pub fn extern_values(&self) -> impl Iterator<Item = Located<&ExternValue>> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::ExternValue(ev) => Some(ev.as_ref()),
            _ => None,
        })
    }
    pub fn functions(&self) -> impl Iterator<Item = Located<&Function>> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Function(func) => Some(func.as_ref()),
            _ => None,
        })
    }
    pub fn definitions(&self) -> impl Iterator<Item = Located<&ItemDefinition>> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Definition(def) => Some(def.as_ref()),
            _ => None,
        })
    }
    pub fn impls(&self) -> impl Iterator<Item = &FunctionBlock> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Impl(impl_block) => Some(impl_block),
            _ => None,
        })
    }
    pub fn backends(&self) -> impl Iterator<Item = &Backend> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Backend(backend) => Some(backend),
            _ => None,
        })
    }
}
