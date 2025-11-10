use std::{fmt, path::Path};

use crate::span::{Span, Spanned, StructuralEq};

pub mod test_aliases {
    pub type M = super::Module;
    pub type MC = super::ModuleChild;
    pub type ID = super::ItemDefinition;
    pub type TS = super::TypeStatement;
    pub type TC = super::TypeChild;
    pub type TD = super::TypeDefinition;
    pub type ES = super::EnumStatement;
    pub type EC = super::EnumChild;
    pub type ED = super::EnumDefinition;
    pub type BFS = super::BitflagsStatement;
    pub type BFC = super::BitflagsChild;
    pub type BFD = super::BitflagsDefinition;
    pub type VD = super::VftableDefinition;
    pub type VC = super::VftableChild;
    pub type T = super::Type;
    pub type A = super::Attribute;
    pub type As = super::Attributes;
    pub type Ar = super::Argument;
    pub type AC = super::ArgumentChild;
    pub type TF = super::TypeField;
    pub type E = super::Expr;
    pub type F = super::Function;
    pub type FB = super::FunctionBlock;
    pub type IC = super::ImplChild;
    pub type IP = super::ItemPath;
    pub type B = super::Backend;
    pub type V = super::Visibility;
    pub type EV = super::ExternValue;
    pub type C = super::Comment;
}

// Helper to create dummy spans for tests
pub fn dummy_span() -> Span {
    Span::new(0, 0)
}

pub fn spanned<T>(node: T) -> Spanned<T> {
    Spanned::new(node, dummy_span())
}

// Comments
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Comment {
    Line(String),       // // regular comment
    Doc(String),        // /// doc comment
    ModuleDoc(String),  // //! module doc comment
    Block(String),      // /* */ block comment
}

impl StructuralEq for Comment {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Comment::Line(a), Comment::Line(b)) => a == b,
            (Comment::Doc(a), Comment::Doc(b)) => a == b,
            (Comment::ModuleDoc(a), Comment::ModuleDoc(b)) => a == b,
            (Comment::Block(a), Comment::Block(b)) => a == b,
            _ => false,
        }
    }
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

impl StructuralEq for Ident {
    fn structural_eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    ConstPointer(Box<Spanned<Type>>),
    MutPointer(Box<Spanned<Type>>),
    Array(Box<Spanned<Type>>, usize),
    Ident(Ident),
    Unknown(usize),
}

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

    #[cfg(test)]
    pub fn const_pointer(self) -> Type {
        Type::ConstPointer(Box::new(spanned(self)))
    }

    #[cfg(test)]
    pub fn mut_pointer(self) -> Type {
        Type::MutPointer(Box::new(spanned(self)))
    }

    #[cfg(test)]
    pub fn array(self, size: usize) -> Type {
        Type::Array(Box::new(spanned(self)), size)
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

impl StructuralEq for Type {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::ConstPointer(a), Type::ConstPointer(b)) => a.structural_eq(b),
            (Type::MutPointer(a), Type::MutPointer(b)) => a.structural_eq(b),
            (Type::Array(a, sa), Type::Array(b, sb)) => sa == sb && a.structural_eq(b),
            (Type::Ident(a), Type::Ident(b)) => a.structural_eq(b),
            (Type::Unknown(a), Type::Unknown(b)) => a == b,
            _ => false,
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

impl StructuralEq for ItemPathSegment {
    fn structural_eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[derive(PartialEq, Hash, Eq, Clone, Debug, PartialOrd, Ord)]
pub struct ItemPath(pub Vec<ItemPathSegment>);

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
            write!(f, "{}", segment)?;
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

impl StructuralEq for ItemPath {
    fn structural_eq(&self, other: &Self) -> bool {
        self.0.structural_eq(&other.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    IntLiteral(isize),
    StringLiteral(String),
    Ident(Ident),
}

impl Expr {
    pub fn int_literal(&self) -> Option<isize> {
        match self {
            Expr::IntLiteral(value) => Some(*value),
            _ => None,
        }
    }
    pub fn string_literal(&self) -> Option<&str> {
        match self {
            Expr::StringLiteral(value) => Some(value.as_str()),
            _ => None,
        }
    }
}

impl StructuralEq for Expr {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::IntLiteral(a), Expr::IntLiteral(b)) => a == b,
            (Expr::StringLiteral(a), Expr::StringLiteral(b)) => a == b,
            (Expr::Ident(a), Expr::Ident(b)) => a.structural_eq(b),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Attribute {
    Ident(Ident),
    Function(Ident, Vec<Spanned<Expr>>),
    Assign(Ident, Spanned<Expr>),
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
    pub fn function(&self) -> Option<(&Ident, &Vec<Spanned<Expr>>)> {
        match self {
            Attribute::Function(ident, exprs) => Some((ident, exprs)),
            _ => None,
        }
    }

    #[cfg(test)]
    pub fn integer_fn(name: &str, value: isize) -> Self {
        Attribute::Function(name.into(), vec![spanned(Expr::IntLiteral(value))])
    }

    #[cfg(test)]
    pub fn address(address: usize) -> Self {
        Self::integer_fn("address", address as isize)
    }

    #[cfg(test)]
    pub fn size(size: usize) -> Self {
        Self::integer_fn("size", size as isize)
    }

    #[cfg(test)]
    pub fn min_size(min_size: usize) -> Self {
        Self::integer_fn("min_size", min_size as isize)
    }

    #[cfg(test)]
    pub fn align(align: usize) -> Self {
        Self::integer_fn("align", align as isize)
    }

    #[cfg(test)]
    pub fn singleton(address: usize) -> Self {
        Self::integer_fn("singleton", address as isize)
    }

    #[cfg(test)]
    pub fn index(index: usize) -> Self {
        Self::integer_fn("index", index as isize)
    }

    #[cfg(test)]
    pub fn calling_convention(name: &str) -> Self {
        Attribute::Function(
            "calling_convention".into(),
            vec![spanned(Expr::StringLiteral(name.into()))],
        )
    }

    // Assign attributes
    pub fn assign(&self) -> Option<(&Ident, &Spanned<Expr>)> {
        match self {
            Attribute::Assign(ident, expr) => Some((ident, expr)),
            _ => None,
        }
    }

    #[cfg(test)]
    pub fn doc(doc: &str) -> Self {
        Attribute::Assign("doc".into(), spanned(Expr::StringLiteral(doc.into())))
    }
}

impl StructuralEq for Attribute {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Attribute::Ident(a), Attribute::Ident(b)) => a.structural_eq(b),
            (Attribute::Function(na, ea), Attribute::Function(nb, eb)) => {
                na.structural_eq(nb) && ea.structural_eq(eb)
            }
            (Attribute::Assign(na, ea), Attribute::Assign(nb, eb)) => {
                na.structural_eq(nb) && ea.structural_eq(eb)
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Attributes(pub Vec<Spanned<Attribute>>);

impl<const N: usize> From<[Attribute; N]> for Attributes {
    fn from(s: [Attribute; N]) -> Self {
        Attributes(s.into_iter().map(spanned).collect())
    }
}

impl From<Vec<Attribute>> for Attributes {
    fn from(s: Vec<Attribute>) -> Self {
        Attributes(s.into_iter().map(spanned).collect())
    }
}

impl IntoIterator for Attributes {
    type Item = Spanned<Attribute>;
    type IntoIter = std::vec::IntoIter<Spanned<Attribute>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a Attributes {
    type Item = &'a Spanned<Attribute>;
    type IntoIter = std::slice::Iter<'a, Spanned<Attribute>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl FromIterator<Attribute> for Attributes {
    fn from_iter<I: IntoIterator<Item = Attribute>>(iter: I) -> Self {
        Attributes(iter.into_iter().map(spanned).collect())
    }
}

impl Attributes {
    pub fn doc(&self, path: &ItemPath) -> anyhow::Result<Option<String>> {
        let mut doc = None;
        for attr in &self.0 {
            let Some((key, value)) = attr.node.assign() else {
                continue;
            };
            if key.as_str() != "doc" {
                continue;
            }

            let Some(value) = value.node.string_literal() else {
                anyhow::bail!("doc attribute for `{path}` must be a string literal");
            };

            let doc = doc.get_or_insert_with(String::new);
            if !doc.is_empty() {
                doc.push('\n');
            }
            doc.push_str(value);
        }
        Ok(doc)
    }
}

impl StructuralEq for Attributes {
    fn structural_eq(&self, other: &Self) -> bool {
        self.0.structural_eq(&other.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Visibility {
    Public,
    Private,
}

impl StructuralEq for Visibility {
    fn structural_eq(&self, other: &Self) -> bool {
        self == other
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Argument {
    ConstSelf,
    MutSelf,
    Named(Spanned<Ident>, Spanned<Type>),
}

impl Argument {
    #[cfg(test)]
    pub fn named(ident: impl Into<Ident>, type_: impl Into<Type>) -> Argument {
        Argument::Named(spanned(ident.into()), spanned(type_.into()))
    }
}

impl StructuralEq for Argument {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Argument::ConstSelf, Argument::ConstSelf) => true,
            (Argument::MutSelf, Argument::MutSelf) => true,
            (Argument::Named(na, ta), Argument::Named(nb, tb)) => {
                na.structural_eq(nb) && ta.structural_eq(tb)
            }
            _ => false,
        }
    }
}

// Child enums for containers that can hold comments

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArgumentChild {
    Argument(Spanned<Argument>),
    Comment(Comment),
}

impl StructuralEq for ArgumentChild {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ArgumentChild::Argument(a), ArgumentChild::Argument(b)) => a.structural_eq(b),
            (ArgumentChild::Comment(a), ArgumentChild::Comment(b)) => a.structural_eq(b),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub visibility: Visibility,
    pub name: Spanned<Ident>,
    pub attributes: Attributes,
    pub doc_comments: Vec<Spanned<String>>,
    pub arguments: Vec<Spanned<Argument>>,
    pub return_type: Option<Spanned<Type>>,
}

impl Function {
    #[cfg(test)]
    pub fn new(
        (visibility, name): (Visibility, &str),
        arguments: impl Into<Vec<Argument>>,
    ) -> Self {
        Self {
            visibility,
            name: spanned(name.into()),
            attributes: Default::default(),
            doc_comments: vec![],
            arguments: arguments.into().into_iter().map(spanned).collect(),
            return_type: None,
        }
    }

    #[cfg(test)]
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }

    #[cfg(test)]
    pub fn with_return_type(mut self, return_type: impl Into<Type>) -> Self {
        self.return_type = Some(spanned(return_type.into()));
        self
    }

    #[cfg(test)]
    pub fn with_doc_comments<I, S>(mut self, doc_comments: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.doc_comments = doc_comments
            .into_iter()
            .map(|s| spanned(s.into()))
            .collect();
        self
    }
}

impl StructuralEq for Function {
    fn structural_eq(&self, other: &Self) -> bool {
        self.visibility.structural_eq(&other.visibility)
            && self.name.structural_eq(&other.name)
            && self.attributes.structural_eq(&other.attributes)
            && self.doc_comments.structural_eq(&other.doc_comments)
            && self.arguments.structural_eq(&other.arguments)
            && self.return_type.structural_eq(&other.return_type)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprField(pub Spanned<Ident>, pub Spanned<Expr>);

impl ExprField {
    pub fn ident(&self) -> &Ident {
        &self.0.node
    }

    pub fn ident_as_str(&self) -> &str {
        self.0.node.as_str()
    }
}

impl From<(Ident, Expr)> for ExprField {
    fn from(item: (Ident, Expr)) -> Self {
        ExprField(spanned(item.0), spanned(item.1))
    }
}

impl StructuralEq for ExprField {
    fn structural_eq(&self, other: &Self) -> bool {
        self.0.structural_eq(&other.0) && self.1.structural_eq(&other.1)
    }
}

// types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeField {
    Field(Visibility, Spanned<Ident>, Spanned<Type>),
    Vftable(VftableDefinition),
}

impl TypeField {
    #[cfg(test)]
    pub fn field(
        visibility: Visibility,
        name: impl Into<Ident>,
        type_: impl Into<Type>,
    ) -> TypeField {
        TypeField::Field(visibility, spanned(name.into()), spanned(type_.into()))
    }

    #[cfg(test)]
    pub fn vftable(functions: impl IntoIterator<Item = Function>) -> TypeField {
        TypeField::Vftable(VftableDefinition::new(functions.into_iter().collect::<Vec<_>>()))
    }

    pub fn is_vftable(&self) -> bool {
        matches!(self, TypeField::Vftable(_))
    }
}

impl StructuralEq for TypeField {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeField::Field(va, na, ta), TypeField::Field(vb, nb, tb)) => {
                va == vb && na.structural_eq(nb) && ta.structural_eq(tb)
            }
            (TypeField::Vftable(a), TypeField::Vftable(b)) => a.structural_eq(b),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeStatement {
    pub field: TypeField,
    pub attributes: Attributes,
    pub doc_comments: Vec<Spanned<String>>,
}

impl TypeStatement {
    #[cfg(test)]
    pub fn field((visibility, name): (Visibility, &str), type_: Type) -> TypeStatement {
        TypeStatement {
            field: TypeField::Field(visibility, spanned(name.into()), spanned(type_)),
            attributes: Default::default(),
            doc_comments: vec![],
        }
    }

    #[cfg(test)]
    pub fn vftable(functions: impl IntoIterator<Item = Function>) -> TypeStatement {
        TypeStatement {
            field: TypeField::vftable(functions),
            attributes: Default::default(),
            doc_comments: vec![],
        }
    }

    #[cfg(test)]
    pub fn vftable_with_children(vftable_def: VftableDefinition) -> TypeStatement {
        TypeStatement {
            field: TypeField::Vftable(vftable_def),
            attributes: Default::default(),
            doc_comments: vec![],
        }
    }

    #[cfg(test)]
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }

    #[cfg(test)]
    pub fn with_doc_comments<I, S>(mut self, doc_comments: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.doc_comments = doc_comments
            .into_iter()
            .map(|s| spanned(s.into()))
            .collect();
        self
    }
}

impl StructuralEq for TypeStatement {
    fn structural_eq(&self, other: &Self) -> bool {
        self.field.structural_eq(&other.field)
            && self.attributes.structural_eq(&other.attributes)
            && self.doc_comments.structural_eq(&other.doc_comments)
    }
}

// Vftable definition
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VftableDefinition {
    pub children: Vec<Spanned<VftableChild>>,
}

impl VftableDefinition {
    #[cfg(test)]
    pub fn new(functions: impl Into<Vec<Function>>) -> Self {
        Self {
            children: functions
                .into()
                .into_iter()
                .map(|f| spanned(VftableChild::Function(spanned(f))))
                .collect(),
        }
    }

    #[cfg(test)]
    pub fn with_children(children: impl Into<Vec<VftableChild>>) -> Self {
        Self {
            children: children
                .into()
                .into_iter()
                .map(spanned)
                .collect(),
        }
    }
}

impl VftableDefinition {
    pub fn functions(&self) -> impl Iterator<Item = &Spanned<Function>> + '_ {
        self.children
            .iter()
            .filter_map(|child| {
                if let VftableChild::Function(f) = &child.node {
                    Some(f)
                } else {
                    None
                }
            })
    }
}

impl StructuralEq for VftableDefinition {
    fn structural_eq(&self, other: &Self) -> bool {
        self.children.structural_eq(&other.children)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VftableChild {
    Function(Spanned<Function>),
    Comment(Comment),
}

impl VftableChild {
    #[cfg(test)]
    pub fn function(func: Function) -> Self {
        VftableChild::Function(spanned(func))
    }

    #[cfg(test)]
    pub fn comment(comment: Comment) -> Self {
        VftableChild::Comment(comment)
    }

    #[cfg(test)]
    pub fn line_comment(text: impl Into<String>) -> Self {
        VftableChild::Comment(Comment::Line(text.into()))
    }

    #[cfg(test)]
    pub fn block_comment(text: impl Into<String>) -> Self {
        VftableChild::Comment(Comment::Block(text.into()))
    }
}

impl StructuralEq for VftableChild {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VftableChild::Function(a), VftableChild::Function(b)) => a.structural_eq(b),
            (VftableChild::Comment(a), VftableChild::Comment(b)) => a.structural_eq(b),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeChild {
    Statement(Spanned<TypeStatement>),
    Comment(Comment),
}

impl TypeChild {
    #[cfg(test)]
    pub fn statement(stmt: TypeStatement) -> Self {
        TypeChild::Statement(spanned(stmt))
    }

    #[cfg(test)]
    pub fn comment(comment: Comment) -> Self {
        TypeChild::Comment(comment)
    }

    #[cfg(test)]
    pub fn line_comment(text: impl Into<String>) -> Self {
        TypeChild::Comment(Comment::Line(text.into()))
    }

    #[cfg(test)]
    pub fn block_comment(text: impl Into<String>) -> Self {
        TypeChild::Comment(Comment::Block(text.into()))
    }
}

impl StructuralEq for TypeChild {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeChild::Statement(a), TypeChild::Statement(b)) => a.structural_eq(b),
            (TypeChild::Comment(a), TypeChild::Comment(b)) => a.structural_eq(b),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDefinition {
    pub children: Vec<Spanned<TypeChild>>,
    pub attributes: Attributes,
}

impl TypeDefinition {
    #[cfg(test)]
    pub fn new(statements: impl Into<Vec<TypeStatement>>) -> Self {
        Self {
            children: statements
                .into()
                .into_iter()
                .map(|s| spanned(TypeChild::Statement(spanned(s))))
                .collect(),
            attributes: Default::default(),
        }
    }

    #[cfg(test)]
    pub fn with_children(children: impl Into<Vec<TypeChild>>) -> Self {
        Self {
            children: children
                .into()
                .into_iter()
                .map(spanned)
                .collect(),
            attributes: Default::default(),
        }
    }

    #[cfg(test)]
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}

impl TypeDefinition {
    pub fn statements(&self) -> impl Iterator<Item = &Spanned<TypeStatement>> + '_ {
        self.children
            .iter()
            .filter_map(|child| {
                if let TypeChild::Statement(stmt) = &child.node {
                    Some(stmt)
                } else {
                    None
                }
            })
    }
}

impl StructuralEq for TypeDefinition {
    fn structural_eq(&self, other: &Self) -> bool {
        self.children.structural_eq(&other.children)
            && self.attributes.structural_eq(&other.attributes)
    }
}

// enums
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumStatement {
    pub name: Spanned<Ident>,
    pub expr: Option<Spanned<Expr>>,
    pub attributes: Attributes,
    pub doc_comments: Vec<Spanned<String>>,
}

impl EnumStatement {
    pub fn new(name: Ident, expr: Option<Expr>) -> EnumStatement {
        let dummy_span = Span::new(0, 0);
        EnumStatement {
            name: Spanned::new(name, dummy_span),
            expr: expr.map(|e| Spanned::new(e, dummy_span)),
            attributes: Default::default(),
            doc_comments: vec![],
        }
    }

    #[cfg(test)]
    pub fn field(name: &str) -> EnumStatement {
        Self::new(name.into(), None)
    }

    #[cfg(test)]
    pub fn field_with_expr(name: &str, expr: Expr) -> EnumStatement {
        Self::new(name.into(), Some(expr))
    }

    #[cfg(test)]
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}

impl StructuralEq for EnumStatement {
    fn structural_eq(&self, other: &Self) -> bool {
        self.name.structural_eq(&other.name)
            && self.expr.structural_eq(&other.expr)
            && self.attributes.structural_eq(&other.attributes)
            && self.doc_comments.structural_eq(&other.doc_comments)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EnumChild {
    Statement(Spanned<EnumStatement>),
    Comment(Comment),
}

impl StructuralEq for EnumChild {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (EnumChild::Statement(a), EnumChild::Statement(b)) => a.structural_eq(b),
            (EnumChild::Comment(a), EnumChild::Comment(b)) => a.structural_eq(b),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDefinition {
    pub type_: Spanned<Type>,
    pub children: Vec<Spanned<EnumChild>>,
    pub attributes: Attributes,
}

impl EnumDefinition {
    #[cfg(test)]
    pub fn new(
        type_: Type,
        statements: impl Into<Vec<EnumStatement>>,
        attributes: impl Into<Attributes>,
    ) -> Self {
        Self {
            type_: spanned(type_),
            children: statements
                .into()
                .into_iter()
                .map(|s| spanned(EnumChild::Statement(spanned(s))))
                .collect(),
            attributes: attributes.into(),
        }
    }

    #[cfg(test)]
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}

impl EnumDefinition {
    pub fn statements(&self) -> impl Iterator<Item = &Spanned<EnumStatement>> + '_ {
        self.children
            .iter()
            .filter_map(|child| {
                if let EnumChild::Statement(stmt) = &child.node {
                    Some(stmt)
                } else {
                    None
                }
            })
    }
}

impl StructuralEq for EnumDefinition {
    fn structural_eq(&self, other: &Self) -> bool {
        self.type_.structural_eq(&other.type_)
            && self.children.structural_eq(&other.children)
            && self.attributes.structural_eq(&other.attributes)
    }
}

// bitflags
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BitflagsStatement {
    pub name: Spanned<Ident>,
    pub expr: Spanned<Expr>,
    pub attributes: Attributes,
    pub doc_comments: Vec<Spanned<String>>,
}

impl BitflagsStatement {
    pub fn new(name: Ident, expr: Expr) -> BitflagsStatement {
        let dummy_span = Span::new(0, 0);
        BitflagsStatement {
            name: Spanned::new(name, dummy_span),
            expr: Spanned::new(expr, dummy_span),
            attributes: Default::default(),
            doc_comments: vec![],
        }
    }

    #[cfg(test)]
    pub fn field(name: &str, expr: Expr) -> BitflagsStatement {
        Self::new(name.into(), expr)
    }

    #[cfg(test)]
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}

impl StructuralEq for BitflagsStatement {
    fn structural_eq(&self, other: &Self) -> bool {
        self.name.structural_eq(&other.name)
            && self.expr.structural_eq(&other.expr)
            && self.attributes.structural_eq(&other.attributes)
            && self.doc_comments.structural_eq(&other.doc_comments)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BitflagsChild {
    Statement(Spanned<BitflagsStatement>),
    Comment(Comment),
}

impl StructuralEq for BitflagsChild {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (BitflagsChild::Statement(a), BitflagsChild::Statement(b)) => a.structural_eq(b),
            (BitflagsChild::Comment(a), BitflagsChild::Comment(b)) => a.structural_eq(b),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BitflagsDefinition {
    pub type_: Spanned<Type>,
    pub children: Vec<Spanned<BitflagsChild>>,
    pub attributes: Attributes,
}

impl BitflagsDefinition {
    #[cfg(test)]
    pub fn new(
        type_: Type,
        statements: impl Into<Vec<BitflagsStatement>>,
        attributes: impl Into<Attributes>,
    ) -> Self {
        Self {
            type_: spanned(type_),
            children: statements
                .into()
                .into_iter()
                .map(|s| spanned(BitflagsChild::Statement(spanned(s))))
                .collect(),
            attributes: attributes.into(),
        }
    }

    #[cfg(test)]
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}

impl BitflagsDefinition {
    pub fn statements(&self) -> impl Iterator<Item = &Spanned<BitflagsStatement>> + '_ {
        self.children
            .iter()
            .filter_map(|child| {
                if let BitflagsChild::Statement(stmt) = &child.node {
                    Some(stmt)
                } else {
                    None
                }
            })
    }
}

impl StructuralEq for BitflagsDefinition {
    fn structural_eq(&self, other: &Self) -> bool {
        self.type_.structural_eq(&other.type_)
            && self.children.structural_eq(&other.children)
            && self.attributes.structural_eq(&other.attributes)
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

impl StructuralEq for ItemDefinitionInner {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ItemDefinitionInner::Type(a), ItemDefinitionInner::Type(b)) => a.structural_eq(b),
            (ItemDefinitionInner::Enum(a), ItemDefinitionInner::Enum(b)) => a.structural_eq(b),
            (ItemDefinitionInner::Bitflags(a), ItemDefinitionInner::Bitflags(b)) => {
                a.structural_eq(b)
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemDefinition {
    pub visibility: Visibility,
    pub name: Spanned<Ident>,
    pub doc_comments: Vec<Spanned<String>>,
    pub inner: ItemDefinitionInner,
}

impl ItemDefinition {
    #[cfg(test)]
    pub fn new(
        (visibility, name): (Visibility, &str),
        inner: impl Into<ItemDefinitionInner>,
    ) -> Self {
        Self {
            visibility,
            name: spanned(name.into()),
            doc_comments: vec![],
            inner: inner.into(),
        }
    }

    #[cfg(test)]
    pub fn with_doc_comments<I, S>(mut self, doc_comments: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.doc_comments = doc_comments
            .into_iter()
            .map(|s| spanned(s.into()))
            .collect();
        self
    }
}

impl StructuralEq for ItemDefinition {
    fn structural_eq(&self, other: &Self) -> bool {
        self.visibility.structural_eq(&other.visibility)
            && self.name.structural_eq(&other.name)
            && self.doc_comments.structural_eq(&other.doc_comments)
            && self.inner.structural_eq(&other.inner)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImplChild {
    Function(Spanned<Function>),
    Comment(Comment),
}

impl StructuralEq for ImplChild {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ImplChild::Function(a), ImplChild::Function(b)) => a.structural_eq(b),
            (ImplChild::Comment(a), ImplChild::Comment(b)) => a.structural_eq(b),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionBlock {
    pub name: Spanned<Ident>,
    pub children: Vec<Spanned<ImplChild>>,
    pub attributes: Attributes,
}

impl FunctionBlock {
    #[cfg(test)]
    pub fn new(name: impl Into<Ident>, functions: impl Into<Vec<Function>>) -> Self {
        Self {
            name: spanned(name.into()),
            children: functions
                .into()
                .into_iter()
                .map(|f| spanned(ImplChild::Function(spanned(f))))
                .collect(),
            attributes: Default::default(),
        }
    }

    #[cfg(test)]
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}

impl FunctionBlock {
    pub fn functions(&self) -> impl Iterator<Item = &Spanned<Function>> + '_ {
        self.children
            .iter()
            .filter_map(|child| {
                if let ImplChild::Function(f) = &child.node {
                    Some(f)
                } else {
                    None
                }
            })
    }
}

impl StructuralEq for FunctionBlock {
    fn structural_eq(&self, other: &Self) -> bool {
        self.name.structural_eq(&other.name)
            && self.children.structural_eq(&other.children)
            && self.attributes.structural_eq(&other.attributes)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Backend {
    pub name: Spanned<Ident>,
    pub prologue: Option<String>,
    pub epilogue: Option<String>,
}

impl Backend {
    #[cfg(test)]
    pub fn new(name: &str) -> Self {
        Self {
            name: spanned(name.into()),
            prologue: None,
            epilogue: None,
        }
    }

    #[cfg(test)]
    pub fn with_prologue(mut self, prologue: impl Into<String>) -> Self {
        self.prologue = Some(prologue.into());
        self
    }

    #[cfg(test)]
    pub fn with_epilogue(mut self, epilogue: impl Into<String>) -> Self {
        self.epilogue = Some(epilogue.into());
        self
    }
}

impl StructuralEq for Backend {
    fn structural_eq(&self, other: &Self) -> bool {
        self.name.structural_eq(&other.name)
            && self.prologue == other.prologue
            && self.epilogue == other.epilogue
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternValue {
    pub visibility: Visibility,
    pub name: Spanned<Ident>,
    pub type_: Spanned<Type>,
    pub attributes: Attributes,
}

impl ExternValue {
    #[cfg(test)]
    pub fn new(
        visibility: Visibility,
        name: &str,
        type_: Type,
        attributes: impl Into<Attributes>,
    ) -> Self {
        Self {
            visibility,
            name: spanned(name.into()),
            type_: spanned(type_),
            attributes: attributes.into(),
        }
    }
}

impl StructuralEq for ExternValue {
    fn structural_eq(&self, other: &Self) -> bool {
        self.visibility.structural_eq(&other.visibility)
            && self.name.structural_eq(&other.name)
            && self.type_.structural_eq(&other.type_)
            && self.attributes.structural_eq(&other.attributes)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModuleChild {
    Use(Spanned<ItemPath>),
    ExternType(Spanned<Ident>, Attributes),
    ExternValue(Spanned<ExternValue>),
    Backend(Spanned<Backend>),
    Impl(Spanned<FunctionBlock>),
    Function(Spanned<Function>),
    Definition(Spanned<ItemDefinition>),
    Comment(Comment),
}

impl StructuralEq for ModuleChild {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ModuleChild::Use(a), ModuleChild::Use(b)) => a.structural_eq(b),
            (ModuleChild::ExternType(na, aa), ModuleChild::ExternType(nb, ab)) => {
                na.structural_eq(nb) && aa.structural_eq(ab)
            }
            (ModuleChild::ExternValue(a), ModuleChild::ExternValue(b)) => a.structural_eq(b),
            (ModuleChild::Backend(a), ModuleChild::Backend(b)) => a.structural_eq(b),
            (ModuleChild::Impl(a), ModuleChild::Impl(b)) => a.structural_eq(b),
            (ModuleChild::Function(a), ModuleChild::Function(b)) => a.structural_eq(b),
            (ModuleChild::Definition(a), ModuleChild::Definition(b)) => a.structural_eq(b),
            (ModuleChild::Comment(a), ModuleChild::Comment(b)) => a.structural_eq(b),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Module {
    pub children: Vec<Spanned<ModuleChild>>,
    pub attributes: Attributes,
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    #[cfg(test)]
    pub fn with_uses(mut self, uses: impl Into<Vec<ItemPath>>) -> Self {
        for use_item in uses.into() {
            self.children
                .push(spanned(ModuleChild::Use(spanned(use_item))));
        }
        self
    }

    #[cfg(test)]
    pub fn with_extern_types(mut self, extern_types: impl Into<Vec<(Ident, Attributes)>>) -> Self {
        for (ident, attrs) in extern_types.into() {
            self.children
                .push(spanned(ModuleChild::ExternType(spanned(ident), attrs)));
        }
        self
    }

    #[cfg(test)]
    pub fn with_extern_values(mut self, extern_values: impl Into<Vec<ExternValue>>) -> Self {
        for ev in extern_values.into() {
            self.children
                .push(spanned(ModuleChild::ExternValue(spanned(ev))));
        }
        self
    }

    #[cfg(test)]
    pub fn with_functions(mut self, functions: impl Into<Vec<Function>>) -> Self {
        for func in functions.into() {
            self.children
                .push(spanned(ModuleChild::Function(spanned(func))));
        }
        self
    }

    #[cfg(test)]
    pub fn with_definitions(mut self, definitions: impl Into<Vec<ItemDefinition>>) -> Self {
        for def in definitions.into() {
            self.children
                .push(spanned(ModuleChild::Definition(spanned(def))));
        }
        self
    }

    #[cfg(test)]
    pub fn with_impls(mut self, impls: impl Into<Vec<FunctionBlock>>) -> Self {
        for impl_block in impls.into() {
            self.children
                .push(spanned(ModuleChild::Impl(spanned(impl_block))));
        }
        self
    }

    #[cfg(test)]
    pub fn with_backends(mut self, backends: impl Into<Vec<Backend>>) -> Self {
        for backend in backends.into() {
            self.children
                .push(spanned(ModuleChild::Backend(spanned(backend))));
        }
        self
    }

    #[cfg(test)]
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }

    #[cfg(test)]
    pub fn with_module_doc_comments<I, S>(mut self, module_doc_comments: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        for comment in module_doc_comments.into_iter() {
            self.children.push(spanned(ModuleChild::Comment(Comment::ModuleDoc(
                comment.into(),
            ))));
        }
        self
    }
}

impl Module {
    // Helper methods to extract items from children
    pub fn uses(&self) -> impl Iterator<Item = &Spanned<ItemPath>> + '_ {
        self.children
            .iter()
            .filter_map(|child| {
                if let ModuleChild::Use(path) = &child.node {
                    Some(path)
                } else {
                    None
                }
            })
    }

    pub fn extern_types(&self) -> impl Iterator<Item = (&Spanned<Ident>, &Attributes)> + '_ {
        self.children
            .iter()
            .filter_map(|child| {
                if let ModuleChild::ExternType(name, attrs) = &child.node {
                    Some((name, attrs))
                } else {
                    None
                }
            })
    }

    pub fn extern_values(&self) -> impl Iterator<Item = &Spanned<ExternValue>> + '_ {
        self.children
            .iter()
            .filter_map(|child| {
                if let ModuleChild::ExternValue(ev) = &child.node {
                    Some(ev)
                } else {
                    None
                }
            })
    }

    pub fn backends(&self) -> impl Iterator<Item = &Spanned<Backend>> + '_ {
        self.children
            .iter()
            .filter_map(|child| {
                if let ModuleChild::Backend(b) = &child.node {
                    Some(b)
                } else {
                    None
                }
            })
    }

    pub fn impls(&self) -> impl Iterator<Item = &Spanned<FunctionBlock>> + '_ {
        self.children
            .iter()
            .filter_map(|child| {
                if let ModuleChild::Impl(ib) = &child.node {
                    Some(ib)
                } else {
                    None
                }
            })
    }

    pub fn functions(&self) -> impl Iterator<Item = &Spanned<Function>> + '_ {
        self.children
            .iter()
            .filter_map(|child| {
                if let ModuleChild::Function(f) = &child.node {
                    Some(f)
                } else {
                    None
                }
            })
    }

    pub fn definitions(&self) -> impl Iterator<Item = &Spanned<ItemDefinition>> + '_ {
        self.children
            .iter()
            .filter_map(|child| {
                if let ModuleChild::Definition(d) = &child.node {
                    Some(d)
                } else {
                    None
                }
            })
    }

    pub fn module_doc_comments(&self) -> impl Iterator<Item = Spanned<String>> + '_ {
        self.children
            .iter()
            .filter_map(|child| {
                if let ModuleChild::Comment(Comment::ModuleDoc(s)) = &child.node {
                    Some(Spanned::new(s.clone(), child.span))
                } else {
                    None
                }
            })
    }
}

impl StructuralEq for Module {
    fn structural_eq(&self, other: &Self) -> bool {
        self.children.structural_eq(&other.children)
            && self.attributes.structural_eq(&other.attributes)
    }
}
