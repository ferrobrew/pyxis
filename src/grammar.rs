use std::{fmt, path::Path};

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
    ConstPointer(Box<Type>),
    MutPointer(Box<Type>),
    Array(Box<Type>, usize),
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

    pub fn const_pointer(self) -> Type {
        Type::ConstPointer(Box::new(self))
    }

    pub fn mut_pointer(self) -> Type {
        Type::MutPointer(Box::new(self))
    }

    pub fn array(self, size: usize) -> Type {
        Type::Array(Box::new(self), size)
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Attribute {
    Ident(Ident),
    Function(Ident, Vec<Expr>),
    Assign(Ident, Expr),
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
    pub fn function(&self) -> Option<(&Ident, &Vec<Expr>)> {
        match self {
            Attribute::Function(ident, exprs) => Some((ident, exprs)),
            _ => None,
        }
    }
    pub fn integer_fn(name: &str, value: isize) -> Self {
        Attribute::Function(name.into(), vec![Expr::IntLiteral(value)])
    }
    pub fn address(address: usize) -> Self {
        Self::integer_fn("address", address as isize)
    }
    pub fn size(size: usize) -> Self {
        Self::integer_fn("size", size as isize)
    }
    pub fn align(align: usize) -> Self {
        Self::integer_fn("align", align as isize)
    }
    pub fn singleton(address: usize) -> Self {
        Self::integer_fn("singleton", address as isize)
    }
    pub fn index(index: usize) -> Self {
        Self::integer_fn("index", index as isize)
    }
    pub fn calling_convention(name: &str) -> Self {
        Attribute::Function(
            "calling_convention".into(),
            vec![Expr::StringLiteral(name.into())],
        )
    }

    // Assign attributes
    pub fn assign(&self) -> Option<(&Ident, &Expr)> {
        match self {
            Attribute::Assign(ident, expr) => Some((ident, expr)),
            _ => None,
        }
    }
    pub fn doc(doc: &str) -> Self {
        Attribute::Assign("doc".into(), Expr::StringLiteral(doc.into()))
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
impl Attributes {
    pub fn doc(&self, path: &ItemPath) -> anyhow::Result<Option<String>> {
        let mut doc = None;
        for attr in &self.0 {
            let Some((key, value)) = attr.assign() else {
                continue;
            };
            if key.as_str() != "doc" {
                continue;
            }

            let Some(value) = value.string_literal() else {
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Argument {
    ConstSelf,
    MutSelf,
    Named(Ident, Type),
}
impl Argument {
    pub fn named(ident: impl Into<Ident>, type_: impl Into<Type>) -> Argument {
        Argument::Named(ident.into(), type_.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub visibility: Visibility,
    pub name: Ident,
    pub attributes: Attributes,
    pub arguments: Vec<Argument>,
    pub return_type: Option<Type>,
}
impl Function {
    pub fn new(
        (visibility, name): (Visibility, &str),
        arguments: impl Into<Vec<Argument>>,
    ) -> Self {
        Self {
            visibility,
            name: name.into(),
            attributes: Default::default(),
            arguments: arguments.into(),
            return_type: None,
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
    pub fn with_return_type(mut self, return_type: impl Into<Type>) -> Self {
        self.return_type = Some(return_type.into());
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
    Field(Visibility, Ident, Type),
    Vftable(Vec<Function>),
}
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

    pub fn is_vftable(&self) -> bool {
        matches!(self, TypeField::Vftable(_))
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeStatement {
    pub field: TypeField,
    pub attributes: Attributes,
}
impl TypeStatement {
    pub fn field((visibility, name): (Visibility, &str), type_: Type) -> TypeStatement {
        TypeStatement {
            field: TypeField::Field(visibility, name.into(), type_),
            attributes: Default::default(),
        }
    }
    pub fn vftable(functions: impl IntoIterator<Item = Function>) -> TypeStatement {
        TypeStatement {
            field: TypeField::vftable(functions),
            attributes: Default::default(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDefinition {
    pub statements: Vec<TypeStatement>,
    pub attributes: Attributes,
}
impl TypeDefinition {
    pub fn new(statements: impl Into<Vec<TypeStatement>>) -> Self {
        Self {
            statements: statements.into(),
            attributes: Default::default(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}

// enums
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumStatement {
    pub name: Ident,
    pub expr: Option<Expr>,
    pub attributes: Attributes,
}
impl EnumStatement {
    pub fn new(name: Ident, expr: Option<Expr>) -> EnumStatement {
        EnumStatement {
            name,
            expr,
            attributes: Default::default(),
        }
    }
    pub fn field(name: &str) -> EnumStatement {
        Self::new(name.into(), None)
    }
    pub fn field_with_expr(name: &str, expr: Expr) -> EnumStatement {
        Self::new(name.into(), Some(expr))
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDefinition {
    pub type_: Type,
    pub statements: Vec<EnumStatement>,
    pub attributes: Attributes,
}
impl EnumDefinition {
    pub fn new(
        type_: Type,
        statements: impl Into<Vec<EnumStatement>>,
        attributes: impl Into<Attributes>,
    ) -> Self {
        Self {
            type_,
            statements: statements.into(),
            attributes: attributes.into(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}

// bitflags
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BitflagsStatement {
    pub name: Ident,
    pub expr: Expr,
    pub attributes: Attributes,
}
impl BitflagsStatement {
    pub fn new(name: Ident, expr: Expr) -> BitflagsStatement {
        BitflagsStatement {
            name,
            expr,
            attributes: Default::default(),
        }
    }
    pub fn field(name: &str, expr: Expr) -> BitflagsStatement {
        Self::new(name.into(), expr)
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BitflagsDefinition {
    pub type_: Type,
    pub statements: Vec<BitflagsStatement>,
    pub attributes: Attributes,
}
impl BitflagsDefinition {
    pub fn new(
        type_: Type,
        statements: impl Into<Vec<BitflagsStatement>>,
        attributes: impl Into<Attributes>,
    ) -> Self {
        Self {
            type_,
            statements: statements.into(),
            attributes: attributes.into(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
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
    pub inner: ItemDefinitionInner,
}
impl ItemDefinition {
    pub fn new(
        (visibility, name): (Visibility, &str),
        inner: impl Into<ItemDefinitionInner>,
    ) -> Self {
        Self {
            visibility,
            name: name.into(),
            inner: inner.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionBlock {
    pub name: Ident,
    pub functions: Vec<Function>,
    pub attributes: Attributes,
}
impl FunctionBlock {
    pub fn new(name: impl Into<Ident>, functions: impl Into<Vec<Function>>) -> Self {
        Self {
            name: name.into(),
            functions: functions.into(),
            attributes: Default::default(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Backend {
    pub name: Ident,
    pub prologue: Option<String>,
    pub epilogue: Option<String>,
}
impl Backend {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.into(),
            prologue: None,
            epilogue: None,
        }
    }
    pub fn with_prologue(mut self, prologue: impl Into<String>) -> Self {
        self.prologue = Some(prologue.into());
        self
    }
    pub fn with_epilogue(mut self, epilogue: impl Into<String>) -> Self {
        self.epilogue = Some(epilogue.into());
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternValue {
    pub visibility: Visibility,
    pub name: Ident,
    pub type_: Type,
    pub attributes: Attributes,
}
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
            type_,
            attributes: attributes.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Module {
    pub uses: Vec<ItemPath>,
    pub extern_types: Vec<(Ident, Attributes)>,
    pub extern_values: Vec<ExternValue>,
    pub definitions: Vec<ItemDefinition>,
    pub impls: Vec<FunctionBlock>,
    pub backends: Vec<Backend>,
    pub attributes: Attributes,
}
impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_uses(mut self, uses: impl Into<Vec<ItemPath>>) -> Self {
        self.uses = uses.into();
        self
    }
    pub fn with_extern_types(mut self, extern_types: impl Into<Vec<(Ident, Attributes)>>) -> Self {
        self.extern_types = extern_types.into();
        self
    }
    pub fn with_extern_values(mut self, extern_values: impl Into<Vec<ExternValue>>) -> Self {
        self.extern_values = extern_values.into();
        self
    }
    pub fn with_definitions(mut self, definitions: impl Into<Vec<ItemDefinition>>) -> Self {
        self.definitions = definitions.into();
        self
    }
    pub fn with_impls(mut self, impls: impl Into<Vec<FunctionBlock>>) -> Self {
        self.impls = impls.into();
        self
    }
    pub fn with_backends(mut self, backends: impl Into<Vec<Backend>>) -> Self {
        self.backends = backends.into();
        self
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}
