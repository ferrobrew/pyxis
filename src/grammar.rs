use std::{fmt, path::Path};

pub mod test_aliases {
    pub type M = super::Module;
    pub type ID = super::ItemDefinition;
    pub type TS = super::TypeStatement;
    pub type TD = super::TypeDefinition;
    pub type ES = super::EnumStatement;
    pub type ED = super::EnumDefinition;
    pub type T = super::Type;
    pub type A = super::Attribute;
    pub type Ar = super::Argument;
    pub type TF = super::TypeField;
    pub type E = super::Expr;
    pub type F = super::Function;
    pub type FB = super::FunctionBlock;
    pub type IP = super::ItemPath;
    pub type B = super::Backend;
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Attribute {
    Ident(Ident),
    Function(Ident, Vec<Expr>),
}
impl Attribute {
    pub fn integer_fn(name: &str, value: isize) -> Self {
        Attribute::Function(name.into(), vec![Expr::IntLiteral(value)])
    }

    pub fn address(address: usize) -> Self {
        Self::integer_fn("address", address as isize)
    }

    pub fn size(size: usize) -> Self {
        Self::integer_fn("size", size as isize)
    }

    pub fn singleton(address: usize) -> Self {
        Self::integer_fn("singleton", address as isize)
    }

    pub fn index(index: usize) -> Self {
        Self::integer_fn("index", index as isize)
    }

    pub fn function(&self) -> Option<(&Ident, &Vec<Expr>)> {
        match self {
            Attribute::Ident(_) => None,
            Attribute::Function(ident, exprs) => Some((ident, exprs)),
        }
    }

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

    // HACK_SKIP_VFTABLE: <https://github.com/philpax/pyxis/issues/13>
    pub fn hack_skip_vftable() -> Self {
        Attribute::Ident("hack_skip_vftable".into())
    }
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
    pub name: Ident,
    pub attributes: Vec<Attribute>,
    pub arguments: Vec<Argument>,
    pub return_type: Option<Type>,
}
impl Function {
    pub fn new(name: &str, arguments: impl Into<Vec<Argument>>) -> Self {
        Self {
            name: name.into(),
            attributes: vec![],
            arguments: arguments.into(),
            return_type: None,
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Vec<Attribute>>) -> Self {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeField {
    Field(Ident, Type),
    Vftable(Vec<Function>),
}
impl TypeField {
    pub fn field(name: impl Into<Ident>, type_: impl Into<Type>) -> TypeField {
        TypeField::Field(name.into(), type_.into())
    }

    pub fn vftable(functions: impl IntoIterator<Item = Function>) -> TypeField {
        TypeField::Vftable(functions.into_iter().collect())
    }

    pub fn is_vftable(&self) -> bool {
        matches!(self, TypeField::Vftable(_))
    }
}
impl From<(Ident, Type)> for TypeField {
    fn from(item: (Ident, Type)) -> Self {
        TypeField::Field(item.0, item.1)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeStatement {
    pub field: TypeField,
    pub attributes: Vec<Attribute>,
}
impl TypeStatement {
    pub fn field(name: &str, type_: Type) -> TypeStatement {
        TypeStatement {
            field: (name.into(), type_).into(),
            attributes: vec![],
        }
    }
    pub fn vftable(
        functions: impl IntoIterator<Item = Function>,
        attributes: impl Into<Vec<Attribute>>,
    ) -> TypeStatement {
        TypeStatement {
            field: TypeField::vftable(functions),
            attributes: attributes.into(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Vec<Attribute>>) -> Self {
        self.attributes = attributes.into();
        self
    }
}
impl From<TypeField> for TypeStatement {
    fn from(field: TypeField) -> Self {
        TypeStatement {
            field,
            attributes: vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDefinition {
    pub statements: Vec<TypeStatement>,
    pub attributes: Vec<Attribute>,
}
impl TypeDefinition {
    pub fn new(statements: impl Into<Vec<TypeStatement>>) -> Self {
        Self {
            statements: statements.into(),
            attributes: vec![],
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Vec<Attribute>>) -> Self {
        self.attributes = attributes.into();
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumStatement {
    pub name: Ident,
    pub expr: Option<Expr>,
    pub attributes: Vec<Attribute>,
}
impl EnumStatement {
    pub fn new(name: Ident, expr: Option<Expr>) -> EnumStatement {
        EnumStatement {
            name,
            expr,
            attributes: vec![],
        }
    }
    pub fn field(name: &str) -> EnumStatement {
        Self::new(name.into(), None)
    }
    pub fn field_with_expr(name: &str, expr: Expr) -> EnumStatement {
        Self::new(name.into(), Some(expr))
    }
    pub fn with_attributes(mut self, attributes: impl Into<Vec<Attribute>>) -> Self {
        self.attributes = attributes.into();
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDefinition {
    pub type_: Type,
    pub statements: Vec<EnumStatement>,
    pub attributes: Vec<Attribute>,
}
impl EnumDefinition {
    pub fn new(
        type_: Type,
        statements: impl Into<Vec<EnumStatement>>,
        attributes: impl Into<Vec<Attribute>>,
    ) -> Self {
        Self {
            type_,
            statements: statements.into(),
            attributes: attributes.into(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Vec<Attribute>>) -> Self {
        self.attributes = attributes.into();
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ItemDefinitionInner {
    Type(TypeDefinition),
    Enum(EnumDefinition),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemDefinition {
    pub name: Ident,
    pub inner: ItemDefinitionInner,
}
impl ItemDefinition {
    pub fn new(name: &str, inner: impl Into<ItemDefinitionInner>) -> Self {
        Self {
            name: name.into(),
            inner: inner.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionBlock {
    pub name: Ident,
    pub functions: Vec<Function>,
    pub attributes: Vec<Attribute>,
}
impl FunctionBlock {
    pub fn new(name: impl Into<Ident>, functions: impl Into<Vec<Function>>) -> Self {
        Self {
            name: name.into(),
            functions: functions.into(),
            attributes: vec![],
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Vec<Attribute>>) -> Self {
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

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Module {
    pub uses: Vec<ItemPath>,
    pub extern_types: Vec<(Ident, Vec<Attribute>)>,
    pub extern_values: Vec<(Ident, Type, Vec<Attribute>)>,
    pub definitions: Vec<ItemDefinition>,
    pub impls: Vec<FunctionBlock>,
    pub backends: Vec<Backend>,
}
impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_uses(mut self, uses: impl Into<Vec<ItemPath>>) -> Self {
        self.uses = uses.into();
        self
    }

    pub fn with_extern_types(
        mut self,
        extern_types: impl Into<Vec<(Ident, Vec<Attribute>)>>,
    ) -> Self {
        self.extern_types = extern_types.into();
        self
    }

    pub fn with_extern_values(
        mut self,
        extern_values: impl Into<Vec<(Ident, Type, Vec<Attribute>)>>,
    ) -> Self {
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
}
