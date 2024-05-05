use std::{fmt, path::Path};

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    ConstPointer(Box<Type>),
    MutPointer(Box<Type>),
    Array(Box<Type>, usize),
    Ident(Ident),
}
impl Type {
    pub fn ident(ident: &str) -> Type {
        Type::Ident(ident.into())
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

    pub fn from_colon_delimited_str(path: &str) -> ItemPath {
        ItemPath(path.split("::").map(|s| s.into()).collect())
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroCall {
    pub name: Ident,
    pub arguments: Vec<Expr>,
}
impl MacroCall {
    pub fn new(name: &str, arguments: &[Expr]) -> Self {
        Self {
            name: name.into(),
            arguments: arguments.to_vec(),
        }
    }

    pub fn unk(size: usize) -> Self {
        MacroCall {
            name: "unk".into(),
            arguments: vec![Expr::IntLiteral(size as isize)],
        }
    }

    pub fn match_repr(&self) -> (&str, &[Expr]) {
        (self.name.as_str(), self.arguments.as_slice())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    IntLiteral(isize),
    StringLiteral(String),
    Macro(MacroCall),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attribute {
    Function(Ident, Vec<Expr>),
}
impl Attribute {
    pub fn address(address: usize) -> Self {
        Attribute::Function("address".into(), vec![Expr::IntLiteral(address as isize)])
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Argument {
    ConstSelf,
    MutSelf,
    Field(TypeField),
}
impl Argument {
    pub fn field(ident: &str, type_ref: TypeRef) -> Argument {
        Argument::Field(TypeField::new(ident, type_ref))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: Ident,
    pub attributes: Vec<Attribute>,
    pub arguments: Vec<Argument>,
    pub return_type: Option<Type>,
}
impl Function {
    pub fn new(
        name: &str,
        attributes: &[Attribute],
        arguments: &[Argument],
        return_type: Option<Type>,
    ) -> Self {
        Self {
            name: name.into(),
            attributes: attributes.to_vec(),
            arguments: arguments.to_vec(),
            return_type,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeRef {
    Type(Type),
    Macro(MacroCall),
}
impl TypeRef {
    pub fn ident_type(name: &str) -> TypeRef {
        TypeRef::Type(name.into())
    }

    pub fn macro_(name: &str, args: &[Expr]) -> TypeRef {
        TypeRef::Macro(MacroCall::new(name, args))
    }
}
impl From<Type> for TypeRef {
    fn from(item: Type) -> Self {
        TypeRef::Type(item)
    }
}
impl From<MacroCall> for TypeRef {
    fn from(item: MacroCall) -> Self {
        TypeRef::Macro(item)
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OptionalExprField(pub Ident, pub Option<Expr>);
impl OptionalExprField {
    pub fn ident(&self) -> &Ident {
        &self.0
    }

    pub fn ident_as_str(&self) -> &str {
        self.0.as_str()
    }
}
impl From<Ident> for OptionalExprField {
    fn from(item: Ident) -> Self {
        OptionalExprField(item, None)
    }
}
impl From<(Ident, Expr)> for OptionalExprField {
    fn from(item: (Ident, Expr)) -> Self {
        OptionalExprField(item.0, Some(item.1))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeField(pub Ident, pub TypeRef);
impl TypeField {
    pub fn new(name: &str, type_ref: TypeRef) -> TypeField {
        TypeField(name.into(), type_ref)
    }
}
impl From<(Ident, TypeRef)> for TypeField {
    fn from(item: (Ident, TypeRef)) -> Self {
        TypeField(item.0, item.1)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeStatement {
    Meta(Vec<ExprField>),
    Address(usize, Vec<TypeField>),
    Functions(Vec<(Ident, Vec<Function>)>),
    Field(TypeField),
    Macro(MacroCall),
}
impl TypeStatement {
    pub fn meta(fields: &[(&str, Expr)]) -> TypeStatement {
        TypeStatement::Meta(
            fields
                .iter()
                .map(|(n, e)| ((*n).into(), e.clone()).into())
                .collect(),
        )
    }
    pub fn address(address: usize, fields: &[(&str, TypeRef)]) -> TypeStatement {
        TypeStatement::Address(
            address,
            fields
                .iter()
                .map(|(n, t)| ((*n).into(), t.clone()).into())
                .collect(),
        )
    }
    pub fn functions(functions: &[(&str, &[Function])]) -> TypeStatement {
        TypeStatement::Functions(
            functions
                .iter()
                .map(|&(i, f)| (i.into(), f.to_vec()))
                .collect(),
        )
    }
    pub fn field(name: &str, type_ref: TypeRef) -> TypeStatement {
        TypeStatement::Field((name.into(), type_ref).into())
    }
    pub fn macro_(ident: &str, exprs: &[Expr]) -> TypeStatement {
        TypeStatement::Macro(MacroCall::new(ident, exprs))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinition {
    pub statements: Vec<TypeStatement>,
}
impl TypeDefinition {
    pub fn new(statements: &[TypeStatement]) -> Self {
        Self {
            statements: statements.to_vec(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EnumStatement {
    Meta(Vec<ExprField>),
    Field(OptionalExprField),
}
impl EnumStatement {
    pub fn meta(fields: &[(&str, Expr)]) -> EnumStatement {
        EnumStatement::Meta(
            fields
                .iter()
                .map(|(n, e)| ((*n).into(), e.clone()).into())
                .collect(),
        )
    }
    pub fn field(name: &str) -> EnumStatement {
        EnumStatement::Field(Ident::from(name).into())
    }
    pub fn field_with_expr(name: &str, expr: Expr) -> EnumStatement {
        EnumStatement::Field((name.into(), expr).into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDefinition {
    pub ty: TypeRef,
    pub statements: Vec<EnumStatement>,
}
impl EnumDefinition {
    pub fn new(ty: TypeRef, statements: &[EnumStatement]) -> Self {
        Self {
            ty,
            statements: statements.to_vec(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Module {
    pub uses: Vec<ItemPath>,
    pub extern_types: Vec<(Ident, Vec<ExprField>)>,
    pub extern_values: Vec<(Ident, Type, usize)>,
    pub definitions: Vec<ItemDefinition>,
}
impl Module {
    pub fn new(
        uses: &[ItemPath],
        extern_types: &[(Ident, Vec<ExprField>)],
        extern_values: &[(Ident, Type, usize)],
        definitions: &[ItemDefinition],
    ) -> Self {
        Self {
            uses: uses.to_vec(),
            extern_types: extern_types.to_vec(),
            extern_values: extern_values.to_vec(),
            definitions: definitions.to_vec(),
        }
    }
}
