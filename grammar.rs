#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident(pub String);
impl From<&str> for Ident {
    fn from(item: &str) -> Self {
        Ident(item.to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    ConstPointer(Box<Type>),
    MutPointer(Box<Type>),
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
}
impl From<&str> for Type {
    fn from(item: &str) -> Self {
        Type::Ident(item.into())
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    IntLiteral(isize),
    StringLiteral(String),
    Macro(MacroCall),
    Ident(Ident),
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
impl From<(Ident, Expr)> for ExprField {
    fn from(item: (Ident, Expr)) -> Self {
        ExprField(item.0, item.1)
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
    pub name: Ident,
    pub statements: Vec<TypeStatement>,
}
impl TypeDefinition {
    pub fn new(name: &str, statements: &[TypeStatement]) -> Self {
        Self {
            name: name.into(),
            statements: statements.to_vec(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub definitions: Vec<TypeDefinition>,
}
impl Module {
    pub fn new(definitions: &[TypeDefinition]) -> Self {
        Self {
            definitions: definitions.to_vec(),
        }
    }
}
