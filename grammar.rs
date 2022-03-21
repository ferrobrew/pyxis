use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{braced, bracketed, parenthesized, Token};

mod kw {
    syn::custom_keyword!(meta);
    syn::custom_keyword!(address);
    syn::custom_keyword!(functions);
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident(String);
impl Parse for Ident {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Ident(input.parse::<syn::Ident>()?.to_string()))
    }
}
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
impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::Ident) {
            // dodgy hack to "support" generics for now
            let ident: syn::Ident = input.parse()?;
            let mut name = ident.to_string();

            loop {
                if input.peek(syn::Token![<]) {
                    input.parse::<syn::Token![<]>()?;
                    name += "<";
                } else if input.peek(syn::Ident) {
                    let ident: syn::Ident = input.parse()?;
                    name += &ident.to_string();
                } else if input.peek(syn::Token![>]) {
                    input.parse::<syn::Token![>]>()?;
                    name += ">";
                } else {
                    break;
                }
            }

            Ok(Type::Ident(name.as_str().into()))
        } else if lookahead.peek(syn::Token![*]) {
            input.parse::<syn::Token![*]>()?;

            let lookahead = input.lookahead1();
            if lookahead.peek(syn::Token![const]) {
                input.parse::<syn::Token![const]>()?;
                Ok(Type::ConstPointer(Box::new(input.parse()?)))
            } else if lookahead.peek(syn::Token![mut]) {
                input.parse::<syn::Token![mut]>()?;
                Ok(Type::MutPointer(Box::new(input.parse()?)))
            } else {
                Err(lookahead.error())
            }
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroCall {
    name: Ident,
    arguments: Vec<Expr>,
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
impl Parse for MacroCall {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![!]>()?;

        let content;
        parenthesized!(content in input);

        let arguments: Punctuated<_, Token![,]> = content.parse_terminated(Expr::parse)?;
        let arguments = Vec::from_iter(arguments.into_iter());

        Ok(MacroCall { name, arguments })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    IntLiteral(isize),
    StringLiteral(String),
    Macro(MacroCall),
    Ident(Ident),
}
impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::Ident) {
            if input.peek2(syn::Token![!]) {
                Ok(Expr::Macro(input.parse()?))
            } else {
                Ok(Expr::Ident(input.parse()?))
            }
        } else if lookahead.peek(syn::LitInt) {
            let lit: syn::LitInt = input.parse()?;
            Ok(Expr::IntLiteral(lit.base10_parse()?))
        } else if lookahead.peek(syn::LitStr) {
            let lit: syn::LitStr = input.parse()?;
            Ok(Expr::StringLiteral(lit.value()))
        } else {
            Err(lookahead.error())
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
impl Parse for Attribute {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<syn::Token![#]>()?;

        let content;
        bracketed!(content in input);

        let name = content.parse()?;

        let content2;
        parenthesized!(content2 in content);

        let arguments: Punctuated<_, Token![,]> = content2.parse_terminated(Expr::parse)?;
        let arguments = Vec::from_iter(arguments.into_iter());

        Ok(Attribute::Function(name, arguments))
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
impl Parse for Argument {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::Token![&]) {
            input.parse::<syn::Token![&]>()?;

            let lookahead = input.lookahead1();
            if lookahead.peek(syn::Token![mut]) {
                input.parse::<syn::Token![mut]>()?;
                input.parse::<syn::Token![self]>()?;

                Ok(Argument::MutSelf)
            } else if lookahead.peek(syn::Token![self]) {
                input.parse::<syn::Token![self]>()?;

                Ok(Argument::ConstSelf)
            } else {
                Err(lookahead.error())
            }
        } else if lookahead.peek(syn::Ident) {
            Ok(Argument::Field(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    name: Ident,
    attributes: Vec<Attribute>,
    arguments: Vec<Argument>,
    return_type: Option<Type>,
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
impl Parse for Function {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut attributes = vec![];
        while input.peek(syn::Token![#]) {
            attributes.push(input.parse()?);
        }

        input.parse::<syn::Token![fn]>()?;
        let name: Ident = input.parse()?;

        let content;
        parenthesized!(content in input);

        let arguments: Punctuated<_, Token![,]> = content.parse_terminated(Argument::parse)?;
        let arguments = Vec::from_iter(arguments.into_iter());

        let return_type = if input.peek(syn::Token![->]) {
            input.parse::<syn::Token![->]>()?;
            Some(input.parse()?)
        } else {
            None
        };

        Ok(Function {
            name,
            attributes,
            arguments,
            return_type,
        })
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
impl Parse for TypeRef {
    fn parse(input: ParseStream) -> Result<Self> {
        use syn::parse::discouraged::Speculative;

        let ahead = input.fork();
        if let Ok(macro_call) = ahead.call(MacroCall::parse) {
            input.advance_to(&ahead);
            Ok(TypeRef::Macro(macro_call))
        } else {
            Ok(TypeRef::Type(input.parse()?))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprField(Ident, Expr);
impl From<(Ident, Expr)> for ExprField {
    fn from(item: (Ident, Expr)) -> Self {
        ExprField(item.0, item.1)
    }
}
impl Parse for ExprField {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        Ok(ExprField(name, input.parse()?))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeField(Ident, TypeRef);
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
impl Parse for TypeField {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        Ok(TypeField(name, input.parse()?))
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
impl Parse for TypeStatement {
    fn parse(input: ParseStream) -> Result<Self> {
        use syn::parse::discouraged::Speculative;

        let lookahead = input.lookahead1();
        if lookahead.peek(kw::meta) {
            input.parse::<kw::meta>()?;
            let content;
            braced!(content in input);

            let fields: Punctuated<_, Token![,]> = content.parse_terminated(ExprField::parse)?;
            Ok(TypeStatement::Meta(Vec::from_iter(fields.into_iter())))
        } else if lookahead.peek(kw::address) {
            input.parse::<kw::address>()?;
            // keep the grammar strict for now, we can loosen it to an expr later
            let content;
            parenthesized!(content in input);

            let offset: syn::LitInt = content.parse()?;
            let offset = offset.base10_parse()?;

            let content;
            braced!(content in input);

            let fields: Punctuated<_, Token![,]> = content.parse_terminated(TypeField::parse)?;
            let fields = Vec::from_iter(fields.into_iter());
            Ok(TypeStatement::Address(offset, fields))
        } else if lookahead.peek(kw::functions) {
            input.parse::<kw::functions>()?;

            let content;
            braced!(content in input);

            let function_blocks: Punctuated<(Ident, Vec<Function>), Token![,]> = content
                .parse_terminated(|input| {
                    let name: Ident = input.parse()?;

                    let content;
                    braced!(content in input);

                    let functions: Punctuated<_, Token![,]> =
                        content.parse_terminated(Function::parse)?;
                    let functions = Vec::from_iter(functions.into_iter());

                    Ok((name, functions))
                })?;
            let function_blocks = Vec::from_iter(function_blocks.into_iter());

            Ok(TypeStatement::Functions(function_blocks))
        } else if lookahead.peek(syn::Ident) {
            let ahead = input.fork();
            if let Ok(macro_call) = ahead.call(MacroCall::parse) {
                input.advance_to(&ahead);
                Ok(TypeStatement::Macro(macro_call))
            } else {
                Ok(TypeStatement::Field(input.parse()?))
            }
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinition {
    name: Ident,
    statements: Vec<TypeStatement>,
}
impl TypeDefinition {
    pub fn new(name: &str, statements: &[TypeStatement]) -> Self {
        Self {
            name: name.into(),
            statements: statements.to_vec(),
        }
    }
}
impl Parse for TypeDefinition {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![type]>()?;
        let name: Ident = input.parse()?;

        let content;
        braced!(content in input);

        let statements: Punctuated<TypeStatement, Token![,]> =
            content.parse_terminated(TypeStatement::parse)?;
        let statements = Vec::from_iter(statements.into_iter());

        Ok(TypeDefinition { name, statements })
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
impl Parse for Module {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut definitions = vec![];

        while !input.is_empty() {
            definitions.push(input.parse()?);
        }

        Ok(Module::new(&definitions))
    }
}

pub fn parse(token_stream: proc_macro2::TokenStream) -> Result<Module> {
    syn::parse2(token_stream)
}

pub fn parse_str(input: &str) -> Result<Module> {
    use std::str::FromStr;
    parse(proc_macro2::TokenStream::from_str(input)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_parse_basic_struct() {
        let text = r#"
        type TestType {
            field_1: i32,
            field_2: i32,
        }
        "#;

        let ast = {
            type TS = TypeStatement;
            type TR = TypeRef;

            Module::new(&[TypeDefinition::new(
                "TestType",
                &[
                    TS::field("field_1", TR::ident_type("i32")),
                    TS::field("field_2", TR::ident_type("i32")),
                ],
            )])
        };

        assert_eq!(parse_str(text).ok(), Some(ast));
    }

    #[test]
    fn can_parse_vehicle_types() {
        let text = r#"
        type VehicleTypes {
            hash_edacd65b_likely_max_models: i32,
            hash_2ff58884: i32,

            maximum_gpu_cost: i32,
            maximum_cpu_cost: i32,

            field_10: i32,

            accumulated_gpu_cost: i32,
            accumulated_cpu_cost: i32,

            field_1c: i32,
            loaded_models: *const LoadedModel,
            padding!(0x10),
        }
        "#;

        let ast = {
            type T = Type;
            type TS = TypeStatement;
            type TR = TypeRef;

            Module::new(&[TypeDefinition::new(
                "VehicleTypes",
                &[
                    TS::field("hash_edacd65b_likely_max_models", TR::ident_type("i32")),
                    TS::field("hash_2ff58884", TR::ident_type("i32")),
                    TS::field("maximum_gpu_cost", TR::ident_type("i32")),
                    TS::field("maximum_cpu_cost", TR::ident_type("i32")),
                    TS::field("field_10", TR::ident_type("i32")),
                    TS::field("accumulated_gpu_cost", TR::ident_type("i32")),
                    TS::field("accumulated_cpu_cost", TR::ident_type("i32")),
                    TS::field("field_1c", TR::ident_type("i32")),
                    TS::field(
                        "loaded_models",
                        T::ident("LoadedModel").const_pointer().into(),
                    ),
                    TS::macro_("padding", &[Expr::IntLiteral(0x10)]),
                ],
            )])
        };

        assert_eq!(parse_str(text).ok(), Some(ast));
    }

    #[test]
    fn can_parse_spawn_manager() {
        let text = r#"
        type SpawnManager {
            meta {
                size: 0x1754,
                singleton: 0x1_191_918,
            },

            address(0x78) {
                max_num_characters: u16,
                max_num_vehicles: u16,
            },

            address(0xA00) {
                world_sim: WorldSim,
                enemy_type_spawn_settings: unk!(804),
                character_types: unk!(0x74),
                vehicle_types: VehicleTypes,
            },

            functions {
                free {
                    #[address(0x84C_4C0)]
                    fn engine_spawn_vehicle(
                        &mut self,
                        vehicle: *mut SharedPtr<Vehicle>,
                        context: i32,
                        unk1: *mut StdString,
                        model_id: *const u32,
                        faction: u32,
                        unk2: *mut StdString
                    ) -> *mut SharedPtr<Vehicle>,

                    #[address(0x73F_DB0)]
                    fn request_vehicle_model(
                        &mut self,
                        model_id: *const u32,
                        category: i32
                    )
                }
            }
        }
        "#;

        let ast = {
            use Expr::*;

            type T = Type;
            type TS = TypeStatement;
            type TR = TypeRef;
            type A = Argument;

            Module::new(&[TypeDefinition::new(
                "SpawnManager",
                &[
                    TS::meta(&[
                        ("size", IntLiteral(0x1754)),
                        ("singleton", IntLiteral(0x1_191_918)),
                    ]),
                    TS::address(
                        0x78,
                        &[
                            ("max_num_characters", TR::ident_type("u16")),
                            ("max_num_vehicles", TR::ident_type("u16")),
                        ],
                    ),
                    TS::address(
                        0xA00,
                        &[
                            ("world_sim", TR::ident_type("WorldSim")),
                            ("enemy_type_spawn_settings", MacroCall::unk(804).into()),
                            ("character_types", MacroCall::unk(0x74).into()),
                            ("vehicle_types", TR::ident_type("VehicleTypes")),
                        ],
                    ),
                    TS::functions(&[(
                        "free",
                        &[
                            Function::new(
                                "engine_spawn_vehicle",
                                &[Attribute::address(0x84C_4C0)],
                                &[
                                    A::MutSelf,
                                    A::field(
                                        "vehicle",
                                        T::ident("SharedPtr<Vehicle>").mut_pointer().into(),
                                    ),
                                    A::field("context", TR::ident_type("i32")),
                                    A::field("unk1", T::ident("StdString").mut_pointer().into()),
                                    A::field("model_id", T::ident("u32").const_pointer().into()),
                                    A::field("faction", TR::ident_type("u32")),
                                    A::field("unk2", T::ident("StdString").mut_pointer().into()),
                                ],
                                Some(Type::ident("SharedPtr<Vehicle>").mut_pointer()),
                            ),
                            Function::new(
                                "request_vehicle_model",
                                &[Attribute::address(0x73F_DB0)],
                                &[
                                    A::MutSelf,
                                    A::field("model_id", T::ident("u32").const_pointer().into()),
                                    A::field("category", TR::ident_type("i32")),
                                ],
                                None,
                            ),
                        ],
                    )]),
                ],
            )])
        };

        assert_eq!(parse_str(text).ok(), Some(ast));
    }
}
