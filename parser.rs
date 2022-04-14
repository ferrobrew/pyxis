use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{braced, bracketed, parenthesized, Token};

mod kw {
    syn::custom_keyword!(meta);
    syn::custom_keyword!(address);
    syn::custom_keyword!(functions);
}

use super::grammar::*;

impl Parse for Ident {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Ident(input.parse::<syn::Ident>()?.to_string()))
    }
}

fn parse_type_ident(input: ParseStream) -> Result<String> {
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
            break Ok(name);
        }
    }
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::Ident) {
            Ok(Type::Ident(parse_type_ident(input)?.as_str().into()))
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

impl Parse for ExprField {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        Ok(ExprField(name, input.parse()?))
    }
}

impl Parse for TypeField {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        Ok(TypeField(name, input.parse()?))
    }
}

fn parse_optionally_braced_content<T>(
    input: ParseStream,
    content_parser: fn(ParseStream) -> Result<T>,
) -> Result<Vec<T>> {
    if input.peek(syn::token::Brace) {
        let content;
        braced!(content in input);

        let fields: Punctuated<_, Token![,]> = content.parse_terminated(content_parser)?;
        Ok(Vec::from_iter(fields.into_iter()))
    } else {
        Ok(vec![content_parser(input)?])
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
            let fields = parse_optionally_braced_content(input, TypeField::parse)?;

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

impl Parse for TypeDefinition {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![type]>()?;
        let name: Ident = input.parse()?;

        let statements = if input.peek(syn::Token![;]) {
            input.parse::<syn::Token![;]>()?;
            vec![]
        } else {
            let content;
            braced!(content in input);

            let statements: Punctuated<TypeStatement, Token![,]> =
                content.parse_terminated(TypeStatement::parse)?;
            Vec::from_iter(statements.into_iter())
        };

        Ok(TypeDefinition { name, statements })
    }
}

impl Parse for Module {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut uses = vec![];
        let mut definitions = vec![];
        let mut externs = vec![];

        // Exhaust all of our declarations
        while !input.is_empty() {
            if input.peek(syn::Token![use]) {
                input.parse::<syn::Token![use]>()?;

                // todo: make parsing stricter so that this takes idents
                // separated by double-colons that end in a type, not just
                // all types
                // that is to say, `use lol<lol>::lol` should not parse, but
                // `use lol::lol<lol>` should
                // todo: consider implementing ItemPath::parse somehow
                let mut item_path = ItemPath::empty();
                while !input.peek(syn::Token![;]) {
                    let lookahead = input.lookahead1();
                    if lookahead.peek(syn::Ident) {
                        item_path.push(parse_type_ident(input)?.into());
                    } else if lookahead.peek(syn::Token![::]) {
                        input.parse::<syn::Token![::]>()?;
                    } else if lookahead.peek(syn::Token![super]) {
                        return Err(input.error("super not supported"));
                    } else {
                        return Err(lookahead.error());
                    }
                }
                input.parse::<syn::Token![;]>()?;
                uses.push(item_path);
            } else if input.peek(syn::Token![extern]) {
                input.parse::<syn::Token![extern]>()?;
                input.parse::<syn::Token![type]>()?;
                let item_path = ItemPath::empty().join(parse_type_ident(input)?.into());

                let content;
                braced!(content in input);

                let fields: Punctuated<_, Token![,]> =
                    content.parse_terminated(ExprField::parse)?;

                externs.push((item_path, Vec::from_iter(fields.into_iter())));
            } else if input.peek(syn::Token![type]) {
                definitions.push(input.parse()?);
            } else {
                return Err(input.error("unexpected keyword"));
            }
        }

        Ok(Module::new(&uses, &externs, &definitions))
    }
}

pub fn parse_str(input: &str) -> Result<Module> {
    syn::parse_str(input)
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

            Module::new(
                &[],
                &[],
                &[TypeDefinition::new(
                    "TestType",
                    &[
                        TS::field("field_1", TR::ident_type("i32")),
                        TS::field("field_2", TR::ident_type("i32")),
                    ],
                )],
            )
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

            Module::new(
                &[],
                &[],
                &[TypeDefinition::new(
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
                )],
            )
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

            Module::new(
                &[],
                &[],
                &[TypeDefinition::new(
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
                                        A::field(
                                            "unk1",
                                            T::ident("StdString").mut_pointer().into(),
                                        ),
                                        A::field(
                                            "model_id",
                                            T::ident("u32").const_pointer().into(),
                                        ),
                                        A::field("faction", TR::ident_type("u32")),
                                        A::field(
                                            "unk2",
                                            T::ident("StdString").mut_pointer().into(),
                                        ),
                                    ],
                                    Some(Type::ident("SharedPtr<Vehicle>").mut_pointer()),
                                ),
                                Function::new(
                                    "request_vehicle_model",
                                    &[Attribute::address(0x73F_DB0)],
                                    &[
                                        A::MutSelf,
                                        A::field(
                                            "model_id",
                                            T::ident("u32").const_pointer().into(),
                                        ),
                                        A::field("category", TR::ident_type("i32")),
                                    ],
                                    None,
                                ),
                            ],
                        )]),
                    ],
                )],
            )
        };

        assert_eq!(parse_str(text).ok(), Some(ast));
    }

    #[test]
    fn can_parse_address_field() {
        let text = r#"
        type Test {
            address(0x78) max_num_characters: u16,
        }
        "#;

        let ast = {
            Module::new(
                &[],
                &[],
                &[TypeDefinition::new(
                    "Test",
                    &[TypeStatement::address(
                        0x78,
                        &[("max_num_characters", TypeRef::ident_type("u16"))],
                    )],
                )],
            )
        };

        assert_eq!(parse_str(text).ok(), Some(ast));
    }

    #[test]
    fn can_parse_use() {
        let text = r#"
        use hello::TestType<Hey>;
        type Test {
            test: TestType<Hey>,
        }
        "#;

        let ast = {
            Module::new(
                &[ItemPath::from_colon_delimited_str("hello::TestType<Hey>")],
                &[],
                &[TypeDefinition::new(
                    "Test",
                    &[TypeStatement::field(
                        "test",
                        TypeRef::ident_type("TestType<Hey>"),
                    )],
                )],
            )
        };

        assert_eq!(parse_str(text).ok(), Some(ast));
    }

    #[test]
    fn will_die_on_super_for_now() {
        let text = r#"
        use super::TestType<Hey>;
        "#;

        assert_eq!(
            parse_str(text).err().unwrap().to_string(),
            "super not supported"
        );
    }

    #[test]
    fn can_parse_extern() {
        let text = r#"
        extern type TestType<Hey> { size: 12 }
        type Test {
            test: TestType<Hey>,
        }
        "#;

        let ast = {
            Module::new(
                &[],
                &[(
                    ItemPath::from_colon_delimited_str("TestType<Hey>"),
                    vec![ExprField("size".into(), Expr::IntLiteral(12))],
                )],
                &[TypeDefinition::new(
                    "Test",
                    &[TypeStatement::field(
                        "test",
                        TypeRef::ident_type("TestType<Hey>"),
                    )],
                )],
            )
        };

        assert_eq!(parse_str(text).ok(), Some(ast));
    }

    #[test]
    fn can_parse_an_empty_type() {
        let text = r#"
        type Test;
        "#;

        let ast = { Module::new(&[], &[], &[TypeDefinition::new("Test", &[])]) };

        assert_eq!(parse_str(text).ok(), Some(ast));
    }
}
