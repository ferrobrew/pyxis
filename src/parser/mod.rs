use syn::{
    braced, bracketed, parenthesized,
    parse::{Lookahead1, Parse, ParseStream, Result},
    punctuated::Punctuated,
    Token,
};

use crate::grammar::*;

#[cfg(test)]
mod tests;

mod kw {
    syn::custom_keyword!(meta);
    syn::custom_keyword!(functions);
    syn::custom_keyword!(unknown);
}

impl Parse for Ident {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(syn::Token![_]) {
            input.parse::<syn::Token![_]>()?;
            Ok(Ident("_".to_string()))
        } else if input.peek(syn::Ident) {
            Ok(Ident(input.parse::<syn::Ident>()?.to_string()))
        } else {
            Err(input.error("expected identifier"))
        }
    }
}
impl Ident {
    fn peek(lookahead: &Lookahead1) -> bool {
        lookahead.peek(syn::Token![_]) || lookahead.peek(syn::Ident)
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

impl Parse for ItemPath {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut item_path = ItemPath::empty();
        loop {
            // todo: make parsing stricter so that this takes idents
            // separated by double-colons that end in a type, not just
            // all types
            // that is to say, `use lol<lol>::lol` should not parse, but
            // `use lol::lol<lol>` should
            if input.peek(syn::Ident) {
                item_path.push(parse_type_ident(input)?.into());
            } else if input.peek(syn::Token![::]) {
                input.parse::<syn::Token![::]>()?;
            } else if input.peek(syn::Token![super]) {
                return Err(input.error("super not supported"));
            } else {
                break;
            }
        }
        Ok(item_path)
    }
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::unknown) {
            input.parse::<kw::unknown>()?;
            input.parse::<Token![<]>()?;
            let size: usize = input.parse::<syn::LitInt>()?.base10_parse()?;
            input.parse::<Token![>]>()?;

            Ok(Type::Unknown(size))
        } else if lookahead.peek(syn::Ident) {
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
        } else if lookahead.peek(syn::token::Bracket) {
            let content;
            bracketed!(content in input);

            let type_: Type = content.parse()?;
            content.parse::<Token![;]>()?;
            let size: syn::LitInt = content.parse()?;
            let size: usize = size.base10_parse()?;
            Ok(Type::Array(Box::new(type_), size))
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::Ident) {
            Ok(Expr::Ident(input.parse()?))
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
        let arguments = Vec::from_iter(arguments);

        Ok(Attribute::Function(name, arguments))
    }
}
impl Attribute {
    fn parse_many(input: ParseStream) -> Result<Vec<Attribute>> {
        let mut attributes = vec![];
        while input.peek(syn::Token![#]) {
            attributes.push(input.parse()?);
        }
        Ok(attributes)
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
        let attributes = Attribute::parse_many(input)?;

        input.parse::<syn::Token![fn]>()?;
        let name: Ident = input.parse()?;

        let content;
        parenthesized!(content in input);

        let arguments: Punctuated<_, Token![,]> = content.parse_terminated(Argument::parse)?;
        let arguments = Vec::from_iter(arguments);

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

impl Parse for ExprField {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        Ok(ExprField(name, input.parse()?))
    }
}

impl Parse for OptionalExprField {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        let expr = if input.peek(Token![:]) {
            input.parse::<Token![:]>()?;
            Some(input.parse()?)
        } else {
            None
        };
        Ok(OptionalExprField(name, expr))
    }
}

impl Parse for TypeField {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        Ok(TypeField(name, input.parse()?))
    }
}

impl Parse for TypeStatement {
    fn parse(input: ParseStream) -> Result<Self> {
        fn parse_field(
            input: ParseStream,
            lookahead: Lookahead1,
            attributes: Vec<Attribute>,
        ) -> Result<TypeStatement> {
            if Ident::peek(&lookahead) {
                Ok(TypeStatement::Field {
                    field: input.parse()?,
                    attributes,
                })
            } else {
                Err(lookahead.error())
            }
        }

        let lookahead = input.lookahead1();
        if lookahead.peek(kw::functions) {
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
            let function_blocks = Vec::from_iter(function_blocks);

            Ok(TypeStatement::Functions(function_blocks))
        } else if lookahead.peek(syn::Token![#]) {
            let attributes = Attribute::parse_many(input)?;
            parse_field(input, input.lookahead1(), attributes)
        } else {
            parse_field(input, lookahead, vec![])
        }
    }
}

fn parse_type_definition(
    input: ParseStream,
    attributes: Vec<Attribute>,
) -> Result<(Ident, TypeDefinition)> {
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
        Vec::from_iter(statements)
    };

    Ok((
        name,
        TypeDefinition {
            statements,
            attributes,
        },
    ))
}

impl Parse for EnumStatement {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::Ident) {
            Ok(EnumStatement::Field(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

fn parse_enum_definition(
    input: ParseStream,
    attributes: Vec<Attribute>,
) -> Result<(Ident, EnumDefinition)> {
    input.parse::<Token![enum]>()?;
    let name: Ident = input.parse()?;
    input.parse::<Token![:]>()?;
    let type_: Type = input.parse()?;

    let statements = {
        let content;
        braced!(content in input);

        let statements: Punctuated<EnumStatement, Token![,]> =
            content.parse_terminated(EnumStatement::parse)?;
        Vec::from_iter(statements)
    };

    Ok((
        name,
        EnumDefinition {
            type_,
            statements,
            attributes,
        },
    ))
}

fn parse_item_definition(input: ParseStream, attributes: Vec<Attribute>) -> Result<ItemDefinition> {
    let lookahead = input.lookahead1();
    if lookahead.peek(Token![type]) {
        let (name, inner) = parse_type_definition(input, attributes)?;
        Ok(ItemDefinition {
            name,
            inner: inner.into(),
        })
    } else if lookahead.peek(Token![enum]) {
        let (name, inner) = parse_enum_definition(input, attributes)?;
        Ok(ItemDefinition {
            name,
            inner: inner.into(),
        })
    } else {
        Err(lookahead.error())
    }
}

impl Parse for Module {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut uses = vec![];
        let mut extern_types = vec![];
        let mut extern_values = vec![];
        let mut definitions = vec![];

        // Exhaust all of our declarations
        while !input.is_empty() {
            let attributes = Attribute::parse_many(input)?;

            if input.peek(syn::Token![use]) {
                if !attributes.is_empty() {
                    return Err(input.error("attributes not allowed on use statements"));
                }

                input.parse::<syn::Token![use]>()?;
                let item_path = input.parse()?;
                input.parse::<syn::Token![;]>()?;
                uses.push(item_path);
            } else if input.peek(syn::Token![extern]) {
                input.parse::<syn::Token![extern]>()?;
                if input.peek(syn::Token![type]) {
                    input.parse::<syn::Token![type]>()?;
                    let ident: Ident = parse_type_ident(input)?.as_str().into();
                    input.parse::<syn::Token![;]>()?;

                    extern_types.push((ident, attributes));
                } else {
                    let name: Ident = input.parse()?;
                    input.parse::<syn::Token![:]>()?;
                    let type_: Type = input.parse()?;
                    input.parse::<syn::Token![;]>()?;

                    extern_values.push((name, type_, attributes));
                }
            } else if input.peek(syn::Token![type]) || input.peek(syn::Token![enum]) {
                definitions.push(parse_item_definition(input, attributes)?);
            } else {
                return Err(input.error("unexpected keyword"));
            }
        }

        Ok(Module::new(uses, extern_types, extern_values, definitions))
    }
}

pub fn parse_str(input: &str) -> Result<Module> {
    syn::parse_str(input)
}
