use syn::{
    braced, bracketed, parenthesized,
    parse::{Parse, ParseStream, Result},
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
    syn::custom_keyword!(backend);
    syn::custom_keyword!(prologue);
    syn::custom_keyword!(epilogue);
    syn::custom_keyword!(vftable);
}

impl Parse for Ident {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![_]) {
            input.parse::<Token![_]>()?;
            Ok(Ident("_".to_string()))
        } else if input.peek(syn::Ident) {
            Ok(Ident(input.parse::<syn::Ident>()?.to_string()))
        } else {
            Err(input.error("expected identifier"))
        }
    }
}

fn parse_type_ident(input: ParseStream) -> Result<String> {
    // dodgy hack to "support" generics for now
    let ident: syn::Ident = input.parse()?;
    let mut name = ident.to_string();

    loop {
        if input.peek(Token![<]) {
            input.parse::<Token![<]>()?;
            name += "<";
        } else if input.peek(syn::Ident) {
            let ident: syn::Ident = input.parse()?;
            name += &ident.to_string();
        } else if input.peek(Token![>]) {
            input.parse::<Token![>]>()?;
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
            } else if input.peek(Token![::]) {
                input.parse::<Token![::]>()?;
            } else if input.peek(Token![super]) {
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
        } else if lookahead.peek(Token![*]) {
            input.parse::<Token![*]>()?;

            let lookahead = input.lookahead1();
            if lookahead.peek(Token![const]) {
                input.parse::<Token![const]>()?;
                Ok(Type::ConstPointer(Box::new(input.parse()?)))
            } else if lookahead.peek(Token![mut]) {
                input.parse::<Token![mut]>()?;
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

impl Attribute {
    /// also implicitly handles multiple attributes within the same brackets:
    /// #[a(b), c(d)] -> Function(a, [b]), Function(c, [d])
    fn parse_many(input: ParseStream) -> Result<Vec<Attribute>> {
        enum AttributePart {
            Ident(Ident),
            Function { name: Ident, arguments: Vec<Expr> },
        }
        impl Parse for AttributePart {
            fn parse(input: ParseStream) -> Result<Self> {
                let name = input.parse()?;

                if input.peek(syn::token::Paren) {
                    let content2;
                    parenthesized!(content2 in input);

                    let arguments: Punctuated<_, Token![,]> =
                        content2.parse_terminated(Expr::parse, Token![,])?;
                    let arguments = Vec::from_iter(arguments);

                    Ok(AttributePart::Function { name, arguments })
                } else {
                    Ok(AttributePart::Ident(name))
                }
            }
        }

        let mut attributes = vec![];
        while input.peek(Token![#]) {
            input.parse::<Token![#]>()?;

            let content;
            bracketed!(content in input);

            let attribute_parts: Punctuated<_, Token![,]> =
                content.parse_terminated(AttributePart::parse, Token![,])?;
            for part in attribute_parts {
                attributes.push(match part {
                    AttributePart::Ident(ident) => Attribute::Ident(ident),
                    AttributePart::Function { name, arguments } => {
                        Attribute::Function(name, arguments)
                    }
                });
            }
        }
        Ok(attributes)
    }
}

impl Parse for Argument {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![&]) {
            input.parse::<Token![&]>()?;

            let lookahead = input.lookahead1();
            if lookahead.peek(Token![mut]) {
                input.parse::<Token![mut]>()?;
                input.parse::<Token![self]>()?;

                Ok(Argument::MutSelf)
            } else if lookahead.peek(Token![self]) {
                input.parse::<Token![self]>()?;

                Ok(Argument::ConstSelf)
            } else {
                Err(lookahead.error())
            }
        } else if lookahead.peek(syn::Ident) {
            let name: Ident = input.parse()?;
            input.parse::<Token![:]>()?;
            Ok(Argument::Named(name, input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for Function {
    fn parse(input: ParseStream) -> Result<Self> {
        let attributes = Attribute::parse_many(input)?;

        input.parse::<Token![fn]>()?;
        let name: Ident = input.parse()?;

        let content;
        parenthesized!(content in input);

        let arguments: Punctuated<_, Token![,]> =
            content.parse_terminated(Argument::parse, Token![,])?;
        let arguments = Vec::from_iter(arguments);

        let return_type = if input.peek(Token![->]) {
            input.parse::<Token![->]>()?;
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

impl Parse for TypeField {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(kw::vftable) {
            input.parse::<kw::vftable>()?;

            let content;
            braced!(content in input);

            let functions: Punctuated<Function, Token![;]> =
                content.parse_terminated(Function::parse, Token![;])?;
            let functions = Vec::from_iter(functions);

            Ok(TypeField::Vftable(functions))
        } else {
            let name: Ident = input.parse()?;
            input.parse::<Token![:]>()?;
            Ok(TypeField::Field(name, input.parse()?))
        }
    }
}

impl Parse for TypeStatement {
    fn parse(input: ParseStream) -> Result<Self> {
        let attributes = if input.peek(Token![#]) {
            Attribute::parse_many(input)?
        } else {
            vec![]
        };
        Ok(TypeStatement {
            field: input.parse()?,
            attributes,
        })
    }
}

fn parse_type_definition(input: ParseStream, attributes: Vec<Attribute>) -> Result<TypeDefinition> {
    let statements = if input.peek(Token![;]) {
        input.parse::<Token![;]>()?;
        vec![]
    } else {
        let content;
        braced!(content in input);

        let statements: Punctuated<TypeStatement, Token![,]> =
            content.parse_terminated(TypeStatement::parse, Token![,])?;
        Vec::from_iter(statements)
    };

    Ok(TypeDefinition {
        statements,
        attributes,
    })
}

impl Parse for EnumStatement {
    fn parse(input: ParseStream) -> Result<Self> {
        let attributes = if input.peek(Token![#]) {
            Attribute::parse_many(input)?
        } else {
            vec![]
        };

        let name: Ident = input.parse()?;
        let expr = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            Some(input.parse()?)
        } else {
            None
        };

        Ok(EnumStatement::new(name, expr).with_attributes(attributes))
    }
}

fn parse_enum_definition(input: ParseStream, attributes: Vec<Attribute>) -> Result<EnumDefinition> {
    input.parse::<Token![:]>()?;
    let type_: Type = input.parse()?;

    let statements = {
        let content;
        braced!(content in input);

        let statements: Punctuated<EnumStatement, Token![,]> =
            content.parse_terminated(EnumStatement::parse, Token![,])?;
        Vec::from_iter(statements)
    };

    Ok(EnumDefinition {
        type_,
        statements,
        attributes,
    })
}

fn parse_item_definition(input: ParseStream, attributes: Vec<Attribute>) -> Result<ItemDefinition> {
    let lookahead = input.lookahead1();
    // We return a Result for the inner so that we can annotate it with the name
    let (name, inner) = if lookahead.peek(Token![type]) {
        input.parse::<Token![type]>()?;
        let name = input.parse()?;
        (
            name,
            parse_type_definition(input, attributes).map(ItemDefinitionInner::from),
        )
    } else if lookahead.peek(Token![enum]) {
        input.parse::<Token![enum]>()?;
        let name = input.parse()?;
        (
            name,
            parse_enum_definition(input, attributes).map(ItemDefinitionInner::from),
        )
    } else {
        return Err(lookahead.error());
    };

    let inner = inner
        .map_err(|e| syn::Error::new(e.span(), format!("failed to parse type {name}: {e}")))?;
    Ok(ItemDefinition { name, inner })
}

fn parse_backend(input: ParseStream) -> Result<Backend> {
    input.parse::<kw::backend>()?;
    let name: Ident = input.parse()?;

    if let Some(new_prologue) = parse_block::<kw::prologue>(input, kw::prologue)? {
        return Ok(Backend {
            name,
            prologue: Some(new_prologue),
            epilogue: None,
        });
    } else if let Some(new_epilogue) = parse_block::<kw::epilogue>(input, kw::epilogue)? {
        return Ok(Backend {
            name,
            prologue: None,
            epilogue: Some(new_epilogue),
        });
    }

    let content;
    braced!(content in input);

    let mut prologue = None;
    let mut epilogue = None;

    while !content.is_empty() {
        if let Some(new_prologue) = parse_block::<kw::prologue>(&content, kw::prologue)? {
            prologue = Some(new_prologue);
        } else if let Some(new_epilogue) = parse_block::<kw::epilogue>(&content, kw::epilogue)? {
            epilogue = Some(new_epilogue);
        } else {
            return Err(content.error("expected prologue or epilogue"));
        }
    }

    fn parse_block<T: syn::parse::Parse>(
        input: ParseStream,
        token: impl syn::parse::Peek,
    ) -> Result<Option<String>> {
        if !input.peek(token) {
            return Ok(None);
        }

        input.parse::<T>()?;
        let temp: syn::LitStr = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(Some(temp.value().trim().to_string()))
    }

    Ok(Backend {
        name,
        prologue,
        epilogue,
    })
}

impl Parse for Module {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut uses = vec![];
        let mut extern_types = vec![];
        let mut extern_values = vec![];
        let mut definitions = vec![];
        let mut impls = vec![];
        let mut backends = vec![];

        // Exhaust all of our declarations
        while !input.is_empty() {
            let attributes = Attribute::parse_many(input)?;

            if input.peek(Token![use]) {
                if !attributes.is_empty() {
                    return Err(input.error("attributes not allowed on use statements"));
                }

                input.parse::<Token![use]>()?;
                let item_path = input.parse()?;
                input.parse::<Token![;]>()?;
                uses.push(item_path);
            } else if input.peek(Token![extern]) {
                input.parse::<Token![extern]>()?;
                if input.peek(Token![type]) {
                    input.parse::<Token![type]>()?;
                    let ident: Ident = parse_type_ident(input)?.as_str().into();
                    input.parse::<Token![;]>()?;

                    extern_types.push((ident, attributes));
                } else {
                    let name: Ident = input.parse()?;
                    input.parse::<Token![:]>()?;
                    let type_: Type = input.parse()?;
                    input.parse::<Token![;]>()?;

                    extern_values.push((name, type_, attributes));
                }
            } else if input.peek(Token![type]) || input.peek(Token![enum]) {
                definitions.push(parse_item_definition(input, attributes)?);
            } else if input.peek(Token![impl]) {
                input.parse::<Token![impl]>()?;
                let name: Ident = input.parse()?;

                let content;
                braced!(content in input);
                let functions: Punctuated<Function, Token![;]> =
                    content.parse_terminated(Function::parse, Token![;])?;
                let functions = Vec::from_iter(functions);

                impls.push(FunctionBlock {
                    name,
                    functions,
                    attributes,
                });
            } else if input.peek(kw::backend) {
                if !attributes.is_empty() {
                    return Err(input.error("attributes not allowed on backends"));
                }

                backends.push(parse_backend(input)?);
            } else {
                return Err(input.error("unexpected keyword"));
            }
        }

        Ok(Module {
            uses,
            extern_types,
            extern_values,
            definitions,
            impls,
            backends,
        })
    }
}

pub fn parse_str(input: &str) -> Result<Module> {
    syn::parse_str(input)
}
