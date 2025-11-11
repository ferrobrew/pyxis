use chumsky::prelude::*;

use crate::grammar::*;
use crate::span::{Span, Spanned};

#[cfg(test)]
mod tests;

type ParserInput<'a> = &'a str;
type ParseError<'a> = extra::Err<Rich<'a, char>>;

// Helper to convert SimpleSpan to our Span
fn to_span(simple_span: SimpleSpan) -> Span {
    Span::new(simple_span.start, simple_span.end)
}

// Lexical elements

fn whitespace<'a>() -> impl Parser<'a, ParserInput<'a>, (), ParseError<'a>> + Clone {
    one_of(" \t\r\n").repeated().ignored()
}

fn doc_comment<'a>() -> impl Parser<'a, ParserInput<'a>, Spanned<String>, ParseError<'a>> + Clone {
    just("///")
        .then(none_of("\r\n").repeated().collect::<String>())
        .map(|(_, content)| content)
        .map_with(|content, extra| Spanned::new(content, to_span(extra.span())))
}

fn module_doc_comment<'a>()
-> impl Parser<'a, ParserInput<'a>, Spanned<String>, ParseError<'a>> + Clone {
    just("//!")
        .then(none_of("\r\n").repeated().collect::<String>())
        .map(|(_, content)| content)
        .map_with(|content, extra| Spanned::new(content, to_span(extra.span())))
}

fn single_line_comment<'a>() -> impl Parser<'a, ParserInput<'a>, (), ParseError<'a>> + Clone {
    just("//")
        .then(one_of("/!").not().rewind())
        .then(none_of("\r\n").repeated())
        .ignored()
}

fn multi_line_comment<'a>() -> impl Parser<'a, ParserInput<'a>, (), ParseError<'a>> + Clone {
    just("/*")
        .ignore_then(custom(|input| {
            let start = input.cursor();

            // Find the closing '*/' by tracking if previous char was '*'
            let mut prev_was_star = false;
            loop {
                match input.next() {
                    Some('/') if prev_was_star => {
                        // Found closing */
                        return Ok(());
                    }
                    Some('*') => {
                        prev_was_star = true;
                    }
                    Some(_) => {
                        prev_was_star = false;
                    }
                    None => {
                        return Err(Rich::custom(
                            input.span_since(&start),
                            "Unclosed multi-line comment",
                        ));
                    }
                }
            }
        }))
}

// Comment-capturing parsers - return Comment nodes for inclusion in AST
fn line_comment_node<'a>() -> impl Parser<'a, ParserInput<'a>, Comment, ParseError<'a>> + Clone {
    just("//")
        .then(one_of("/!").not().rewind())
        .ignore_then(none_of("\r\n").repeated().collect::<String>())
        .map(Comment::Line)
}

fn block_comment_node<'a>() -> impl Parser<'a, ParserInput<'a>, Comment, ParseError<'a>> + Clone {
    just("/*")
        .ignore_then(custom(|input| {
            let start = input.cursor();
            let mut content = String::new();

            // Capture content between /* and */
            let mut prev_was_star = false;
            loop {
                match input.next() {
                    Some('/') if prev_was_star => {
                        // Found closing */ - remove the trailing '*' we added
                        content.pop();
                        return Ok(content);
                    }
                    Some(c) => {
                        prev_was_star = c == '*';
                        content.push(c);
                    }
                    None => {
                        return Err(Rich::custom(
                            input.span_since(&start),
                            "Unclosed multi-line comment",
                        ));
                    }
                }
            }
        }))
        .map(Comment::Block)
}

// Skip whitespace and comments - matches multiple whitespace chars and comments in one go
// Used ONLY at strategic locations to avoid exponential parser complexity
fn skip_ws_and_comments<'a>() -> impl Parser<'a, ParserInput<'a>, (), ParseError<'a>> + Clone {
    single_line_comment()
        .or(multi_line_comment())
        .or(one_of(" \t\r\n").ignored())
        .repeated()
        .ignored()
}

// Padding with just whitespace - used for most tokens
fn padded<'a, O: Clone>(
    parser: impl Parser<'a, ParserInput<'a>, O, ParseError<'a>> + Clone + 'a,
) -> impl Parser<'a, ParserInput<'a>, O, ParseError<'a>> + Clone {
    parser.padded_by(whitespace())
}

fn keyword<'a>(kw: &'static str) -> impl Parser<'a, ParserInput<'a>, (), ParseError<'a>> + Clone {
    just(kw)
        .then_ignore(text::ident().not().rewind().or(end().rewind()))
        .ignored()
}

fn ident<'a>() -> impl Parser<'a, ParserInput<'a>, Spanned<Ident>, ParseError<'a>> + Clone {
    text::ident()
        .try_map(|s: &str, span| {
            // Filter out keywords (vftable is intentionally NOT here - it's handled specially in type_field)
            match s {
                "type" | "enum" | "pub" | "fn" | "impl" | "use" | "extern" | "const" | "mut"
                | "self" | "backend" | "prologue" | "epilogue" | "bitflags" | "unknown" => {
                    Err(Rich::custom(span, format!("'{}' is a reserved keyword", s)))
                }
                _ => Ok(Ident(s.to_string())),
            }
        })
        .map_with(|ident, extra| Spanned::new(ident, to_span(extra.span())))
        .or(just("_")
            .to(Ident("_".to_string()))
            .map_with(|ident, extra| Spanned::new(ident, to_span(extra.span()))))
}

// For types that can have generics-like syntax
fn type_ident<'a>() -> impl Parser<'a, ParserInput<'a>, String, ParseError<'a>> + Clone {
    text::ident()
        .then(
            just('<')
                .ignore_then(none_of('>').repeated().collect::<String>())
                .then_ignore(just('>'))
                .or_not(),
        )
        .map(move |(base, generic): (&str, Option<String>)| {
            if let Some(generic_part) = generic {
                format!("{}<{}>", base, generic_part)
            } else {
                base.to_string()
            }
        })
}

// Literals

fn int_literal<'a>() -> impl Parser<'a, ParserInput<'a>, Spanned<Expr>, ParseError<'a>> + Clone {
    let hex = just("0x")
        .ignore_then(
            one_of("0123456789abcdefABCDEF_")
                .repeated()
                .at_least(1)
                .collect::<String>(),
        )
        .map_with(|s: String, extra| (s, extra.span()))
        .validate(|(s, span), _extra, emitter| {
            match isize::from_str_radix(&s.replace('_', ""), 16) {
                Ok(n) => n,
                Err(e) => {
                    emitter.emit(Rich::custom(
                        span,
                        format!("Invalid hexadecimal literal: {}", e),
                    ));
                    0 // Return dummy value so parse can continue
                }
            }
        });

    let bin = just("0b")
        .ignore_then(one_of("01_").repeated().at_least(1).collect::<String>())
        .map_with(|s: String, extra| (s, extra.span()))
        .validate(|(s, span), _extra, emitter| {
            match isize::from_str_radix(&s.replace('_', ""), 2) {
                Ok(n) => n,
                Err(e) => {
                    emitter.emit(Rich::custom(span, format!("Invalid binary literal: {}", e)));
                    0 // Return dummy value so parse can continue
                }
            }
        });

    let dec = just('-')
        .or_not()
        .then(
            one_of("0123456789_")
                .repeated()
                .at_least(1)
                .collect::<String>(),
        )
        .map_with(|(neg, s): (Option<char>, String), extra| ((neg, s), extra.span()))
        .validate(|((neg, s), span), _extra, emitter| {
            match s.replace('_', "").parse::<isize>() {
                Ok(num) => {
                    if neg.is_some() {
                        -num
                    } else {
                        num
                    }
                }
                Err(e) => {
                    emitter.emit(Rich::custom(
                        span,
                        format!("Invalid decimal literal: {}", e),
                    ));
                    0 // Return dummy value so parse can continue
                }
            }
        });

    choice((hex, bin, dec))
        .map(Expr::IntLiteral)
        .map_with(|expr, extra| Spanned::new(expr, to_span(extra.span())))
}

fn string_literal<'a>() -> impl Parser<'a, ParserInput<'a>, Spanned<Expr>, ParseError<'a>> + Clone {
    let escape = just('\\').ignore_then(choice((
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
        just('\\').to('\\'),
        just('"').to('"'),
    )));

    let string_char = escape.or(none_of("\"\\"));

    // Regular string
    let regular_string = just('"')
        .ignore_then(string_char.repeated().collect::<String>())
        .then_ignore(just('"'));

    // Raw string r#"..."# with variable number of # characters
    // This mirrors Rust's raw string behavior: r#"..."#, r##"..."##, etc.
    let raw_string = custom(|input| {
        let start = input.cursor();

        // Match 'r'
        match input.next() {
            Some('r') => {}
            _ => {
                return Err(Rich::custom(
                    input.span_since(&start),
                    "Expected 'r' at start of raw string",
                ));
            }
        }

        // Count the number of '#' characters
        let mut hash_count = 0;
        loop {
            let saved = input.save();
            match input.next() {
                Some('#') => hash_count += 1,
                _ => {
                    input.rewind(saved);
                    break;
                }
            }
        }

        if hash_count == 0 {
            return Err(Rich::custom(
                input.span_since(&start),
                "Expected at least one '#' after 'r'",
            ));
        }

        // Match opening quote
        match input.next() {
            Some('"') => {}
            _ => {
                return Err(Rich::custom(
                    input.span_since(&start),
                    "Expected '\"' after hashes",
                ));
            }
        }

        // Build closing delimiter
        let mut closing = vec!['"'];
        closing.extend(std::iter::repeat_n('#', hash_count));

        // Collect content until we find the closing delimiter
        let mut content = String::new();
        'outer: loop {
            // Check if we've found the closing delimiter
            let saved = input.save();
            let mut found = true;

            for &expected in &closing {
                match input.next() {
                    Some(c) if c == expected => continue,
                    _ => {
                        found = false;
                        break;
                    }
                }
            }

            if found {
                // We found the closing delimiter!
                break 'outer;
            }

            // Not the closing delimiter, restore position and consume one char
            input.rewind(saved);

            match input.next() {
                Some(c) => content.push(c),
                None => {
                    return Err(Rich::custom(
                        input.span_since(&start),
                        "Unclosed raw string literal",
                    ));
                }
            }
        }

        Ok(content)
    });

    choice((raw_string, regular_string))
        .map(Expr::StringLiteral)
        .map_with(|expr, extra| Spanned::new(expr, to_span(extra.span())))
}

// Expressions

fn expr<'a>() -> impl Parser<'a, ParserInput<'a>, Spanned<Expr>, ParseError<'a>> + Clone {
    choice((
        int_literal(),
        string_literal(),
        ident().map(|id| id.map(Expr::Ident)),
    ))
}

// Types

fn type_parser<'a>() -> impl Parser<'a, ParserInput<'a>, Spanned<Type>, ParseError<'a>> + Clone {
    recursive(|type_| {
        let unknown = keyword("unknown")
            .ignore_then(padded(just('<')))
            .ignore_then(padded(int_literal()))
            .then_ignore(padded(just('>')))
            .try_map(|lit, span| {
                if let Expr::IntLiteral(size) = lit.node {
                    Ok(Type::Unknown(size as usize))
                } else {
                    Err(Rich::custom(span, "Expected integer literal"))
                }
            })
            .map_with(|ty, extra| Spanned::new(ty, to_span(extra.span())));

        let ident_type = type_ident()
            .map(|s| Type::Ident(Ident(s)))
            .map_with(|ty, extra| Spanned::new(ty, to_span(extra.span())));

        let pointer = padded(just('*'))
            .ignore_then(choice((
                keyword("const").to(true),
                keyword("mut").to(false),
            )))
            .then(padded(type_.clone()))
            .map(|(is_const, inner)| {
                if is_const {
                    Type::ConstPointer(Box::new(inner))
                } else {
                    Type::MutPointer(Box::new(inner))
                }
            })
            .map_with(|ty, extra| Spanned::new(ty, to_span(extra.span())));

        let array = padded(just('['))
            .ignore_then(padded(type_.clone()))
            .then_ignore(padded(just(';')))
            .then(padded(int_literal()))
            .then_ignore(padded(just(']')))
            .try_map(|(inner, size_expr), span| {
                if let Expr::IntLiteral(size) = size_expr.node {
                    Ok(Type::Array(Box::new(inner), size as usize))
                } else {
                    Err(Rich::custom(span, "Array size must be an integer"))
                }
            })
            .map_with(|ty, extra| Spanned::new(ty, to_span(extra.span())));

        choice((pointer, array, unknown, ident_type))
    })
}

// Item paths (for use statements)

fn item_path<'a>() -> impl Parser<'a, ParserInput<'a>, Spanned<ItemPath>, ParseError<'a>> + Clone {
    type_ident()
        .separated_by(just("::"))
        .at_least(1)
        .collect::<Vec<_>>()
        .try_map(|segments: Vec<String>, span| {
            // Check for "super" which is not supported
            if segments.iter().any(|s| s == "super") {
                Err(Rich::custom(span, "super not supported"))
            } else {
                Ok(ItemPath(
                    segments.into_iter().map(ItemPathSegment::from).collect(),
                ))
            }
        })
        .map_with(|path, extra| Spanned::new(path, to_span(extra.span())))
}

// Attributes

fn attribute<'a>() -> impl Parser<'a, ParserInput<'a>, Spanned<Attribute>, ParseError<'a>> + Clone {
    let function = ident()
        .then(
            padded(just('('))
                .ignore_then(
                    padded(expr())
                        .separated_by(padded(just(',')))
                        .allow_trailing()
                        .collect(),
                )
                .then_ignore(padded(just(')'))),
        )
        .map(|(name, args)| Attribute::Function(name.node, args));

    let assign = ident()
        .then_ignore(padded(just('=')))
        .then(padded(expr()))
        .map(|(name, value)| Attribute::Assign(name.node, value));

    let ident_attr = ident().map(|name| Attribute::Ident(name.node));

    choice((function, assign, ident_attr))
        .map_with(|attr, extra| Spanned::new(attr, to_span(extra.span())))
}

fn attributes<'a>() -> impl Parser<'a, ParserInput<'a>, Attributes, ParseError<'a>> + Clone {
    padded(just('#'))
        .ignore_then(padded(just('[')))
        .ignore_then(
            attribute()
                .separated_by(padded(just(',')))
                .collect::<Vec<_>>(),
        )
        .then_ignore(padded(just(']')))
        .repeated()
        .collect::<Vec<Vec<_>>>()
        .map(|attrs_list| Attributes(attrs_list.into_iter().flatten().collect()))
}

fn module_attributes<'a>() -> impl Parser<'a, ParserInput<'a>, Attributes, ParseError<'a>> + Clone {
    padded(just("#!"))
        .ignore_then(padded(just('[')))
        .ignore_then(
            attribute()
                .separated_by(padded(just(',')))
                .collect::<Vec<_>>(),
        )
        .then_ignore(padded(just(']')))
        .repeated()
        .collect::<Vec<Vec<_>>>()
        .map(|attrs_list| Attributes(attrs_list.into_iter().flatten().collect()))
}

// Visibility

fn visibility<'a>() -> impl Parser<'a, ParserInput<'a>, Visibility, ParseError<'a>> + Clone {
    keyword("pub")
        .to(Visibility::Public)
        .or(empty().to(Visibility::Private))
}

// Arguments

fn argument<'a>() -> impl Parser<'a, ParserInput<'a>, Spanned<Argument>, ParseError<'a>> + Clone {
    let const_self = just('&')
        .then_ignore(whitespace())
        .ignore_then(keyword("self"))
        .to(Argument::ConstSelf)
        .map_with(|arg, extra| Spanned::new(arg, to_span(extra.span())));

    let mut_self = just('&')
        .then_ignore(whitespace())
        .ignore_then(keyword("mut"))
        .then_ignore(whitespace())
        .ignore_then(keyword("self"))
        .to(Argument::MutSelf)
        .map_with(|arg, extra| Spanned::new(arg, to_span(extra.span())));

    let named = ident()
        .then_ignore(padded(just(':')))
        .then(padded(type_parser()))
        .map(|(name, ty)| Argument::Named(name, ty))
        .map_with(|arg, extra| Spanned::new(arg, to_span(extra.span())));

    choice((mut_self, const_self, named))
}

// Parse argument list with interleaved comments
// Captures both arguments and comments in their original order
fn argument_list<'a>() -> impl Parser<'a, ParserInput<'a>, Vec<Spanned<ArgumentChild>>, ParseError<'a>> + Clone {
    padded(just('('))
        .then_ignore(whitespace())
        .ignore_then(
            // Parse a sequence of ArgumentChild items
            // Comments can appear anywhere, arguments are separated by commas
            choice((
                // Parse a comment (can appear anywhere)
                line_comment_node()
                    .or(block_comment_node())
                    .padded_by(whitespace())
                    .map_with(|comment, extra| {
                        Spanned::new(ArgumentChild::Comment(comment), to_span(extra.span()))
                    }),
                // Parse an argument (followed by optional comma and whitespace)
                argument()
                    .then_ignore(whitespace())
                    .then_ignore(just(',').or_not())
                    .then_ignore(whitespace())
                    .map_with(|arg, extra| {
                        Spanned::new(ArgumentChild::Argument(arg), to_span(extra.span()))
                    }),
            ))
            .repeated()
            .collect(),
        )
        .then_ignore(whitespace())
        .then_ignore(just(')'))
}

// Functions

fn function<'a>(
    doc_comments: Vec<Spanned<String>>,
) -> impl Parser<'a, ParserInput<'a>, Spanned<Function>, ParseError<'a>> + Clone {
    let attrs = attributes().or(empty().to(Attributes::default()));

    attrs
        .then(padded(visibility()))
        .then_ignore(padded(keyword("fn")))
        .then(padded(ident()))
        .then(argument_list())
        .then(
            padded(just("->"))
                .ignore_then(padded(type_parser()))
                .or_not(),
        )
        .map(move |((((attrs, vis), name), args), ret_ty)| Function {
            visibility: vis,
            name,
            attributes: attrs,
            doc_comments: doc_comments.clone(),
            arguments: args,
            return_type: ret_ty,
        })
        .map_with(|func, extra| Spanned::new(func, to_span(extra.span())))
}

// Type definitions

fn type_statement<'a>()
-> impl Parser<'a, ParserInput<'a>, Spanned<TypeStatement>, ParseError<'a>> + Clone {
    let vftable_func = doc_comment()
        .then_ignore(one_of(" \t\r\n").repeated())
        .repeated()
        .collect::<Vec<_>>()
        .then(
            attributes()
                .or(empty().to(Attributes::default()))
                .then(padded(visibility()))
                .then_ignore(padded(keyword("fn")))
                .then(padded(ident()))
                .then(argument_list())
                .then(
                    padded(just("->"))
                        .ignore_then(padded(type_parser()))
                        .or_not(),
                ),
        )
        .map(
            |(doc_comments, ((((attrs, vis), name), args), ret_ty))| Function {
                visibility: vis,
                name,
                attributes: attrs,
                doc_comments,
                arguments: args,
                return_type: ret_ty,
            },
        )
        .map_with(|func, extra| Spanned::new(func, to_span(extra.span())));

    // Shared parser: identifier followed by ':' and type
    let field_with_vis = |vis| {
        text::ident()
            .map_with(|s: &str, extra| (s.to_string(), extra.span()))
            .then_ignore(padded(just(':')))
            .then(padded(type_parser()))
            .map(move |((name_str, name_span), ty)| {
                TypeField::Field(vis, Spanned::new(Ident(name_str), to_span(name_span)), ty)
            })
    };

    // Public field: pub name : type
    let pub_field = keyword("pub")
        .ignore_then(whitespace())
        .ignore_then(field_with_vis(Visibility::Public));

    // Vftable: vftable { functions }
    let vftable = text::ident()
        .try_map(|s: &str, span| {
            if s == "vftable" {
                Ok(())
            } else {
                Err(Rich::custom(span, "expected 'vftable'"))
            }
        })
        .ignore_then(padded(just('{')))
        .then_ignore(whitespace())
        .ignore_then(
            choice((
                // Try functions first
                vftable_func
                    .then_ignore(just(';').padded_by(whitespace()).or_not())
                    .map_with(|f, extra| {
                        Spanned::new(VftableChild::Function(f), to_span(extra.span()))
                    }),
                // Then try standalone comments
                line_comment_node()
                    .or(block_comment_node())
                    .padded_by(whitespace())
                    .map_with(|comment, extra| {
                        Spanned::new(VftableChild::Comment(comment), to_span(extra.span()))
                    }),
            ))
            .repeated()
            .collect::<Vec<_>>(),
        )
        .then_ignore(whitespace())
        .then_ignore(just('}'))
        .map(|children| TypeField::Vftable(VftableDefinition { children }));

    // Private field: name : type
    let private_field = field_with_vis(Visibility::Private);

    // Private field or vftable
    let private_or_vftable = vftable.or(private_field);

    // Parse doc comments and attributes, then the field type
    // Note: whitespace is handled by parent parser, we just parse the statement itself
    doc_comment()
        .then_ignore(one_of(" \t\r\n").repeated())
        .repeated()
        .collect::<Vec<_>>()
        .then(attributes().or(empty().to(Attributes::default())))
        .then_ignore(whitespace())
        .then(pub_field.or(private_or_vftable))
        .map(|((doc_comments, attrs), field)| TypeStatement {
            field,
            attributes: attrs,
            doc_comments,
        })
        .map_with(|stmt, extra| Spanned::new(stmt, to_span(extra.span())))
}

// Enum definitions

fn enum_statement<'a>()
-> impl Parser<'a, ParserInput<'a>, Spanned<EnumStatement>, ParseError<'a>> + Clone {
    doc_comment()
        .then_ignore(one_of(" \t\r\n").repeated())
        .repeated()
        .collect::<Vec<_>>()
        .then(attributes().or(empty().to(Attributes::default())))
        .then(padded(ident()))
        .then(padded(just('=')).ignore_then(padded(expr())).or_not())
        .map(|(((doc_comments, attrs), name), expr)| EnumStatement {
            name,
            expr,
            attributes: attrs,
            doc_comments,
        })
        .map_with(|stmt, extra| Spanned::new(stmt, to_span(extra.span())))
}

// Bitflags definitions

fn bitflags_statement<'a>()
-> impl Parser<'a, ParserInput<'a>, Spanned<BitflagsStatement>, ParseError<'a>> + Clone {
    doc_comment()
        .then_ignore(one_of(" \t\r\n").repeated())
        .repeated()
        .collect::<Vec<_>>()
        .then(attributes().or(empty().to(Attributes::default())))
        .then(padded(ident()))
        .then_ignore(padded(just('=')))
        .then(padded(expr()))
        .map(|(((doc_comments, attrs), name), expr)| BitflagsStatement {
            name,
            expr,
            attributes: attrs,
            doc_comments,
        })
        .map_with(|stmt, extra| Spanned::new(stmt, to_span(extra.span())))
}

// Item definitions

fn item_definition<'a>(
    doc_comments: Vec<Spanned<String>>,
) -> impl Parser<'a, ParserInput<'a>, Spanned<ItemDefinition>, ParseError<'a>> + Clone {
    // Type definition
    let type_def = padded(keyword("type"))
        .ignore_then(padded(ident()))
        .then(choice((
            padded(just(';')).to(vec![]),
            padded(just('{'))
                .then_ignore(whitespace())
                .ignore_then(
                    choice((
                        // Try statements first
                        type_statement()
                            .then_ignore(just(',').padded_by(whitespace()).or_not())
                            .map_with(|stmt, extra| {
                                Spanned::new(TypeChild::Statement(stmt), to_span(extra.span()))
                            }),
                        // Then try standalone comments
                        line_comment_node()
                            .or(block_comment_node())
                            .padded_by(whitespace())
                            .map_with(|comment, extra| {
                                Spanned::new(TypeChild::Comment(comment), to_span(extra.span()))
                            }),
                    ))
                    .repeated()
                    .collect(),
                )
                .then_ignore(whitespace())
                .then_ignore(just('}')),
        )))
        .map(|(name, children)| {
            (
                name,
                ItemDefinitionInner::Type(TypeDefinition {
                    children,
                    attributes: Attributes::default(),
                }),
            )
        });

    // Enum definition
    let enum_def = padded(keyword("enum"))
        .ignore_then(padded(ident()))
        .then(
            padded(just(':')).ignore_then(padded(type_parser())).then(
                padded(just('{'))
                    .then_ignore(whitespace())
                    .ignore_then(
                        enum_statement()
                            .map_with(|stmt, extra| {
                                Spanned::new(EnumChild::Statement(stmt), to_span(extra.span()))
                            })
                            .or(
                                // Parse comments between enum items
                                line_comment_node()
                                    .or(block_comment_node())
                                    .padded_by(whitespace())
                                    .map_with(|comment, extra| {
                                        Spanned::new(EnumChild::Comment(comment), to_span(extra.span()))
                                    })
                            )
                            .separated_by(just(',').padded_by(whitespace()))
                            .allow_trailing()
                            .collect(),
                    )
                    .then_ignore(whitespace())
                    .then_ignore(just('}')),
            ),
        )
        .map(|(name, (ty, children))| {
            (
                name,
                ItemDefinitionInner::Enum(EnumDefinition {
                    type_: ty,
                    children,
                    attributes: Attributes::default(),
                }),
            )
        });

    // Bitflags definition
    let bitflags_def = padded(keyword("bitflags"))
        .ignore_then(padded(ident()))
        .then(
            padded(just(':')).ignore_then(padded(type_parser())).then(
                padded(just('{'))
                    .then_ignore(whitespace())
                    .ignore_then(
                        bitflags_statement()
                            .map_with(|stmt, extra| {
                                Spanned::new(BitflagsChild::Statement(stmt), to_span(extra.span()))
                            })
                            .or(
                                // Parse comments between bitflags items
                                line_comment_node()
                                    .or(block_comment_node())
                                    .padded_by(whitespace())
                                    .map_with(|comment, extra| {
                                        Spanned::new(BitflagsChild::Comment(comment), to_span(extra.span()))
                                    })
                            )
                            .separated_by(just(',').padded_by(whitespace()))
                            .allow_trailing()
                            .collect(),
                    )
                    .then_ignore(whitespace())
                    .then_ignore(just('}')),
            ),
        )
        .map(|(name, (ty, children))| {
            (
                name,
                ItemDefinitionInner::Bitflags(BitflagsDefinition {
                    type_: ty,
                    children,
                    attributes: Attributes::default(),
                }),
            )
        });

    attributes()
        .or(empty().to(Attributes::default()))
        .then(padded(visibility()))
        .then(choice((type_def, enum_def, bitflags_def)))
        .map(move |((attrs, vis), (name, mut inner))| {
            // Update the inner attributes
            match &mut inner {
                ItemDefinitionInner::Type(td) => td.attributes = attrs.clone(),
                ItemDefinitionInner::Enum(ed) => ed.attributes = attrs.clone(),
                ItemDefinitionInner::Bitflags(bd) => bd.attributes = attrs.clone(),
            }
            ItemDefinition {
                visibility: vis,
                name,
                doc_comments: doc_comments.clone(),
                inner,
            }
        })
        .map_with(|item, extra| Spanned::new(item, to_span(extra.span())))
}

// Backend definitions

fn backend<'a>() -> impl Parser<'a, ParserInput<'a>, Spanned<Backend>, ParseError<'a>> + Clone {
    padded(keyword("backend"))
        .ignore_then(padded(ident()))
        .then(choice((
            // Inline prologue or epilogue
            padded(keyword("prologue"))
                .ignore_then(padded(string_literal()))
                .then_ignore(padded(just(';')))
                .map(|s| {
                    if let Expr::StringLiteral(content) = s.node {
                        (Some(content.trim().to_string()), None)
                    } else {
                        (None, None)
                    }
                }),
            padded(keyword("epilogue"))
                .ignore_then(padded(string_literal()))
                .then_ignore(padded(just(';')))
                .map(|s| {
                    if let Expr::StringLiteral(content) = s.node {
                        (None, Some(content.trim().to_string()))
                    } else {
                        (None, None)
                    }
                }),
            // Block form
            padded(just('{'))
                .ignore_then(
                    choice((
                        padded(keyword("prologue"))
                            .ignore_then(padded(string_literal()))
                            .then_ignore(padded(just(';')))
                            .map(|s| {
                                if let Expr::StringLiteral(content) = s.node {
                                    (0, content.trim().to_string())
                                } else {
                                    (0, String::new())
                                }
                            }),
                        padded(keyword("epilogue"))
                            .ignore_then(padded(string_literal()))
                            .then_ignore(padded(just(';')))
                            .map(|s| {
                                if let Expr::StringLiteral(content) = s.node {
                                    (1, content.trim().to_string())
                                } else {
                                    (1, String::new())
                                }
                            }),
                    ))
                    .repeated()
                    .collect::<Vec<_>>(),
                )
                .then_ignore(padded(just('}')))
                .map(|items| {
                    let mut prologue = None;
                    let mut epilogue = None;
                    for (kind, content) in items {
                        if kind == 0 {
                            prologue = Some(content);
                        } else {
                            epilogue = Some(content);
                        }
                    }
                    (prologue, epilogue)
                }),
        )))
        .map(|(name, (prologue, epilogue))| Backend {
            name,
            prologue,
            epilogue,
        })
        .map_with(|backend, extra| Spanned::new(backend, to_span(extra.span())))
}

// Extern types and values

fn extern_type<'a>()
-> impl Parser<'a, ParserInput<'a>, (Spanned<Ident>, Attributes), ParseError<'a>> + Clone {
    attributes()
        .or(empty().to(Attributes::default()))
        .then_ignore(padded(keyword("extern")))
        .then_ignore(padded(keyword("type")))
        .then(
            type_ident()
                .map(Ident)
                .map_with(|ident, extra| Spanned::new(ident, to_span(extra.span()))),
        )
        .then_ignore(padded(just(';')))
        .map(|(attrs, ident)| (ident, attrs))
}

fn extern_value<'a>()
-> impl Parser<'a, ParserInput<'a>, Spanned<ExternValue>, ParseError<'a>> + Clone {
    attributes()
        .or(empty().to(Attributes::default()))
        .then(padded(visibility()))
        .then_ignore(padded(keyword("extern")))
        .then(padded(ident()))
        .then_ignore(padded(just(':')))
        .then(padded(type_parser()))
        .then_ignore(padded(just(';')))
        .map(|(((attrs, vis), name), ty)| ExternValue {
            visibility: vis,
            name,
            type_: ty,
            attributes: attrs,
        })
        .map_with(|ev, extra| Spanned::new(ev, to_span(extra.span())))
}

// Impl blocks

fn impl_block<'a>()
-> impl Parser<'a, ParserInput<'a>, Spanned<FunctionBlock>, ParseError<'a>> + Clone {
    let impl_func = doc_comment()
        .then_ignore(one_of(" \t\r\n").repeated())
        .repeated()
        .collect::<Vec<_>>()
        .then(
            attributes()
                .or(empty().to(Attributes::default()))
                .then(padded(visibility()))
                .then_ignore(padded(keyword("fn")))
                .then(padded(ident()))
                .then(argument_list())
                .then(
                    padded(just("->"))
                        .ignore_then(padded(type_parser()))
                        .or_not(),
                ),
        )
        .map(
            |(doc_comments, ((((attrs, vis), name), args), ret_ty))| Function {
                visibility: vis,
                name,
                attributes: attrs,
                doc_comments,
                arguments: args,
                return_type: ret_ty,
            },
        )
        .map_with(|func, extra| Spanned::new(func, to_span(extra.span())));

    attributes()
        .or(empty().to(Attributes::default()))
        .then_ignore(padded(keyword("impl")))
        .then(padded(ident()))
        .then(
            padded(just('{'))
                .then_ignore(whitespace())
                .ignore_then(
                    choice((
                        // Parse a comment (can appear anywhere)
                        line_comment_node()
                            .or(block_comment_node())
                            .padded_by(whitespace())
                            .map_with(|comment, extra| {
                                Spanned::new(ImplChild::Comment(comment), to_span(extra.span()))
                            }),
                        // Parse a function (followed by optional semicolon and whitespace)
                        impl_func
                            .then_ignore(whitespace())
                            .then_ignore(just(';').or_not())
                            .then_ignore(whitespace())
                            .map_with(|f, extra| {
                                Spanned::new(ImplChild::Function(f), to_span(extra.span()))
                            }),
                    ))
                    .repeated()
                    .collect::<Vec<_>>(),
                )
                .then_ignore(whitespace())
                .then_ignore(just('}')),
        )
        .map(|((attrs, name), children)| FunctionBlock {
            name,
            children,
            attributes: attrs,
        })
        .map_with(|fb, extra| Spanned::new(fb, to_span(extra.span())))
}

// Use statements

fn use_statement<'a>() -> impl Parser<'a, ParserInput<'a>, Spanned<ItemPath>, ParseError<'a>> + Clone
{
    keyword("use")
        .ignore_then(padded(item_path()))
        .then_ignore(padded(just(';')))
}

// Module

pub fn module<'a>() -> impl Parser<'a, ParserInput<'a>, Module, ParseError<'a>> + Clone {

    // Freestanding function parser
    let freestanding_func = doc_comment()
        .then_ignore(one_of(" \t\r\n").repeated())
        .repeated()
        .collect::<Vec<_>>()
        .then(function(vec![]))
        .map(|(doc_comments, mut func)| {
            func.node.doc_comments = doc_comments;
            func
        })
        .then_ignore(padded(just(';')));

    // Item definition parser
    let item_def = doc_comment()
        .then_ignore(one_of(" \t\r\n").repeated())
        .repeated()
        .collect::<Vec<_>>()
        .then(item_definition(vec![]))
        .map(|(doc_comments, mut item)| {
            item.node.doc_comments = doc_comments;
            item
        });

    whitespace()
        .ignore_then(
            module_doc_comment()
                .then_ignore(whitespace())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(whitespace())
        .then(module_attributes().or(empty().to(Attributes::default())))
        .then_ignore(whitespace())
        .then(
            choice((
                // Parse standalone comments
                line_comment_node()
                    .or(block_comment_node())
                    .padded_by(whitespace())
                    .map(ModuleChild::Comment)
                    .map_with(|child, extra| Spanned::new(child, to_span(extra.span()))),
                // Use statements
                use_statement().map(ModuleChild::Use)
                    .map_with(|child, extra| Spanned::new(child, to_span(extra.span()))),
                // Extern types
                extern_type().map(|(name, attrs)| ModuleChild::ExternType(name, attrs))
                    .map_with(|child, extra| Spanned::new(child, to_span(extra.span()))),
                // Extern values
                extern_value().map(ModuleChild::ExternValue)
                    .map_with(|child, extra| Spanned::new(child, to_span(extra.span()))),
                // Backend
                backend().map(ModuleChild::Backend)
                    .map_with(|child, extra| Spanned::new(child, to_span(extra.span()))),
                // Impl blocks
                impl_block().map(ModuleChild::Impl)
                    .map_with(|child, extra| Spanned::new(child, to_span(extra.span()))),
                // Freestanding functions
                freestanding_func.map(ModuleChild::Function)
                    .map_with(|child, extra| Spanned::new(child, to_span(extra.span()))),
                // Item definitions
                item_def.map(ModuleChild::Definition)
                    .map_with(|child, extra| Spanned::new(child, to_span(extra.span()))),
            ))
            .then_ignore(whitespace())
            .repeated()
            .collect::<Vec<_>>(),
        )
        .then_ignore(whitespace())
        .then_ignore(end())
        .map(|((module_doc_comments, module_attrs), mut items)| {
            let mut children = vec![];

            // Add module doc comments as Comment children at the beginning
            for comment in module_doc_comments {
                children.push(Spanned::new(
                    ModuleChild::Comment(Comment::ModuleDoc(comment.node)),
                    comment.span,
                ));
            }

            // Add all items as children (they're already spanned)
            children.append(&mut items);

            Module {
                children,
                attributes: module_attrs,
            }
        })
}

pub fn parse_str(input: &str) -> Result<Module, Vec<Rich<'_, char>>> {
    module().parse(input).into_result()
}

/// Format parse errors with nice diagnostics using ariadne
pub fn format_parse_errors(source_name: &str, source: &str, errors: &[Rich<'_, char>]) -> String {
    use ariadne::{Color, Label, Report, ReportKind, Source};

    let mut output = Vec::new();

    for error in errors {
        let span = error.span();
        let report = Report::build(ReportKind::Error, source_name, span.start)
            .with_message(error.to_string())
            .with_label(
                Label::new((source_name, span.into_range()))
                    .with_message(error.reason().to_string())
                    .with_color(Color::Red),
            );

        report
            .finish()
            .write((source_name, Source::from(source)), &mut output)
            .unwrap();
    }

    String::from_utf8(output).unwrap()
}
