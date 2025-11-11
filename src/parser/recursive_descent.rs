use crate::grammar::*;
use crate::span::{Location, Span, Spanned};
use crate::tokenizer::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub location: Location,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse error at {}: {}", self.location, self.message)
    }
}

impl std::error::Error for ParseError {}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn current(&self) -> &Token {
        &self.tokens[self.pos.min(self.tokens.len() - 1)]
    }

    fn peek(&self) -> &TokenKind {
        &self.current().kind
    }

    fn peek_nth(&self, n: usize) -> &TokenKind {
        let pos = (self.pos + n).min(self.tokens.len() - 1);
        &self.tokens[pos].kind
    }

    fn advance(&mut self) -> Token {
        let token = self.current().clone();
        if !matches!(token.kind, TokenKind::Eof) {
            self.pos += 1;
        }
        token
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&kind) {
            Ok(self.advance())
        } else {
            Err(ParseError {
                message: format!("Expected {:?}, found {:?}", kind, self.peek()),
                location: self.current().span.start,
            })
        }
    }

    fn expect_ident(&mut self) -> Result<(Ident, Span), ParseError> {
        match self.peek() {
            TokenKind::Ident(_) => {
                let token = self.advance();
                if let TokenKind::Ident(name) = token.kind {
                    Ok((Ident(name), token.span))
                } else {
                    unreachable!()
                }
            }
            TokenKind::Underscore => {
                let token = self.advance();
                Ok((Ident("_".to_string()), token.span))
            }
            _ => Err(ParseError {
                message: format!("Expected identifier, found {:?}", self.peek()),
                location: self.current().span.start,
            }),
        }
    }

    /// Collect consecutive doc comments (///)
    fn collect_doc_comments(&mut self) -> Vec<String> {
        let mut comments = Vec::new();
        while matches!(self.peek(), TokenKind::DocOuter(_)) {
            if let TokenKind::DocOuter(text) = &self.advance().kind {
                // Strip the /// prefix but preserve spacing
                let content = text.strip_prefix("///").unwrap_or(text).to_string();
                comments.push(content);
            }
        }
        comments
    }

    /// Collect a comment as a Spanned<Comment>
    fn collect_comment(&mut self) -> Option<Spanned<Comment>> {
        match self.peek().clone() {
            TokenKind::DocOuter(ref text) => {
                let token = self.advance();
                let content = text.strip_prefix("///").unwrap_or(text).trim().to_string();
                Some(Spanned::new(
                    Comment::DocOuter(vec![content]),
                    token.span,
                ))
            }
            TokenKind::DocInner(ref text) => {
                let token = self.advance();
                let content = text.strip_prefix("//!").unwrap_or(text).trim().to_string();
                Some(Spanned::new(
                    Comment::DocInner(vec![content]),
                    token.span,
                ))
            }
            TokenKind::Comment(ref text) => {
                let token = self.advance();
                Some(Spanned::new(
                    Comment::Regular(text.clone()),
                    token.span,
                ))
            }
            TokenKind::MultiLineComment(ref text) => {
                let token = self.advance();
                // Split multiline comments into lines
                let lines: Vec<String> = text.lines().map(|s| s.to_string()).collect();
                Some(Spanned::new(
                    Comment::MultiLine(lines),
                    token.span,
                ))
            }
            _ => None,
        }
    }

    /// Skip over all comments and whitespace
    fn skip_trivia(&mut self) {
        while matches!(
            self.peek(),
            TokenKind::Comment(_)
                | TokenKind::DocOuter(_)
                | TokenKind::DocInner(_)
                | TokenKind::MultiLineComment(_)
        ) {
            self.advance();
        }
    }

    pub fn parse_module(&mut self) -> Result<Module, ParseError> {
        let mut items = Vec::new();
        let mut module_doc_comments = Vec::new();

        // Collect module-level doc comments (//!)
        while matches!(self.peek(), TokenKind::DocInner(_)) {
            if let TokenKind::DocInner(text) = &self.advance().kind {
                let content = text.strip_prefix("//!").unwrap_or(text).to_string();
                module_doc_comments.push(content);
            }
        }

        while !matches!(self.peek(), TokenKind::Eof) {
            // Collect non-doc comments (doc comments will be collected by item parsers)
            while matches!(self.peek(), TokenKind::Comment(_) | TokenKind::MultiLineComment(_)) {
                if let Some(comment) = self.collect_comment() {
                    items.push(ModuleItem::Comment(comment));
                }
            }

            if matches!(self.peek(), TokenKind::Eof) {
                break;
            }

            // Parse module-level items
            items.push(self.parse_module_item()?);
        }

        // Convert module doc comments to attributes
        let mut attributes = Vec::new();
        for comment in module_doc_comments {
            attributes.push(Attribute::doc(&comment));
        }

        Ok(Module {
            items,
            attributes: Attributes(attributes),
        })
    }

    fn parse_module_item(&mut self) -> Result<ModuleItem, ParseError> {
        // Attributes can appear before any item
        let has_attributes = matches!(self.peek(), TokenKind::Hash);

        match self.peek() {
            TokenKind::Use => self.parse_use().map(ModuleItem::Use),
            TokenKind::Extern if !has_attributes => {
                // Peek ahead to distinguish extern type from extern value
                if matches!(self.peek_nth(1), TokenKind::Type) {
                    self.parse_extern_type()
                } else {
                    self.parse_extern_value().map(ModuleItem::ExternValue)
                }
            }
            TokenKind::Backend => self.parse_backend().map(ModuleItem::Backend),
            TokenKind::Hash => {
                // Attributes - need to peek ahead to see what comes after
                let mut pos = self.pos;
                // Skip past attributes
                while matches!(self.tokens[pos].kind, TokenKind::Hash) {
                    pos += 1; // skip #
                    if matches!(self.tokens[pos].kind, TokenKind::LBracket) {
                        pos += 1; // skip [
                        // Skip until ]
                        let mut depth = 1;
                        while depth > 0 && pos < self.tokens.len() {
                            match &self.tokens[pos].kind {
                                TokenKind::LBracket => depth += 1,
                                TokenKind::RBracket => depth -= 1,
                                _ => {}
                            }
                            pos += 1;
                        }
                    }
                }

                // Now check what comes after attributes
                match &self.tokens[pos].kind {
                    TokenKind::Extern => {
                        // Could be extern type or extern value
                        if matches!(self.tokens.get(pos + 1).map(|t| &t.kind), Some(TokenKind::Type)) {
                            self.parse_extern_type()
                        } else {
                            self.parse_extern_value().map(ModuleItem::ExternValue)
                        }
                    }
                    TokenKind::Pub => {
                        // Could be pub extern value, pub fn, or pub item definition
                        match self.tokens.get(pos + 1).map(|t| &t.kind) {
                            Some(TokenKind::Extern) => {
                                self.parse_extern_value().map(ModuleItem::ExternValue)
                            }
                            Some(TokenKind::Fn) => {
                                self.parse_function().map(ModuleItem::Function)
                            }
                            _ => {
                                self.parse_item_definition().map(ModuleItem::Definition)
                            }
                        }
                    }
                    TokenKind::Type | TokenKind::Enum | TokenKind::Bitflags => {
                        self.parse_item_definition().map(ModuleItem::Definition)
                    }
                    TokenKind::Impl => self.parse_impl_block().map(ModuleItem::Impl),
                    TokenKind::Fn => self.parse_function().map(ModuleItem::Function),
                    _ => Err(ParseError {
                        message: format!("Unexpected token after attributes: {:?}", self.tokens[pos].kind),
                        location: self.tokens[pos].span.start,
                    }),
                }
            }
            TokenKind::DocOuter(_) | TokenKind::Pub | TokenKind::Type | TokenKind::Enum | TokenKind::Bitflags => {
                self.parse_item_definition().map(ModuleItem::Definition)
            }
            TokenKind::Impl => self.parse_impl_block().map(ModuleItem::Impl),
            TokenKind::Fn => {
                // Freestanding function with attributes
                self.parse_function().map(ModuleItem::Function)
            }
            _ => Err(ParseError {
                message: format!("Unexpected token at module level: {:?}", self.peek()),
                location: self.current().span.start,
            }),
        }
    }

    fn parse_use(&mut self) -> Result<ItemPath, ParseError> {
        self.expect(TokenKind::Use)?;

        // Check for super keyword (not supported yet)
        if let TokenKind::Ident(name) = self.peek() {
            if name == "super" {
                return Err(ParseError {
                    message: "super not supported".to_string(),
                    location: self.current().span.start,
                });
            }
        }

        let path = self.parse_item_path()?;
        self.expect(TokenKind::Semi)?;
        Ok(path)
    }

    fn parse_extern_type(&mut self) -> Result<ModuleItem, ParseError> {
        let doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };
        self.expect(TokenKind::Extern)?;
        self.expect(TokenKind::Type)?;
        let (mut name, _) = self.expect_ident()?;

        // Handle generics - concatenate them into the type name string
        if matches!(self.peek(), TokenKind::Lt) {
            let mut type_str = name.0;
            type_str.push('<');
            self.advance(); // consume <

            let mut depth = 1;
            while depth > 0 && !matches!(self.peek(), TokenKind::Eof) {
                match self.peek().clone() {
                    TokenKind::Lt => {
                        type_str.push('<');
                        depth += 1;
                        self.advance();
                    }
                    TokenKind::Gt => {
                        type_str.push('>');
                        depth -= 1;
                        self.advance();
                    }
                    TokenKind::Comma => {
                        type_str.push_str(", ");
                        self.advance();
                    }
                    TokenKind::Ident(n) => {
                        type_str.push_str(&n);
                        self.advance();
                    }
                    TokenKind::ColonColon => {
                        type_str.push_str("::");
                        self.advance();
                    }
                    _ => {
                        self.advance();
                    }
                }
            }
            name = Ident(type_str);
        }

        self.expect(TokenKind::Semi)?;
        Ok(ModuleItem::ExternType(name, attributes, doc_comments))
    }

    fn parse_extern_value(&mut self) -> Result<ExternValue, ParseError> {
        let doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        let visibility = self.parse_visibility()?;
        self.expect(TokenKind::Extern)?;
        let (name, _) = self.expect_ident()?;
        self.expect(TokenKind::Colon)?;
        let type_ = self.parse_type()?;
        self.expect(TokenKind::Semi)?;

        Ok(ExternValue {
            visibility,
            name,
            type_,
            attributes,
            doc_comments,
        })
    }

    fn parse_backend(&mut self) -> Result<Backend, ParseError> {
        self.expect(TokenKind::Backend)?;
        let (name, _) = self.expect_ident()?;

        let mut prologue = None;
        let mut epilogue = None;

        // Check if we have braces or direct prologue/epilogue
        if matches!(self.peek(), TokenKind::LBrace) {
            // Form: backend name { prologue ...; epilogue ...; }
            self.advance(); // consume {

            while !matches!(self.peek(), TokenKind::RBrace) {
                match self.peek() {
                    TokenKind::Prologue => {
                        self.advance();
                        let string = self.parse_string_literal()?.trim().to_string();
                        self.expect(TokenKind::Semi)?;
                        prologue = Some(string);
                    }
                    TokenKind::Epilogue => {
                        self.advance();
                        let string = self.parse_string_literal()?.trim().to_string();
                        self.expect(TokenKind::Semi)?;
                        epilogue = Some(string);
                    }
                    _ => {
                        return Err(ParseError {
                            message: format!("Expected prologue or epilogue, found {:?}", self.peek()),
                            location: self.current().span.start,
                        });
                    }
                }
            }

            self.expect(TokenKind::RBrace)?;
        } else {
            // Form: backend name prologue ... or backend name epilogue ...
            match self.peek() {
                TokenKind::Prologue => {
                    self.advance();
                    let string = self.parse_string_literal()?.trim().to_string();
                    self.expect(TokenKind::Semi)?;
                    prologue = Some(string);
                }
                TokenKind::Epilogue => {
                    self.advance();
                    let string = self.parse_string_literal()?.trim().to_string();
                    self.expect(TokenKind::Semi)?;
                    epilogue = Some(string);
                }
                _ => {
                    return Err(ParseError {
                        message: format!("Expected LBrace, Prologue, or Epilogue, found {:?}", self.peek()),
                        location: self.current().span.start,
                    });
                }
            }
        }

        Ok(Backend {
            name,
            prologue,
            epilogue,
        })
    }

    fn parse_item_definition(&mut self) -> Result<ItemDefinition, ParseError> {
        let doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        let visibility = self.parse_visibility()?;

        match self.peek() {
            TokenKind::Type => {
                self.advance();
                let (name, _) = self.expect_ident()?;
                let mut def = TypeDefinition {
                    items: Vec::new(),
                    attributes,
                };

                // Support both "type Name;" and "type Name { ... }"
                if matches!(self.peek(), TokenKind::Semi) {
                    self.advance(); // Consume semicolon
                } else {
                    self.expect(TokenKind::LBrace)?;
                    def.items = self.parse_type_def_items()?;
                    self.expect(TokenKind::RBrace)?;
                }

                Ok(ItemDefinition {
                    visibility,
                    name,
                    doc_comments,
                    inner: ItemDefinitionInner::Type(def),
                })
            }
            TokenKind::Enum => {
                self.advance();
                let (name, _) = self.expect_ident()?;
                self.expect(TokenKind::Colon)?;
                let type_ = self.parse_type()?;
                self.expect(TokenKind::LBrace)?;
                let items = self.parse_enum_def_items()?;
                self.expect(TokenKind::RBrace)?;

                Ok(ItemDefinition {
                    visibility,
                    name,
                    doc_comments,
                    inner: ItemDefinitionInner::Enum(EnumDefinition {
                        type_,
                        items,
                        attributes,
                    }),
                })
            }
            TokenKind::Bitflags => {
                self.advance();
                let (name, _) = self.expect_ident()?;
                self.expect(TokenKind::Colon)?;
                let type_ = self.parse_type()?;
                self.expect(TokenKind::LBrace)?;
                let items = self.parse_bitflags_def_items()?;
                self.expect(TokenKind::RBrace)?;

                Ok(ItemDefinition {
                    visibility,
                    name,
                    doc_comments,
                    inner: ItemDefinitionInner::Bitflags(BitflagsDefinition {
                        type_,
                        items,
                        attributes,
                    }),
                })
            }
            _ => Err(ParseError {
                message: format!("Expected type, enum, or bitflags, found {:?}", self.peek()),
                location: self.current().span.start,
            }),
        }
    }

    fn parse_type_def_items(&mut self) -> Result<Vec<TypeDefItem>, ParseError> {
        let mut items = Vec::new();

        while !matches!(self.peek(), TokenKind::RBrace) {
            // Collect non-doc comments (doc comments will be collected by parse_type_statement)
            while matches!(self.peek(), TokenKind::Comment(_) | TokenKind::MultiLineComment(_)) {
                if let Some(comment) = self.collect_comment() {
                    items.push(TypeDefItem::Comment(comment));
                }
            }

            if matches!(self.peek(), TokenKind::RBrace) {
                break;
            }

            items.push(TypeDefItem::Statement(self.parse_type_statement()?));

            // Optional trailing comma
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            }
        }

        Ok(items)
    }

    fn parse_type_statement(&mut self) -> Result<TypeStatement, ParseError> {
        let doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        if matches!(self.peek(), TokenKind::Vftable) {
            self.advance();
            self.expect(TokenKind::LBrace)?;
            let functions = self.parse_functions_in_block()?;
            self.expect(TokenKind::RBrace)?;

            Ok(TypeStatement {
                field: TypeField::Vftable(functions),
                attributes,
                doc_comments,
            })
        } else {
            let visibility = self.parse_visibility()?;
            let (name, _) = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let type_ = self.parse_type()?;

            Ok(TypeStatement {
                field: TypeField::Field(visibility, name, type_),
                attributes,
                doc_comments,
            })
        }
    }

    fn parse_enum_def_items(&mut self) -> Result<Vec<EnumDefItem>, ParseError> {
        let mut items = Vec::new();

        while !matches!(self.peek(), TokenKind::RBrace) {
            // Collect non-doc comments (doc comments will be collected by parse_enum_statement)
            while matches!(self.peek(), TokenKind::Comment(_) | TokenKind::MultiLineComment(_)) {
                if let Some(comment) = self.collect_comment() {
                    items.push(EnumDefItem::Comment(comment));
                }
            }

            if matches!(self.peek(), TokenKind::RBrace) {
                break;
            }

            items.push(EnumDefItem::Statement(self.parse_enum_statement()?));

            // Optional trailing comma
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            }
        }

        Ok(items)
    }

    fn parse_enum_statement(&mut self) -> Result<EnumStatement, ParseError> {
        let doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        let (name, _) = self.expect_ident()?;
        let expr = if matches!(self.peek(), TokenKind::Eq) {
            self.advance();
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(EnumStatement {
            name,
            expr,
            attributes,
            doc_comments,
        })
    }

    fn parse_bitflags_def_items(&mut self) -> Result<Vec<BitflagsDefItem>, ParseError> {
        let mut items = Vec::new();

        while !matches!(self.peek(), TokenKind::RBrace) {
            // Collect non-doc comments (doc comments will be collected by parse_bitflags_statement)
            while matches!(self.peek(), TokenKind::Comment(_) | TokenKind::MultiLineComment(_)) {
                if let Some(comment) = self.collect_comment() {
                    items.push(BitflagsDefItem::Comment(comment));
                }
            }

            if matches!(self.peek(), TokenKind::RBrace) {
                break;
            }

            items.push(BitflagsDefItem::Statement(self.parse_bitflags_statement()?));

            // Optional trailing comma
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            }
        }

        Ok(items)
    }

    fn parse_bitflags_statement(&mut self) -> Result<BitflagsStatement, ParseError> {
        let doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        let (name, _) = self.expect_ident()?;
        self.expect(TokenKind::Eq)?;
        let expr = self.parse_expr()?;

        Ok(BitflagsStatement {
            name,
            expr,
            attributes,
            doc_comments,
        })
    }

    fn parse_impl_block(&mut self) -> Result<FunctionBlock, ParseError> {
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        self.expect(TokenKind::Impl)?;
        let (name, _) = self.expect_ident()?;
        self.expect(TokenKind::LBrace)?;

        let mut items = Vec::new();
        while !matches!(self.peek(), TokenKind::RBrace) {
            // Collect non-doc comments (doc comments will be collected by parse_function)
            while matches!(self.peek(), TokenKind::Comment(_) | TokenKind::MultiLineComment(_)) {
                if let Some(comment) = self.collect_comment() {
                    items.push(ImplItem::Comment(comment));
                }
            }

            if matches!(self.peek(), TokenKind::RBrace) {
                break;
            }

            items.push(ImplItem::Function(self.parse_function()?));
        }

        self.expect(TokenKind::RBrace)?;

        Ok(FunctionBlock {
            name,
            items,
            attributes,
        })
    }

    fn parse_functions_in_block(&mut self) -> Result<Vec<Function>, ParseError> {
        let mut functions = Vec::new();

        while !matches!(self.peek(), TokenKind::RBrace) {
            // Skip regular comments but not doc comments (parse_function will collect those)
            while matches!(self.peek(), TokenKind::Comment(_) | TokenKind::MultiLineComment(_)) {
                self.advance();
            }

            if matches!(self.peek(), TokenKind::RBrace) {
                break;
            }

            functions.push(self.parse_function()?);

            // Optional trailing comma
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            }
        }

        Ok(functions)
    }

    fn parse_function(&mut self) -> Result<Function, ParseError> {
        let doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        let visibility = self.parse_visibility()?;
        self.expect(TokenKind::Fn)?;
        let (name, _) = self.expect_ident()?;
        self.expect(TokenKind::LParen)?;

        let mut arguments = Vec::new();
        while !matches!(self.peek(), TokenKind::RParen) {
            arguments.push(self.parse_argument()?);
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(TokenKind::RParen)?;

        let return_type = if matches!(self.peek(), TokenKind::Arrow) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenKind::Semi)?;

        Ok(Function {
            visibility,
            name,
            attributes,
            doc_comments,
            arguments,
            return_type,
        })
    }

    fn parse_argument(&mut self) -> Result<Argument, ParseError> {
        if matches!(self.peek(), TokenKind::Amp) {
            self.advance();
            if matches!(self.peek(), TokenKind::Mut) {
                self.advance();
                self.expect(TokenKind::SelfValue)?;
                Ok(Argument::MutSelf)
            } else {
                self.expect(TokenKind::SelfValue)?;
                Ok(Argument::ConstSelf)
            }
        } else {
            let (name, _) = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let type_ = self.parse_type()?;
            Ok(Argument::Named(name, type_))
        }
    }

    fn parse_visibility(&mut self) -> Result<Visibility, ParseError> {
        if matches!(self.peek(), TokenKind::Pub) {
            self.advance();
            Ok(Visibility::Public)
        } else {
            Ok(Visibility::Private)
        }
    }

    fn parse_attributes(&mut self) -> Result<Attributes, ParseError> {
        let mut attrs = Vec::new();

        while matches!(self.peek(), TokenKind::Hash) {
            self.advance();
            self.expect(TokenKind::LBracket)?;

            while !matches!(self.peek(), TokenKind::RBracket) {
                attrs.push(self.parse_attribute()?);
                if matches!(self.peek(), TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }

            self.expect(TokenKind::RBracket)?;
        }

        Ok(Attributes(attrs))
    }

    fn parse_attribute(&mut self) -> Result<Attribute, ParseError> {
        let (name, _) = self.expect_ident()?;

        if matches!(self.peek(), TokenKind::LParen) {
            // Function attribute
            self.advance();
            let mut args = Vec::new();
            while !matches!(self.peek(), TokenKind::RParen) {
                args.push(self.parse_expr()?);
                if matches!(self.peek(), TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
            self.expect(TokenKind::RParen)?;
            Ok(Attribute::Function(name, args))
        } else if matches!(self.peek(), TokenKind::Eq) {
            // Assign attribute
            self.advance();
            let expr = self.parse_expr()?;
            Ok(Attribute::Assign(name, expr))
        } else {
            // Ident attribute
            Ok(Attribute::Ident(name))
        }
    }

    fn parse_item_path(&mut self) -> Result<ItemPath, ParseError> {
        let mut segments = Vec::new();

        loop {
            if let TokenKind::Ident(name) = self.peek() {
                segments.push(ItemPathSegment::from(name.clone()));
                self.advance();

                // Handle generics in the path
                if matches!(self.peek(), TokenKind::Lt) {
                    // Parse generic arguments as part of the segment
                    let generic_str = self.parse_generic_args_as_string()?;
                    let last = segments.last_mut().unwrap();
                    *last = ItemPathSegment::from(format!("{}{}", last.as_str(), generic_str));
                }

                if matches!(self.peek(), TokenKind::ColonColon) {
                    self.advance();
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(ItemPath::from_iter(segments))
    }

    fn parse_generic_args_as_string(&mut self) -> Result<String, ParseError> {
        let mut result = String::new();
        result.push('<');
        self.expect(TokenKind::Lt)?;

        let mut first = true;
        while !matches!(self.peek(), TokenKind::Gt) {
            if !first {
                self.expect(TokenKind::Comma)?;
                result.push_str(", ");
            }
            first = false;

            // Parse type as string for now
            result.push_str(&self.parse_type_as_string()?);
        }

        self.expect(TokenKind::Gt)?;
        result.push('>');
        Ok(result)
    }

    fn parse_type_as_string(&mut self) -> Result<String, ParseError> {
        let start_pos = self.pos;
        self.parse_type()?; // Parse but discard
        let end_pos = self.pos;

        // Reconstruct the string from tokens
        let mut result = String::new();
        for i in start_pos..end_pos {
            result.push_str(&self.tokens[i].span.text);
        }
        Ok(result)
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.peek() {
            TokenKind::Unknown => {
                self.advance();
                self.expect(TokenKind::Lt)?;
                let size = self.parse_int_literal()? as usize;
                self.expect(TokenKind::Gt)?;
                Ok(Type::Unknown(size))
            }
            TokenKind::Star => {
                self.advance();
                if matches!(self.peek(), TokenKind::Const) {
                    self.advance();
                    Ok(Type::ConstPointer(Box::new(self.parse_type()?)))
                } else if matches!(self.peek(), TokenKind::Mut) {
                    self.advance();
                    Ok(Type::MutPointer(Box::new(self.parse_type()?)))
                } else {
                    Err(ParseError {
                        message: "Expected const or mut after *".to_string(),
                        location: self.current().span.start,
                    })
                }
            }
            TokenKind::LBracket => {
                self.advance();
                let inner = self.parse_type()?;
                self.expect(TokenKind::Semi)?;
                let size = self.parse_int_literal()? as usize;
                self.expect(TokenKind::RBracket)?;
                Ok(Type::Array(Box::new(inner), size))
            }
            TokenKind::Ident(_) => {
                let (mut ident, _) = self.expect_ident()?;

                // Check for generic arguments - treat them as part of the identifier string
                // This is needed for extern types that need exact reproduction
                if matches!(self.peek(), TokenKind::Lt) {
                    let mut type_str = ident.0;
                    type_str.push('<');
                    self.advance(); // consume <

                    let mut depth = 1;
                    while depth > 0 && !matches!(self.peek(), TokenKind::Eof) {
                        match self.peek().clone() {
                            TokenKind::Lt => {
                                type_str.push('<');
                                depth += 1;
                                self.advance();
                            }
                            TokenKind::Gt => {
                                type_str.push('>');
                                depth -= 1;
                                self.advance();
                            }
                            TokenKind::Comma => {
                                type_str.push_str(", ");
                                self.advance();
                            }
                            TokenKind::Ident(name) => {
                                type_str.push_str(&name);
                                self.advance();
                            }
                            TokenKind::ColonColon => {
                                type_str.push_str("::");
                                self.advance();
                            }
                            _ => {
                                self.advance();
                            }
                        }
                    }
                    ident = Ident(type_str);
                }

                Ok(Type::Ident(ident, vec![]))
            }
            _ => Err(ParseError {
                message: format!("Expected type, found {:?}", self.peek()),
                location: self.current().span.start,
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            TokenKind::IntLiteral(_) => {
                let value = self.parse_int_literal()?;
                Ok(Expr::IntLiteral(value))
            }
            TokenKind::StringLiteral(_) => {
                let value = self.parse_string_literal()?;
                Ok(Expr::StringLiteral(value))
            }
            TokenKind::Ident(_) => {
                let (ident, _) = self.expect_ident()?;
                Ok(Expr::Ident(ident))
            }
            _ => Err(ParseError {
                message: format!("Expected expression, found {:?}", self.peek()),
                location: self.current().span.start,
            }),
        }
    }

    fn parse_int_literal(&mut self) -> Result<isize, ParseError> {
        match self.peek() {
            TokenKind::IntLiteral(_) => {
                let token = self.advance();
                if let TokenKind::IntLiteral(s) = token.kind {
                    // Remove underscores
                    let s = s.replace('_', "");

                    // Parse based on prefix
                    if s.starts_with("0x") || s.starts_with("-0x") {
                        // Hexadecimal
                        let (sign, hex_str) = if s.starts_with('-') {
                            (-1, &s[3..])
                        } else {
                            (1, &s[2..])
                        };

                        i64::from_str_radix(hex_str, 16)
                            .map(|v| (v * sign) as isize)
                            .map_err(|_| ParseError {
                                message: format!("Invalid hex literal: {}", s),
                                location: token.span.start,
                            })
                    } else if s.starts_with("0b") || s.starts_with("-0b") {
                        // Binary
                        let (sign, bin_str) = if s.starts_with('-') {
                            (-1, &s[3..])
                        } else {
                            (1, &s[2..])
                        };

                        i64::from_str_radix(bin_str, 2)
                            .map(|v| (v * sign) as isize)
                            .map_err(|_| ParseError {
                                message: format!("Invalid binary literal: {}", s),
                                location: token.span.start,
                            })
                    } else if s.starts_with("0o") || s.starts_with("-0o") {
                        // Octal
                        let (sign, oct_str) = if s.starts_with('-') {
                            (-1, &s[3..])
                        } else {
                            (1, &s[2..])
                        };

                        i64::from_str_radix(oct_str, 8)
                            .map(|v| (v * sign) as isize)
                            .map_err(|_| ParseError {
                                message: format!("Invalid octal literal: {}", s),
                                location: token.span.start,
                            })
                    } else {
                        // Decimal
                        s.parse::<isize>().map_err(|_| ParseError {
                            message: format!("Invalid integer literal: {}", s),
                            location: token.span.start,
                        })
                    }
                } else {
                    unreachable!()
                }
            }
            _ => Err(ParseError {
                message: format!("Expected integer literal, found {:?}", self.peek()),
                location: self.current().span.start,
            }),
        }
    }

    fn parse_string_literal(&mut self) -> Result<String, ParseError> {
        match self.peek() {
            TokenKind::StringLiteral(_) => {
                let token = self.advance();
                if let TokenKind::StringLiteral(s) = token.kind {
                    Ok(s)
                } else {
                    unreachable!()
                }
            }
            _ => Err(ParseError {
                message: format!("Expected string literal, found {:?}", self.peek()),
                location: self.current().span.start,
            }),
        }
    }
}
