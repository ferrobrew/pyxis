pub mod strip_spans;

use crate::{
    grammar::*,
    span::{Location, Span, Spanned},
    tokenizer::{LexError, Token, TokenKind},
};
use ariadne::{Color, Label, Report, ReportKind, Source};

#[cfg(test)]
mod tests;

/// Parse a Pyxis module from a string
pub fn parse_str(input: &str) -> Result<Module, ParseError> {
    parse_str_with_filename(input, "<input>")
}

/// Parse a Pyxis module from a string with a specific filename for error reporting
pub fn parse_str_with_filename(input: &str, filename: &str) -> Result<Module, ParseError> {
    // First tokenize
    let tokens = crate::tokenizer::tokenize_with_filename(input.to_string(), filename.to_string())?;

    // Then parse
    let mut parser = Parser::new(tokens, filename.to_string(), input.to_string());
    let module = parser.parse_module()?;

    Ok(module)
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    ExpectedToken {
        expected: TokenKind,
        found: TokenKind,
        location: Location,
        filename: Box<str>,
        source: Box<str>,
    },
    ExpectedIdentifier {
        found: TokenKind,
        location: Location,
        filename: Box<str>,
        source: Box<str>,
    },
    ExpectedType {
        found: TokenKind,
        location: Location,
        filename: Box<str>,
        source: Box<str>,
    },
    ExpectedExpression {
        found: TokenKind,
        location: Location,
        filename: Box<str>,
        source: Box<str>,
    },
    ExpectedIntLiteral {
        found: TokenKind,
        location: Location,
        filename: Box<str>,
        source: Box<str>,
    },
    ExpectedStringLiteral {
        found: TokenKind,
        location: Location,
        filename: Box<str>,
        source: Box<str>,
    },
    InvalidIntLiteral {
        kind: String,
        value: String,
        location: Location,
        filename: Box<str>,
        source: Box<str>,
    },
    MissingPointerQualifier {
        location: Location,
        filename: Box<str>,
        source: Box<str>,
    },
    SuperNotSupported {
        location: Location,
        filename: Box<str>,
        source: Box<str>,
    },
    UnexpectedModuleToken {
        found: TokenKind,
        location: Location,
        filename: Box<str>,
        source: Box<str>,
    },
    UnexpectedTokenAfterAttributes {
        found: TokenKind,
        location: Location,
        filename: Box<str>,
        source: Box<str>,
    },
    ExpectedItemDefinition {
        found: TokenKind,
        location: Location,
        filename: Box<str>,
        source: Box<str>,
    },
    ExpectedBackendContent {
        found: TokenKind,
        location: Location,
        filename: Box<str>,
        source: Box<str>,
    },
    ExpectedPrologueOrEpilogue {
        found: TokenKind,
        location: Location,
        filename: Box<str>,
        source: Box<str>,
    },
    Tokenizer(LexError),
}
impl ParseError {
    fn location_to_offset(location: Location, source: &str) -> usize {
        let mut offset = 0;
        let mut current_line = 1;

        for ch in source.chars() {
            if current_line == location.line {
                return offset + (location.column - 1);
            }
            if ch == '\n' {
                current_line += 1;
            }
            offset += ch.len_utf8();
        }
        offset
    }
}
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::ExpectedToken {
                expected,
                found,
                location,
                filename,
                source,
            } => {
                let offset = Self::location_to_offset(*location, source);
                let report = Report::build(ReportKind::Error, filename, offset)
                    .with_message(format!("Expected {:?}, found {:?}", expected, found))
                    .with_label(
                        Label::new((filename, offset..offset + 1))
                            .with_message(format!("expected {:?} here", expected))
                            .with_color(Color::Red),
                    )
                    .finish();

                let mut buffer = Vec::new();
                report
                    .write((filename, Source::from(source)), &mut buffer)
                    .map_err(|_| std::fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&buffer))
            }
            ParseError::ExpectedIdentifier {
                found,
                location,
                filename,
                source,
            } => {
                let offset = Self::location_to_offset(*location, source);
                let report = Report::build(ReportKind::Error, filename, offset)
                    .with_message(format!("Expected identifier, found {:?}", found))
                    .with_label(
                        Label::new((filename, offset..offset + 1))
                            .with_message("expected identifier here")
                            .with_color(Color::Red),
                    )
                    .finish();

                let mut buffer = Vec::new();
                report
                    .write((filename, Source::from(source)), &mut buffer)
                    .map_err(|_| std::fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&buffer))
            }
            ParseError::ExpectedType {
                found,
                location,
                filename,
                source,
            } => {
                let offset = Self::location_to_offset(*location, source);
                let report = Report::build(ReportKind::Error, filename, offset)
                    .with_message(format!("Expected type, found {:?}", found))
                    .with_label(
                        Label::new((filename, offset..offset + 1))
                            .with_message("expected type here")
                            .with_color(Color::Red),
                    )
                    .finish();

                let mut buffer = Vec::new();
                report
                    .write((filename, Source::from(source)), &mut buffer)
                    .map_err(|_| std::fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&buffer))
            }
            ParseError::ExpectedExpression {
                found,
                location,
                filename,
                source,
            } => {
                let offset = Self::location_to_offset(*location, source);
                let report = Report::build(ReportKind::Error, filename, offset)
                    .with_message(format!("Expected expression, found {:?}", found))
                    .with_label(
                        Label::new((filename, offset..offset + 1))
                            .with_message("expected expression here")
                            .with_color(Color::Red),
                    )
                    .finish();

                let mut buffer = Vec::new();
                report
                    .write((filename, Source::from(source)), &mut buffer)
                    .map_err(|_| std::fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&buffer))
            }
            ParseError::ExpectedIntLiteral {
                found,
                location,
                filename,
                source,
            } => {
                let offset = Self::location_to_offset(*location, source);
                let report = Report::build(ReportKind::Error, filename, offset)
                    .with_message(format!("Expected integer literal, found {:?}", found))
                    .with_label(
                        Label::new((filename, offset..offset + 1))
                            .with_message("expected integer literal here")
                            .with_color(Color::Red),
                    )
                    .finish();

                let mut buffer = Vec::new();
                report
                    .write((filename, Source::from(source)), &mut buffer)
                    .map_err(|_| std::fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&buffer))
            }
            ParseError::ExpectedStringLiteral {
                found,
                location,
                filename,
                source,
            } => {
                let offset = Self::location_to_offset(*location, source);
                let report = Report::build(ReportKind::Error, filename, offset)
                    .with_message(format!("Expected string literal, found {:?}", found))
                    .with_label(
                        Label::new((filename, offset..offset + 1))
                            .with_message("expected string literal here")
                            .with_color(Color::Red),
                    )
                    .finish();

                let mut buffer = Vec::new();
                report
                    .write((filename, Source::from(source)), &mut buffer)
                    .map_err(|_| std::fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&buffer))
            }
            ParseError::InvalidIntLiteral {
                kind,
                value,
                location,
                filename,
                source,
            } => {
                let offset = Self::location_to_offset(*location, source);
                let report = Report::build(ReportKind::Error, filename, offset)
                    .with_message(format!("Invalid {} literal: {}", kind, value))
                    .with_label(
                        Label::new((filename, offset..offset + 1))
                            .with_message("invalid literal")
                            .with_color(Color::Red),
                    )
                    .finish();

                let mut buffer = Vec::new();
                report
                    .write((filename, Source::from(source)), &mut buffer)
                    .map_err(|_| std::fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&buffer))
            }
            ParseError::MissingPointerQualifier {
                location,
                filename,
                source,
            } => {
                let offset = Self::location_to_offset(*location, source);
                let report = Report::build(ReportKind::Error, filename, offset)
                    .with_message("Expected const or mut after *")
                    .with_label(
                        Label::new((filename, offset..offset + 1))
                            .with_message("expected 'const' or 'mut' here")
                            .with_color(Color::Red),
                    )
                    .finish();

                let mut buffer = Vec::new();
                report
                    .write((filename, Source::from(source)), &mut buffer)
                    .map_err(|_| std::fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&buffer))
            }
            ParseError::SuperNotSupported {
                location,
                filename,
                source,
            } => {
                let offset = Self::location_to_offset(*location, source);
                let report = Report::build(ReportKind::Error, filename, offset)
                    .with_message("super not supported")
                    .with_label(
                        Label::new((filename, offset..offset + 1))
                            .with_message("super keyword not supported")
                            .with_color(Color::Red),
                    )
                    .finish();

                let mut buffer = Vec::new();
                report
                    .write((filename, Source::from(source)), &mut buffer)
                    .map_err(|_| std::fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&buffer))
            }
            ParseError::UnexpectedModuleToken {
                found,
                location,
                filename,
                source,
            } => {
                let offset = Self::location_to_offset(*location, source);
                let report = Report::build(ReportKind::Error, filename, offset)
                    .with_message(format!("Unexpected token at module level: {:?}", found))
                    .with_label(
                        Label::new((filename, offset..offset + 1))
                            .with_message("unexpected token")
                            .with_color(Color::Red),
                    )
                    .finish();

                let mut buffer = Vec::new();
                report
                    .write((filename, Source::from(source)), &mut buffer)
                    .map_err(|_| std::fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&buffer))
            }
            ParseError::UnexpectedTokenAfterAttributes {
                found,
                location,
                filename,
                source,
            } => {
                let offset = Self::location_to_offset(*location, source);
                let report = Report::build(ReportKind::Error, filename, offset)
                    .with_message(format!("Unexpected token after attributes: {:?}", found))
                    .with_label(
                        Label::new((filename, offset..offset + 1))
                            .with_message("unexpected token after attributes")
                            .with_color(Color::Red),
                    )
                    .finish();

                let mut buffer = Vec::new();
                report
                    .write((filename, Source::from(source)), &mut buffer)
                    .map_err(|_| std::fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&buffer))
            }
            ParseError::ExpectedItemDefinition {
                found,
                location,
                filename,
                source,
            } => {
                let offset = Self::location_to_offset(*location, source);
                let report = Report::build(ReportKind::Error, filename, offset)
                    .with_message(format!(
                        "Expected type, enum, or bitflags, found {:?}",
                        found
                    ))
                    .with_label(
                        Label::new((filename, offset..offset + 1))
                            .with_message("expected type, enum, or bitflags here")
                            .with_color(Color::Red),
                    )
                    .finish();

                let mut buffer = Vec::new();
                report
                    .write((filename, Source::from(source)), &mut buffer)
                    .map_err(|_| std::fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&buffer))
            }
            ParseError::ExpectedBackendContent {
                found,
                location,
                filename,
                source,
            } => {
                let offset = Self::location_to_offset(*location, source);
                let report = Report::build(ReportKind::Error, filename, offset)
                    .with_message(format!(
                        "Expected LBrace, Prologue, or Epilogue, found {:?}",
                        found
                    ))
                    .with_label(
                        Label::new((filename, offset..offset + 1))
                            .with_message("expected backend content here")
                            .with_color(Color::Red),
                    )
                    .finish();

                let mut buffer = Vec::new();
                report
                    .write((filename, Source::from(source)), &mut buffer)
                    .map_err(|_| std::fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&buffer))
            }
            ParseError::ExpectedPrologueOrEpilogue {
                found,
                location,
                filename,
                source,
            } => {
                let offset = Self::location_to_offset(*location, source);
                let report = Report::build(ReportKind::Error, filename, offset)
                    .with_message(format!("Expected prologue or epilogue, found {:?}", found))
                    .with_label(
                        Label::new((filename, offset..offset + 1))
                            .with_message("expected prologue or epilogue here")
                            .with_color(Color::Red),
                    )
                    .finish();

                let mut buffer = Vec::new();
                report
                    .write((filename, Source::from(source)), &mut buffer)
                    .map_err(|_| std::fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&buffer))
            }
            ParseError::Tokenizer(err) => write!(f, "{}", err),
        }
    }
}
impl std::error::Error for ParseError {}
impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        ParseError::Tokenizer(err)
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    pending_comments: Vec<Spanned<Comment>>,
    filename: String,
    source: String,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, filename: String, source: String) -> Self {
        Self {
            tokens,
            pos: 0,
            pending_comments: Vec::new(),
            filename,
            source,
        }
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
            Err(ParseError::ExpectedToken {
                expected: kind,
                found: self.peek().clone(),
                location: self.current().span.start,
                filename: self.filename.clone().into(),
                source: self.source.clone().into(),
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
            TokenKind::Unknown => {
                // "unknown" keyword can also be used as an identifier (e.g., field name)
                let token = self.advance();
                Ok((Ident("unknown".to_string()), token.span))
            }
            _ => Err(ParseError::ExpectedIdentifier {
                found: self.peek().clone(),
                location: self.current().span.start,
                filename: self.filename.clone().into(),
                source: self.source.clone().into(),
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
                Some(Spanned::new(Comment::DocOuter(vec![content]), token.span))
            }
            TokenKind::DocInner(ref text) => {
                let token = self.advance();
                let content = text.strip_prefix("//!").unwrap_or(text).trim().to_string();
                Some(Spanned::new(Comment::DocInner(vec![content]), token.span))
            }
            TokenKind::Comment(ref text) => {
                let token = self.advance();
                Some(Spanned::new(Comment::Regular(text.clone()), token.span))
            }
            TokenKind::MultiLineComment(ref text) => {
                let token = self.advance();
                // Split multiline comments into lines
                let lines: Vec<String> = text.lines().map(|s| s.to_string()).collect();
                Some(Spanned::new(Comment::MultiLine(lines), token.span))
            }
            _ => None,
        }
    }

    /// Skip over all comments and whitespace
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
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                if let Some(comment) = self.collect_comment() {
                    items.push(ModuleItem::Comment(comment));
                }
            }

            if matches!(self.peek(), TokenKind::Eof) {
                break;
            }

            // Parse module-level items
            items.push(self.parse_module_item()?);

            // Add any pending comments that were collected during parsing (e.g., inline comments after attributes)
            for comment in self.pending_comments.drain(..) {
                items.push(ModuleItem::Comment(comment));
            }

            // Collect any inline comments that appeared after the item
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                if let Some(comment) = self.collect_comment() {
                    items.push(ModuleItem::Comment(comment));
                }
            }
        }

        Ok(Module {
            items,
            attributes: Attributes::default(),
            doc_comments: module_doc_comments,
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

                // Skip over any comments (including doc comments) after attributes in lookahead
                while pos < self.tokens.len()
                    && matches!(
                        &self.tokens[pos].kind,
                        TokenKind::Comment(_)
                            | TokenKind::MultiLineComment(_)
                            | TokenKind::DocOuter(_)
                            | TokenKind::DocInner(_)
                    )
                {
                    pos += 1;
                }

                // Now check what comes after attributes (and comments)
                match &self.tokens[pos].kind {
                    TokenKind::Extern => {
                        // Could be extern type or extern value
                        if matches!(
                            self.tokens.get(pos + 1).map(|t| &t.kind),
                            Some(TokenKind::Type)
                        ) {
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
                            Some(TokenKind::Fn) => self.parse_function().map(ModuleItem::Function),
                            _ => self.parse_item_definition().map(ModuleItem::Definition),
                        }
                    }
                    TokenKind::Type | TokenKind::Enum | TokenKind::Bitflags => {
                        self.parse_item_definition().map(ModuleItem::Definition)
                    }
                    TokenKind::Impl => self.parse_impl_block().map(ModuleItem::Impl),
                    TokenKind::Fn => self.parse_function().map(ModuleItem::Function),
                    _ => Err(ParseError::UnexpectedTokenAfterAttributes {
                        found: self.tokens[pos].kind.clone(),
                        location: self.tokens[pos].span.start,
                        filename: self.filename.clone().into(),
                        source: self.source.clone().into(),
                    }),
                }
            }
            TokenKind::DocOuter(_)
            | TokenKind::Pub
            | TokenKind::Type
            | TokenKind::Enum
            | TokenKind::Bitflags => self.parse_item_definition().map(ModuleItem::Definition),
            TokenKind::Impl => self.parse_impl_block().map(ModuleItem::Impl),
            TokenKind::Fn => {
                // Freestanding function with attributes
                self.parse_function().map(ModuleItem::Function)
            }
            _ => Err(ParseError::UnexpectedModuleToken {
                found: self.peek().clone(),
                location: self.current().span.start,
                filename: self.filename.clone().into(),
                source: self.source.clone().into(),
            }),
        }
    }

    fn parse_use(&mut self) -> Result<ItemPath, ParseError> {
        self.expect(TokenKind::Use)?;

        // Check for super keyword (not supported yet)
        if let TokenKind::Ident(name) = self.peek()
            && name == "super"
        {
            return Err(ParseError::SuperNotSupported {
                location: self.current().span.start,
                filename: self.filename.clone().into(),
                source: self.source.clone().into(),
            });
        }

        let path = self.parse_item_path()?;
        self.expect(TokenKind::Semi)?;
        Ok(path)
    }

    fn parse_extern_type(&mut self) -> Result<ModuleItem, ParseError> {
        let mut doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        // Also collect doc comments that appear after attributes
        let after_attr_doc_comments = self.collect_doc_comments();
        doc_comments.extend(after_attr_doc_comments);

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
        let mut doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        // Also collect doc comments that appear after attributes
        let after_attr_doc_comments = self.collect_doc_comments();
        doc_comments.extend(after_attr_doc_comments);

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
                        return Err(ParseError::ExpectedPrologueOrEpilogue {
                            found: self.peek().clone(),
                            location: self.current().span.start,
                            filename: self.filename.clone().into(),
                            source: self.source.clone().into(),
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
                    return Err(ParseError::ExpectedBackendContent {
                        found: self.peek().clone(),
                        location: self.current().span.start,
                        filename: self.filename.clone().into(),
                        source: self.source.clone().into(),
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
        // Capture the start position
        let start_pos = self.current().span.start;

        let mut doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        // Remember the line where attributes ended (or where we currently are if no attributes)
        // We need to check this before we start collecting comments
        let attributes_end_line = if !attributes.0.is_empty() && self.pos > 0 {
            // Get the line from the previous token (the ] that closed the attributes)
            self.tokens[self.pos - 1].span.end.line
        } else {
            // No attributes, so comments can't be inline with them
            0 // Use 0 as a sentinel value that won't match any real line
        };

        // Collect comments after attributes, separating inline from following
        let mut inline_trailing_comments = Vec::new();
        let mut following_comments = Vec::new();
        while matches!(
            self.peek(),
            TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
        ) {
            let comment_line = self.current().span.start.line;
            if let Some(comment) = self.collect_comment() {
                if comment_line == attributes_end_line {
                    // Comment is on the same line as the attributes
                    inline_trailing_comments.push(comment);
                } else {
                    // Comment is on a following line
                    following_comments.push(comment);
                }
            }
        }

        // Also collect doc comments that appear after attributes
        let after_attr_doc_comments = self.collect_doc_comments();
        doc_comments.extend(after_attr_doc_comments);

        let visibility = self.parse_visibility()?;

        match self.peek() {
            TokenKind::Type => {
                self.advance();
                let (name, _) = self.expect_ident()?;
                let mut def = TypeDefinition {
                    items: Vec::new(),
                    attributes,
                    inline_trailing_comments: inline_trailing_comments.clone(),
                    following_comments: following_comments.clone(),
                };

                // Support both "type Name;" and "type Name { ... }"
                if matches!(self.peek(), TokenKind::Semi) {
                    self.advance(); // Consume semicolon
                } else {
                    self.expect(TokenKind::LBrace)?;
                    def.items = self.parse_type_def_items()?;
                    self.expect(TokenKind::RBrace)?;
                }

                // Capture the end position
                let end_pos = if self.pos > 0 {
                    self.tokens[self.pos - 1].span.end
                } else {
                    self.current().span.end
                };

                Ok(ItemDefinition {
                    visibility,
                    name,
                    doc_comments,
                    inner: ItemDefinitionInner::Type(def),
                    span: Span {
                        start: start_pos,
                        end: end_pos,
                        text: String::new(),
                    },
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

                // Capture the end position
                let end_pos = if self.pos > 0 {
                    self.tokens[self.pos - 1].span.end
                } else {
                    self.current().span.end
                };

                Ok(ItemDefinition {
                    visibility,
                    name,
                    doc_comments,
                    inner: ItemDefinitionInner::Enum(EnumDefinition {
                        type_,
                        items,
                        attributes,
                        inline_trailing_comments: inline_trailing_comments.clone(),
                        following_comments: following_comments.clone(),
                    }),
                    span: Span {
                        start: start_pos,
                        end: end_pos,
                        text: String::new(),
                    },
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

                // Capture the end position
                let end_pos = if self.pos > 0 {
                    self.tokens[self.pos - 1].span.end
                } else {
                    self.current().span.end
                };

                Ok(ItemDefinition {
                    visibility,
                    name,
                    doc_comments,
                    inner: ItemDefinitionInner::Bitflags(BitflagsDefinition {
                        type_,
                        items,
                        attributes,
                        inline_trailing_comments,
                        following_comments,
                    }),
                    span: Span {
                        start: start_pos,
                        end: end_pos,
                        text: String::new(),
                    },
                })
            }
            _ => Err(ParseError::ExpectedItemDefinition {
                found: self.peek().clone(),
                location: self.current().span.start,
                filename: self.filename.clone().into(),
                source: self.source.clone().into(),
            }),
        }
    }

    fn parse_type_def_items(&mut self) -> Result<Vec<TypeDefItem>, ParseError> {
        let mut items = Vec::new();

        while !matches!(self.peek(), TokenKind::RBrace) {
            // Collect non-doc comments (doc comments will be collected by parse_type_statement)
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                if let Some(comment) = self.collect_comment() {
                    items.push(TypeDefItem::Comment(comment));
                }
            }

            if matches!(self.peek(), TokenKind::RBrace) {
                break;
            }

            let mut stmt = self.parse_type_statement()?;
            let statement_line = self.current().span.end.line;

            // Optional trailing comma
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            }

            // Collect trailing comments after the comma, separating inline from following
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                let comment_line = self.current().span.start.line;
                if let Some(comment) = self.collect_comment() {
                    if comment_line == statement_line {
                        // Comment is on the same line as the field
                        stmt.inline_trailing_comments.push(comment);
                    } else {
                        // Comment is on a following line
                        stmt.following_comments.push(comment);
                    }
                }
            }

            items.push(TypeDefItem::Statement(stmt));
        }

        Ok(items)
    }

    fn parse_type_statement(&mut self) -> Result<TypeStatement, ParseError> {
        let start_pos = self.current().span.start;

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

            let end_pos = if self.pos > 0 {
                self.tokens[self.pos - 1].span.end
            } else {
                self.current().span.end
            };

            Ok(TypeStatement {
                field: TypeField::Vftable(functions),
                attributes,
                doc_comments,
                inline_trailing_comments: Vec::new(), // Will be populated by parse_type_def_items
                following_comments: Vec::new(),
                span: Span {
                    start: start_pos,
                    end: end_pos,
                    text: String::new(),
                },
            })
        } else {
            let visibility = self.parse_visibility()?;
            let (name, _) = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let type_ = self.parse_type()?;

            let end_pos = if self.pos > 0 {
                self.tokens[self.pos - 1].span.end
            } else {
                self.current().span.end
            };

            Ok(TypeStatement {
                field: TypeField::Field(visibility, name, type_),
                attributes,
                doc_comments,
                inline_trailing_comments: Vec::new(), // Will be populated by parse_type_def_items
                following_comments: Vec::new(),
                span: Span {
                    start: start_pos,
                    end: end_pos,
                    text: String::new(),
                },
            })
        }
    }

    fn parse_enum_def_items(&mut self) -> Result<Vec<EnumDefItem>, ParseError> {
        let mut items = Vec::new();

        while !matches!(self.peek(), TokenKind::RBrace) {
            // Collect non-doc comments (doc comments will be collected by parse_enum_statement)
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                if let Some(comment) = self.collect_comment() {
                    items.push(EnumDefItem::Comment(comment));
                }
            }

            if matches!(self.peek(), TokenKind::RBrace) {
                break;
            }

            let mut stmt = self.parse_enum_statement()?;
            let statement_line = self.current().span.end.line;

            // Optional trailing comma
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            }

            // Collect trailing comments after the comma, separating inline from following
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                let comment_line = self.current().span.start.line;
                if let Some(comment) = self.collect_comment() {
                    if comment_line == statement_line {
                        // Comment is on the same line as the enum variant
                        stmt.inline_trailing_comments.push(comment);
                    } else {
                        // Comment is on a following line
                        stmt.following_comments.push(comment);
                    }
                }
            }

            items.push(EnumDefItem::Statement(stmt));
        }

        Ok(items)
    }

    fn parse_enum_statement(&mut self) -> Result<EnumStatement, ParseError> {
        let start_pos = self.current().span.start;

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

        let end_pos = if self.pos > 0 {
            self.tokens[self.pos - 1].span.end
        } else {
            self.current().span.end
        };

        Ok(EnumStatement {
            name,
            expr,
            attributes,
            doc_comments,
            inline_trailing_comments: Vec::new(), // Will be populated by parse_enum_def_items
            following_comments: Vec::new(),
            span: Span {
                start: start_pos,
                end: end_pos,
                text: String::new(),
            },
        })
    }

    fn parse_bitflags_def_items(&mut self) -> Result<Vec<BitflagsDefItem>, ParseError> {
        let mut items = Vec::new();

        while !matches!(self.peek(), TokenKind::RBrace) {
            // Collect non-doc comments (doc comments will be collected by parse_bitflags_statement)
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                if let Some(comment) = self.collect_comment() {
                    items.push(BitflagsDefItem::Comment(comment));
                }
            }

            if matches!(self.peek(), TokenKind::RBrace) {
                break;
            }

            let mut stmt = self.parse_bitflags_statement()?;
            let statement_line = self.current().span.end.line;

            // Optional trailing comma
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            }

            // Collect trailing comments after the comma, separating inline from following
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                let comment_line = self.current().span.start.line;
                if let Some(comment) = self.collect_comment() {
                    if comment_line == statement_line {
                        // Comment is on the same line as the bitflag
                        stmt.inline_trailing_comments.push(comment);
                    } else {
                        // Comment is on a following line
                        stmt.following_comments.push(comment);
                    }
                }
            }

            items.push(BitflagsDefItem::Statement(stmt));
        }

        Ok(items)
    }

    fn parse_bitflags_statement(&mut self) -> Result<BitflagsStatement, ParseError> {
        let start_pos = self.current().span.start;

        let doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        let (name, _) = self.expect_ident()?;
        self.expect(TokenKind::Eq)?;
        let expr = self.parse_expr()?;

        let end_pos = if self.pos > 0 {
            self.tokens[self.pos - 1].span.end
        } else {
            self.current().span.end
        };

        Ok(BitflagsStatement {
            name,
            expr,
            attributes,
            doc_comments,
            inline_trailing_comments: Vec::new(), // Will be populated by parse_bitflags_def_items
            following_comments: Vec::new(),
            span: Span {
                start: start_pos,
                end: end_pos,
                text: String::new(),
            },
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
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
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
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
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
        let mut doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        // Also collect doc comments that appear after attributes
        let after_attr_doc_comments = self.collect_doc_comments();
        doc_comments.extend(after_attr_doc_comments);

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
            let mut items = Vec::new();
            while !matches!(self.peek(), TokenKind::RParen) {
                // Collect any comments before the expression
                while matches!(
                    self.peek(),
                    TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
                ) {
                    let token = self.advance();
                    let comment_text = match &token.kind {
                        TokenKind::Comment(text) => text.clone(),
                        TokenKind::MultiLineComment(text) => text.clone(),
                        _ => unreachable!(),
                    };
                    items.push(AttributeItem::Comment(comment_text));
                }

                // Skip doc comments (they don't belong in attribute expressions)
                while matches!(self.peek(), TokenKind::DocOuter(_) | TokenKind::DocInner(_)) {
                    self.advance();
                }

                if matches!(self.peek(), TokenKind::RParen) {
                    break;
                }

                items.push(AttributeItem::Expr(self.parse_expr()?));

                // Collect trailing comments after the expression
                while matches!(
                    self.peek(),
                    TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
                ) {
                    let token = self.advance();
                    let comment_text = match &token.kind {
                        TokenKind::Comment(text) => text.clone(),
                        TokenKind::MultiLineComment(text) => text.clone(),
                        _ => unreachable!(),
                    };
                    items.push(AttributeItem::Comment(comment_text));
                }

                if matches!(self.peek(), TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
            self.expect(TokenKind::RParen)?;
            Ok(Attribute::Function(name, items))
        } else if matches!(self.peek(), TokenKind::Eq) {
            // Assign attribute
            self.advance();
            let mut items = Vec::new();

            // Collect comments before the expression
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                let token = self.advance();
                let comment_text = match &token.kind {
                    TokenKind::Comment(text) => text.clone(),
                    TokenKind::MultiLineComment(text) => text.clone(),
                    _ => unreachable!(),
                };
                items.push(AttributeItem::Comment(comment_text));
            }

            items.push(AttributeItem::Expr(self.parse_expr()?));

            // Collect comments after the expression
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                let token = self.advance();
                let comment_text = match &token.kind {
                    TokenKind::Comment(text) => text.clone(),
                    TokenKind::MultiLineComment(text) => text.clone(),
                    _ => unreachable!(),
                };
                items.push(AttributeItem::Comment(comment_text));
            }

            Ok(Attribute::Assign(name, items))
        } else {
            // Ident attribute
            Ok(Attribute::Ident(name))
        }
    }

    fn parse_item_path(&mut self) -> Result<ItemPath, ParseError> {
        let mut segments = Vec::new();

        while let TokenKind::Ident(name) = self.peek() {
            segments.push(ItemPathSegment::from(name.clone()));
            self.advance();

            // Handle generics in the path
            if matches!(self.peek(), TokenKind::Lt) {
                // Parse generic arguments as part of the segment
                let generic_str = self.parse_generic_args_as_string()?;
                let last = segments.last_mut().unwrap();
                *last = ItemPathSegment::from(format!("{}{}", last.as_str(), generic_str));
            }

            if !matches!(self.peek(), TokenKind::ColonColon) {
                break;
            }
            self.advance();
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
                    Err(ParseError::MissingPointerQualifier {
                        location: self.current().span.start,
                        filename: self.filename.clone().into(),
                        source: self.source.clone().into(),
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
            _ => Err(ParseError::ExpectedType {
                found: self.peek().clone(),
                location: self.current().span.start,
                filename: self.filename.clone().into(),
                source: self.source.clone().into(),
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
            _ => Err(ParseError::ExpectedExpression {
                found: self.peek().clone(),
                location: self.current().span.start,
                filename: self.filename.clone().into(),
                source: self.source.clone().into(),
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
                            .map_err(|_| ParseError::InvalidIntLiteral {
                                kind: "hex".to_string(),
                                value: s.clone(),
                                location: token.span.start,
                                filename: self.filename.clone().into(),
                                source: self.source.clone().into(),
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
                            .map_err(|_| ParseError::InvalidIntLiteral {
                                kind: "binary".to_string(),
                                value: s.clone(),
                                location: token.span.start,
                                filename: self.filename.clone().into(),
                                source: self.source.clone().into(),
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
                            .map_err(|_| ParseError::InvalidIntLiteral {
                                kind: "octal".to_string(),
                                value: s.clone(),
                                location: token.span.start,
                                filename: self.filename.clone().into(),
                                source: self.source.clone().into(),
                            })
                    } else {
                        // Decimal
                        s.parse::<isize>()
                            .map_err(|_| ParseError::InvalidIntLiteral {
                                kind: "integer".to_string(),
                                value: s.clone(),
                                location: token.span.start,
                                filename: self.filename.clone().into(),
                                source: self.source.clone().into(),
                            })
                    }
                } else {
                    unreachable!()
                }
            }
            _ => Err(ParseError::ExpectedIntLiteral {
                found: self.peek().clone(),
                location: self.current().span.start,
                filename: self.filename.clone().into(),
                source: self.source.clone().into(),
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
            _ => Err(ParseError::ExpectedStringLiteral {
                found: self.peek().clone(),
                location: self.current().span.start,
                filename: self.filename.clone().into(),
                source: self.source.clone().into(),
            }),
        }
    }
}
