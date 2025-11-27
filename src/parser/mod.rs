use crate::{
    grammar::*,
    source_store::SourceStore,
    span::{ItemLocation, Located, Span},
    tokenizer::{LexError, Token, TokenKind},
};
use ariadne::{Color, Label, Report, ReportKind, Source};
use std::sync::Arc;

#[cfg(test)]
use crate::span::StripLocations;

#[cfg(test)]
mod tests;

#[cfg(test)]
/// Parse a Pyxis module from a string for tests, with the spans stripped out
pub fn parse_str_for_tests(input: &str) -> Result<Module, ParseError> {
    parse_str_with_filename(input, "<test>")
}

/// Parse a Pyxis module from a string with a specific filename for error reporting
pub fn parse_str_with_filename(input: &str, filename: &str) -> Result<Module, ParseError> {
    // First tokenize
    let tokens = crate::tokenizer::tokenize_with_filename(input.to_string(), filename.to_string())?;

    // Then parse
    let filename_arc: Arc<str> = filename.into();
    let mut parser = Parser::new(tokens, filename_arc, input.to_string());
    let module = parser.parse_module()?;

    Ok(module)
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    ExpectedToken {
        expected: TokenKind,
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedIdentifier {
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedType {
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedExpression {
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedIntLiteral {
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedStringLiteral {
        found: TokenKind,
        location: ItemLocation,
    },
    InvalidIntLiteral {
        kind: String,
        value: String,
        location: ItemLocation,
    },
    MissingPointerQualifier {
        location: ItemLocation,
    },
    SuperNotSupported {
        location: ItemLocation,
    },
    UnexpectedModuleToken {
        found: TokenKind,
        location: ItemLocation,
    },
    UnexpectedTokenAfterAttributes {
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedItemDefinition {
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedBackendContent {
        found: TokenKind,
        location: ItemLocation,
    },
    ExpectedPrologueOrEpilogue {
        found: TokenKind,
        location: ItemLocation,
    },
    Tokenizer(LexError),
}
impl ParseError {
    /// Helper to format ariadne report without source code (creates report with location note)
    fn format_ariadne_without_source(location: &ItemLocation, message: &str) -> String {
        let placeholder_filename = location.filename.as_ref();
        let report = Report::<(&str, std::ops::Range<usize>)>::build(
            ReportKind::Error,
            placeholder_filename,
            0,
        )
        .with_message(message)
        .with_note(format!(
            "Error location: {}:{}:{}",
            location.filename, location.span.start.line, location.span.start.column
        ))
        .finish();

        let mut buffer = Vec::new();
        if report
            .write((placeholder_filename, Source::from("")), &mut buffer)
            .is_ok()
        {
            return String::from_utf8_lossy(&buffer).to_string();
        }
        message.to_string()
    }

    /// Helper to build ariadne report with source
    fn format_ariadne_with_source(
        location: &ItemLocation,
        source: &str,
        message: &str,
        label_message: &str,
    ) -> String {
        let offset = crate::span::span_to_offset(source, &location.span);
        let report = Report::build(ReportKind::Error, location.filename.as_ref(), offset)
            .with_message(message)
            .with_label(
                Label::new((location.filename.as_ref(), offset..offset + 1))
                    .with_message(label_message)
                    .with_color(Color::Red),
            )
            .finish();

        let mut buffer = Vec::new();
        if report
            .write(
                (location.filename.as_ref(), Source::from(source)),
                &mut buffer,
            )
            .is_ok()
        {
            return String::from_utf8_lossy(&buffer).to_string();
        }
        message.to_string()
    }

    /// Helper to format ariadne report with optional source code
    fn format_ariadne_with_optional_source(
        location: &ItemLocation,
        source_store: &mut dyn SourceStore,
        message: &str,
        label_message: &str,
    ) -> String {
        if let Some(source) = source_store.get(location.filename.as_ref()) {
            Self::format_ariadne_with_source(location, source, message, label_message)
        } else {
            Self::format_ariadne_without_source(location, message)
        }
    }

    /// Format error with ariadne using a source store. Always produces ariadne-formatted output.
    pub fn format_with_ariadne(&self, source_store: &mut dyn SourceStore) -> String {
        match self {
            ParseError::ExpectedToken {
                expected,
                found,
                location,
            } => {
                let message = format!("Expected {expected:?}, found {found:?}");
                let label = format!("expected {expected:?} here");
                if let Some(source) = source_store.get(location.filename.as_ref()) {
                    Self::format_ariadne_with_source(location, source, &message, &label)
                } else {
                    Self::format_ariadne_without_source(location, &message)
                }
            }
            ParseError::ExpectedIdentifier { found, location } => {
                let message = format!("Expected identifier, found {found:?}");
                Self::format_ariadne_with_optional_source(
                    location,
                    source_store,
                    &message,
                    "expected identifier here",
                )
            }
            ParseError::ExpectedType { found, location } => {
                let message = format!("Expected type, found {found:?}");
                Self::format_ariadne_with_optional_source(
                    location,
                    source_store,
                    &message,
                    "expected type here",
                )
            }
            ParseError::ExpectedExpression { found, location } => {
                let message = format!("Expected expression, found {found:?}");
                Self::format_ariadne_with_optional_source(
                    location,
                    source_store,
                    &message,
                    "expected expression here",
                )
            }
            ParseError::ExpectedIntLiteral { found, location } => {
                let message = format!("Expected integer literal, found {found:?}");
                Self::format_ariadne_with_optional_source(
                    location,
                    source_store,
                    &message,
                    "expected integer literal here",
                )
            }
            ParseError::ExpectedStringLiteral { found, location } => {
                let message = format!("Expected string literal, found {found:?}");
                Self::format_ariadne_with_optional_source(
                    location,
                    source_store,
                    &message,
                    "expected string literal here",
                )
            }
            ParseError::InvalidIntLiteral {
                kind,
                value,
                location,
            } => {
                let message = format!("Invalid {kind} literal: {value}");
                Self::format_ariadne_with_optional_source(
                    location,
                    source_store,
                    &message,
                    "invalid literal",
                )
            }
            ParseError::MissingPointerQualifier { location } => {
                Self::format_ariadne_with_optional_source(
                    location,
                    source_store,
                    "Expected const or mut after *",
                    "expected 'const' or 'mut' here",
                )
            }
            ParseError::SuperNotSupported { location } => {
                Self::format_ariadne_with_optional_source(
                    location,
                    source_store,
                    "super not supported",
                    "super keyword not supported",
                )
            }
            ParseError::UnexpectedModuleToken { found, location } => {
                let message = format!("Unexpected token at module level: {found:?}");
                Self::format_ariadne_with_optional_source(
                    location,
                    source_store,
                    &message,
                    "unexpected token",
                )
            }
            ParseError::UnexpectedTokenAfterAttributes { found, location } => {
                let message = format!("Unexpected token after attributes: {found:?}");
                Self::format_ariadne_with_optional_source(
                    location,
                    source_store,
                    &message,
                    "unexpected token after attributes",
                )
            }
            ParseError::ExpectedItemDefinition { found, location } => {
                let message = format!("Expected type, enum, or bitflags, found {found:?}");
                Self::format_ariadne_with_optional_source(
                    location,
                    source_store,
                    &message,
                    "expected type, enum, or bitflags here",
                )
            }
            ParseError::ExpectedBackendContent { found, location } => {
                let message = format!("Expected LBrace, Prologue, or Epilogue, found {found:?}");
                Self::format_ariadne_with_optional_source(
                    location,
                    source_store,
                    &message,
                    "expected backend content here",
                )
            }
            ParseError::ExpectedPrologueOrEpilogue { found, location } => {
                let message = format!("Expected prologue or epilogue, found {found:?}");
                Self::format_ariadne_with_optional_source(
                    location,
                    source_store,
                    &message,
                    "expected prologue or epilogue here",
                )
            }
            ParseError::Tokenizer(err) => err.format_with_ariadne(source_store),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::ExpectedToken {
                expected, found, ..
            } => write!(f, "Expected {expected:?}, found {found:?}"),
            ParseError::ExpectedIdentifier { found, .. } => {
                write!(f, "Expected identifier, found {found:?}")
            }
            ParseError::ExpectedType { found, .. } => {
                write!(f, "Expected type, found {found:?}")
            }
            ParseError::ExpectedExpression { found, .. } => {
                write!(f, "Expected expression, found {found:?}")
            }
            ParseError::ExpectedIntLiteral { found, .. } => {
                write!(f, "Expected integer literal, found {found:?}")
            }
            ParseError::ExpectedStringLiteral { found, .. } => {
                write!(f, "Expected string literal, found {found:?}")
            }
            ParseError::InvalidIntLiteral { kind, value, .. } => {
                write!(f, "Invalid {kind} literal: {value}")
            }
            ParseError::MissingPointerQualifier { .. } => {
                write!(f, "Expected const or mut after *")
            }
            ParseError::SuperNotSupported { .. } => write!(f, "super not supported"),
            ParseError::UnexpectedModuleToken { found, .. } => {
                write!(f, "Unexpected token at module level: {found:?}")
            }
            ParseError::UnexpectedTokenAfterAttributes { found, .. } => {
                write!(f, "Unexpected token after attributes: {found:?}")
            }
            ParseError::ExpectedItemDefinition { found, .. } => {
                write!(f, "Expected type, enum, or bitflags, found {found:?}")
            }
            ParseError::ExpectedBackendContent { found, .. } => {
                write!(f, "Expected LBrace, Prologue, or Epilogue, found {found:?}")
            }
            ParseError::ExpectedPrologueOrEpilogue { found, .. } => {
                write!(f, "Expected prologue or epilogue, found {found:?}")
            }
            ParseError::Tokenizer(err) => write!(f, "{err}"),
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
    pending_comments: Vec<Located<Comment>>,
    filename: Arc<str>,
    source: String,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, filename: impl Into<Arc<str>>, source: String) -> Self {
        Self {
            tokens,
            pos: 0,
            pending_comments: Vec::new(),
            filename: filename.into(),
            source,
        }
    }

    /// Extract text from a span using the source
    fn span_text(&self, span: &Span) -> &str {
        let start_offset = self
            .source
            .lines()
            .take(span.start.line.saturating_sub(1))
            .map(|line| line.len() + 1)
            .sum::<usize>()
            + span.start.column.saturating_sub(1);

        let end_offset = if span.start.line == span.end.line {
            start_offset + (span.end.column.saturating_sub(span.start.column))
        } else {
            let first_line_len = self
                .source
                .lines()
                .nth(span.start.line.saturating_sub(1))
                .map(|line| line.len())
                .unwrap_or(0)
                .saturating_sub(span.start.column.saturating_sub(1));
            let middle_lines_len: usize = self
                .source
                .lines()
                .skip(span.start.line)
                .take(
                    span.end
                        .line
                        .saturating_sub(span.start.line)
                        .saturating_sub(1),
                )
                .map(|line| line.len() + 1)
                .sum();
            let last_line_len = span.end.column.saturating_sub(1);
            start_offset + first_line_len + middle_lines_len + last_line_len
        };

        &self.source[start_offset..end_offset.min(self.source.len())]
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
                location: self.current().location.clone(),
            })
        }
    }

    fn expect_ident(&mut self) -> Result<(Ident, Span), ParseError> {
        match self.peek() {
            TokenKind::Ident(_) => {
                let token = self.advance();
                if let TokenKind::Ident(name) = token.kind {
                    Ok((Ident(name), token.location.span))
                } else {
                    unreachable!()
                }
            }
            TokenKind::Underscore => {
                let token = self.advance();
                Ok((Ident("_".to_string()), token.location.span))
            }
            TokenKind::Unknown => {
                // "unknown" keyword can also be used as an identifier (e.g., field name)
                let token = self.advance();
                Ok((Ident("unknown".to_string()), token.location.span))
            }
            _ => Err(ParseError::ExpectedIdentifier {
                found: self.peek().clone(),
                location: self.current().location.clone(),
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

    /// Collect a comment as a Located<Comment>
    fn collect_comment(&mut self) -> Option<Located<Comment>> {
        match self.peek().clone() {
            TokenKind::DocOuter(ref text) => {
                let token = self.advance();
                let content = text.strip_prefix("///").unwrap_or(text).trim().to_string();
                Some(Located::new(
                    Comment::DocOuter(vec![content]),
                    token.location,
                ))
            }
            TokenKind::DocInner(ref text) => {
                let token = self.advance();
                let content = text.strip_prefix("//!").unwrap_or(text).trim().to_string();
                Some(Located::new(
                    Comment::DocInner(vec![content]),
                    token.location,
                ))
            }
            TokenKind::Comment(ref text) => {
                let token = self.advance();
                Some(Located::new(Comment::Regular(text.clone()), token.location))
            }
            TokenKind::MultiLineComment(ref text) => {
                let token = self.advance();
                // Split multiline comments into lines
                let lines: Vec<String> = text.lines().map(|s| s.to_string()).collect();
                Some(Located::new(Comment::MultiLine(lines), token.location))
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
                        location: self.tokens[pos].location.clone(),
                    }),
                }
            }
            TokenKind::DocOuter(_) => {
                // Peek ahead to see what comes after doc comments
                let mut pos = self.pos;
                while matches!(
                    self.tokens.get(pos).map(|t| &t.kind),
                    Some(TokenKind::DocOuter(_))
                ) {
                    pos += 1;
                }

                // Check if this is an extern type
                if matches!(self.tokens.get(pos).map(|t| &t.kind), Some(TokenKind::Hash)) {
                    // Skip attributes
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
                }

                // Now check what comes after doc comments (and possibly attributes)
                if matches!(
                    self.tokens.get(pos).map(|t| &t.kind),
                    Some(TokenKind::Extern)
                ) && matches!(
                    self.tokens.get(pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Type)
                ) {
                    self.parse_extern_type()
                } else {
                    self.parse_item_definition().map(ModuleItem::Definition)
                }
            }
            TokenKind::Pub | TokenKind::Type | TokenKind::Enum | TokenKind::Bitflags => {
                self.parse_item_definition().map(ModuleItem::Definition)
            }
            TokenKind::Impl => self.parse_impl_block().map(ModuleItem::Impl),
            TokenKind::Fn => {
                // Freestanding function with attributes
                self.parse_function().map(ModuleItem::Function)
            }
            _ => Err(ParseError::UnexpectedModuleToken {
                found: self.peek().clone(),
                location: self.current().location.clone(),
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
                location: self.current().location.clone(),
            });
        }

        let path = self.parse_item_path()?;
        self.expect(TokenKind::Semi)?;
        Ok(path)
    }

    fn parse_extern_type(&mut self) -> Result<ModuleItem, ParseError> {
        let start_pos = self.current().location.span.start;
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
        let end_pos = if self.pos > 0 {
            self.tokens[self.pos - 1].location.span.end
        } else {
            self.current().location.span.end
        };
        let location = ItemLocation::new(self.filename.clone(), Span::new(start_pos, end_pos));
        Ok(ModuleItem::ExternType(
            name,
            attributes,
            doc_comments,
            location,
        ))
    }

    fn parse_extern_value(&mut self) -> Result<ExternValue, ParseError> {
        let start_pos = self.current().location.span.start;
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
        let end_pos = if self.pos > 0 {
            self.tokens[self.pos - 1].location.span.end
        } else {
            self.current().location.span.end
        };
        let location = ItemLocation::new(self.filename.clone(), Span::new(start_pos, end_pos));

        Ok(ExternValue {
            visibility,
            name,
            type_,
            attributes,
            doc_comments,
            location,
        })
    }

    fn parse_backend(&mut self) -> Result<Backend, ParseError> {
        self.expect(TokenKind::Backend)?;
        let (name, _) = self.expect_ident()?;

        let mut prologue = None;
        let mut prologue_format = StringFormat::Regular;
        let mut epilogue = None;
        let mut epilogue_format = StringFormat::Regular;

        // Check if we have braces or direct prologue/epilogue
        if matches!(self.peek(), TokenKind::LBrace) {
            // Form: backend name { prologue ...; epilogue ...; }
            self.advance(); // consume {

            while !matches!(self.peek(), TokenKind::RBrace) {
                match self.peek() {
                    TokenKind::Prologue => {
                        self.advance();
                        let expr = self.parse_expr()?;
                        if let Expr::StringLiteral { value, format } = expr {
                            prologue = Some(value);
                            prologue_format = format;
                        } else {
                            return Err(ParseError::ExpectedStringLiteral {
                                found: self.peek().clone(),
                                location: self.current().location.clone(),
                            });
                        }
                        self.expect(TokenKind::Semi)?;
                    }
                    TokenKind::Epilogue => {
                        self.advance();
                        let expr = self.parse_expr()?;
                        if let Expr::StringLiteral { value, format } = expr {
                            epilogue = Some(value);
                            epilogue_format = format;
                        } else {
                            return Err(ParseError::ExpectedStringLiteral {
                                found: self.peek().clone(),
                                location: self.current().location.clone(),
                            });
                        }
                        self.expect(TokenKind::Semi)?;
                    }
                    _ => {
                        return Err(ParseError::ExpectedPrologueOrEpilogue {
                            found: self.peek().clone(),
                            location: self.current().location.clone(),
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
                    let expr = self.parse_expr()?;
                    if let Expr::StringLiteral { value, format } = expr {
                        prologue = Some(value);
                        prologue_format = format;
                    } else {
                        return Err(ParseError::ExpectedStringLiteral {
                            found: self.peek().clone(),
                            location: self.current().location.clone(),
                        });
                    }
                    self.expect(TokenKind::Semi)?;
                }
                TokenKind::Epilogue => {
                    self.advance();
                    let expr = self.parse_expr()?;
                    if let Expr::StringLiteral { value, format } = expr {
                        epilogue = Some(value);
                        epilogue_format = format;
                    } else {
                        return Err(ParseError::ExpectedStringLiteral {
                            found: self.peek().clone(),
                            location: self.current().location.clone(),
                        });
                    }
                    self.expect(TokenKind::Semi)?;
                }
                _ => {
                    return Err(ParseError::ExpectedBackendContent {
                        found: self.peek().clone(),
                        location: self.current().location.clone(),
                    });
                }
            }
        }

        Ok(Backend {
            name,
            prologue,
            prologue_format,
            epilogue,
            epilogue_format,
        })
    }

    fn parse_item_definition(&mut self) -> Result<ItemDefinition, ParseError> {
        // Capture the start position
        let start_pos = self.current().location.span.start;

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
            self.tokens[self.pos - 1].location.span.end.line
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
            let comment_line = self.current().location.span.start.line;
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
                    self.tokens[self.pos - 1].location.span.end
                } else {
                    self.current().location.span.end
                };

                Ok(ItemDefinition {
                    visibility,
                    name,
                    doc_comments,
                    inner: ItemDefinitionInner::Type(def),
                    location: ItemLocation::new(
                        self.filename.clone(),
                        Span::new(start_pos, end_pos),
                    ),
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
                    self.tokens[self.pos - 1].location.span.end
                } else {
                    self.current().location.span.end
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
                    location: ItemLocation::new(
                        self.filename.clone(),
                        Span::new(start_pos, end_pos),
                    ),
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
                    self.tokens[self.pos - 1].location.span.end
                } else {
                    self.current().location.span.end
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
                    location: ItemLocation::new(
                        self.filename.clone(),
                        Span::new(start_pos, end_pos),
                    ),
                })
            }
            _ => Err(ParseError::ExpectedItemDefinition {
                found: self.peek().clone(),
                location: self.current().location.clone(),
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
            let statement_line = self.current().location.span.end.line;

            // Optional trailing comma
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            }

            // Collect trailing comments after the comma, separating inline from following
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                let comment_line = self.current().location.span.start.line;
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
        let start_pos = self.current().location.span.start;

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
                self.tokens[self.pos - 1].location.span.end
            } else {
                self.current().location.span.end
            };

            Ok(TypeStatement {
                field: TypeField::Vftable(functions),
                attributes,
                doc_comments,
                inline_trailing_comments: Vec::new(), // Will be populated by parse_type_def_items
                following_comments: Vec::new(),
                location: ItemLocation::new(self.filename.clone(), Span::new(start_pos, end_pos)),
            })
        } else {
            let visibility = self.parse_visibility()?;
            let (name, _) = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let type_ = self.parse_type()?;

            let end_pos = if self.pos > 0 {
                self.tokens[self.pos - 1].location.span.end
            } else {
                self.current().location.span.end
            };

            Ok(TypeStatement {
                field: TypeField::Field(visibility, name, type_),
                attributes,
                doc_comments,
                inline_trailing_comments: Vec::new(), // Will be populated by parse_type_def_items
                following_comments: Vec::new(),
                location: ItemLocation::new(self.filename.clone(), Span::new(start_pos, end_pos)),
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
            let statement_line = self.current().location.span.end.line;

            // Optional trailing comma
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            }

            // Collect trailing comments after the comma, separating inline from following
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                let comment_line = self.current().location.span.start.line;
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
        let start_pos = self.current().location.span.start;

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
            self.tokens[self.pos - 1].location.span.end
        } else {
            self.current().location.span.end
        };

        Ok(EnumStatement {
            name,
            expr,
            attributes,
            doc_comments,
            inline_trailing_comments: Vec::new(), // Will be populated by parse_enum_def_items
            following_comments: Vec::new(),
            location: ItemLocation::new(self.filename.clone(), Span::new(start_pos, end_pos)),
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
            let statement_line = self.current().location.span.end.line;

            // Optional trailing comma
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            }

            // Collect trailing comments after the comma, separating inline from following
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                let comment_line = self.current().location.span.start.line;
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
        let start_pos = self.current().location.span.start;

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
            self.tokens[self.pos - 1].location.span.end
        } else {
            self.current().location.span.end
        };

        Ok(BitflagsStatement {
            name,
            expr,
            attributes,
            doc_comments,
            inline_trailing_comments: Vec::new(), // Will be populated by parse_bitflags_def_items
            following_comments: Vec::new(),
            location: ItemLocation::new(self.filename.clone(), Span::new(start_pos, end_pos)),
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
        let start_pos = self.current().location.span.start;
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
        let end_pos = if self.pos > 0 {
            self.tokens[self.pos - 1].location.span.end
        } else {
            self.current().location.span.end
        };
        let location = ItemLocation::new(self.filename.clone(), Span::new(start_pos, end_pos));

        Ok(Function {
            visibility,
            name,
            attributes,
            doc_comments,
            arguments,
            return_type,
            location,
        })
    }

    fn parse_argument(&mut self) -> Result<Argument, ParseError> {
        if matches!(self.peek(), TokenKind::Amp) {
            self.advance();
            if matches!(self.peek(), TokenKind::Mut) {
                self.advance();
                let tok = self.expect(TokenKind::SelfValue)?;
                Ok(Argument::MutSelf(tok.location.clone()))
            } else {
                let tok = self.expect(TokenKind::SelfValue)?;
                Ok(Argument::ConstSelf(tok.location.clone()))
            }
        } else {
            let start_pos = self.current().location.span.start;
            let (name, _) = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let type_ = self.parse_type()?;
            let end_pos = self.current().location.span.end;
            let location = ItemLocation::new(self.filename.clone(), Span::new(start_pos, end_pos));

            Ok(Argument::Named(name, type_, location))
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
            result.push_str(self.span_text(&self.tokens[i].location.span));
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
                        location: self.current().location.clone(),
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
                location: self.current().location.clone(),
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            TokenKind::IntLiteral(_) => {
                let token = self.current().clone();
                let value = self.parse_int_literal()?;
                // Detect format from the original token text
                let text = self.span_text(&token.location.span);
                let format = if text.starts_with("0x") || text.starts_with("-0x") {
                    IntFormat::Hex
                } else if text.starts_with("0b") || text.starts_with("-0b") {
                    IntFormat::Binary
                } else if text.starts_with("0o") || text.starts_with("-0o") {
                    IntFormat::Octal
                } else {
                    IntFormat::Decimal
                };
                Ok(Expr::IntLiteral { value, format })
            }
            TokenKind::StringLiteral(_) => {
                let token = self.current().clone();
                let value = self.parse_string_literal()?;
                // Detect if this was a raw string from the original token text
                let text = self.span_text(&token.location.span);
                let format = if text.starts_with('r') {
                    StringFormat::Raw
                } else {
                    StringFormat::Regular
                };
                Ok(Expr::StringLiteral { value, format })
            }
            TokenKind::Ident(_) => {
                let (ident, _) = self.expect_ident()?;
                Ok(Expr::Ident(ident))
            }
            _ => Err(ParseError::ExpectedExpression {
                found: self.peek().clone(),
                location: self.current().location.clone(),
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
                                location: token.location.clone(),
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
                                location: token.location.clone(),
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
                                location: token.location.clone(),
                            })
                    } else {
                        // Decimal
                        s.parse::<isize>()
                            .map_err(|_| ParseError::InvalidIntLiteral {
                                kind: "integer".to_string(),
                                value: s.clone(),
                                location: token.location.clone(),
                            })
                    }
                } else {
                    unreachable!()
                }
            }
            _ => Err(ParseError::ExpectedIntLiteral {
                found: self.peek().clone(),
                location: self.current().location.clone(),
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
                location: self.current().location.clone(),
            }),
        }
    }
}

#[cfg(test)]
impl StripLocations for Module {
    fn strip_locations(&self) -> Self {
        Module {
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    ModuleItem::Comment(_) => None, // Filter out comments
                    _ => Some(item.strip_locations()),
                })
                .collect(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.clone(),
        }
    }
}

#[cfg(test)]
impl StripLocations for ModuleItem {
    fn strip_locations(&self) -> Self {
        match self {
            ModuleItem::Comment(c) => ModuleItem::Comment(c.strip_locations()),
            ModuleItem::Use(p) => ModuleItem::Use(p.clone()),
            ModuleItem::ExternType(n, a, d, _) => ModuleItem::ExternType(
                n.clone(),
                a.strip_locations(),
                d.clone(),
                ItemLocation::test(),
            ),
            ModuleItem::Backend(b) => ModuleItem::Backend(b.clone()),
            ModuleItem::Definition(d) => ModuleItem::Definition(d.strip_locations()),
            ModuleItem::Impl(i) => ModuleItem::Impl(i.strip_locations()),
            ModuleItem::ExternValue(e) => ModuleItem::ExternValue(e.strip_locations()),
            ModuleItem::Function(f) => ModuleItem::Function(f.strip_locations()),
        }
    }
}

#[cfg(test)]
impl StripLocations for ItemDefinition {
    fn strip_locations(&self) -> Self {
        ItemDefinition {
            visibility: self.visibility,
            name: self.name.clone(),
            doc_comments: self.doc_comments.clone(),
            inner: self.inner.strip_locations(),
            location: ItemLocation::test(),
        }
    }
}

#[cfg(test)]
impl StripLocations for ItemDefinitionInner {
    fn strip_locations(&self) -> Self {
        match self {
            ItemDefinitionInner::Type(t) => ItemDefinitionInner::Type(t.strip_locations()),
            ItemDefinitionInner::Enum(e) => ItemDefinitionInner::Enum(e.strip_locations()),
            ItemDefinitionInner::Bitflags(b) => ItemDefinitionInner::Bitflags(b.strip_locations()),
        }
    }
}

#[cfg(test)]
impl StripLocations for TypeDefinition {
    fn strip_locations(&self) -> Self {
        TypeDefinition {
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    TypeDefItem::Comment(_) => None, // Filter out comments
                    TypeDefItem::Statement(s) => Some(TypeDefItem::Statement(s.strip_locations())),
                })
                .collect(),
            attributes: self.attributes.strip_locations(),
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
        }
    }
}

#[cfg(test)]
impl StripLocations for TypeStatement {
    fn strip_locations(&self) -> Self {
        TypeStatement {
            field: self.field.strip_locations(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.clone(),
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
            location: ItemLocation::test(),
        }
    }
}

#[cfg(test)]
impl StripLocations for TypeField {
    fn strip_locations(&self) -> Self {
        match self {
            TypeField::Field(v, n, t) => TypeField::Field(*v, n.clone(), t.clone()),
            TypeField::Vftable(funcs) => {
                TypeField::Vftable(funcs.iter().map(|f| f.strip_locations()).collect())
            }
        }
    }
}

#[cfg(test)]
impl StripLocations for EnumDefinition {
    fn strip_locations(&self) -> Self {
        EnumDefinition {
            type_: self.type_.clone(),
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    EnumDefItem::Comment(_) => None, // Filter out comments
                    EnumDefItem::Statement(s) => Some(EnumDefItem::Statement(s.strip_locations())),
                })
                .collect(),
            attributes: self.attributes.strip_locations(),
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
        }
    }
}

#[cfg(test)]
impl StripLocations for Expr {
    fn strip_locations(&self) -> Self {
        // Expr already doesn't contain spans, just clone it
        self.clone()
    }
}

#[cfg(test)]
impl StripLocations for EnumStatement {
    fn strip_locations(&self) -> Self {
        EnumStatement {
            name: self.name.clone(),
            expr: self.expr.as_ref().map(|e| e.strip_locations()),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.clone(),
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
            location: ItemLocation::test(),
        }
    }
}

#[cfg(test)]
impl StripLocations for BitflagsDefinition {
    fn strip_locations(&self) -> Self {
        BitflagsDefinition {
            type_: self.type_.clone(),
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    BitflagsDefItem::Comment(_) => None, // Filter out comments
                    BitflagsDefItem::Statement(s) => {
                        Some(BitflagsDefItem::Statement(s.strip_locations()))
                    }
                })
                .collect(),
            attributes: self.attributes.strip_locations(),
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
        }
    }
}

#[cfg(test)]
impl StripLocations for BitflagsStatement {
    fn strip_locations(&self) -> Self {
        BitflagsStatement {
            name: self.name.clone(),
            expr: self.expr.strip_locations(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.clone(),
            inline_trailing_comments: Vec::new(), // Strip trailing comments
            following_comments: Vec::new(),
            location: ItemLocation::test(),
        }
    }
}

#[cfg(test)]
impl StripLocations for FunctionBlock {
    fn strip_locations(&self) -> Self {
        FunctionBlock {
            name: self.name.clone(),
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    ImplItem::Comment(_) => None, // Filter out comments
                    ImplItem::Function(f) => Some(ImplItem::Function(f.strip_locations())),
                })
                .collect(),
            attributes: self.attributes.strip_locations(),
        }
    }
}

#[cfg(test)]
impl StripLocations for Function {
    fn strip_locations(&self) -> Self {
        Function {
            visibility: self.visibility,
            name: self.name.clone(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.clone(),
            arguments: self.arguments.strip_locations(),
            return_type: self.return_type.clone(),
            location: ItemLocation::test(),
        }
    }
}

#[cfg(test)]
impl StripLocations for Argument {
    fn strip_locations(&self) -> Self {
        match self {
            Argument::ConstSelf(_) => Argument::ConstSelf(ItemLocation::test()),
            Argument::MutSelf(_) => Argument::MutSelf(ItemLocation::test()),
            Argument::Named(ident, ty, _) => {
                Argument::Named(ident.clone(), ty.clone(), ItemLocation::test())
            }
        }
    }
}

#[cfg(test)]
impl StripLocations for ExternValue {
    fn strip_locations(&self) -> Self {
        ExternValue {
            visibility: self.visibility,
            name: self.name.clone(),
            type_: self.type_.clone(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.clone(),
            location: ItemLocation::test(),
        }
    }
}

#[cfg(test)]
impl StripLocations for Attribute {
    fn strip_locations(&self) -> Self {
        match self {
            Attribute::Ident(i) => Attribute::Ident(i.clone()),
            Attribute::Function(name, items) => Attribute::Function(
                name.clone(),
                items
                    .iter()
                    .map(|item| match item {
                        AttributeItem::Expr(e) => AttributeItem::Expr(e.strip_locations()),
                        AttributeItem::Comment(c) => AttributeItem::Comment(c.clone()),
                    })
                    .collect(),
            ),
            Attribute::Assign(name, items) => Attribute::Assign(
                name.clone(),
                items
                    .iter()
                    .map(|item| match item {
                        AttributeItem::Expr(e) => AttributeItem::Expr(e.strip_locations()),
                        AttributeItem::Comment(c) => AttributeItem::Comment(c.clone()),
                    })
                    .collect(),
            ),
        }
    }
}

#[cfg(test)]
impl StripLocations for Attributes {
    fn strip_locations(&self) -> Self {
        Attributes(self.0.iter().map(|a| a.strip_locations()).collect())
    }
}
