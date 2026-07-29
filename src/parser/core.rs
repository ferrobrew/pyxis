use crate::{
    grammar::{Comment, Ident},
    span::{FileId, ItemLocation, Location, Span},
    tokenizer::{Token, TokenKind},
};

use super::ParseError;

pub struct Parser {
    pub(crate) tokens: Vec<Token>,
    pub(crate) pos: usize,
    pub(crate) pending_comments: Vec<Comment>,
    pub(crate) file_id: FileId,
    pub(crate) source: String,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, file_id: FileId, source: String) -> Self {
        Self {
            tokens,
            pos: 0,
            pending_comments: Vec::new(),
            file_id,
            source,
        }
    }

    /// Extract text from a span using the source.
    ///
    /// Delegates to [`crate::span::span_to_offset`] and
    /// [`crate::span::span_length`], which are the single source of truth for
    /// byte-offset computation. See those functions for the rationale on
    /// using `split('\n')` rather than `lines()` (CRLF safety).
    pub(crate) fn span_text(&self, span: &Span) -> &str {
        let start = crate::span::span_to_offset(&self.source, span);
        let len = crate::span::span_length(&self.source, span);
        &self.source[start..(start + len).min(self.source.len())]
    }

    pub(crate) fn current(&self) -> &Token {
        &self.tokens[self.pos.min(self.tokens.len() - 1)]
    }

    pub(crate) fn peek(&self) -> &TokenKind {
        &self.current().kind
    }

    pub(crate) fn peek_nth(&self, n: usize) -> &TokenKind {
        let pos = (self.pos + n).min(self.tokens.len() - 1);
        &self.tokens[pos].kind
    }

    pub(crate) fn advance(&mut self) -> Token {
        let token = self.current().clone();
        if !matches!(token.kind, TokenKind::Eof) {
            self.pos += 1;
        }
        token
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&kind) {
            Ok(self.advance())
        } else {
            Err(ParseError::ExpectedToken {
                expected: vec![kind],
                found: self.peek().clone(),
                location: self.current().location,
            })
        }
    }

    pub(crate) fn expect_ident(&mut self) -> Result<(Ident, Span), ParseError> {
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
                location: self.current().location,
            }),
        }
    }

    /// Collect consecutive doc comments (///)
    pub(crate) fn collect_doc_comments(&mut self) -> Vec<String> {
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

    /// Collect a comment as a Comment (with inline location)
    pub(crate) fn collect_comment(&mut self) -> Option<Comment> {
        match self.peek().clone() {
            TokenKind::DocOuter(ref text) => {
                let token = self.advance();
                let content = text.strip_prefix("///").unwrap_or(text).trim().to_string();
                Some(Comment::DocOuter {
                    lines: vec![content],
                    location: token.location,
                })
            }
            TokenKind::DocInner(ref text) => {
                let token = self.advance();
                let content = text.strip_prefix("//!").unwrap_or(text).trim().to_string();
                Some(Comment::DocInner {
                    lines: vec![content],
                    location: token.location,
                })
            }
            TokenKind::Comment(ref text) => {
                let token = self.advance();
                Some(Comment::Regular {
                    text: text.clone(),
                    location: token.location,
                })
            }
            TokenKind::MultiLineComment(ref text) => {
                let token = self.advance();
                // Split multiline comments into lines
                let lines: Vec<String> = text.lines().map(|s| s.to_string()).collect();
                Some(Comment::MultiLine {
                    lines,
                    location: token.location,
                })
            }
            _ => None,
        }
    }

    pub(crate) fn item_location_from_span(&self, span: Span) -> ItemLocation {
        ItemLocation::new(self.file_id, span)
    }

    pub(crate) fn item_location_from_locations(
        &self,
        start: Location,
        end: Location,
    ) -> ItemLocation {
        self.item_location_from_span(Span::new(start, end))
    }
}
