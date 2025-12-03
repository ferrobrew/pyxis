use crate::{
    grammar::{Comment, Ident},
    span::{HasLocation, ItemLocation, Located, Location, Span},
    tokenizer::{Token, TokenKind},
};
use std::sync::Arc;

use super::ParseError;

pub struct Parser {
    pub(crate) tokens: Vec<Token>,
    pub(crate) pos: usize,
    pub(crate) pending_comments: Vec<Located<Comment>>,
    pub(crate) filename: Arc<str>,
    pub(crate) source: String,
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
    pub(crate) fn span_text(&self, span: &Span) -> &str {
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
                expected: kind,
                found: self.peek().clone(),
                location: self.current().location.clone(),
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
                location: self.current().location.clone(),
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

    /// Collect a comment and wrap it in Located for backwards compatibility
    /// This will be removed once all callers are updated
    pub(crate) fn collect_comment_located(&mut self) -> Option<Located<Comment>> {
        self.collect_comment().map(|c| {
            let location = c.location().clone();
            Located::new(c, location)
        })
    }

    pub(crate) fn item_location_from_span(&self, span: Span) -> ItemLocation {
        ItemLocation::new(self.filename.clone(), span)
    }

    pub(crate) fn item_location_from_locations(
        &self,
        start: Location,
        end: Location,
    ) -> ItemLocation {
        self.item_location_from_span(Span::new(start, end))
    }

    pub(crate) fn item_location_from_token_range(
        &self,
        start: &Token,
        end: &Token,
    ) -> ItemLocation {
        self.item_location_from_span(Span::new(start.location.span.start, end.location.span.end))
    }
}
