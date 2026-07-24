mod error;
mod lexers;
#[cfg(test)]
mod tests;
mod token;

pub use error::LexError;
pub use token::{KEYWORDS, Token, TokenKind};

use crate::span::{FileId, ItemLocation, Location, Span};

pub struct Lexer {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    column: usize,
    file_id: FileId,
}

impl Lexer {
    pub fn new(input: String, file_id: FileId) -> Self {
        let chars: Vec<char> = input.chars().collect();
        Self {
            chars,
            pos: 0,
            line: 1,
            column: 1,
            file_id,
        }
    }

    /// Helper to create an ItemLocation from start and end locations
    fn make_location(&self, start: Location, end: Location) -> ItemLocation {
        ItemLocation::new(self.file_id, Span::new(start, end))
    }

    /// Helper to create an ItemLocation from start to current location
    fn loc_to_current(&self, start: Location) -> ItemLocation {
        self.make_location(start, self.current_location())
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();

        loop {
            self.skip_whitespace();
            if self.is_eof() {
                let loc = self.current_location();
                tokens.push(Token::new(
                    TokenKind::Eof,
                    ItemLocation::new(self.file_id, Span::new(loc, loc)),
                ));
                break;
            }

            let token = self.next_token()?;
            tokens.push(token);
        }

        Ok(tokens)
    }

    fn current_location(&self) -> Location {
        Location::new(self.line, self.column)
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.chars.len()
    }

    fn peek(&self) -> Option<char> {
        if self.is_eof() {
            None
        } else {
            Some(self.chars[self.pos])
        }
    }

    fn peek_nth(&self, n: usize) -> Option<char> {
        let pos = self.pos + n;
        if pos >= self.chars.len() {
            None
        } else {
            Some(self.chars[pos])
        }
    }

    fn advance(&mut self) -> Option<char> {
        if self.is_eof() {
            return None;
        }

        let ch = self.chars[self.pos];
        self.pos += 1;

        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            // Column is a byte offset within the line (1-indexed), so that
            // `span_to_offset` / `span_length` / `Parser::span_text` — which
            // all index into the underlying `&str` — line up with multibyte
            // UTF-8 characters instead of drifting by one byte per non-ASCII
            // char.
            self.column += ch.len_utf8();
        }

        Some(ch)
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn next_token(&mut self) -> Result<Token, LexError> {
        let start = self.current_location();
        let start_pos = self.pos;

        let ch = self.peek().unwrap();

        // Handle comments
        if ch == '/' {
            if self.peek_nth(1) == Some('/') {
                return self.lex_line_comment(start, start_pos);
            } else if self.peek_nth(1) == Some('*') {
                return self.lex_multiline_comment(start, start_pos);
            }
        }

        // Handle raw string literals (must come before identifier check)
        if ch == 'r' && (self.peek_nth(1) == Some('"') || self.peek_nth(1) == Some('#')) {
            return self.lex_raw_string(start, start_pos);
        }

        // Handle C-string literals: `c"..."` (regular) and `cr#"..."#` (raw).
        // Must come before the identifier check so `c`/`cr` prefixes aren't
        // lexed as identifiers, but only trigger when immediately followed by
        // `"` (regular) or `r` then `"`/`#` (raw) — identifiers like `copyable`
        // or `const` are unaffected.
        if ch == 'c' {
            if self.peek_nth(1) == Some('"') {
                return self.lex_c_string(start, start_pos);
            }
            if self.peek_nth(1) == Some('r')
                && (self.peek_nth(2) == Some('"') || self.peek_nth(2) == Some('#'))
            {
                return self.lex_raw_c_string(start, start_pos);
            }
        }

        // Handle identifiers and keywords
        if ch.is_alphabetic() || ch == '_' {
            return self.lex_ident_or_keyword(start, start_pos);
        }

        // Handle numbers
        if ch.is_ascii_digit() {
            return self.lex_number(start, start_pos);
        }

        // Handle string literals
        if ch == '"' {
            return self.lex_string(start, start_pos, 0);
        }

        // Handle char literals
        if ch == '\'' {
            return self.lex_char(start, start_pos);
        }

        // Handle punctuation and operators
        match ch {
            ':' => {
                self.advance();
                if self.peek() == Some(':') {
                    self.advance();
                    let end = self.current_location();
                    Ok(Token::new(
                        TokenKind::ColonColon,
                        ItemLocation::new(self.file_id, Span::new(start, end)),
                    ))
                } else {
                    let end = self.current_location();
                    Ok(Token::new(
                        TokenKind::Colon,
                        ItemLocation::new(self.file_id, Span::new(start, end)),
                    ))
                }
            }
            '-' => {
                self.advance();
                if self.peek() == Some('>') {
                    self.advance();
                    let end = self.current_location();
                    Ok(Token::new(
                        TokenKind::Arrow,
                        ItemLocation::new(self.file_id, Span::new(start, end)),
                    ))
                } else {
                    // Could be negative number
                    if let Some(ch) = self.peek()
                        && ch.is_ascii_digit()
                    {
                        return self.lex_number(start, start_pos);
                    }
                    Err(LexError::UnexpectedCharacter {
                        character: '-',
                        location: self.loc_to_current(start),
                    })
                }
            }
            '&' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Amp,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            '*' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Star,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            '[' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::LBracket,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            ']' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::RBracket,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            '{' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::LBrace,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            '}' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::RBrace,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            '(' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::LParen,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            ')' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::RParen,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            '<' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Lt,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            '>' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Gt,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            '=' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Eq,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            ';' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Semi,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            ',' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Comma,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            '!' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Bang,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            '#' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Hash,
                    ItemLocation::new(self.file_id, Span::new(start, end)),
                ))
            }
            _ => Err(LexError::UnexpectedCharacter {
                character: ch,
                location: self.loc_to_current(start),
            }),
        }
    }
}

pub fn tokenize(input: String) -> Result<Vec<Token>, LexError> {
    tokenize_with_file_id(input, FileId::INTERNAL)
}

pub fn tokenize_with_file_id(input: String, file_id: FileId) -> Result<Vec<Token>, LexError> {
    Lexer::new(input, file_id).tokenize()
}
