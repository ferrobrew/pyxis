use crate::span::{ItemLocation, Location, Span};

use super::{
    Lexer,
    error::LexError,
    token::{KEYWORD_MAP, Token, TokenKind},
};

impl Lexer {
    pub(super) fn lex_line_comment(
        &mut self,
        start: Location,
        start_pos: usize,
    ) -> Result<Token, LexError> {
        self.advance(); // consume first '/'
        self.advance(); // consume second '/'

        // Check for doc comment markers
        let is_doc_outer = self.peek() == Some('/') && self.peek_nth(1) != Some('/');
        let is_doc_inner = self.peek() == Some('!');

        if is_doc_outer {
            self.advance(); // consume third '/'
        } else if is_doc_inner {
            self.advance(); // consume '!'
        }

        // Read until end of line
        while let Some(ch) = self.peek() {
            if ch == '\n' {
                break;
            }
            self.advance();
        }

        let end = self.current_location();
        let text: String = self.chars[start_pos..self.pos].iter().collect();

        let kind = if is_doc_outer {
            TokenKind::DocOuter(text.clone())
        } else if is_doc_inner {
            TokenKind::DocInner(text.clone())
        } else {
            TokenKind::Comment(text.clone())
        };

        Ok(Token::new(
            kind,
            ItemLocation::new(self.file_id, Span::new(start, end)),
        ))
    }

    pub(super) fn lex_multiline_comment(
        &mut self,
        start: Location,
        start_pos: usize,
    ) -> Result<Token, LexError> {
        self.advance(); // consume '/'
        self.advance(); // consume '*'

        let mut depth = 1;

        while depth > 0 && !self.is_eof() {
            if self.peek() == Some('*') && self.peek_nth(1) == Some('/') {
                self.advance();
                self.advance();
                depth -= 1;
            } else if self.peek() == Some('/') && self.peek_nth(1) == Some('*') {
                self.advance();
                self.advance();
                depth += 1;
            } else {
                self.advance();
            }
        }

        if depth > 0 {
            return Err(LexError::UnterminatedMultilineComment {
                location: self.loc_to_current(start),
            });
        }

        let end = self.current_location();
        let text: String = self.chars[start_pos..self.pos].iter().collect();

        Ok(Token::new(
            TokenKind::MultiLineComment(text.clone()),
            ItemLocation::new(self.file_id, Span::new(start, end)),
        ))
    }

    pub(super) fn lex_ident_or_keyword(
        &mut self,
        start: Location,
        start_pos: usize,
    ) -> Result<Token, LexError> {
        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let end = self.current_location();
        let text: String = self.chars[start_pos..self.pos].iter().collect();

        let kind = KEYWORD_MAP
            .get(text.as_str())
            .cloned()
            .unwrap_or_else(|| TokenKind::Ident(text.clone()));

        Ok(Token::new(
            kind,
            ItemLocation::new(self.file_id, Span::new(start, end)),
        ))
    }

    pub(super) fn lex_number(
        &mut self,
        start: Location,
        start_pos: usize,
    ) -> Result<Token, LexError> {
        // Handle negative numbers
        if self.peek() == Some('-') {
            self.advance();
        }

        // Handle different number bases
        if self.peek() == Some('0') {
            match self.peek_nth(1) {
                Some('x') => {
                    // Hexadecimal
                    self.advance();
                    self.advance();
                    while let Some(ch) = self.peek() {
                        if ch.is_ascii_hexdigit() || ch == '_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                Some('b') => {
                    // Binary
                    self.advance();
                    self.advance();
                    while let Some(ch) = self.peek() {
                        if ch == '0' || ch == '1' || ch == '_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                Some('o') => {
                    // Octal
                    self.advance();
                    self.advance();
                    while let Some(ch) = self.peek() {
                        if ch.is_digit(8) || ch == '_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                _ => {
                    // Decimal number starting with 0
                    while let Some(ch) = self.peek() {
                        if ch.is_ascii_digit() || ch == '_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
            }
        } else {
            // Handle decimal numbers
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() || ch == '_' {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        let end = self.current_location();
        let text: String = self.chars[start_pos..self.pos].iter().collect();

        // Check for a fractional part: `.` followed by a digit.
        // Since pyxis has no method-call or field-access syntax that uses `.`
        // after a number, a `.` here is unambiguously a float decimal point.
        if self.peek() == Some('.') && self.peek_nth(1).is_some_and(|c| c.is_ascii_digit()) {
            self.advance(); // consume '.'
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() || ch == '_' {
                    self.advance();
                } else {
                    break;
                }
            }

            let end = self.current_location();
            let text: String = self.chars[start_pos..self.pos].iter().collect();
            return Ok(Token::new(
                TokenKind::FloatLiteral(text.clone()),
                ItemLocation::new(self.file_id, Span::new(start, end)),
            ));
        }

        Ok(Token::new(
            TokenKind::IntLiteral(text.clone()),
            ItemLocation::new(self.file_id, Span::new(start, end)),
        ))
    }

    pub(super) fn lex_string(
        &mut self,
        start: Location,
        _start_pos: usize,
        _hash_count: usize,
    ) -> Result<Token, LexError> {
        self.advance(); // consume opening '"'

        let mut value = String::new();

        while let Some(ch) = self.peek() {
            if ch == '"' {
                self.advance(); // consume closing '"'
                break;
            } else if ch == '\\' {
                let escape_start = self.current_location();
                self.advance();
                if let Some(escaped) = self.peek() {
                    self.advance();
                    match escaped {
                        'n' => value.push('\n'),
                        'r' => value.push('\r'),
                        't' => value.push('\t'),
                        '\\' => value.push('\\'),
                        '"' => value.push('"'),
                        '\'' => value.push('\''),
                        '0' => value.push('\0'),
                        _ => {
                            return Err(LexError::InvalidEscapeSequence {
                                character: escaped,
                                location: self.loc_to_current(escape_start),
                            });
                        }
                    }
                } else {
                    return Err(LexError::UnexpectedEofInStringLiteral {
                        location: self.loc_to_current(start),
                    });
                }
            } else {
                value.push(ch);
                self.advance();
            }
        }

        let end = self.current_location();

        Ok(Token::new(
            TokenKind::StringLiteral(value),
            ItemLocation::new(self.file_id, Span::new(start, end)),
        ))
    }

    pub(super) fn lex_raw_string(
        &mut self,
        start: Location,
        _start_pos: usize,
    ) -> Result<Token, LexError> {
        self.advance(); // consume 'r'

        // Count the number of '#' characters
        let mut hash_count = 0;
        while self.peek() == Some('#') {
            hash_count += 1;
            self.advance();
        }

        if self.peek() != Some('"') {
            return Err(LexError::InvalidRawStringStart {
                location: self.loc_to_current(start),
            });
        }

        self.advance(); // consume opening '"'

        let mut value = String::new();

        loop {
            if self.is_eof() {
                return Err(LexError::UnterminatedRawString {
                    location: self.loc_to_current(start),
                });
            }

            if self.peek() == Some('"') {
                // Check if we have the right number of '#' characters
                let mut matching_hashes = 0;
                for i in 1..=hash_count {
                    if self.peek_nth(i) == Some('#') {
                        matching_hashes += 1;
                    } else {
                        break;
                    }
                }

                if matching_hashes == hash_count {
                    self.advance(); // consume '"'
                    for _ in 0..hash_count {
                        self.advance(); // consume '#' characters
                    }
                    break;
                } else {
                    value.push('"');
                    self.advance();
                }
            } else {
                value.push(self.peek().unwrap());
                self.advance();
            }
        }

        let end = self.current_location();

        Ok(Token::new(
            TokenKind::StringLiteral(value),
            ItemLocation::new(self.file_id, Span::new(start, end)),
        ))
    }

    /// Lex a regular C-string literal `c"..."` with escape processing.
    /// Mirrors `lex_string` but consumes the leading `c` and emits a
    /// `CStringLiteral` token.
    pub(super) fn lex_c_string(
        &mut self,
        start: Location,
        _start_pos: usize,
    ) -> Result<Token, LexError> {
        self.advance(); // consume 'c'
        self.advance(); // consume opening '"'

        let mut value = String::new();

        while let Some(ch) = self.peek() {
            if ch == '"' {
                self.advance(); // consume closing '"'
                break;
            } else if ch == '\\' {
                let escape_start = self.current_location();
                self.advance();
                if let Some(escaped) = self.peek() {
                    self.advance();
                    match escaped {
                        'n' => value.push('\n'),
                        'r' => value.push('\r'),
                        't' => value.push('\t'),
                        '\\' => value.push('\\'),
                        '"' => value.push('"'),
                        '\'' => value.push('\''),
                        '0' => value.push('\0'),
                        _ => {
                            return Err(LexError::InvalidEscapeSequence {
                                character: escaped,
                                location: self.loc_to_current(escape_start),
                            });
                        }
                    }
                } else {
                    return Err(LexError::UnexpectedEofInStringLiteral {
                        location: self.loc_to_current(start),
                    });
                }
            } else {
                value.push(ch);
                self.advance();
            }
        }

        let end = self.current_location();

        Ok(Token::new(
            TokenKind::CStringLiteral(value),
            ItemLocation::new(self.file_id, Span::new(start, end)),
        ))
    }

    /// Lex a raw C-string literal `cr#"..."#` (with variable hash counts)
    /// without escape processing. Mirrors `lex_raw_string` but consumes the
    /// leading `cr` and emits a `CStringLiteral` token.
    pub(super) fn lex_raw_c_string(
        &mut self,
        start: Location,
        _start_pos: usize,
    ) -> Result<Token, LexError> {
        self.advance(); // consume 'c'
        self.advance(); // consume 'r'

        // Count the number of '#' characters
        let mut hash_count = 0;
        while self.peek() == Some('#') {
            hash_count += 1;
            self.advance();
        }

        if self.peek() != Some('"') {
            return Err(LexError::InvalidRawStringStart {
                location: self.loc_to_current(start),
            });
        }

        self.advance(); // consume opening '"'

        let mut value = String::new();

        loop {
            if self.is_eof() {
                return Err(LexError::UnterminatedRawString {
                    location: self.loc_to_current(start),
                });
            }

            if self.peek() == Some('"') {
                // Check if we have the right number of '#' characters
                let mut matching_hashes = 0;
                for i in 1..=hash_count {
                    if self.peek_nth(i) == Some('#') {
                        matching_hashes += 1;
                    } else {
                        break;
                    }
                }

                if matching_hashes == hash_count {
                    self.advance(); // consume '"'
                    for _ in 0..hash_count {
                        self.advance(); // consume '#' characters
                    }
                    break;
                } else {
                    value.push('"');
                    self.advance();
                }
            } else {
                value.push(self.peek().unwrap());
                self.advance();
            }
        }

        let end = self.current_location();

        Ok(Token::new(
            TokenKind::CStringLiteral(value),
            ItemLocation::new(self.file_id, Span::new(start, end)),
        ))
    }

    pub(super) fn lex_char(
        &mut self,
        start: Location,
        _start_pos: usize,
    ) -> Result<Token, LexError> {
        self.advance(); // consume opening '\''

        let ch = if self.peek() == Some('\\') {
            let escape_start = self.current_location();
            self.advance();
            if let Some(escaped) = self.peek() {
                self.advance();
                match escaped {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '\'' => '\'',
                    '"' => '"',
                    '0' => '\0',
                    _ => {
                        return Err(LexError::InvalidEscapeSequence {
                            character: escaped,
                            location: self.loc_to_current(escape_start),
                        });
                    }
                }
            } else {
                return Err(LexError::UnexpectedEofInCharLiteral {
                    location: self.loc_to_current(start),
                });
            }
        } else if let Some(ch) = self.peek() {
            self.advance();
            ch
        } else {
            return Err(LexError::UnexpectedEofInCharLiteral {
                location: self.loc_to_current(start),
            });
        };

        if self.peek() != Some('\'') {
            return Err(LexError::UnclosedCharLiteral {
                location: self.loc_to_current(start),
            });
        }

        self.advance(); // consume closing '\''

        let end = self.current_location();

        Ok(Token::new(
            TokenKind::CharLiteral(ch),
            ItemLocation::new(self.file_id, Span::new(start, end)),
        ))
    }
}
