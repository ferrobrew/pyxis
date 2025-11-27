use crate::source_store::SourceStore;
use crate::span::{self, ItemLocation, Location, Span};
use ariadne::{Color, Label, Report, ReportKind, Source};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // Keywords
    Pub,
    Type,
    Enum,
    Bitflags,
    Impl,
    Fn,
    Extern,
    Use,
    Backend,
    Meta,
    Functions,
    Vftable,
    Unknown,
    Prologue,
    Epilogue,

    // Literals
    Ident(String),
    IntLiteral(String),
    StringLiteral(String), // Already processed, escape sequences resolved
    CharLiteral(char),

    // Comments (preserve the original text including markers)
    DocOuter(String),         // ///
    DocInner(String),         // //!
    Comment(String),          // //
    MultiLineComment(String), // /* */

    // Punctuation and Operators
    ColonColon, // ::
    Arrow,      // ->
    Amp,        // &
    Star,       // *
    LBracket,   // [
    RBracket,   // ]
    LBrace,     // {
    RBrace,     // }
    LParen,     // (
    RParen,     // )
    Lt,         // <
    Gt,         // >
    Eq,         // =
    Colon,      // :
    Semi,       // ;
    Comma,      // ,
    Bang,       // !
    Hash,       // #
    Underscore, // _
    Mut,        // mut (for pointers)
    Const,      // const (for pointers)
    SelfValue,  // self
    SelfType,   // Self

    // Special
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub location: ItemLocation,
}

impl Token {
    pub fn new(kind: TokenKind, location: ItemLocation) -> Self {
        Self { kind, location }
    }

    pub fn start_location(&self) -> Location {
        self.location.span.start
    }

    pub fn end_location(&self) -> Location {
        self.location.span.end
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LexError {
    pub message: String,
    pub location: ItemLocation,
    pub source: String,
}

impl LexError {
    /// Format the error using ariadne with the provided source store.
    /// Always produces ariadne-formatted output.
    pub fn format_with_ariadne(&self, source_store: &mut dyn SourceStore) -> String {
        if let Some(source) = source_store.get(self.location.filename.as_ref()) {
            let offset = span::span_to_offset(source, &self.location.span);
            let report = Report::build(ReportKind::Error, self.location.filename.as_ref(), offset)
                .with_message(&self.message)
                .with_label(
                    Label::new((self.location.filename.as_ref(), offset..offset + 1))
                        .with_message(&self.message)
                        .with_color(Color::Red),
                )
                .finish();

            let mut buffer = Vec::new();
            if report
                .write(
                    (self.location.filename.as_ref(), Source::from(source)),
                    &mut buffer,
                )
                .is_ok()
            {
                return String::from_utf8_lossy(&buffer).to_string();
            }
        }

        // Source not available - create a report without source code labels
        let placeholder_filename = self.location.filename.as_ref();
        let report = Report::<(&str, std::ops::Range<usize>)>::build(
            ReportKind::Error,
            placeholder_filename,
            0,
        )
        .with_message(&self.message)
        .with_note(format!(
            "Error location: {}:{}:{}",
            self.location.filename, self.location.span.start.line, self.location.span.start.column
        ))
        .finish();

        let mut buffer = Vec::new();
        if report
            .write((placeholder_filename, Source::from("")), &mut buffer)
            .is_ok()
        {
            return String::from_utf8_lossy(&buffer).to_string();
        }

        self.message.clone()
    }
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Lexer error at {}: {}", self.location, self.message)
    }
}

impl std::error::Error for LexError {}

pub struct Lexer {
    input: String,
    chars: Vec<char>,
    pos: usize,
    line: usize,
    column: usize,
    filename: Arc<str>,
}

impl Lexer {
    pub fn new(input: String, filename: impl Into<Arc<str>>) -> Self {
        let chars: Vec<char> = input.chars().collect();
        Self {
            input,
            chars,
            pos: 0,
            line: 1,
            column: 1,
            filename: filename.into(),
        }
    }

    /// Helper to create a LexError with filename and source context
    fn error(&self, message: String, location: Location) -> LexError {
        LexError {
            message,
            location: ItemLocation::new(self.filename.clone(), Span::new(location, location)),
            source: self.input.clone(),
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();

        loop {
            self.skip_whitespace();
            if self.is_eof() {
                let loc = self.current_location();
                tokens.push(Token::new(
                    TokenKind::Eof,
                    ItemLocation::new(self.filename.clone(), Span::new(loc, loc)),
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
            self.column += 1;
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
                        ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                    ))
                } else {
                    let end = self.current_location();
                    Ok(Token::new(
                        TokenKind::Colon,
                        ItemLocation::new(self.filename.clone(), Span::new(start, end)),
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
                        ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                    ))
                } else {
                    // Could be negative number
                    if let Some(ch) = self.peek()
                        && ch.is_ascii_digit()
                    {
                        return self.lex_number(start, start_pos);
                    }
                    Err(self.error("Unexpected '-' character".to_string(), start))
                }
            }
            '&' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Amp,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            '*' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Star,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            '[' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::LBracket,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            ']' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::RBracket,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            '{' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::LBrace,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            '}' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::RBrace,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            '(' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::LParen,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            ')' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::RParen,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            '<' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Lt,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            '>' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Gt,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            '=' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Eq,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            ';' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Semi,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            ',' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Comma,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            '!' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Bang,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            '#' => {
                self.advance();
                let end = self.current_location();
                Ok(Token::new(
                    TokenKind::Hash,
                    ItemLocation::new(self.filename.clone(), Span::new(start, end)),
                ))
            }
            _ => Err(self.error(format!("Unexpected character: '{ch}'"), start)),
        }
    }

    fn lex_line_comment(&mut self, start: Location, start_pos: usize) -> Result<Token, LexError> {
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
        let text = self.input[start_pos..self.pos].to_string();

        let kind = if is_doc_outer {
            TokenKind::DocOuter(text.clone())
        } else if is_doc_inner {
            TokenKind::DocInner(text.clone())
        } else {
            TokenKind::Comment(text.clone())
        };

        Ok(Token::new(
            kind,
            ItemLocation::new(self.filename.clone(), Span::new(start, end)),
        ))
    }

    fn lex_multiline_comment(
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
            return Err(self.error("Unterminated multiline comment".to_string(), start));
        }

        let end = self.current_location();
        let text = self.input[start_pos..self.pos].to_string();

        Ok(Token::new(
            TokenKind::MultiLineComment(text.clone()),
            ItemLocation::new(self.filename.clone(), Span::new(start, end)),
        ))
    }

    fn lex_ident_or_keyword(
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
        let text = self.input[start_pos..self.pos].to_string();

        let kind = match text.as_str() {
            "pub" => TokenKind::Pub,
            "type" => TokenKind::Type,
            "enum" => TokenKind::Enum,
            "bitflags" => TokenKind::Bitflags,
            "impl" => TokenKind::Impl,
            "fn" => TokenKind::Fn,
            "extern" => TokenKind::Extern,
            "use" => TokenKind::Use,
            "backend" => TokenKind::Backend,
            "meta" => TokenKind::Meta,
            "functions" => TokenKind::Functions,
            "vftable" => TokenKind::Vftable,
            "unknown" => TokenKind::Unknown,
            "prologue" => TokenKind::Prologue,
            "epilogue" => TokenKind::Epilogue,
            "mut" => TokenKind::Mut,
            "const" => TokenKind::Const,
            "self" => TokenKind::SelfValue,
            "Self" => TokenKind::SelfType,
            "_" => TokenKind::Underscore,
            _ => TokenKind::Ident(text.clone()),
        };

        Ok(Token::new(
            kind,
            ItemLocation::new(self.filename.clone(), Span::new(start, end)),
        ))
    }

    fn lex_number(&mut self, start: Location, start_pos: usize) -> Result<Token, LexError> {
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
        let text = self.input[start_pos..self.pos].to_string();

        Ok(Token::new(
            TokenKind::IntLiteral(text.clone()),
            ItemLocation::new(self.filename.clone(), Span::new(start, end)),
        ))
    }

    fn lex_string(
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
                            return Err(self.error(
                                format!("Invalid escape sequence: \\{escaped}"),
                                self.current_location(),
                            ));
                        }
                    }
                } else {
                    return Err(self.error(
                        "Unexpected end of file in string literal".to_string(),
                        self.current_location(),
                    ));
                }
            } else {
                value.push(ch);
                self.advance();
            }
        }

        let end = self.current_location();

        Ok(Token::new(
            TokenKind::StringLiteral(value),
            ItemLocation::new(self.filename.clone(), Span::new(start, end)),
        ))
    }

    fn lex_raw_string(&mut self, start: Location, _start_pos: usize) -> Result<Token, LexError> {
        self.advance(); // consume 'r'

        // Count the number of '#' characters
        let mut hash_count = 0;
        while self.peek() == Some('#') {
            hash_count += 1;
            self.advance();
        }

        if self.peek() != Some('"') {
            return Err(self.error(
                "Expected '\"' after 'r' and '#' in raw string literal".to_string(),
                self.current_location(),
            ));
        }

        self.advance(); // consume opening '"'

        let mut value = String::new();

        loop {
            if self.is_eof() {
                return Err(self.error("Unterminated raw string literal".to_string(), start));
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
            ItemLocation::new(self.filename.clone(), Span::new(start, end)),
        ))
    }

    fn lex_char(&mut self, start: Location, _start_pos: usize) -> Result<Token, LexError> {
        self.advance(); // consume opening '\''

        let ch = if self.peek() == Some('\\') {
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
                        return Err(self.error(
                            format!("Invalid escape sequence: \\{escaped}"),
                            self.current_location(),
                        ));
                    }
                }
            } else {
                return Err(self.error(
                    "Unexpected end of file in char literal".to_string(),
                    self.current_location(),
                ));
            }
        } else if let Some(ch) = self.peek() {
            self.advance();
            ch
        } else {
            return Err(self.error(
                "Unexpected end of file in char literal".to_string(),
                self.current_location(),
            ));
        };

        if self.peek() != Some('\'') {
            return Err(self.error(
                "Expected closing '\'' in char literal".to_string(),
                self.current_location(),
            ));
        }

        self.advance(); // consume closing '\''

        let end = self.current_location();

        Ok(Token::new(
            TokenKind::CharLiteral(ch),
            ItemLocation::new(self.filename.clone(), Span::new(start, end)),
        ))
    }
}

pub fn tokenize(input: String) -> Result<Vec<Token>, LexError> {
    tokenize_with_filename(input, "<input>")
}

pub fn tokenize_with_filename(
    input: String,
    filename: impl Into<Arc<str>>,
) -> Result<Vec<Token>, LexError> {
    Lexer::new(input, filename).tokenize()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let input = "pub type enum bitflags impl fn extern use".to_string();
        let tokens = tokenize(input).unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Pub);
        assert_eq!(tokens[1].kind, TokenKind::Type);
        assert_eq!(tokens[2].kind, TokenKind::Enum);
        assert_eq!(tokens[3].kind, TokenKind::Bitflags);
        assert_eq!(tokens[4].kind, TokenKind::Impl);
        assert_eq!(tokens[5].kind, TokenKind::Fn);
        assert_eq!(tokens[6].kind, TokenKind::Extern);
        assert_eq!(tokens[7].kind, TokenKind::Use);
    }

    #[test]
    fn test_comments() {
        let input = "// regular\n/// doc outer\n//! doc inner\n/* multiline */".to_string();
        let tokens = tokenize(input).unwrap();

        match &tokens[0].kind {
            TokenKind::Comment(s) => assert_eq!(s, "// regular"),
            _ => panic!("Expected Comment"),
        }

        match &tokens[1].kind {
            TokenKind::DocOuter(s) => assert_eq!(s, "/// doc outer"),
            _ => panic!("Expected DocOuter"),
        }

        match &tokens[2].kind {
            TokenKind::DocInner(s) => assert_eq!(s, "//! doc inner"),
            _ => panic!("Expected DocInner"),
        }

        match &tokens[3].kind {
            TokenKind::MultiLineComment(s) => assert_eq!(s, "/* multiline */"),
            _ => panic!("Expected MultiLineComment"),
        }
    }

    #[test]
    fn test_raw_strings() {
        let input = r###"r"hello" r#"with "quotes""# r##"with #"##"###.to_string();
        let tokens = tokenize(input).unwrap();

        match &tokens[0].kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected StringLiteral"),
        }

        match &tokens[1].kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, "with \"quotes\""),
            _ => panic!("Expected StringLiteral"),
        }

        match &tokens[2].kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, "with #"),
            _ => panic!("Expected StringLiteral"),
        }
    }

    #[test]
    fn test_punctuation() {
        let input = ":: -> & * [ ] { } ( ) < > = : ; , ! #".to_string();
        let tokens = tokenize(input).unwrap();
        assert_eq!(tokens[0].kind, TokenKind::ColonColon);
        assert_eq!(tokens[1].kind, TokenKind::Arrow);
        assert_eq!(tokens[2].kind, TokenKind::Amp);
        assert_eq!(tokens[3].kind, TokenKind::Star);
        assert_eq!(tokens[4].kind, TokenKind::LBracket);
        assert_eq!(tokens[5].kind, TokenKind::RBracket);
        assert_eq!(tokens[6].kind, TokenKind::LBrace);
        assert_eq!(tokens[7].kind, TokenKind::RBrace);
        assert_eq!(tokens[8].kind, TokenKind::LParen);
        assert_eq!(tokens[9].kind, TokenKind::RParen);
        assert_eq!(tokens[10].kind, TokenKind::Lt);
        assert_eq!(tokens[11].kind, TokenKind::Gt);
        assert_eq!(tokens[12].kind, TokenKind::Eq);
        assert_eq!(tokens[13].kind, TokenKind::Colon);
        assert_eq!(tokens[14].kind, TokenKind::Semi);
        assert_eq!(tokens[15].kind, TokenKind::Comma);
        assert_eq!(tokens[16].kind, TokenKind::Bang);
        assert_eq!(tokens[17].kind, TokenKind::Hash);
    }
}
