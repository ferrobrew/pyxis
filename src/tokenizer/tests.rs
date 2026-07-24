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

#[test]
fn test_float_literals() {
    let input = "3.14159 0.0 -1.5 42.0".to_string();
    let tokens = tokenize(input).unwrap();
    assert_eq!(tokens.len(), 5); // 4 floats + EOF
    match &tokens[0].kind {
        TokenKind::FloatLiteral(s) => assert_eq!(s, "3.14159"),
        _ => panic!("Expected FloatLiteral, got {:?}", tokens[0].kind),
    }
    match &tokens[1].kind {
        TokenKind::FloatLiteral(s) => assert_eq!(s, "0.0"),
        _ => panic!("Expected FloatLiteral, got {:?}", tokens[1].kind),
    }
    match &tokens[2].kind {
        TokenKind::FloatLiteral(s) => assert_eq!(s, "-1.5"),
        _ => panic!("Expected FloatLiteral, got {:?}", tokens[2].kind),
    }
    match &tokens[3].kind {
        TokenKind::FloatLiteral(s) => assert_eq!(s, "42.0"),
        _ => panic!("Expected FloatLiteral, got {:?}", tokens[3].kind),
    }
}

#[test]
fn test_non_ascii_in_comments() {
    // An emdash (U+2014, 3 bytes in UTF-8) and other non-ASCII inside a
    // doc comment must round-trip verbatim. Previously the lexer sliced
    // `self.input` (a byte-indexed `String`) by char indices, which either
    // truncated the comment or panicked on a non-char-boundary.
    let input = "/// hello — world — café\n".to_string();
    let tokens = tokenize(input).unwrap();
    match &tokens[0].kind {
        TokenKind::DocOuter(s) => assert_eq!(s, "/// hello — world — café"),
        _ => panic!("Expected DocOuter, got {:?}", tokens[0].kind),
    }

    // Regular line comment.
    let input = "// café — naïve\n".to_string();
    let tokens = tokenize(input).unwrap();
    match &tokens[0].kind {
        TokenKind::Comment(s) => assert_eq!(s, "// café — naïve"),
        _ => panic!("Expected Comment, got {:?}", tokens[0].kind),
    }

    // Multiline comment.
    let input = "/* café — naïve */".to_string();
    let tokens = tokenize(input).unwrap();
    match &tokens[0].kind {
        TokenKind::MultiLineComment(s) => assert_eq!(s, "/* café — naïve */"),
        _ => panic!("Expected MultiLineComment, got {:?}", tokens[0].kind),
    }

    // Non-ASCII *before* a comment, then a comment. The earlier char must
    // not corrupt the offset used to slice the later comment's text.
    let input = "é // comment\n".to_string();
    let tokens = tokenize(input).unwrap();
    match &tokens[1].kind {
        TokenKind::Comment(s) => assert_eq!(s, "// comment"),
        other => panic!("Expected Comment, got {other:?}"),
    }
}

#[test]
fn test_non_ascii_in_string_and_char_literals() {
    // String/char lexing builds values char-by-char, so it already handles
    // non-ASCII — pinned here to guard against regressions.
    let tokens = tokenize("\"café — naïve\"".to_string()).unwrap();
    match &tokens[0].kind {
        TokenKind::StringLiteral(s) => assert_eq!(s, "café — naïve"),
        _ => panic!("Expected StringLiteral"),
    }

    let tokens = tokenize("'é'".to_string()).unwrap();
    match &tokens[0].kind {
        TokenKind::CharLiteral(c) => assert_eq!(*c, 'é'),
        _ => panic!("Expected CharLiteral"),
    }
}

#[test]
fn test_column_is_byte_offset_with_non_ascii() {
    // `é` (U+00E9, 2 bytes) inside a multiline comment shifts the
    // following `;` by one byte relative to one char. `Location::column`
    // is a byte offset within the line (matching `span_to_offset` /
    // `span_length` / `Parser::span_text`), so the `;` sits at column 9,
    // not 8.
    let input = "/* é */;".to_string();
    let tokens = tokenize(input).unwrap();
    assert_eq!(tokens[1].kind, TokenKind::Semi);
    assert_eq!(
        tokens[1].location.span.start.column, 9,
        "column must be a byte offset, not a char offset"
    );
}
