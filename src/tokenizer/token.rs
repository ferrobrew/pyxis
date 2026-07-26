use std::{collections::HashMap, sync::LazyLock};

use crate::span::{ItemLocation, Location};

#[cfg(test)]
use crate::span::StripLocations;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub enum TokenKind {
    // Keywords
    Pub,
    Type,
    Enum,
    Bitflags,
    Union,
    Impl,
    Fn,
    Extern,
    Use,
    Meta,
    Functions,
    Vftable,
    Unknown,
    Prologue,
    Epilogue,

    // Literals
    Ident(String),
    IntLiteral(String),
    FloatLiteral(String),
    StringLiteral(String),  // Already processed, escape sequences resolved
    CStringLiteral(String), // C-string literal `c"..."` / `cr#"..."#`, escapes resolved
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

/// Canonical table of keyword spellings and the token each lexes to. The single
/// source of truth for keyword spellings: both the lexer and [`TokenKind::keyword_str`]
/// derive from it, so consumers (e.g. editor tooling) never duplicate the spellings.
pub const KEYWORDS: &[(&str, TokenKind)] = &[
    ("pub", TokenKind::Pub),
    ("type", TokenKind::Type),
    ("enum", TokenKind::Enum),
    ("bitflags", TokenKind::Bitflags),
    ("union", TokenKind::Union),
    ("impl", TokenKind::Impl),
    ("fn", TokenKind::Fn),
    ("extern", TokenKind::Extern),
    ("use", TokenKind::Use),
    ("meta", TokenKind::Meta),
    ("functions", TokenKind::Functions),
    ("vftable", TokenKind::Vftable),
    ("unknown", TokenKind::Unknown),
    ("prologue", TokenKind::Prologue),
    ("epilogue", TokenKind::Epilogue),
    ("mut", TokenKind::Mut),
    ("const", TokenKind::Const),
    ("self", TokenKind::SelfValue),
    ("Self", TokenKind::SelfType),
    ("_", TokenKind::Underscore),
];

/// O(1) spelling → keyword token lookup, built once from [`KEYWORDS`] (still the
/// single source of truth). Avoids the per-identifier linear scan on the hot
/// tokenization path.
pub(super) static KEYWORD_MAP: LazyLock<HashMap<&'static str, TokenKind>> =
    LazyLock::new(|| KEYWORDS.iter().map(|(s, k)| (*s, k.clone())).collect());

impl TokenKind {
    /// The canonical source spelling for a keyword token (e.g. `Pub` → `"pub"`),
    /// or `None` for non-keyword tokens.
    pub fn keyword_str(&self) -> Option<&'static str> {
        KEYWORDS
            .iter()
            .find(|(_, kind)| kind == self)
            .map(|(spelling, _)| *spelling)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
