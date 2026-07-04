use crate::{
    span::{HasLocation, ItemLocation},
    tokenizer::TokenKind,
};

#[cfg(test)]
use crate::span::StripLocations;

use super::{ParseError, core::Parser, paths::ItemPath, types::Ident};

/// Format information for integer literals
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
#[cfg_attr(test, strip_locations(copy))]
pub enum IntFormat {
    Decimal,
    Hex,
    Binary,
    Octal,
}

/// Format information for string literals
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
#[cfg_attr(test, strip_locations(copy))]
pub enum StringFormat {
    Regular,
    Raw,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub enum Expr {
    IntLiteral {
        value: isize,
        format: IntFormat,
        location: ItemLocation,
    },
    /// A float literal. The raw source text is stored (not an `f64`) so that
    /// `Expr` can derive `Eq`/`Hash` — `f64` does not implement either. Two
    /// floats with the same value but different source text (e.g. `3.0` vs
    /// `3.00`) are therefore not equal as `Expr` variants, which is fine for a
    /// source-preserving AST.
    FloatLiteral {
        raw_text: String,
        location: ItemLocation,
    },
    StringLiteral {
        value: String,
        format: StringFormat,
        location: ItemLocation,
    },
    Ident {
        ident: Ident,
        location: ItemLocation,
    },
    /// A multi-segment path expression, e.g. `Color::Red` for enum-value
    /// references in `const` declarations.
    Path {
        path: ItemPath,
        location: ItemLocation,
    },
}
impl Expr {
    pub fn int_literal(&self) -> Option<isize> {
        match self {
            Expr::IntLiteral { value, .. } => Some(*value),
            _ => None,
        }
    }
    pub fn float_literal(&self) -> Option<&str> {
        match self {
            Expr::FloatLiteral { raw_text, .. } => Some(raw_text.as_str()),
            _ => None,
        }
    }
    /// Parse the stored raw text into an `f64`. Returns `None` if the text is
    /// not a valid float (should not happen for tokens produced by the lexer).
    pub fn float_value(&self) -> Option<f64> {
        match self {
            Expr::FloatLiteral { raw_text, .. } => raw_text.parse().ok(),
            _ => None,
        }
    }
    pub fn string_literal(&self) -> Option<&str> {
        match self {
            Expr::StringLiteral { value, .. } => Some(value.as_str()),
            _ => None,
        }
    }
    pub fn path(&self) -> Option<&ItemPath> {
        match self {
            Expr::Path { path, .. } => Some(path),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprField(pub Ident, pub Expr);
impl ExprField {
    pub fn ident(&self) -> &Ident {
        &self.0
    }

    pub fn ident_as_str(&self) -> &str {
        self.0.as_str()
    }
}
impl From<(Ident, Expr)> for ExprField {
    fn from(item: (Ident, Expr)) -> Self {
        ExprField(item.0, item.1)
    }
}

impl Parser {
    pub(crate) fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            TokenKind::IntLiteral(_) => {
                let token = self.current().clone();
                let (value, location) = self.parse_int_literal()?;
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
                Ok(Expr::IntLiteral {
                    value,
                    format,
                    location,
                })
            }
            TokenKind::FloatLiteral(_) => {
                let token = self.advance();
                let TokenKind::FloatLiteral(s) = token.kind else {
                    unreachable!()
                };
                Ok(Expr::FloatLiteral {
                    raw_text: s,
                    location: token.location,
                })
            }
            TokenKind::StringLiteral(_) => {
                let token = self.current().clone();
                let (value, location) = self.parse_string_literal()?;
                // Detect if this was a raw string from the original token text
                let text = self.span_text(&token.location.span);
                let format = if text.starts_with('r') {
                    StringFormat::Raw
                } else {
                    StringFormat::Regular
                };
                Ok(Expr::StringLiteral {
                    value,
                    format,
                    location,
                })
            }
            TokenKind::Ident(_) => {
                // Check if this is a path expression (Ident::Ident...) for
                // enum-value references like `Color::Red`.
                if matches!(self.peek_nth(1), TokenKind::ColonColon) {
                    let (path, location) = self.parse_item_path()?;
                    Ok(Expr::Path { path, location })
                } else {
                    let (ident, ident_span) = self.expect_ident()?;
                    let location = self.item_location_from_span(ident_span);
                    Ok(Expr::Ident { ident, location })
                }
            }
            _ => Err(ParseError::ExpectedExpression {
                found: self.peek().clone(),
                location: self.current().location,
            }),
        }
    }

    pub(crate) fn parse_int_literal(&mut self) -> Result<(isize, ItemLocation), ParseError> {
        match self.peek() {
            TokenKind::IntLiteral(_) => {
                let token = self.advance();
                let TokenKind::IntLiteral(s) = token.kind else {
                    unreachable!()
                };
                // Remove underscores
                let s = s.replace('_', "");

                // Parse based on prefix
                let value = if s.starts_with("0x") || s.starts_with("-0x") {
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
                            location: token.location,
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
                            location: token.location,
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
                            location: token.location,
                        })
                } else {
                    // Decimal
                    s.parse::<isize>()
                        .map_err(|_| ParseError::InvalidIntLiteral {
                            kind: "integer".to_string(),
                            value: s.clone(),
                            location: token.location,
                        })
                }?;

                Ok((value, token.location))
            }
            _ => Err(ParseError::ExpectedIntLiteral {
                found: self.peek().clone(),
                location: self.current().location,
            }),
        }
    }

    pub(crate) fn parse_string_literal(&mut self) -> Result<(String, ItemLocation), ParseError> {
        match self.peek() {
            TokenKind::StringLiteral(_) => {
                let token = self.advance();
                let TokenKind::StringLiteral(s) = token.kind else {
                    unreachable!()
                };
                Ok((s, token.location))
            }
            _ => Err(ParseError::ExpectedStringLiteral {
                found: self.peek().clone(),
                location: self.current().location,
            }),
        }
    }
}
