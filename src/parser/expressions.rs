use crate::{span::Located, tokenizer::TokenKind};

#[cfg(test)]
use crate::span::StripLocations;

use super::{ParseError, core::Parser, types::Ident};

/// Format information for integer literals
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntFormat {
    Decimal,
    Hex,
    Binary,
    Octal,
}
#[cfg(test)]
impl StripLocations for IntFormat {
    fn strip_locations(&self) -> Self {
        *self
    }
}

/// Format information for string literals
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StringFormat {
    Regular,
    Raw,
}
#[cfg(test)]
impl StripLocations for StringFormat {
    fn strip_locations(&self) -> Self {
        *self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    IntLiteral { value: isize, format: IntFormat },
    StringLiteral { value: String, format: StringFormat },
    Ident(Ident),
}
#[cfg(test)]
impl StripLocations for Expr {
    fn strip_locations(&self) -> Self {
        match self {
            Expr::IntLiteral { value, format } => Expr::IntLiteral {
                value: value.strip_locations(),
                format: format.strip_locations(),
            },
            Expr::StringLiteral { value, format } => Expr::StringLiteral {
                value: value.strip_locations(),
                format: format.strip_locations(),
            },
            Expr::Ident(ident) => Expr::Ident(ident.strip_locations()),
        }
    }
}
impl Expr {
    pub fn int_literal(&self) -> Option<isize> {
        match self {
            Expr::IntLiteral { value, .. } => Some(*value),
            _ => None,
        }
    }
    pub fn string_literal(&self) -> Option<&str> {
        match self {
            Expr::StringLiteral { value, .. } => Some(value.as_str()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub(crate) fn parse_expr(&mut self) -> Result<Located<Expr>, ParseError> {
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
                Ok(value.map(|value| Expr::IntLiteral { value, format }))
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
                Ok(value.map(|value| Expr::StringLiteral { value, format }))
            }
            TokenKind::Ident(_) => {
                let (ident, ident_span) = self.expect_ident()?;
                Ok(Located::new(
                    Expr::Ident(ident),
                    self.item_location_from_span(ident_span),
                ))
            }
            _ => Err(ParseError::ExpectedExpression {
                found: self.peek().clone(),
                location: self.current().location.clone(),
            }),
        }
    }

    pub(crate) fn parse_int_literal(&mut self) -> Result<Located<isize>, ParseError> {
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
                }?;

                Ok(Located::new(value, token.location))
            }
            _ => Err(ParseError::ExpectedIntLiteral {
                found: self.peek().clone(),
                location: self.current().location.clone(),
            }),
        }
    }

    pub(crate) fn parse_string_literal(&mut self) -> Result<Located<String>, ParseError> {
        match self.peek() {
            TokenKind::StringLiteral(_) => {
                let token = self.advance();
                let TokenKind::StringLiteral(s) = token.kind else {
                    unreachable!()
                };
                Ok(Located::new(s, token.location))
            }
            _ => Err(ParseError::ExpectedStringLiteral {
                found: self.peek().clone(),
                location: self.current().location.clone(),
            }),
        }
    }
}
