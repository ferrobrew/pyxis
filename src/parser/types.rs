use std::fmt;

use crate::{
    span::{EqualsIgnoringLocations, Located},
    tokenizer::TokenKind,
};

#[cfg(test)]
use crate::span::StripLocations;

use super::{ParseError, core::Parser};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub String);
#[cfg(test)]
impl StripLocations for Ident {
    fn strip_locations(&self) -> Self {
        Ident(self.0.strip_locations())
    }
}
impl EqualsIgnoringLocations for Ident {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl From<&str> for Ident {
    fn from(item: &str) -> Self {
        Ident(item.to_string())
    }
}
impl Ident {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}
impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}
impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    ConstPointer(Box<Located<Type>>),
    MutPointer(Box<Located<Type>>),
    Array(Box<Located<Type>>, usize),
    Ident(Ident),
    Unknown(usize),
}
impl EqualsIgnoringLocations for Type {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::ConstPointer(t), Type::ConstPointer(t2)) => t.equals_ignoring_locations(t2),
            (Type::MutPointer(t), Type::MutPointer(t2)) => t.equals_ignoring_locations(t2),
            (Type::Array(t, n), Type::Array(t2, n2)) => {
                t.equals_ignoring_locations(t2) && n.equals_ignoring_locations(n2)
            }
            (Type::Ident(ident), Type::Ident(ident2)) => ident.equals_ignoring_locations(ident2),
            (Type::Unknown(size), Type::Unknown(size2)) => size.equals_ignoring_locations(size2),
            _ => false,
        }
    }
}
#[cfg(test)]
impl StripLocations for Type {
    fn strip_locations(&self) -> Self {
        match self {
            Type::ConstPointer(located) => Type::ConstPointer(located.strip_locations()),
            Type::MutPointer(located) => Type::MutPointer(located.strip_locations()),
            Type::Array(located, len) => {
                Type::Array(located.strip_locations(), len.strip_locations())
            }
            Type::Ident(ident) => Type::Ident(ident.strip_locations()),
            Type::Unknown(size) => Type::Unknown(size.strip_locations()),
        }
    }
}
#[cfg(test)]
impl Type {
    pub fn ident(ident: &str) -> Type {
        Type::Ident(ident.into())
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Type::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn const_pointer(self) -> Type {
        Type::ConstPointer(Box::new(Located::test(self)))
    }

    pub fn mut_pointer(self) -> Type {
        Type::MutPointer(Box::new(Located::test(self)))
    }

    pub fn array(self, size: usize) -> Type {
        Type::Array(Box::new(Located::test(self)), size)
    }

    pub fn unknown(size: usize) -> Type {
        Type::Unknown(size)
    }
}
impl From<&str> for Type {
    fn from(item: &str) -> Self {
        Type::Ident(item.into())
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::ConstPointer(inner) => write!(f, "*const {inner}"),
            Type::MutPointer(inner) => write!(f, "*mut {inner}"),
            Type::Array(inner, size) => write!(f, "[{inner}; {size}]"),
            Type::Ident(ident) => write!(f, "{ident}"),
            Type::Unknown(size) => write!(f, "unknown({size})"),
        }
    }
}

impl Parser {
    pub(crate) fn parse_type(&mut self) -> Result<Located<Type>, ParseError> {
        match self.peek() {
            TokenKind::Unknown => {
                let start = self.advance();
                self.expect(TokenKind::Lt)?;
                let size = self.parse_int_literal()?.value as usize;
                let end = self.expect(TokenKind::Gt)?;

                Ok(Located::new(
                    Type::Unknown(size),
                    self.item_location_from_token_range(&start, &end),
                ))
            }
            TokenKind::Star => {
                let start = self.advance();
                if matches!(self.peek(), TokenKind::Const) {
                    self.advance();
                    let inner = self.parse_type()?;
                    let location = self.item_location_from_locations(
                        start.start_location(),
                        inner.location.span.end,
                    );
                    Ok(Located::new(Type::ConstPointer(Box::new(inner)), location))
                } else if matches!(self.peek(), TokenKind::Mut) {
                    self.advance();
                    let inner = self.parse_type()?;
                    let location = self.item_location_from_locations(
                        start.start_location(),
                        inner.location.span.end,
                    );
                    Ok(Located::new(Type::MutPointer(Box::new(inner)), location))
                } else {
                    Err(ParseError::MissingPointerQualifier {
                        location: self.current().location.clone(),
                    })
                }
            }
            TokenKind::LBracket => {
                let start = self.advance();
                let inner = self.parse_type()?;
                self.expect(TokenKind::Semi)?;
                let size = self.parse_int_literal()?.value as usize;
                let end = self.expect(TokenKind::RBracket)?;
                Ok(Located::new(
                    Type::Array(Box::new(inner), size),
                    self.item_location_from_token_range(&start, &end),
                ))
            }
            TokenKind::Ident(_) => {
                let (mut ident, ident_span) = self.expect_ident()?;
                let start_pos = ident_span.start;
                let mut end_pos = ident_span.end;

                // Check for generic arguments - treat them as part of the identifier string
                // This is needed for extern types that need exact reproduction
                if matches!(self.peek(), TokenKind::Lt) {
                    let mut type_str = ident.0;
                    type_str.push('<');
                    end_pos = self.advance().end_location(); // consume <

                    let mut depth = 1;
                    while depth > 0 && !matches!(self.peek(), TokenKind::Eof) {
                        match self.peek().clone() {
                            TokenKind::Lt => {
                                type_str.push('<');
                                depth += 1;
                            }
                            TokenKind::Gt => {
                                type_str.push('>');
                                depth -= 1;
                            }
                            TokenKind::Comma => {
                                type_str.push_str(", ");
                            }
                            TokenKind::Ident(name) => {
                                type_str.push_str(&name);
                            }
                            TokenKind::ColonColon => {
                                type_str.push_str("::");
                            }
                            _ => {}
                        }
                        end_pos = self.advance().end_location();
                    }
                    ident = Ident(type_str);
                }

                Ok(Located::new(
                    Type::Ident(ident),
                    self.item_location_from_locations(start_pos, end_pos),
                ))
            }
            _ => Err(ParseError::ExpectedType {
                found: self.peek().clone(),
                location: self.current().location.clone(),
            }),
        }
    }
}
