use std::fmt;

use crate::{
    span::{EqualsIgnoringLocations, HasLocation, ItemLocation},
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
    ConstPointer {
        pointee: Box<Type>,
        location: ItemLocation,
    },
    MutPointer {
        pointee: Box<Type>,
        location: ItemLocation,
    },
    Array {
        element: Box<Type>,
        size: usize,
        location: ItemLocation,
    },
    Ident {
        ident: Ident,
        location: ItemLocation,
    },
    Unknown {
        size: usize,
        location: ItemLocation,
    },
}
impl HasLocation for Type {
    fn location(&self) -> &ItemLocation {
        match self {
            Type::ConstPointer { location, .. } => location,
            Type::MutPointer { location, .. } => location,
            Type::Array { location, .. } => location,
            Type::Ident { location, .. } => location,
            Type::Unknown { location, .. } => location,
        }
    }
}
impl EqualsIgnoringLocations for Type {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Type::ConstPointer { pointee, .. },
                Type::ConstPointer {
                    pointee: pointee2, ..
                },
            ) => pointee.equals_ignoring_locations(pointee2),
            (
                Type::MutPointer { pointee, .. },
                Type::MutPointer {
                    pointee: pointee2, ..
                },
            ) => pointee.equals_ignoring_locations(pointee2),
            (
                Type::Array { element, size, .. },
                Type::Array {
                    element: element2,
                    size: size2,
                    ..
                },
            ) => {
                element.equals_ignoring_locations(element2) && size.equals_ignoring_locations(size2)
            }
            (Type::Ident { ident, .. }, Type::Ident { ident: ident2, .. }) => {
                ident.equals_ignoring_locations(ident2)
            }
            (Type::Unknown { size, .. }, Type::Unknown { size: size2, .. }) => {
                size.equals_ignoring_locations(size2)
            }
            _ => false,
        }
    }
}
#[cfg(test)]
impl StripLocations for Type {
    fn strip_locations(&self) -> Self {
        match self {
            Type::ConstPointer { pointee, .. } => Type::ConstPointer {
                pointee: pointee.strip_locations(),
                location: ItemLocation::test(),
            },
            Type::MutPointer { pointee, .. } => Type::MutPointer {
                pointee: pointee.strip_locations(),
                location: ItemLocation::test(),
            },
            Type::Array { element, size, .. } => Type::Array {
                element: element.strip_locations(),
                size: size.strip_locations(),
                location: ItemLocation::test(),
            },
            Type::Ident { ident, .. } => Type::Ident {
                ident: ident.strip_locations(),
                location: ItemLocation::test(),
            },
            Type::Unknown { size, .. } => Type::Unknown {
                size: size.strip_locations(),
                location: ItemLocation::test(),
            },
        }
    }
}
#[cfg(test)]
impl Type {
    pub fn ident(name: &str) -> Type {
        Type::Ident {
            ident: name.into(),
            location: ItemLocation::test(),
        }
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Type::Ident { ident, .. } => Some(ident),
            _ => None,
        }
    }

    pub fn const_pointer(self) -> Type {
        Type::ConstPointer {
            pointee: Box::new(self),
            location: ItemLocation::test(),
        }
    }

    pub fn mut_pointer(self) -> Type {
        Type::MutPointer {
            pointee: Box::new(self),
            location: ItemLocation::test(),
        }
    }

    pub fn array(self, size: usize) -> Type {
        Type::Array {
            element: Box::new(self),
            size,
            location: ItemLocation::test(),
        }
    }

    pub fn unknown(size: usize) -> Type {
        Type::Unknown {
            size,
            location: ItemLocation::test(),
        }
    }
}
impl From<&str> for Type {
    fn from(item: &str) -> Self {
        Type::Ident {
            ident: item.into(),
            location: ItemLocation::internal(),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::ConstPointer { pointee, .. } => write!(f, "*const {pointee}"),
            Type::MutPointer { pointee, .. } => write!(f, "*mut {pointee}"),
            Type::Array { element, size, .. } => write!(f, "[{element}; {size}]"),
            Type::Ident { ident, .. } => write!(f, "{ident}"),
            Type::Unknown { size, .. } => write!(f, "unknown({size})"),
        }
    }
}

impl Parser {
    pub(crate) fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.peek() {
            TokenKind::Unknown => {
                let start = self.advance();
                self.expect(TokenKind::Lt)?;
                let (size, _) = self.parse_int_literal()?;
                let size = size as usize;
                let end = self.expect(TokenKind::Gt)?;
                let location = self.item_location_from_token_range(&start, &end);

                Ok(Type::Unknown { size, location })
            }
            TokenKind::Star => {
                let start = self.advance();
                if matches!(self.peek(), TokenKind::Const) {
                    self.advance();
                    let pointee = self.parse_type()?;
                    let location = self.item_location_from_locations(
                        start.start_location(),
                        pointee.location().span.end,
                    );
                    Ok(Type::ConstPointer {
                        pointee: Box::new(pointee),
                        location,
                    })
                } else if matches!(self.peek(), TokenKind::Mut) {
                    self.advance();
                    let pointee = self.parse_type()?;
                    let location = self.item_location_from_locations(
                        start.start_location(),
                        pointee.location().span.end,
                    );
                    Ok(Type::MutPointer {
                        pointee: Box::new(pointee),
                        location,
                    })
                } else {
                    Err(ParseError::MissingPointerQualifier {
                        location: self.current().location.clone(),
                    })
                }
            }
            TokenKind::LBracket => {
                let start = self.advance();
                let element = self.parse_type()?;
                self.expect(TokenKind::Semi)?;
                let (size, _) = self.parse_int_literal()?;
                let size = size as usize;
                let end = self.expect(TokenKind::RBracket)?;
                let location = self.item_location_from_token_range(&start, &end);
                Ok(Type::Array {
                    element: Box::new(element),
                    size,
                    location,
                })
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

                let location = self.item_location_from_locations(start_pos, end_pos);
                Ok(Type::Ident { ident, location })
            }
            _ => Err(ParseError::ExpectedType {
                found: self.peek().clone(),
                location: self.current().location.clone(),
            }),
        }
    }
}
