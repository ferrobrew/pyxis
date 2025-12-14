use std::fmt;

use crate::{
    span::{EqualsIgnoringLocations, HasLocation, ItemLocation},
    tokenizer::TokenKind,
};

#[cfg(test)]
use crate::span::StripLocations;

use super::{ParseError, core::Parser, paths::ItemPath};

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
        path: ItemPath,
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
            (Type::Ident { path, .. }, Type::Ident { path: path2, .. }) => {
                path.equals_ignoring_locations(path2)
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
            Type::Ident { path, .. } => Type::Ident {
                path: path.strip_locations(),
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
            path: name.into(),
            location: ItemLocation::test(),
        }
    }

    pub fn as_path(&self) -> Option<&ItemPath> {
        match self {
            Type::Ident { path, .. } => Some(path),
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
            path: item.into(),
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
            Type::Ident { path, .. } => write!(f, "{path}"),
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
                        location: self.current().location,
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
                let (first_ident, ident_span) = self.expect_ident()?;
                let start_pos = ident_span.start;
                let mut end_pos = ident_span.end;
                let mut segments = vec![first_ident.0];

                // Handle paths like module::Type - continue parsing while we see ::
                while matches!(self.peek(), TokenKind::ColonColon) {
                    self.advance(); // consume ::
                    let (next_ident, next_span) = self.expect_ident()?;
                    segments.push(next_ident.0);
                    end_pos = next_span.end;
                }

                // Check for generic arguments on the last segment
                // This is needed for extern types that need exact reproduction
                if matches!(self.peek(), TokenKind::Lt) {
                    let mut generic_str = String::from("<");
                    end_pos = self.advance().end_location(); // consume <

                    let mut depth = 1;
                    while depth > 0 && !matches!(self.peek(), TokenKind::Eof) {
                        match self.peek().clone() {
                            TokenKind::Lt => {
                                generic_str.push('<');
                                depth += 1;
                            }
                            TokenKind::Gt => {
                                generic_str.push('>');
                                depth -= 1;
                            }
                            TokenKind::Comma => {
                                generic_str.push_str(", ");
                            }
                            TokenKind::Ident(name) => {
                                generic_str.push_str(&name);
                            }
                            TokenKind::ColonColon => {
                                generic_str.push_str("::");
                            }
                            _ => {}
                        }
                        end_pos = self.advance().end_location();
                    }
                    // Append generic args to the last segment
                    if let Some(last) = segments.last_mut() {
                        last.push_str(&generic_str);
                    }
                }

                let path: ItemPath = segments
                    .into_iter()
                    .map(|s| s.into())
                    .collect::<Vec<_>>()
                    .into_iter()
                    .collect();
                let location = self.item_location_from_locations(start_pos, end_pos);
                Ok(Type::Ident { path, location })
            }
            _ => Err(ParseError::ExpectedType {
                found: self.peek().clone(),
                location: self.current().location,
            }),
        }
    }
}
