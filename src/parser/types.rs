use std::fmt;

use crate::{
    span::{EqualsIgnoringLocations, HasLocation, ItemLocation},
    tokenizer::TokenKind,
};

#[cfg(test)]
use crate::span::StripLocations;

use super::{ParseError, core::Parser, paths::ItemPath};

/// A type parameter in a generic type definition (e.g., `T` in `type Shared<T>`)
#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct TypeParameter {
    pub name: String,
    pub location: ItemLocation,
}
impl EqualsIgnoringLocations for TypeParameter {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
#[cfg(test)]
impl TypeParameter {
    pub fn new(name: &str) -> Self {
        TypeParameter {
            name: name.to_string(),
            location: ItemLocation::test(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct Ident(pub String);
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
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
        /// Generic type arguments (e.g., `[GameObject, u32]` in `Map<GameObject, u32>`)
        generic_args: Vec<Type>,
        location: ItemLocation,
    },
    Unknown {
        size: usize,
        location: ItemLocation,
    },
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
            (
                Type::Ident {
                    path, generic_args, ..
                },
                Type::Ident {
                    path: path2,
                    generic_args: generic_args2,
                    ..
                },
            ) => {
                path.equals_ignoring_locations(path2)
                    && generic_args.equals_ignoring_locations(generic_args2)
            }
            (Type::Unknown { size, .. }, Type::Unknown { size: size2, .. }) => {
                size.equals_ignoring_locations(size2)
            }
            _ => false,
        }
    }
}
#[cfg(test)]
impl Type {
    pub fn ident(name: &str) -> Type {
        Type::Ident {
            path: name.into(),
            generic_args: vec![],
            location: ItemLocation::test(),
        }
    }

    pub fn generic(name: &str, args: impl IntoIterator<Item = Type>) -> Type {
        Type::Ident {
            path: name.into(),
            generic_args: args.into_iter().collect(),
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
            generic_args: vec![],
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
            Type::Ident {
                path, generic_args, ..
            } => {
                write!(f, "{path}")?;
                if !generic_args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in generic_args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{arg}")?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
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

                // Parse generic arguments properly as types
                let generic_args = if matches!(self.peek(), TokenKind::Lt) {
                    self.advance(); // consume <
                    let mut args = Vec::new();

                    // Parse first type argument (if any)
                    if !matches!(self.peek(), TokenKind::Gt) {
                        args.push(self.parse_type()?);

                        // Parse remaining comma-separated type arguments
                        while matches!(self.peek(), TokenKind::Comma) {
                            self.advance(); // consume ,
                            args.push(self.parse_type()?);
                        }
                    }

                    let gt_token = self.expect(TokenKind::Gt)?;
                    end_pos = gt_token.location.span.end;
                    args
                } else {
                    vec![]
                };

                let path: ItemPath = segments
                    .into_iter()
                    .map(|s| s.into())
                    .collect::<Vec<_>>()
                    .into_iter()
                    .collect();
                let location = self.item_location_from_locations(start_pos, end_pos);
                Ok(Type::Ident {
                    path,
                    generic_args,
                    location,
                })
            }
            _ => Err(ParseError::ExpectedType {
                found: self.peek().clone(),
                location: self.current().location,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{error::ParseError, parse_str_for_tests},
        span::{ItemLocation, StripLocations},
        tokenizer::TokenKind,
    };

    // ========================================================================
    // Pointer error tests
    // ========================================================================

    #[test]
    fn pointer_missing_qualifier_errors() {
        let text = r#"
        type Test {
            field: *i32,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::MissingPointerQualifier {
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn pointer_missing_target_type_errors() {
        let text = r#"
        type Test {
            field: *const,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedType {
                found: TokenKind::Comma,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn deeply_nested_pointer_is_valid() {
        let text = r#"
        type Test {
            field: *const *mut *const i32,
        }
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    // ========================================================================
    // Array error tests
    // ========================================================================

    #[test]
    fn array_missing_size_errors() {
        let text = r#"
        type Test {
            field: [i32;],
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIntLiteral {
                found: TokenKind::RBracket,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn array_missing_semicolon_between_type_and_size_errors() {
        let text = r#"
        type Test {
            field: [i32 4],
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Semi],
                found: TokenKind::IntLiteral("4".to_string()),
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn array_missing_closing_bracket_errors() {
        let text = r#"
        type Test {
            field: [i32; 4,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RBracket],
                found: TokenKind::Comma,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn array_missing_type_errors() {
        let text = r#"
        type Test {
            field: [; 4],
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedType {
                found: TokenKind::Semi,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn array_size_with_invalid_hex_errors() {
        let text = r#"
        type Test {
            field: [i32; 0xZZZ],
        }
        "#;
        // Parser catches the invalid hex literal (0x with no valid digits)
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::InvalidIntLiteral {
                kind: "hex".to_string(),
                value: "0x".to_string(),
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn array_of_pointers_is_valid() {
        let text = r#"
        type Test {
            field: [*mut i32; 4],
        }
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn pointer_to_array_is_valid() {
        let text = r#"
        type Test {
            field: *const [i32; 4],
        }
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn pointer_to_invalid_array_errors() {
        let text = r#"
        type Test {
            field: *const [i32],
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Semi],
                found: TokenKind::RBracket,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    // ========================================================================
    // Generic type error tests
    // ========================================================================

    #[test]
    fn generic_missing_closing_angle_errors() {
        let text = r#"
        type Container {
            field: Shared<i32,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        // After the comma, parser expects another type parameter
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedType {
                found: TokenKind::RBrace,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn generic_empty_type_params_in_field_parses_ok() {
        // Empty generic params in a field type reference parse OK - semantic layer catches
        let text = r#"
        type Container {
            field: Shared<>,
        }
        "#;
        // This actually parses fine, semantic layer would catch the invalid usage
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn nested_generic_missing_outer_closing_errors() {
        let text = r#"
        type Test {
            field: Outer<Inner<i32>,
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        // After comma, parser expects another type but finds closing brace
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedType {
                found: TokenKind::RBrace,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }
}
