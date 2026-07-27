use std::fmt;

use crate::{
    span::{EqualsIgnoringLocations, HasLocation, ItemLocation},
    tokenizer::TokenKind,
};

#[cfg(test)]
use crate::span::StripLocations;

use super::{ParseError, attributes::Attributes, core::Parser, paths::ItemPath};

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

/// A type expression. Attributes may precede any type (`#[calling_convention(cdecl)] fn()`);
/// the semantic layer decides which kinds actually consume them and rejects the rest.
#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation, EqualsIgnoringLocations)]
#[cfg_attr(test, derive(StripLocations))]
pub struct Type {
    pub attributes: Attributes,
    pub kind: TypeKind,
    pub location: ItemLocation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EqualsIgnoringLocations)]
#[cfg_attr(test, derive(StripLocations))]
pub enum TypeKind {
    ConstPointer {
        pointee: Box<Type>,
    },
    MutPointer {
        pointee: Box<Type>,
    },
    Array {
        element: Box<Type>,
        size: usize,
    },
    Ident {
        path: ItemPath,
        /// Generic type arguments (e.g., `[GameObject, u32]` in `Map<GameObject, u32>`)
        generic_args: Vec<Type>,
    },
    Unknown {
        size: usize,
    },
    /// A function-pointer type: `fn(*mut Engine, f32) -> bool`. The calling
    /// convention comes from a `#[calling_convention(...)]` attribute on the
    /// type, defaulting the same way a freestanding function does.
    Function {
        arguments: Vec<FunctionArg>,
        return_type: Option<Box<Type>>,
    },
}

/// One parameter of a function-pointer type. The name is optional -
/// `fn(u32)` and `fn(count: u32)` are both accepted, and an unnamed
/// parameter stays unnamed all the way through to the emitted code.
#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation, EqualsIgnoringLocations)]
#[cfg_attr(test, derive(StripLocations))]
pub struct FunctionArg {
    pub name: Option<Ident>,
    pub type_: Type,
    pub location: ItemLocation,
}
#[cfg(test)]
impl FunctionArg {
    pub fn new(name: Option<&str>, type_: Type) -> Self {
        FunctionArg {
            name: name.map(Ident::from),
            type_,
            location: ItemLocation::test(),
        }
    }
}

#[cfg(test)]
use super::attributes::Attribute;

#[cfg(test)]
impl Type {
    pub fn ident(name: &str) -> Type {
        TypeKind::Ident {
            path: name.into(),
            generic_args: vec![],
        }
        .into_type()
    }

    pub fn generic(name: &str, args: impl IntoIterator<Item = Type>) -> Type {
        TypeKind::Ident {
            path: name.into(),
            generic_args: args.into_iter().collect(),
        }
        .into_type()
    }

    pub fn const_pointer(self) -> Type {
        TypeKind::ConstPointer {
            pointee: Box::new(self),
        }
        .into_type()
    }

    pub fn mut_pointer(self) -> Type {
        TypeKind::MutPointer {
            pointee: Box::new(self),
        }
        .into_type()
    }

    pub fn array(self, size: usize) -> Type {
        TypeKind::Array {
            element: Box::new(self),
            size,
        }
        .into_type()
    }

    pub fn unknown(size: usize) -> Type {
        TypeKind::Unknown { size }.into_type()
    }

    pub fn function(
        arguments: impl IntoIterator<Item = FunctionArg>,
        return_type: Option<Type>,
    ) -> Type {
        TypeKind::Function {
            arguments: arguments.into_iter().collect(),
            return_type: return_type.map(Box::new),
        }
        .into_type()
    }

    pub fn with_attributes(mut self, attributes: impl IntoIterator<Item = Attribute>) -> Type {
        self.attributes = Attributes(attributes.into_iter().collect());
        self
    }
}

#[cfg(test)]
impl TypeKind {
    fn into_type(self) -> Type {
        Type::new(Attributes(vec![]), self, ItemLocation::test())
    }
}

impl Type {
    pub fn new(attributes: Attributes, kind: TypeKind, location: ItemLocation) -> Self {
        Type {
            attributes,
            kind,
            location,
        }
    }

    /// Returns the item path if this is an `Ident` type, `None` otherwise.
    pub fn as_path(&self) -> Option<&ItemPath> {
        match &self.kind {
            TypeKind::Ident { path, .. } => Some(path),
            _ => None,
        }
    }
}
impl From<&str> for Type {
    fn from(item: &str) -> Self {
        Type::new(
            Attributes(vec![]),
            TypeKind::Ident {
                path: item.into(),
                generic_args: vec![],
            },
            ItemLocation::internal(),
        )
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Attributes are deliberately not printed: this rendering feeds
        // error messages and the extern-type name lookup in
        // `resolve_grammar_type`, both of which want the bare type name.
        // The pretty-printer reconstructs attributes itself.
        match &self.kind {
            TypeKind::ConstPointer { pointee, .. } => write!(f, "*const {pointee}"),
            TypeKind::MutPointer { pointee, .. } => write!(f, "*mut {pointee}"),
            TypeKind::Array { element, size, .. } => write!(f, "[{element}; {size}]"),
            TypeKind::Ident {
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
            TypeKind::Unknown { size, .. } => write!(f, "unknown({size})"),
            TypeKind::Function {
                arguments,
                return_type,
            } => {
                write!(f, "fn(")?;
                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    if let Some(name) = &arg.name {
                        write!(f, "{name}: ")?;
                    }
                    write!(f, "{}", arg.type_)?;
                }
                write!(f, ")")?;
                if let Some(return_type) = return_type {
                    write!(f, " -> {return_type}")?;
                }
                Ok(())
            }
        }
    }
}

impl Parser {
    /// Parse a type expression, including any attributes that precede it.
    pub(crate) fn parse_type(&mut self) -> Result<Type, ParseError> {
        let attributes = self.parse_attributes()?;
        let attributes_start = attributes
            .0
            .first()
            .map(|a| a.location().span.start)
            .unwrap_or_else(|| self.current().location.span.start);

        let (kind, kind_end) = self.parse_type_kind()?;
        let location = self.item_location_from_locations(attributes_start, kind_end);
        Ok(Type::new(attributes, kind, location))
    }

    /// Parse the type expression itself, returning the kind and the source
    /// position just past it.
    fn parse_type_kind(&mut self) -> Result<(TypeKind, crate::span::Location), ParseError> {
        match self.peek() {
            TokenKind::Unknown => {
                self.advance();
                self.expect(TokenKind::Lt)?;
                let (size, _) = self.parse_int_literal()?;
                let size = size as usize;
                let end = self.expect(TokenKind::Gt)?;
                Ok((TypeKind::Unknown { size }, end.location.span.end))
            }
            TokenKind::Star => {
                self.advance();
                let is_const = match self.peek() {
                    TokenKind::Const => true,
                    TokenKind::Mut => false,
                    _ => {
                        return Err(ParseError::MissingPointerQualifier {
                            location: self.current().location,
                        });
                    }
                };
                self.advance();
                let pointee = self.parse_type()?;
                let end = pointee.location().span.end;
                let pointee = Box::new(pointee);
                Ok((
                    if is_const {
                        TypeKind::ConstPointer { pointee }
                    } else {
                        TypeKind::MutPointer { pointee }
                    },
                    end,
                ))
            }
            TokenKind::LBracket => {
                self.advance();
                let element = self.parse_type()?;
                self.expect(TokenKind::Semi)?;
                let (size, _) = self.parse_int_literal()?;
                let size = size as usize;
                let end = self.expect(TokenKind::RBracket)?;
                Ok((
                    TypeKind::Array {
                        element: Box::new(element),
                        size,
                    },
                    end.location.span.end,
                ))
            }
            TokenKind::Fn => {
                self.advance();
                self.expect(TokenKind::LParen)?;

                let mut arguments = Vec::new();
                while !matches!(self.peek(), TokenKind::RParen) {
                    arguments.push(self.parse_function_type_argument()?);
                    if matches!(self.peek(), TokenKind::Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }
                let rparen = self.expect(TokenKind::RParen)?;
                let mut end = rparen.location.span.end;

                let return_type = if matches!(self.peek(), TokenKind::Arrow) {
                    self.advance();
                    let return_type = self.parse_type()?;
                    end = return_type.location().span.end;
                    Some(Box::new(return_type))
                } else {
                    None
                };

                Ok((
                    TypeKind::Function {
                        arguments,
                        return_type,
                    },
                    end,
                ))
            }
            TokenKind::Ident(_) => {
                let (first_ident, ident_span) = self.expect_ident()?;
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
                Ok((TypeKind::Ident { path, generic_args }, end_pos))
            }
            _ => Err(ParseError::ExpectedType {
                found: self.peek().clone(),
                location: self.current().location,
            }),
        }
    }

    /// A function-type parameter: `name: T` or just `T`. The name is only
    /// taken when an identifier is directly followed by `:`, so a bare type
    /// path stays a type. `_` is accepted as a name, matching field position.
    fn parse_function_type_argument(&mut self) -> Result<FunctionArg, ParseError> {
        let start = self.current().location.span.start;
        let named = matches!(self.peek(), TokenKind::Ident(_) | TokenKind::Underscore)
            && matches!(self.peek_nth(1), TokenKind::Colon);
        let name = if named {
            let name = match self.peek() {
                TokenKind::Underscore => {
                    self.advance();
                    Ident::from("_")
                }
                _ => self.expect_ident()?.0,
            };
            self.advance(); // consume :
            Some(name)
        } else {
            None
        };
        let type_ = self.parse_type()?;
        let location = self.item_location_from_locations(start, type_.location().span.end);
        Ok(FunctionArg {
            name,
            type_,
            location,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{FunctionArg, Type};
    use crate::{
        grammar::Attribute,
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

    // ========================================================================
    // Function pointer types
    // ========================================================================

    /// Pull the single field's type out of `type Test { field: <ty>, }`.
    fn field_type(text: &str) -> Type {
        use crate::grammar::{ItemDefinitionInner, ModuleItem, TypeDefItem, TypeField};
        let module = parse_str_for_tests(text).unwrap();
        let ModuleItem::Definition { definition } = &module.items[0] else {
            panic!("expected an item definition")
        };
        let ItemDefinitionInner::Type(type_definition) = &definition.inner else {
            panic!("expected a type")
        };
        let TypeDefItem::Statement(statement) = &type_definition.items[0] else {
            panic!("expected a statement")
        };
        let TypeField::Field(_, _, type_) = &statement.field else {
            panic!("expected a field")
        };
        type_.strip_locations()
    }

    #[test]
    fn function_pointer_parses_with_and_without_argument_names() {
        assert_eq!(
            field_type("type Test { field: fn(engine: *mut Engine, dt: f32), }"),
            Type::function(
                [
                    FunctionArg::new(Some("engine"), Type::ident("Engine").mut_pointer()),
                    FunctionArg::new(Some("dt"), Type::ident("f32")),
                ],
                None,
            )
        );
        assert_eq!(
            field_type("type Test { field: fn(*mut Engine, f32), }"),
            Type::function(
                [
                    FunctionArg::new(None, Type::ident("Engine").mut_pointer()),
                    FunctionArg::new(None, Type::ident("f32")),
                ],
                None,
            )
        );
    }

    #[test]
    fn function_pointer_parses_empty_arguments_and_return_type() {
        assert_eq!(
            field_type("type Test { field: fn(), }"),
            Type::function([], None)
        );
        assert_eq!(
            field_type("type Test { field: fn() -> bool, }"),
            Type::function([], Some(Type::ident("bool")))
        );
    }

    #[test]
    fn function_pointer_accepts_underscore_as_an_argument_name() {
        // `_` names a field, so it should name a parameter too.
        assert_eq!(
            field_type("type Test { field: fn(_: u32), }"),
            Type::function([FunctionArg::new(Some("_"), Type::ident("u32"))], None)
        );
    }

    #[test]
    fn function_pointer_accepts_a_trailing_comma() {
        assert_eq!(
            field_type("type Test { field: fn(a: u32,), }"),
            Type::function([FunctionArg::new(Some("a"), Type::ident("u32"))], None)
        );
    }

    #[test]
    fn function_pointer_nests_in_pointers_arrays_and_itself() {
        assert_eq!(
            field_type("type Test { field: [fn(u32); 4], }"),
            Type::function([FunctionArg::new(None, Type::ident("u32"))], None).array(4)
        );
        assert_eq!(
            field_type("type Test { field: *mut fn(), }"),
            Type::function([], None).mut_pointer()
        );
        assert_eq!(
            field_type("type Test { field: fn(cb: fn(u32)) -> fn(), }"),
            Type::function(
                [FunctionArg::new(
                    Some("cb"),
                    Type::function([FunctionArg::new(None, Type::ident("u32"))], None)
                )],
                Some(Type::function([], None)),
            )
        );
    }

    #[test]
    fn attributes_attach_to_the_type_they_precede() {
        assert_eq!(
            field_type("type Test { field: #[calling_convention(cdecl)] fn(), }"),
            Type::function([], None).with_attributes([Attribute::calling_convention("cdecl")])
        );
        // Inside an array, the attribute binds to the element, not the array.
        assert_eq!(
            field_type("type Test { field: [#[calling_convention(cdecl)] fn(); 2], }"),
            Type::function([], None)
                .with_attributes([Attribute::calling_convention("cdecl")])
                .array(2)
        );
    }

    #[test]
    fn function_pointer_missing_parens_errors() {
        let err = parse_str_for_tests("type Test { field: fn, }").unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::LParen],
                found: TokenKind::Comma,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn function_pointer_missing_return_type_errors() {
        let err = parse_str_for_tests("type Test { field: fn() ->, }").unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedType {
                found: TokenKind::Comma,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }
}
