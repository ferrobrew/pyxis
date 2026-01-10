use crate::{
    span::{HasLocation, ItemLocation},
    tokenizer::TokenKind,
};

#[cfg(test)]
use crate::span::StripLocations;

use super::{
    ParseError,
    attributes::{Attributes, Visibility},
    core::Parser,
    items::Comment,
    types::{Ident, Type},
};

#[cfg(test)]
use super::attributes::Attribute;

#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub enum Argument {
    ConstSelf {
        location: ItemLocation,
    },
    MutSelf {
        location: ItemLocation,
    },
    Named {
        ident: Ident,
        type_: Type,
        location: ItemLocation,
    },
}
#[cfg(test)]
impl Argument {
    pub fn const_self() -> Argument {
        Argument::ConstSelf {
            location: ItemLocation::test(),
        }
    }
    pub fn mut_self() -> Argument {
        Argument::MutSelf {
            location: ItemLocation::test(),
        }
    }
    pub fn named(ident: impl Into<Ident>, type_: impl Into<Type>) -> Argument {
        let type_ = type_.into();
        Argument::Named {
            ident: ident.into(),
            location: *type_.location(),
            type_,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct Function {
    pub visibility: Visibility,
    pub name: Ident,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    pub arguments: Vec<Argument>,
    pub return_type: Option<Type>,
    pub location: ItemLocation,
}
#[cfg(test)]
impl Function {
    pub fn new(
        (visibility, name): (Visibility, &str),
        arguments: impl IntoIterator<Item = Argument>,
    ) -> Self {
        Self {
            visibility,
            name: name.into(),
            attributes: Default::default(),
            doc_comments: vec![],
            arguments: arguments.into_iter().collect(),
            return_type: None,
            location: ItemLocation::test(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl IntoIterator<Item = Attribute>) -> Self {
        self.attributes = Attributes::from_iter(attributes);
        self
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
    pub fn with_return_type(mut self, return_type: impl Into<Type>) -> Self {
        self.return_type = Some(return_type.into());
        self
    }
}

/// Items in an impl block (preserves ordering and comments)
#[derive(Debug, Clone, PartialEq, Eq, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub enum ImplItem {
    Comment(Comment),
    Function(Function),
}

#[derive(Debug, Clone, PartialEq, Eq, HasLocation)]
pub struct FunctionBlock {
    pub name: Ident,
    pub items: Vec<ImplItem>,
    pub attributes: Attributes,
    pub location: ItemLocation,
}
#[cfg(test)]
impl StripLocations for FunctionBlock {
    fn strip_locations(&self) -> Self {
        FunctionBlock {
            name: self.name.strip_locations(),
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    ImplItem::Comment(_) => None, // Filter out comments
                    ImplItem::Function(f) => Some(ImplItem::Function(f.strip_locations())),
                })
                .collect(),
            attributes: self.attributes.strip_locations(),
            location: ItemLocation::test(),
        }
    }
}
#[cfg(test)]
impl FunctionBlock {
    pub fn new(name: impl Into<Ident>, functions: impl IntoIterator<Item = Function>) -> Self {
        Self {
            name: name.into(),
            items: functions.into_iter().map(ImplItem::Function).collect(),
            attributes: Default::default(),
            location: ItemLocation::test(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
}
impl FunctionBlock {
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.items.iter().filter_map(|item| match item {
            ImplItem::Function(func) => Some(func),
            _ => None,
        })
    }
}

impl Parser {
    pub(crate) fn parse_impl_block(&mut self) -> Result<FunctionBlock, ParseError> {
        let start_pos = self.current().location.span.start;
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        self.expect(TokenKind::Impl)?;
        let (name, _) = self.expect_ident()?;
        self.expect(TokenKind::LBrace)?;

        let mut items = Vec::new();
        while !matches!(self.peek(), TokenKind::RBrace) {
            // Collect non-doc comments (doc comments will be collected by parse_function)
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                if let Some(comment) = self.collect_comment() {
                    items.push(ImplItem::Comment(comment));
                }
            }

            if matches!(self.peek(), TokenKind::RBrace) {
                break;
            }

            let function = self.parse_function()?;
            items.push(ImplItem::Function(function));
        }

        let last_token = self.expect(TokenKind::RBrace)?;

        let location = self.item_location_from_locations(start_pos, last_token.end_location());
        Ok(FunctionBlock {
            name,
            items,
            attributes,
            location,
        })
    }

    pub(crate) fn parse_functions_in_block(&mut self) -> Result<Vec<Function>, ParseError> {
        let mut functions = Vec::new();

        while !matches!(self.peek(), TokenKind::RBrace) {
            // Skip regular comments but not doc comments (parse_function will collect those)
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                self.advance();
            }

            if matches!(self.peek(), TokenKind::RBrace) {
                break;
            }

            functions.push(self.parse_function()?);

            // Optional trailing comma
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            }
        }

        Ok(functions)
    }

    pub(crate) fn parse_function(&mut self) -> Result<Function, ParseError> {
        let start_pos = self.current().location.span.start;
        let mut doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        // Also collect doc comments that appear after attributes
        let after_attr_doc_comments = self.collect_doc_comments();
        doc_comments.extend(after_attr_doc_comments);

        let visibility = self.parse_visibility()?;
        self.expect(TokenKind::Fn)?;
        let (name, _) = self.expect_ident()?;
        self.expect(TokenKind::LParen)?;

        let mut arguments = Vec::new();
        while !matches!(self.peek(), TokenKind::RParen) {
            arguments.push(self.parse_argument()?);
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(TokenKind::RParen)?;

        let return_type = if matches!(self.peek(), TokenKind::Arrow) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenKind::Semi)?;
        let end_pos = if self.pos > 0 {
            self.tokens[self.pos - 1].location.span.end
        } else {
            self.current().location.span.end
        };
        let location = self.item_location_from_locations(start_pos, end_pos);

        Ok(Function {
            visibility,
            name,
            attributes,
            doc_comments,
            arguments,
            return_type,
            location,
        })
    }

    pub(crate) fn parse_argument(&mut self) -> Result<Argument, ParseError> {
        if matches!(self.peek(), TokenKind::Amp) {
            self.advance();
            if matches!(self.peek(), TokenKind::Mut) {
                self.advance();
                let tok = self.expect(TokenKind::SelfValue)?;
                Ok(Argument::MutSelf {
                    location: tok.location,
                })
            } else {
                let tok = self.expect(TokenKind::SelfValue)?;
                Ok(Argument::ConstSelf {
                    location: tok.location,
                })
            }
        } else {
            let start_pos = self.current().location.span.start;
            let (name, _) = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let type_ = self.parse_type()?;
            let end_pos = self.current().location.span.end;
            let location = self.item_location_from_locations(start_pos, end_pos);

            Ok(Argument::Named {
                ident: name,
                type_,
                location,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        grammar::test_aliases::*,
        parser::{error::ParseError, parse_str_for_tests},
        span::{ItemLocation, StripLocations},
        tokenizer::TokenKind,
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn can_parse_freestanding_functions() {
        let text = r#"
        enum A: i32 {}
        impl A {
            #[address(0x123)]
            fn test();
        }

        #[address(0x456)]
        pub fn freestanding();

        #[address(0x789)]
        fn another_freestanding(arg1: i32) -> i32;
        "#;

        let ast = M::new()
            .with_definitions([ID::new((V::Private, "A"), ED::new(T::ident("i32"), [], []))])
            .with_impls([FB::new(
                "A",
                [F::new((V::Private, "test"), []).with_attributes([A::address(0x123)])],
            )])
            .with_functions([
                F::new((V::Public, "freestanding"), []).with_attributes([A::address(0x456)]),
                F::new(
                    (V::Private, "another_freestanding"),
                    [Ar::named("arg1", T::ident("i32"))],
                )
                .with_attributes([A::address(0x789)])
                .with_return_type(T::ident("i32")),
            ]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_pfx_instance_with_vftable_and_impl() {
        let text = r#"
        #[size(0x10)]
        /// `CPfxInstance` in original game
        pub type PfxInstance {
            vftable {},
            pub instance: SharedPtr<PfxInstanceInterface>,
        }
        impl PfxInstance {
            #[address(0x6B7C40)]
            pub fn set_game_object(&mut self, game_object: *mut PfxGameObject);
        }
        "#;

        let ast = M::new()
            .with_definitions([ID::new(
                (V::Public, "PfxInstance"),
                TD::new([
                    TS::vftable([]),
                    TS::field(
                        (V::Public, "instance"),
                        T::generic("SharedPtr", [T::ident("PfxInstanceInterface")]),
                    ),
                ])
                .with_attributes([A::size(0x10)]),
            )
            .with_doc_comments(vec![" `CPfxInstance` in original game".to_string()])])
            .with_impls([FB::new(
                "PfxInstance",
                [F::new(
                    (V::Public, "set_game_object"),
                    [
                        Ar::mut_self(),
                        Ar::named("game_object", T::ident("PfxGameObject").mut_pointer()),
                    ],
                )
                .with_attributes([A::address(0x6B7C40)])],
            )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    // ========================================================================
    // Impl block error tests
    // ========================================================================

    #[test]
    fn impl_missing_name() {
        let text = r#"
        impl {
            fn test(&self);
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIdentifier {
                found: TokenKind::LBrace,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn impl_missing_opening_brace() {
        let text = r#"
        impl TestType
            fn test(&self);
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::LBrace],
                found: TokenKind::Fn,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn impl_missing_closing_brace() {
        let text = r#"
        impl TestType {
            fn test(&self);
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Fn],
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn impl_functions_using_comma_instead_of_semicolon() {
        let text = r#"
        impl TestType {
            fn test1(&self),
            fn test2(&self);
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Semi],
                found: TokenKind::Comma,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn impl_functions_missing_separator_entirely() {
        let text = r#"
        impl TestType {
            fn test1(&self)
            fn test2(&self);
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Semi],
                found: TokenKind::Fn,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn impl_functions_missing_separator_with_attributes() {
        let text = r#"
        impl TestType {
            fn test1(&self)
            #[address(0x123)]
            fn test2(&self);
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Semi],
                found: TokenKind::Hash,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn empty_impl_is_valid() {
        let text = r#"
        impl Test {}
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    // ========================================================================
    // Function signature error tests
    // ========================================================================

    #[test]
    fn incomplete_function() {
        let text = r#"
        pub fn test(
        "#;

        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIdentifier {
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn function_missing_name() {
        let text = r#"
        impl TestType {
            fn (&self);
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIdentifier {
                found: TokenKind::LParen,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn function_missing_opening_paren() {
        let text = r#"
        impl TestType {
            fn test;
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::LParen],
                found: TokenKind::Semi,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn function_missing_closing_paren() {
        let text = r#"
        impl TestType {
            fn test(&self;
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RParen],
                found: TokenKind::Semi,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn function_arg_missing_type() {
        let text = r#"
        impl TestType {
            fn test(arg:);
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedType {
                found: TokenKind::RParen,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn function_arg_missing_colon() {
        let text = r#"
        impl TestType {
            fn test(arg i32);
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::Colon],
                found: TokenKind::Ident("i32".to_string()),
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn function_with_only_comma_in_params() {
        let text = r#"
        impl Test {
            fn foo(,);
        }
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedIdentifier {
                found: TokenKind::Comma,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn function_with_trailing_comma_is_valid() {
        let text = r#"
        impl Test {
            fn foo(&self,);
        }
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    // ========================================================================
    // Freestanding function tests
    // ========================================================================

    #[test]
    fn freestanding_function_with_address_parses_ok() {
        let text = r#"
        #[address(0x123)]
        pub fn test();
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn freestanding_private_function_parses_ok() {
        let text = r#"
        #[address(0x123)]
        fn test();
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn pub_fn_without_attributes_parses_ok() {
        // `pub fn` at module level parses - semantic layer catches missing address
        let text = r#"
        pub fn test();
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn private_fn_without_attributes_parses_ok() {
        // Private `fn` at module level parses - semantic layer catches missing address
        let text = r#"
        fn test();
        "#;
        assert!(parse_str_for_tests(text).is_ok());
    }
}
