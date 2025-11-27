use crate::{
    grammar::{Argument, Attributes, Function, FunctionBlock, ImplItem},
    span::{ItemLocation, Span},
    tokenizer::TokenKind,
};

use super::{ParseError, core::Parser};

impl Parser {
    pub(crate) fn parse_impl_block(&mut self) -> Result<FunctionBlock, ParseError> {
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

            items.push(ImplItem::Function(self.parse_function()?));
        }

        self.expect(TokenKind::RBrace)?;

        Ok(FunctionBlock {
            name,
            items,
            attributes,
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
        let location = ItemLocation::new(self.filename.clone(), Span::new(start_pos, end_pos));

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
                Ok(Argument::MutSelf(tok.location.clone()))
            } else {
                let tok = self.expect(TokenKind::SelfValue)?;
                Ok(Argument::ConstSelf(tok.location.clone()))
            }
        } else {
            let start_pos = self.current().location.span.start;
            let (name, _) = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let type_ = self.parse_type()?;
            let end_pos = self.current().location.span.end;
            let location = ItemLocation::new(self.filename.clone(), Span::new(start_pos, end_pos));

            Ok(Argument::Named(name, type_, location))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        grammar::{
            AttributeItem, IntFormat,
            test_aliases::{int_literal_with_format, *},
        },
        parser::parse_str_for_tests,
        span::StripLocations,
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
                        T::ident("SharedPtr<PfxInstanceInterface>"),
                    ),
                ])
                .with_attributes([A::Function(
                    "size".into(),
                    vec![AttributeItem::Expr(int_literal_with_format(
                        0x10,
                        IntFormat::Hex,
                    ))],
                )]),
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
}
