use crate::{
    grammar::{Attribute, AttributeItem, AttributeItems, Attributes, Visibility},
    span::Located,
    tokenizer::TokenKind,
};

use super::{ParseError, core::Parser};

impl Parser {
    pub(crate) fn parse_visibility(&mut self) -> Result<Visibility, ParseError> {
        if matches!(self.peek(), TokenKind::Pub) {
            self.advance();
            Ok(Visibility::Public)
        } else {
            Ok(Visibility::Private)
        }
    }

    pub(crate) fn parse_attributes(&mut self) -> Result<Attributes, ParseError> {
        let mut attrs = Vec::new();

        while matches!(self.peek(), TokenKind::Hash) {
            self.advance();
            self.expect(TokenKind::LBracket)?;

            while !matches!(self.peek(), TokenKind::RBracket) {
                attrs.push(self.parse_attribute()?);
                if matches!(self.peek(), TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }

            self.expect(TokenKind::RBracket)?;
        }

        Ok(Attributes(attrs))
    }

    pub(crate) fn parse_attribute(&mut self) -> Result<Located<Attribute>, ParseError> {
        let (name, name_span) = self.expect_ident()?;
        let start_pos = name_span.start;

        if matches!(self.peek(), TokenKind::LParen) {
            // Function attribute
            self.advance();
            let mut items = Vec::new();
            while !matches!(self.peek(), TokenKind::RParen) {
                // Collect any comments before the expression
                while matches!(
                    self.peek(),
                    TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
                ) {
                    let token = self.advance();
                    let comment_text = match &token.kind {
                        TokenKind::Comment(text) => text.clone(),
                        TokenKind::MultiLineComment(text) => text.clone(),
                        _ => unreachable!(),
                    };
                    items.push(Located::new(
                        AttributeItem::Comment(comment_text),
                        token.location,
                    ));
                }

                // Skip doc comments (they don't belong in attribute expressions)
                while matches!(self.peek(), TokenKind::DocOuter(_) | TokenKind::DocInner(_)) {
                    self.advance();
                }

                if matches!(self.peek(), TokenKind::RParen) {
                    break;
                }

                items.push(self.parse_expr()?.map(AttributeItem::Expr));

                // Collect trailing comments after the expression
                while matches!(
                    self.peek(),
                    TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
                ) {
                    let token = self.advance();
                    let comment_text = match &token.kind {
                        TokenKind::Comment(text) => text.clone(),
                        TokenKind::MultiLineComment(text) => text.clone(),
                        _ => unreachable!(),
                    };
                    items.push(Located::new(
                        AttributeItem::Comment(comment_text),
                        token.location.clone(),
                    ));
                }

                if matches!(self.peek(), TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
            let end_pos = self.expect(TokenKind::RParen)?.end_location();
            let location = self.item_location_from_locations(start_pos, end_pos);
            Ok(Located::new(
                Attribute::Function(name, AttributeItems(items)),
                location,
            ))
        } else if matches!(self.peek(), TokenKind::Eq) {
            // Assign attribute
            self.advance();
            let mut items = Vec::new();

            // Collect comments before the expression
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                let token = self.advance();
                let comment_text = match &token.kind {
                    TokenKind::Comment(text) => text.clone(),
                    TokenKind::MultiLineComment(text) => text.clone(),
                    _ => unreachable!(),
                };
                items.push(Located::new(
                    AttributeItem::Comment(comment_text),
                    token.location,
                ));
            }

            let expr = self.parse_expr()?;
            let mut end_pos = expr.location.span.end;
            items.push(expr.map(AttributeItem::Expr));

            // Collect comments after the expression
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                let token = self.advance();
                let comment_text = match &token.kind {
                    TokenKind::Comment(text) => text.clone(),
                    TokenKind::MultiLineComment(text) => text.clone(),
                    _ => unreachable!(),
                };
                items.push(Located::new(
                    AttributeItem::Comment(comment_text),
                    token.location.clone(),
                ));
                end_pos = token.location.span.end;
            }

            let location = self.item_location_from_locations(start_pos, end_pos);
            Ok(Located::new(
                Attribute::Assign(name, AttributeItems(items)),
                location,
            ))
        } else {
            // Ident attribute
            let end_pos = name_span.end;
            let location = self.item_location_from_locations(start_pos, end_pos);
            Ok(Located::new(Attribute::Ident(name), location))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        grammar::{
            IntFormat, ItemDefinitionInner, ModuleItem, TypeDefItem, Visibility,
            test_aliases::{int_literal_with_format, *},
        },
        parser::parse_str_for_tests,
        span::StripLocations,
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn can_parse_ident_attributes() {
        let text = r#"
        #[copyable, cloneable]
        type TestType {
            field_1: i32,
        }
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Private, "TestType"),
            TD::new([TS::field((V::Private, "field_1"), T::ident("i32"))])
                .with_attributes([A::copyable(), A::cloneable()]),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_multiple_attributes_with_underscored_literals() {
        let text = r#"
        #[singleton(0x1_18F_B64), size(0x40), align(16)] // 0x3C
        pub type InputDeviceManager {
            #[address(0x18)]
            pub enabled: bool,

            #[address(0x38)]
            pub in_focus: bool,
        }
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Public, "InputDeviceManager"),
            TD::new([
                TS::field((V::Public, "enabled"), T::ident("bool"))
                    .with_attributes([A::address(0x18)]),
                TS::field((V::Public, "in_focus"), T::ident("bool"))
                    .with_attributes([A::address(0x38)]),
            ])
            .with_attributes([
                A::singleton(0x118FB64),
                A::Function(
                    "size".into(),
                    AIs::from_iter([AI::Expr(int_literal_with_format(0x40, IntFormat::Hex))]),
                ),
                A::align(16),
            ]),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_type_with_comment_in_attribute() {
        let text = r#"
#[singleton(0x1_18F_C20), size(0x620 /* actually 0x61C */), align(16)]
pub type AnarkGui {
    vftable {},

    #[address(0x1A0)]
    pub next_state: AnarkState,
    pub active_state: AnarkState,
}
    "#;

        let module = parse_str_for_tests(text).unwrap().strip_locations();

        // Verify we have one type definition
        assert_eq!(module.items.len(), 1);

        // Verify it's the correct type with attributes and fields
        match &module.items[0].value {
            ModuleItem::Definition(def) => {
                assert_eq!(def.name.0, "AnarkGui");
                assert_eq!(def.visibility, Visibility::Public);

                // Check the type has attributes
                if let ItemDefinitionInner::Type(td) = &def.inner {
                    assert_eq!(td.attributes.0.len(), 3); // singleton, size, align

                    // Verify we have vftable and two fields
                    // vftable + 2 fields = 3 statements
                    let statement_count = td
                        .items
                        .iter()
                        .filter(|item| matches!(item.value, TypeDefItem::Statement(_)))
                        .count();
                    assert_eq!(statement_count, 3);
                } else {
                    panic!("Expected Type definition");
                }
            }
            _ => panic!("Expected Definition"),
        }
    }
}
