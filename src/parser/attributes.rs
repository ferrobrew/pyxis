use std::ops::{Deref, DerefMut};

use crate::{span::Located, tokenizer::TokenKind};

#[cfg(test)]
use crate::span::{ItemLocation, StripLocations};

use super::{ParseError, core::Parser, expressions::Expr, types::Ident};

#[cfg(test)]
use super::expressions::{IntFormat, StringFormat};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttributeItem {
    Expr(Expr),
    Comment(String),
}
#[cfg(test)]
impl StripLocations for AttributeItem {
    fn strip_locations(&self) -> Self {
        match self {
            AttributeItem::Expr(expr) => AttributeItem::Expr(expr.strip_locations()),
            AttributeItem::Comment(comment) => AttributeItem::Comment(comment.strip_locations()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttributeItems(pub Vec<Located<AttributeItem>>);
#[cfg(test)]
impl StripLocations for AttributeItems {
    fn strip_locations(&self) -> Self {
        AttributeItems(self.0.strip_locations())
    }
}
#[cfg(test)]
impl FromIterator<AttributeItem> for AttributeItems {
    fn from_iter<I: IntoIterator<Item = AttributeItem>>(iter: I) -> Self {
        AttributeItems(iter.into_iter().map(Located::test).collect())
    }
}
impl IntoIterator for AttributeItems {
    type Item = Located<AttributeItem>;
    type IntoIter = std::vec::IntoIter<Located<AttributeItem>>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<'a> IntoIterator for &'a AttributeItems {
    type Item = &'a Located<AttributeItem>;
    type IntoIter = std::slice::Iter<'a, Located<AttributeItem>>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
impl AttributeItems {
    pub fn exprs(&self) -> impl Iterator<Item = Located<&Expr>> {
        self.0.iter().filter_map(|item| {
            item.as_ref()
                .map(|item| match item {
                    AttributeItem::Expr(expr) => Some(expr),
                    _ => None,
                })
                .transpose()
        })
    }
    pub fn exprs_vec(&self) -> Vec<Located<&Expr>> {
        self.exprs().collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Attribute {
    Ident(Ident),
    /// Function attribute with expressions and comments
    /// Example: size(0x620 /* actually 0x61C */)
    Function(Ident, AttributeItems),
    /// Assign attribute with expression and optional comments
    /// Example: foo = bar /* comment */
    Assign(Ident, AttributeItems),
}
#[cfg(test)]
impl StripLocations for Attribute {
    fn strip_locations(&self) -> Self {
        match self {
            Attribute::Ident(i) => Attribute::Ident(i.strip_locations()),
            Attribute::Function(name, items) => {
                Attribute::Function(name.clone(), items.strip_locations())
            }
            Attribute::Assign(name, items) => {
                Attribute::Assign(name.clone(), items.strip_locations())
            }
        }
    }
}
#[cfg(test)]
impl Attribute {
    // Ident attributes
    pub fn copyable() -> Self {
        Attribute::Ident("copyable".into())
    }
    pub fn cloneable() -> Self {
        Attribute::Ident("cloneable".into())
    }
    pub fn defaultable() -> Self {
        Attribute::Ident("defaultable".into())
    }
    #[allow(clippy::should_implement_trait)]
    pub fn default() -> Self {
        Attribute::Ident("default".into())
    }
    pub fn base() -> Self {
        Attribute::Ident("base".into())
    }
    pub fn packed() -> Self {
        Attribute::Ident("packed".into())
    }

    pub fn integer_fn(name: &str, value: isize) -> Self {
        Attribute::Function(
            name.into(),
            AttributeItems(vec![Located::test(AttributeItem::Expr(Expr::IntLiteral {
                value,
                format: IntFormat::Decimal,
                location: ItemLocation::test(),
            }))]),
        )
    }
    fn integer_fn_hex(name: &str, value: isize) -> Self {
        Attribute::Function(
            name.into(),
            AttributeItems(vec![Located::test(AttributeItem::Expr(Expr::IntLiteral {
                value,
                format: IntFormat::Hex,
                location: ItemLocation::test(),
            }))]),
        )
    }
    pub fn address(address: usize) -> Self {
        Self::integer_fn_hex("address", address as isize)
    }
    pub fn size(size: usize) -> Self {
        Self::integer_fn("size", size as isize)
    }
    pub fn min_size(min_size: usize) -> Self {
        Self::integer_fn("min_size", min_size as isize)
    }
    pub fn align(align: usize) -> Self {
        Self::integer_fn("align", align as isize)
    }
    pub fn singleton(address: usize) -> Self {
        Self::integer_fn_hex("singleton", address as isize)
    }
    pub fn index(index: usize) -> Self {
        Self::integer_fn_hex("index", index as isize)
    }
    pub fn calling_convention(name: &str) -> Self {
        Attribute::Function(
            "calling_convention".into(),
            AttributeItems(vec![Located::test(AttributeItem::Expr(
                Expr::StringLiteral {
                    value: name.into(),
                    format: StringFormat::Regular,
                    location: ItemLocation::test(),
                },
            ))]),
        )
    }
}
impl Attribute {
    pub fn function(&self) -> Option<(&Ident, &AttributeItems)> {
        match self {
            Attribute::Function(ident, items) => Some((ident, items)),
            _ => None,
        }
    }
    pub fn assign(&self) -> Option<(&Ident, &AttributeItems)> {
        match self {
            Attribute::Assign(ident, items) => Some((ident, items)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Attributes(pub Vec<Located<Attribute>>);
#[cfg(test)]
impl StripLocations for Attributes {
    fn strip_locations(&self) -> Self {
        Attributes(self.0.strip_locations())
    }
}
#[cfg(test)]
impl FromIterator<Attribute> for Attributes {
    fn from_iter<I: IntoIterator<Item = Attribute>>(iter: I) -> Self {
        Attributes(iter.into_iter().map(Located::test).collect())
    }
}
impl IntoIterator for Attributes {
    type Item = Located<Attribute>;
    type IntoIter = std::vec::IntoIter<Located<Attribute>>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<'a> IntoIterator for &'a Attributes {
    type Item = &'a Located<Attribute>;
    type IntoIter = std::slice::Iter<'a, Located<Attribute>>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
impl Deref for Attributes {
    type Target = Vec<Located<Attribute>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Attributes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Visibility {
    Public,
    Private,
}
#[cfg(test)]
impl StripLocations for Visibility {
    fn strip_locations(&self) -> Self {
        *self
    }
}

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

                items.push(self.parse_expr_located()?.map(AttributeItem::Expr));

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

            let expr = self.parse_expr_located()?;
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
