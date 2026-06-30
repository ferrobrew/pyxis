use std::ops::{Deref, DerefMut};

use crate::{span::ItemLocation, tokenizer::TokenKind};

#[cfg(test)]
use crate::span::StripLocations;

use super::{ParseError, core::Parser, expressions::Expr, types::Ident};

#[cfg(test)]
use super::expressions::{IntFormat, StringFormat};

#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub enum AttributeItem {
    Expr {
        expr: Expr,
        location: ItemLocation,
    },
    Comment {
        text: String,
        location: ItemLocation,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct AttributeItems(pub Vec<AttributeItem>);
#[cfg(test)]
impl FromIterator<AttributeItem> for AttributeItems {
    fn from_iter<I: IntoIterator<Item = AttributeItem>>(iter: I) -> Self {
        AttributeItems(iter.into_iter().collect())
    }
}
impl IntoIterator for AttributeItems {
    type Item = AttributeItem;
    type IntoIter = std::vec::IntoIter<AttributeItem>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<'a> IntoIterator for &'a AttributeItems {
    type Item = &'a AttributeItem;
    type IntoIter = std::slice::Iter<'a, AttributeItem>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
impl AttributeItems {
    pub fn exprs(&self) -> impl Iterator<Item = &Expr> {
        self.0.iter().filter_map(|item| match item {
            AttributeItem::Expr { expr, .. } => Some(expr),
            _ => None,
        })
    }
    pub fn exprs_vec(&self) -> Vec<&Expr> {
        self.exprs().collect()
    }
}

use crate::span::HasLocation;

#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub enum Attribute {
    Ident {
        ident: Ident,
        location: ItemLocation,
    },
    /// Function attribute with expressions and comments
    /// Example: size(0x620 /* actually 0x61C */)
    Function {
        name: Ident,
        items: AttributeItems,
        location: ItemLocation,
    },
    /// Assign attribute with expression and optional comments
    /// Example: foo = bar /* comment */
    Assign {
        name: Ident,
        items: AttributeItems,
        location: ItemLocation,
    },
    /// `#[cfg(...)]` predicate. Parsed into a structured AST since the
    /// nested `any(...)` / `all(...)` / `not(...)` forms can't be
    /// represented by `AttributeItems`.
    Cfg {
        predicate: crate::parser::cfg::CfgPredicate,
        location: ItemLocation,
    },
}
#[cfg(test)]
impl Attribute {
    // Ident attributes
    pub fn copyable() -> Self {
        Attribute::Ident {
            ident: "copyable".into(),
            location: ItemLocation::test(),
        }
    }
    pub fn cloneable() -> Self {
        Attribute::Ident {
            ident: "cloneable".into(),
            location: ItemLocation::test(),
        }
    }
    pub fn defaultable() -> Self {
        Attribute::Ident {
            ident: "defaultable".into(),
            location: ItemLocation::test(),
        }
    }
    #[allow(clippy::should_implement_trait)]
    pub fn default() -> Self {
        Attribute::Ident {
            ident: "default".into(),
            location: ItemLocation::test(),
        }
    }
    pub fn base() -> Self {
        Attribute::Ident {
            ident: "base".into(),
            location: ItemLocation::test(),
        }
    }
    pub fn packed() -> Self {
        Attribute::Ident {
            ident: "packed".into(),
            location: ItemLocation::test(),
        }
    }
    pub fn external_body() -> Self {
        Attribute::Ident {
            ident: "external_body".into(),
            location: ItemLocation::test(),
        }
    }
    /// The (intentionally wrong) `#[external_body(...)]` function form,
    /// used by semantic tests that exercise `AttributeWrongForm`.
    pub fn external_body_function_form() -> Self {
        Attribute::Function {
            name: "external_body".into(),
            items: AttributeItems(vec![]),
            location: ItemLocation::test(),
        }
    }
    /// The (intentionally wrong) `#[external_body = ...]` assign form,
    /// used by semantic tests that exercise `AttributeWrongForm`.
    pub fn external_body_assign_form() -> Self {
        Attribute::Assign {
            name: "external_body".into(),
            items: AttributeItems(vec![AttributeItem::Expr {
                expr: Expr::IntLiteral {
                    value: 0,
                    format: IntFormat::Decimal,
                    location: ItemLocation::test(),
                },
                location: ItemLocation::test(),
            }]),
            location: ItemLocation::test(),
        }
    }

    pub fn integer_fn(attr_name: &str, value: isize) -> Self {
        Attribute::Function {
            name: attr_name.into(),
            items: AttributeItems(vec![AttributeItem::Expr {
                expr: Expr::IntLiteral {
                    value,
                    format: IntFormat::Decimal,
                    location: ItemLocation::test(),
                },
                location: ItemLocation::test(),
            }]),
            location: ItemLocation::test(),
        }
    }
    fn integer_fn_hex(attr_name: &str, value: isize) -> Self {
        Attribute::Function {
            name: attr_name.into(),
            items: AttributeItems(vec![AttributeItem::Expr {
                expr: Expr::IntLiteral {
                    value,
                    format: IntFormat::Hex,
                    location: ItemLocation::test(),
                },
                location: ItemLocation::test(),
            }]),
            location: ItemLocation::test(),
        }
    }
    pub fn address(address: usize) -> Self {
        Self::integer_fn_hex("address", address as isize)
    }
    pub fn size(size: usize) -> Self {
        Self::integer_fn_hex("size", size as isize)
    }
    pub fn size_decimal(size: usize) -> Self {
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
    pub fn calling_convention(conv_name: &str) -> Self {
        Attribute::Function {
            name: "calling_convention".into(),
            items: AttributeItems(vec![AttributeItem::Expr {
                expr: Expr::Ident {
                    ident: Ident::from(conv_name),
                    location: ItemLocation::test(),
                },
                location: ItemLocation::test(),
            }]),
            location: ItemLocation::test(),
        }
    }
    /// A `name = "value"` assign attribute (e.g. `rust_name`, `cpp_name`).
    pub fn string_assign(name: &str, value: &str) -> Self {
        Attribute::Assign {
            name: name.into(),
            items: AttributeItems(vec![AttributeItem::Expr {
                expr: Expr::StringLiteral {
                    value: value.into(),
                    format: StringFormat::Regular,
                    location: ItemLocation::test(),
                },
                location: ItemLocation::test(),
            }]),
            location: ItemLocation::test(),
        }
    }
}
impl Attribute {
    pub fn function(&self) -> Option<(&Ident, &AttributeItems)> {
        match self {
            Attribute::Function { name, items, .. } => Some((name, items)),
            _ => None,
        }
    }
    pub fn assign(&self) -> Option<(&Ident, &AttributeItems)> {
        match self {
            Attribute::Assign { name, items, .. } => Some((name, items)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
#[cfg_attr(test, derive(StripLocations))]
pub struct Attributes(pub Vec<Attribute>);
#[cfg(test)]
impl FromIterator<Attribute> for Attributes {
    fn from_iter<I: IntoIterator<Item = Attribute>>(iter: I) -> Self {
        Attributes(iter.into_iter().collect())
    }
}
impl IntoIterator for Attributes {
    type Item = Attribute;
    type IntoIter = std::vec::IntoIter<Attribute>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<'a> IntoIterator for &'a Attributes {
    type Item = &'a Attribute;
    type IntoIter = std::slice::Iter<'a, Attribute>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
impl Attributes {
    /// Find the first `#[cfg(...)]` attribute and return its predicate, or
    /// `None` if no cfg attribute is present. If multiple cfgs are stacked
    /// they're conjoined into a single `all(...)` predicate.
    pub fn cfg(&self) -> Option<crate::parser::cfg::CfgPredicate> {
        use crate::parser::cfg::CfgPredicate;
        let mut found: Vec<CfgPredicate> = Vec::new();
        let mut location = None;
        for attr in &self.0 {
            if let Attribute::Cfg {
                predicate,
                location: loc,
            } = attr
            {
                found.push(predicate.clone());
                location = Some(*loc);
            }
        }
        match found.len() {
            0 => None,
            1 => Some(found.into_iter().next().unwrap()),
            _ => Some(CfgPredicate::All {
                predicates: found,
                location: location.unwrap(),
            }),
        }
    }
}
impl Deref for Attributes {
    type Target = Vec<Attribute>;
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
#[cfg_attr(test, derive(StripLocations))]
#[cfg_attr(test, strip_locations(copy))]
pub enum Visibility {
    Public,
    Private,
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
                } else if !matches!(self.peek(), TokenKind::RBracket) {
                    // Not a comma and not a closing bracket - error
                    return Err(ParseError::ExpectedToken {
                        expected: vec![TokenKind::RBracket, TokenKind::Comma],
                        found: self.peek().clone(),
                        location: self.current().location,
                    });
                }
            }

            self.expect(TokenKind::RBracket)?;
        }

        Ok(Attributes(attrs))
    }

    /// Accept a name in cfg-attribute context: either a regular identifier
    /// or one of the keyword tokens that can plausibly appear as a cfg key
    /// (currently just `backend`, since it's the only hard-keyword we
    /// expect to use as a cfg key). Future keys like `pointer_size` /
    /// `feature` aren't keywords, so they fall through to `Ident`.
    fn parse_cfg_name(&mut self) -> Result<(String, crate::span::Span), ParseError> {
        let token = self.current().clone();
        match &token.kind {
            TokenKind::Ident(s) => {
                let s = s.clone();
                self.advance();
                Ok((s, token.location.span))
            }
            TokenKind::Backend => {
                self.advance();
                Ok(("backend".to_string(), token.location.span))
            }
            _ => Err(ParseError::ExpectedIdentifier {
                found: token.kind,
                location: token.location,
            }),
        }
    }

    /// Parse a single cfg predicate (atom or `any/all/not`). Caller is
    /// responsible for the surrounding parentheses on the top-level
    /// `cfg(...)` attribute; this function handles its own parens for
    /// nested combinators.
    pub(crate) fn parse_cfg_predicate(
        &mut self,
    ) -> Result<crate::parser::cfg::CfgPredicate, ParseError> {
        use crate::parser::cfg::{CfgAtom, CfgPredicate};

        let (name, name_span) = self.parse_cfg_name()?;
        let start_pos = name_span.start;

        // `any(...)`, `all(...)`, `not(...)` are combinators recognised by
        // their bare names. A bare ident OR a `name = "value"` falls
        // through to the atom branch.
        if matches!(self.peek(), TokenKind::LParen)
            && matches!(name.as_str(), "any" | "all" | "not")
        {
            self.advance(); // consume (
            let mut children = Vec::new();
            while !matches!(self.peek(), TokenKind::RParen) {
                children.push(self.parse_cfg_predicate()?);
                if matches!(self.peek(), TokenKind::Comma) {
                    self.advance();
                } else if !matches!(self.peek(), TokenKind::RParen) {
                    return Err(ParseError::ExpectedToken {
                        expected: vec![TokenKind::RParen, TokenKind::Comma],
                        found: self.peek().clone(),
                        location: self.current().location,
                    });
                }
            }
            let end_token = self.expect(TokenKind::RParen)?;
            let location =
                self.item_location_from_locations(start_pos, end_token.location.span.end);
            return Ok(match name.as_str() {
                "any" => CfgPredicate::Any {
                    predicates: children,
                    location,
                },
                "all" => CfgPredicate::All {
                    predicates: children,
                    location,
                },
                "not" => {
                    if children.len() != 1 {
                        return Err(ParseError::ExpectedToken {
                            expected: vec![TokenKind::RParen],
                            found: self.peek().clone(),
                            location,
                        });
                    }
                    CfgPredicate::Not {
                        predicate: Box::new(children.into_iter().next().unwrap()),
                        location,
                    }
                }
                _ => unreachable!(),
            });
        }

        // Atom: `name` or `name = "value"`.
        let atom = if matches!(self.peek(), TokenKind::Eq) {
            self.advance(); // consume =
            let (value, value_loc) = self.parse_string_literal()?;
            let atom_location = self.item_location_from_locations(start_pos, value_loc.span.end);
            CfgAtom::KeyValue {
                key: name,
                value,
                location: atom_location,
            }
        } else {
            let atom_location = self.item_location_from_locations(start_pos, name_span.end);
            CfgAtom::Ident {
                name,
                location: atom_location,
            }
        };
        let location = *atom.location();
        Ok(CfgPredicate::Atom { atom, location })
    }

    pub(crate) fn parse_attribute(&mut self) -> Result<Attribute, ParseError> {
        let (name, name_span) = self.expect_ident()?;
        let start_pos = name_span.start;

        // `#[cfg(...)]` is special-cased because its argument grammar is a
        // nested predicate (any/all/not), not a flat list of expressions.
        if name.as_str() == "cfg" && matches!(self.peek(), TokenKind::LParen) {
            self.advance(); // consume (
            let predicate = self.parse_cfg_predicate()?;
            let end_token = self.expect(TokenKind::RParen)?;
            let location =
                self.item_location_from_locations(start_pos, end_token.location.span.end);
            return Ok(Attribute::Cfg {
                predicate,
                location,
            });
        }

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
                    items.push(AttributeItem::Comment {
                        text: comment_text,
                        location: token.location,
                    });
                }

                // Skip doc comments (they don't belong in attribute expressions)
                while matches!(self.peek(), TokenKind::DocOuter(_) | TokenKind::DocInner(_)) {
                    self.advance();
                }

                if matches!(self.peek(), TokenKind::RParen) {
                    break;
                }

                let expr = self.parse_expr()?;
                let location = *expr.location();
                items.push(AttributeItem::Expr { expr, location });

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
                    items.push(AttributeItem::Comment {
                        text: comment_text,
                        location: token.location,
                    });
                }

                if matches!(self.peek(), TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
            let end_pos = self.expect(TokenKind::RParen)?.end_location();
            let location = self.item_location_from_locations(start_pos, end_pos);
            Ok(Attribute::Function {
                name,
                items: AttributeItems(items),
                location,
            })
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
                items.push(AttributeItem::Comment {
                    text: comment_text,
                    location: token.location,
                });
            }

            let expr = self.parse_expr()?;
            let mut end_pos = expr.location().span.end;
            let expr_location = *expr.location();
            items.push(AttributeItem::Expr {
                expr,
                location: expr_location,
            });

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
                items.push(AttributeItem::Comment {
                    text: comment_text,
                    location: token.location,
                });
                end_pos = token.location.span.end;
            }

            let location = self.item_location_from_locations(start_pos, end_pos);
            Ok(Attribute::Assign {
                name,
                items: AttributeItems(items),
                location,
            })
        } else {
            // Ident attribute
            let end_pos = name_span.end;
            let location = self.item_location_from_locations(start_pos, end_pos);
            Ok(Attribute::Ident {
                ident: name,
                location,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        grammar::{ItemDefinitionInner, ModuleItem, TypeDefItem, Visibility, test_aliases::*},
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
            .with_attributes([A::singleton(0x118FB64), A::size(0x40), A::align(16)]),
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
        match &module.items[0] {
            ModuleItem::Definition { definition } => {
                assert_eq!(definition.name.0, "AnarkGui");
                assert_eq!(definition.visibility, Visibility::Public);

                // Check the type has attributes
                if let ItemDefinitionInner::Type(td) = &definition.inner {
                    assert_eq!(td.attributes.0.len(), 3); // singleton, size, align

                    // Verify we have vftable and two fields
                    // vftable + 2 fields = 3 statements
                    let statement_count = td
                        .items
                        .iter()
                        .filter(|item| matches!(item, TypeDefItem::Statement(_)))
                        .count();
                    assert_eq!(statement_count, 3);
                } else {
                    panic!("Expected Type definition");
                }
            }
            _ => panic!("Expected Definition"),
        }
    }

    // ========================================================================
    // Attribute error tests - parser bounds checking
    // ========================================================================

    use super::ParseError;
    use crate::{span::ItemLocation, tokenizer::TokenKind};

    #[test]
    fn attribute_at_eof_errors() {
        // Just a # with nothing after - should error about missing [
        let text = "#";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::LBracket],
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn incomplete_attribute_bracket_errors() {
        // #[ with nothing after - should error about missing attribute name
        let text = "#[";
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
    fn unclosed_attribute_errors() {
        // #[foo without closing ] - should error about missing ] or ,
        let text = "#[foo";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RBracket, TokenKind::Comma],
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn attribute_without_item_errors() {
        // Complete attribute but nothing after - should error about missing item
        let text = "#[size(4)]";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedItemDefinition {
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn multiple_attributes_at_eof_errors() {
        // Multiple attributes but nothing after - should error about missing item
        let text = "#[size(4)] #[align(4)]";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedItemDefinition {
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn doc_comment_then_incomplete_attribute_errors() {
        // Doc comment followed by incomplete attribute - should error about missing attribute name
        let text = "/// My doc\n#[";
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
    fn doc_comment_then_attribute_at_eof_errors() {
        // Doc comment followed by complete attribute but nothing else
        let text = "/// My doc\n#[size(4)]";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedItemDefinition {
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn unclosed_attribute_with_parens_errors() {
        // #[size(4) without closing ]
        let text = "#[size(4)";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RBracket, TokenKind::Comma],
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn multiple_attrs_missing_bracket_errors() {
        // #[size(0x3540), align(4) without closing ]
        let text = "#[size(0x3540), align(4)";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RBracket, TokenKind::Comma],
                found: TokenKind::Eof,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn multiple_attrs_missing_bracket_with_item_errors() {
        // #[size(0x3540), align(4) followed by item - should error about missing ] or ,
        let text = "#[size(0x3540), align(4)\npub type Foo {}";
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RBracket, TokenKind::Comma],
                found: TokenKind::Pub,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn attribute_empty_parens_parses_ok() {
        // #[size()] is parsed as a function-like attribute with no arguments
        // Semantic validation catches that size requires an argument
        let text = r#"
        #[size()]
        type Test {}
        "#;
        // Parser accepts this, semantic layer validates argument count
        assert!(parse_str_for_tests(text).is_ok());
    }

    #[test]
    fn attribute_missing_closing_paren_errors() {
        let text = r#"
        #[size(4]
        type Test {}
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        assert_eq!(
            err.strip_locations(),
            ParseError::ExpectedToken {
                expected: vec![TokenKind::RParen],
                found: TokenKind::RBracket,
                location: ItemLocation::test(),
            }
            .strip_locations()
        );
    }

    #[test]
    fn attribute_unknown_character_errors() {
        let text = r#"
        #[size(4) @]
        type Test {}
        "#;
        let err = parse_str_for_tests(text).unwrap_err();
        // Tokenizer error for unknown character
        assert_eq!(
            err.strip_locations(),
            ParseError::Tokenizer(crate::tokenizer::LexError::UnexpectedCharacter {
                character: '@',
                location: ItemLocation::test(),
            })
            .strip_locations()
        );
    }

    mod cfg_attribute {
        use crate::{
            parser::{
                Parser,
                attributes::Attribute,
                cfg::{CfgAtom, CfgPredicate},
            },
            span::{FileId, StripLocations},
            tokenizer::tokenize_with_file_id,
        };

        fn parse_cfg(text: &str) -> CfgPredicate {
            let tokens = tokenize_with_file_id(text.to_string(), FileId::TEST).unwrap();
            let mut parser = Parser::new(tokens, FileId::TEST, text.to_string());
            let attrs = parser.parse_attributes().unwrap();
            for attr in attrs.0 {
                if let Attribute::Cfg { predicate, .. } = attr {
                    return predicate.strip_locations();
                }
            }
            panic!("expected a cfg attribute");
        }

        fn ident(name: &str) -> CfgAtom {
            CfgAtom::Ident {
                name: name.into(),
                location: crate::span::ItemLocation::test(),
            }
        }
        fn kv(k: &str, v: &str) -> CfgAtom {
            CfgAtom::KeyValue {
                key: k.into(),
                value: v.into(),
                location: crate::span::ItemLocation::test(),
            }
        }
        fn atom(a: CfgAtom) -> CfgPredicate {
            CfgPredicate::Atom {
                atom: a,
                location: crate::span::ItemLocation::test(),
            }
        }

        #[test]
        fn parses_bare_ident() {
            assert_eq!(parse_cfg("#[cfg(test)]"), atom(ident("test")));
        }

        #[test]
        fn parses_key_value() {
            assert_eq!(
                parse_cfg(r#"#[cfg(backend = "cpp")]"#),
                atom(kv("backend", "cpp"))
            );
        }

        #[test]
        fn parses_any() {
            let p = parse_cfg(r#"#[cfg(any(backend = "cpp", backend = "json"))]"#);
            assert_eq!(
                p,
                CfgPredicate::Any {
                    predicates: vec![atom(kv("backend", "cpp")), atom(kv("backend", "json"))],
                    location: crate::span::ItemLocation::test(),
                }
            );
        }

        #[test]
        fn parses_not() {
            assert_eq!(
                parse_cfg(r#"#[cfg(not(backend = "rust"))]"#),
                CfgPredicate::Not {
                    predicate: Box::new(atom(kv("backend", "rust"))),
                    location: crate::span::ItemLocation::test(),
                }
            );
        }

        #[test]
        fn parses_nested() {
            // any(all(backend = "cpp", not(test)), backend = "rust")
            let p = parse_cfg(r#"#[cfg(any(all(backend = "cpp", not(test)), backend = "rust"))]"#);
            let inner_any = CfgPredicate::All {
                predicates: vec![
                    atom(kv("backend", "cpp")),
                    CfgPredicate::Not {
                        predicate: Box::new(atom(ident("test"))),
                        location: crate::span::ItemLocation::test(),
                    },
                ],
                location: crate::span::ItemLocation::test(),
            };
            assert_eq!(
                p,
                CfgPredicate::Any {
                    predicates: vec![inner_any, atom(kv("backend", "rust"))],
                    location: crate::span::ItemLocation::test(),
                }
            );
        }

        #[test]
        fn stacked_cfgs_collapse_to_all() {
            let text = r#"#[cfg(backend = "cpp")]
            #[cfg(not(test))]"#;
            let tokens = tokenize_with_file_id(text.to_string(), FileId::TEST).unwrap();
            let mut parser = Parser::new(tokens, FileId::TEST, text.to_string());
            let attrs = parser.parse_attributes().unwrap();
            let cfg = attrs.cfg().unwrap().strip_locations();
            assert_eq!(
                cfg,
                CfgPredicate::All {
                    predicates: vec![
                        atom(kv("backend", "cpp")),
                        CfgPredicate::Not {
                            predicate: Box::new(atom(ident("test"))),
                            location: crate::span::ItemLocation::test(),
                        },
                    ],
                    location: crate::span::ItemLocation::test(),
                }
            );
        }
    }
}
