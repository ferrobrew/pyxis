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
            TS::field((V::Public, "enabled"), T::ident("bool")).with_attributes([A::address(0x18)]),
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
