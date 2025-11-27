use crate::{
    grammar::{Attributes, Backend, Expr, ExternValue, Ident, ItemPath, ModuleItem, StringFormat},
    span::{ItemLocation, Located, Span},
    tokenizer::TokenKind,
};

use super::{ParseError, core::Parser};

impl Parser {
    pub(crate) fn parse_use(&mut self) -> Result<Located<ItemPath>, ParseError> {
        let first_token = self.expect(TokenKind::Use)?;

        // Check for super keyword (not supported yet)
        if let TokenKind::Ident(name) = self.peek()
            && name == "super"
        {
            return Err(ParseError::SuperNotSupported {
                location: self.current().location.clone(),
            });
        }

        let path = self.parse_item_path()?;
        let last_token = self.expect(TokenKind::Semi)?;

        let location = self.item_location_from_token_range(&first_token, &last_token);
        Ok(Located::new(path, location))
    }

    pub(crate) fn parse_extern_type(&mut self) -> Result<Located<ModuleItem>, ParseError> {
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

        self.expect(TokenKind::Extern)?;
        self.expect(TokenKind::Type)?;
        let (mut name, _) = self.expect_ident()?;

        // Handle generics - concatenate them into the type name string
        if matches!(self.peek(), TokenKind::Lt) {
            let mut type_str = name.0;
            type_str.push('<');
            self.advance(); // consume <

            let mut depth = 1;
            while depth > 0 && !matches!(self.peek(), TokenKind::Eof) {
                match self.peek().clone() {
                    TokenKind::Lt => {
                        type_str.push('<');
                        depth += 1;
                        self.advance();
                    }
                    TokenKind::Gt => {
                        type_str.push('>');
                        depth -= 1;
                        self.advance();
                    }
                    TokenKind::Comma => {
                        type_str.push_str(", ");
                        self.advance();
                    }
                    TokenKind::Ident(n) => {
                        type_str.push_str(&n);
                        self.advance();
                    }
                    TokenKind::ColonColon => {
                        type_str.push_str("::");
                        self.advance();
                    }
                    _ => {
                        self.advance();
                    }
                }
            }
            name = Ident(type_str);
        }

        self.expect(TokenKind::Semi)?;
        let end_pos = if self.pos > 0 {
            self.tokens[self.pos - 1].location.span.end
        } else {
            self.current().location.span.end
        };
        let location = ItemLocation::new(self.filename.clone(), Span::new(start_pos, end_pos));
        Ok(Located::new(
            ModuleItem::ExternType(name, attributes, doc_comments),
            location,
        ))
    }

    pub(crate) fn parse_extern_value(&mut self) -> Result<Located<ExternValue>, ParseError> {
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
        self.expect(TokenKind::Extern)?;
        let (name, _) = self.expect_ident()?;
        self.expect(TokenKind::Colon)?;
        let type_ = self.parse_type()?;
        self.expect(TokenKind::Semi)?;
        let end_pos = if self.pos > 0 {
            self.tokens[self.pos - 1].location.span.end
        } else {
            self.current().location.span.end
        };
        let location = self.item_location_from_locations(start_pos, end_pos);

        Ok(Located::new(
            ExternValue {
                visibility,
                name,
                type_,
                attributes,
                doc_comments,
            },
            location,
        ))
    }

    pub(crate) fn parse_backend(&mut self) -> Result<Located<Backend>, ParseError> {
        let first_token = self.expect(TokenKind::Backend)?;
        let (name, _) = self.expect_ident()?;

        let mut prologue = None;
        let mut prologue_format = StringFormat::Regular;
        let mut epilogue = None;
        let mut epilogue_format = StringFormat::Regular;

        // Check if we have braces or direct prologue/epilogue
        let last_token = if matches!(self.peek(), TokenKind::LBrace) {
            // Form: backend name { prologue ...; epilogue ...; }
            self.advance(); // consume {

            while !matches!(self.peek(), TokenKind::RBrace) {
                match self.peek() {
                    TokenKind::Prologue => {
                        self.advance();
                        let expr = self.parse_expr()?;
                        if let Expr::StringLiteral { value, format } = expr {
                            prologue = Some(value);
                            prologue_format = format;
                        } else {
                            return Err(ParseError::ExpectedStringLiteral {
                                found: self.peek().clone(),
                                location: self.current().location.clone(),
                            });
                        }
                        self.expect(TokenKind::Semi)?;
                    }
                    TokenKind::Epilogue => {
                        self.advance();
                        let expr = self.parse_expr()?;
                        if let Expr::StringLiteral { value, format } = expr {
                            epilogue = Some(value);
                            epilogue_format = format;
                        } else {
                            return Err(ParseError::ExpectedStringLiteral {
                                found: self.peek().clone(),
                                location: self.current().location.clone(),
                            });
                        }
                        self.expect(TokenKind::Semi)?;
                    }
                    _ => {
                        return Err(ParseError::ExpectedPrologueOrEpilogue {
                            found: self.peek().clone(),
                            location: self.current().location.clone(),
                        });
                    }
                }
            }

            self.expect(TokenKind::RBrace)?
        } else {
            // Form: backend name prologue ... or backend name epilogue ...
            match self.peek() {
                TokenKind::Prologue => {
                    self.advance();
                    let expr = self.parse_expr()?;
                    if let Expr::StringLiteral { value, format } = expr {
                        prologue = Some(value);
                        prologue_format = format;
                    } else {
                        return Err(ParseError::ExpectedStringLiteral {
                            found: self.peek().clone(),
                            location: self.current().location.clone(),
                        });
                    }
                    self.expect(TokenKind::Semi)?
                }
                TokenKind::Epilogue => {
                    self.advance();
                    let expr = self.parse_expr()?;
                    if let Expr::StringLiteral { value, format } = expr {
                        epilogue = Some(value);
                        epilogue_format = format;
                    } else {
                        return Err(ParseError::ExpectedStringLiteral {
                            found: self.peek().clone(),
                            location: self.current().location.clone(),
                        });
                    }
                    self.expect(TokenKind::Semi)?
                }
                _ => {
                    return Err(ParseError::ExpectedBackendContent {
                        found: self.peek().clone(),
                        location: self.current().location.clone(),
                    });
                }
            }
        };

        let location = self.item_location_from_token_range(&first_token, &last_token);
        Ok(Located::new(
            Backend {
                name,
                prologue,
                prologue_format,
                epilogue,
                epilogue_format,
            },
            location,
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        grammar::{ModuleItem, StringFormat, test_aliases::*},
        parser::{ParseError, parse_str_for_tests},
        span::StripLocations,
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn can_parse_use() {
        let text = r#"
        use hello::TestType<Hey>;
        type Test {
            test: TestType<Hey>,
        }
        "#;

        let ast = M::new()
            .with_uses([IP::from("hello::TestType<Hey>")])
            .with_definitions([ID::new(
                (V::Private, "Test"),
                TD::new([TS::field((V::Private, "test"), T::ident("TestType<Hey>"))]),
            )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn will_die_on_super_for_now() {
        let text = r#"
        use super::TestType<Hey>;
        "#;

        let error = parse_str_for_tests(text).err().unwrap();
        assert!(
            matches!(error, ParseError::SuperNotSupported { .. }),
            "Expected SuperNotSupported error, got: {error:?}"
        );
    }

    #[test]
    fn can_parse_extern() {
        let text = r#"
        #[size(12)]
        extern type TestType<Hey>;
        type Test {
            test: TestType<Hey>,
        }
        "#;

        let ast = M::new()
            .with_extern_types([("TestType<Hey>".into(), As::from_iter([A::size(12)]))])
            .with_definitions([ID::new(
                (V::Private, "Test"),
                TD::new([TS::field((V::Private, "test"), T::ident("TestType<Hey>"))]),
            )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_extern_with_multiline_doc_comment() {
        let text = r#"
#[size(8), align(4)]
/// `ManuallyDrop<SharedPtr<u32>>` is used instead of `SharedPtr<u32>` to avoid
/// the `Drop` implementation of `SharedPtr<u32>` being called when the `RenderBlock`
/// is dropped. The destructor, which we call in `drop`, will decrement the refcount
/// for us.
extern type ManuallyDrop<SharedPtr<u32>>;
    "#;

        let module = parse_str_for_tests(text).unwrap().strip_locations();

        // Verify we have one extern type item
        assert_eq!(module.items.len(), 1);

        // Verify it's an ExternType with the correct attributes and doc comments
        match &module.items[0].value {
            ModuleItem::ExternType(name, attrs, doc_comments) => {
                assert_eq!(name.0, "ManuallyDrop<SharedPtr<u32>>");
                assert_eq!(attrs.0.len(), 2);
                assert_eq!(doc_comments.len(), 4); // 4 lines of doc comment
                assert!(doc_comments[0].contains("ManuallyDrop<SharedPtr<u32>>"));
                assert!(doc_comments[1].contains("Drop` implementation"));
                assert!(doc_comments[2].contains("dropped"));
                assert!(doc_comments[3].contains("for us"));
            }
            _ => panic!("Expected ExternType"),
        }
    }

    #[test]
    fn can_parse_extern_value() {
        let text = r#"
        #[size(4)]
        extern type SomeType;
        #[address(0x1337)]
        pub extern some_value: *mut SomeType;
        #[address(0x1338)]
        extern some_private_value: *mut SomeType;
        "#;

        let ast = M::new()
            .with_extern_types([("SomeType".into(), As::from_iter([A::size(4)]))])
            .with_extern_values([
                EV::new(
                    V::Public,
                    "some_value",
                    T::ident("SomeType").mut_pointer(),
                    [A::address(0x1337)],
                ),
                EV::new(
                    V::Private,
                    "some_private_value",
                    T::ident("SomeType").mut_pointer(),
                    [A::address(0x1338)],
                ),
            ]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_backends() {
        let text = r##"
backend rust {
    prologue r#"
        use std::ffi::CString;
        use std::os::raw::c_char;
    "#;

    epilogue r#"
        fn main() {
            println!("Hello, world!");
        }
    "#;
}


backend rust prologue r#"
    use std::ffi::CString;
    use std::os::raw::c_char;
"#;

backend rust epilogue r#"
    fn main() {
        println!("Hello, world!");
    }
"#;
"##;

        let ast = M::new().with_backends([
            B::new("rust")
                .with_prologue_format(
                    r#"
        use std::ffi::CString;
        use std::os::raw::c_char;
    "#,
                    StringFormat::Raw,
                )
                .with_epilogue_format(
                    r#"
        fn main() {
            println!("Hello, world!");
        }
    "#,
                    StringFormat::Raw,
                ),
            B::new("rust").with_prologue_format(
                r#"
    use std::ffi::CString;
    use std::os::raw::c_char;
"#,
                StringFormat::Raw,
            ),
            B::new("rust").with_epilogue_format(
                r#"
    fn main() {
        println!("Hello, world!");
    }
"#,
                StringFormat::Raw,
            ),
        ]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_backend_with_multiline_prologue() {
        let text = r#"
backend rust prologue "
    use crate::shared_ptr::*;
    use std::mem::ManuallyDrop;
";
    "#;

        let ast = M::new().with_backends([B::new("rust")
            .with_prologue("\n    use crate::shared_ptr::*;\n    use std::mem::ManuallyDrop;\n")]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }
}
