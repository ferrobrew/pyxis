use crate::{
    grammar::{Ident, Type},
    span::Located,
    tokenizer::TokenKind,
};

use super::{ParseError, core::Parser};

impl Parser {
    pub(crate) fn parse_type(&mut self) -> Result<Located<Type>, ParseError> {
        match self.peek() {
            TokenKind::Unknown => {
                let start = self.advance();
                self.expect(TokenKind::Lt)?;
                let size = self.parse_int_literal()?.value as usize;
                let end = self.expect(TokenKind::Gt)?;

                Ok(Located::new(
                    Type::Unknown(size),
                    self.item_location_from_token_range(&start, &end),
                ))
            }
            TokenKind::Star => {
                let start = self.advance();
                if matches!(self.peek(), TokenKind::Const) {
                    self.advance();
                    let inner = self.parse_type()?;
                    let location = self.item_location_from_locations(
                        start.start_location(),
                        inner.location.span.end,
                    );
                    Ok(Located::new(Type::ConstPointer(Box::new(inner)), location))
                } else if matches!(self.peek(), TokenKind::Mut) {
                    self.advance();
                    let inner = self.parse_type()?;
                    let location = self.item_location_from_locations(
                        start.start_location(),
                        inner.location.span.end,
                    );
                    Ok(Located::new(Type::MutPointer(Box::new(inner)), location))
                } else {
                    Err(ParseError::MissingPointerQualifier {
                        location: self.current().location.clone(),
                    })
                }
            }
            TokenKind::LBracket => {
                let start = self.advance();
                let inner = self.parse_type()?;
                self.expect(TokenKind::Semi)?;
                let size = self.parse_int_literal()?.value as usize;
                let end = self.expect(TokenKind::RBracket)?;
                Ok(Located::new(
                    Type::Array(Box::new(inner), size),
                    self.item_location_from_token_range(&start, &end),
                ))
            }
            TokenKind::Ident(_) => {
                let (mut ident, ident_span) = self.expect_ident()?;
                let start_pos = ident_span.start;
                let mut end_pos = ident_span.end;

                // Check for generic arguments - treat them as part of the identifier string
                // This is needed for extern types that need exact reproduction
                if matches!(self.peek(), TokenKind::Lt) {
                    let mut type_str = ident.0;
                    type_str.push('<');
                    end_pos = self.advance().end_location(); // consume <

                    let mut depth = 1;
                    while depth > 0 && !matches!(self.peek(), TokenKind::Eof) {
                        match self.peek().clone() {
                            TokenKind::Lt => {
                                type_str.push('<');
                                depth += 1;
                            }
                            TokenKind::Gt => {
                                type_str.push('>');
                                depth -= 1;
                            }
                            TokenKind::Comma => {
                                type_str.push_str(", ");
                            }
                            TokenKind::Ident(name) => {
                                type_str.push_str(&name);
                            }
                            TokenKind::ColonColon => {
                                type_str.push_str("::");
                            }
                            _ => {}
                        }
                        end_pos = self.advance().end_location();
                    }
                    ident = Ident(type_str);
                }

                Ok(Located::new(
                    Type::Ident(ident),
                    self.item_location_from_locations(start_pos, end_pos),
                ))
            }
            _ => Err(ParseError::ExpectedType {
                found: self.peek().clone(),
                location: self.current().location.clone(),
            }),
        }
    }
}
