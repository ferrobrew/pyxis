use crate::{
    grammar::{ItemPath, ItemPathSegment},
    tokenizer::TokenKind,
};

use super::{ParseError, core::Parser};

impl Parser {
    pub(crate) fn parse_item_path(&mut self) -> Result<ItemPath, ParseError> {
        let mut segments = Vec::new();

        while let TokenKind::Ident(name) = self.peek() {
            segments.push(ItemPathSegment::from(name.clone()));
            self.advance();

            // Handle generics in the path
            if matches!(self.peek(), TokenKind::Lt) {
                // Parse generic arguments as part of the segment
                let generic_str = self.parse_generic_args_as_string()?;
                let last = segments.last_mut().unwrap();
                *last = ItemPathSegment::from(format!("{}{}", last.as_str(), generic_str));
            }

            if !matches!(self.peek(), TokenKind::ColonColon) {
                break;
            }
            self.advance();
        }

        Ok(ItemPath::from_iter(segments))
    }

    pub(crate) fn parse_generic_args_as_string(&mut self) -> Result<String, ParseError> {
        let mut result = String::new();
        result.push('<');
        self.expect(TokenKind::Lt)?;

        let mut first = true;
        while !matches!(self.peek(), TokenKind::Gt) {
            if !first {
                self.expect(TokenKind::Comma)?;
                result.push_str(", ");
            }
            first = false;

            // Parse type as string for now
            result.push_str(&self.parse_type_as_string()?);
        }

        self.expect(TokenKind::Gt)?;
        result.push('>');
        Ok(result)
    }

    pub(crate) fn parse_type_as_string(&mut self) -> Result<String, ParseError> {
        let start_pos = self.pos;
        self.parse_type()?; // Parse but discard
        let end_pos = self.pos;

        // Reconstruct the string from tokens
        let mut result = String::new();
        for i in start_pos..end_pos {
            result.push_str(self.span_text(&self.tokens[i].location.span));
        }
        Ok(result)
    }
}
