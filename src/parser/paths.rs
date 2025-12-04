use std::{fmt, path::Path};

use crate::{
    span::{EqualsIgnoringLocations, ItemLocation, Location},
    tokenizer::TokenKind,
};

#[cfg(test)]
use crate::span::StripLocations;

use super::{ParseError, core::Parser};

#[derive(PartialEq, Hash, Eq, Clone, Debug, PartialOrd, Ord)]
pub struct ItemPathSegment(String);
#[cfg(test)]
impl StripLocations for ItemPathSegment {
    fn strip_locations(&self) -> Self {
        ItemPathSegment(self.0.strip_locations())
    }
}
impl EqualsIgnoringLocations for ItemPathSegment {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl ItemPathSegment {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}
impl From<&str> for ItemPathSegment {
    fn from(value: &str) -> Self {
        ItemPathSegment(value.to_string())
    }
}
impl From<String> for ItemPathSegment {
    fn from(value: String) -> Self {
        ItemPathSegment(value)
    }
}
impl fmt::Display for ItemPathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Hash, Eq, Clone, Debug, PartialOrd, Ord)]
pub struct ItemPath(Vec<ItemPathSegment>);
#[cfg(test)]
impl StripLocations for ItemPath {
    fn strip_locations(&self) -> Self {
        ItemPath(self.0.strip_locations())
    }
}
impl EqualsIgnoringLocations for ItemPath {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self.0.equals_ignoring_locations(&other.0)
    }
}
impl ItemPath {
    pub fn empty() -> ItemPath {
        ItemPath(vec![])
    }

    pub fn from_path(path: &Path) -> ItemPath {
        // consider making this a result
        assert!(path.is_relative());

        ItemPath(
            path.with_extension("")
                .iter()
                .map(|s| s.to_string_lossy().as_ref().into())
                .collect(),
        )
    }

    pub fn iter(&self) -> impl Iterator<Item = &ItemPathSegment> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn parent(&self) -> Option<ItemPath> {
        (!self.0.is_empty()).then(|| ItemPath(self.0[..self.0.len() - 1].to_vec()))
    }

    pub fn push(&mut self, segment: ItemPathSegment) {
        self.0.push(segment);
    }

    pub fn join(&self, segment: ItemPathSegment) -> ItemPath {
        let mut path = self.0.clone();
        path.push(segment);
        ItemPath(path)
    }

    pub fn last(&self) -> Option<&ItemPathSegment> {
        self.0.last()
    }
}
impl fmt::Display for ItemPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (index, segment) in self.0.iter().enumerate() {
            if index > 0 {
                write!(f, "::")?;
            }
            write!(f, "{segment}")?;
        }
        Ok(())
    }
}
impl FromIterator<ItemPathSegment> for ItemPath {
    fn from_iter<I: IntoIterator<Item = ItemPathSegment>>(iter: I) -> Self {
        ItemPath(Vec::from_iter(iter))
    }
}
impl From<&str> for ItemPath {
    fn from(value: &str) -> Self {
        ItemPath(value.split("::").map(|s| s.into()).collect())
    }
}

impl Parser {
    pub(crate) fn parse_item_path(&mut self) -> Result<(ItemPath, ItemLocation), ParseError> {
        let first_token = self.current();
        let start_pos = first_token.location.span.start;
        let mut end_pos = first_token.location.span.end;
        let mut segments = Vec::new();

        while let TokenKind::Ident(name) = self.peek() {
            segments.push(ItemPathSegment::from(name.clone()));
            end_pos = self.current().location.span.end;
            self.advance();

            // Handle generics in the path
            if matches!(self.peek(), TokenKind::Lt) {
                // Parse generic arguments as part of the segment
                let (generic_str, generic_end) = self.parse_generic_args_as_string()?;
                let last = segments.last_mut().unwrap();
                *last = ItemPathSegment::from(format!("{}{}", last.as_str(), generic_str));
                end_pos = generic_end;
            }

            if !matches!(self.peek(), TokenKind::ColonColon) {
                break;
            }
            self.advance();
        }

        let location = self.item_location_from_locations(start_pos, end_pos);
        Ok((ItemPath::from_iter(segments), location))
    }

    pub(crate) fn parse_generic_args_as_string(
        &mut self,
    ) -> Result<(String, Location), ParseError> {
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

        let gt_token = self.expect(TokenKind::Gt)?;
        result.push('>');
        Ok((result, gt_token.location.span.end))
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
