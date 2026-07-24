use crate::span::{HasLocation, ItemLocation};

#[cfg(test)]
use crate::span::StripLocations;

use crate::parser::{attributes::Attributes, expressions::Expr, types::Type};

#[cfg(test)]
use crate::parser::attributes::Attribute;

// type aliases
#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct TypeAliasDefinition {
    pub target: Type,
    pub attributes: Attributes,
    pub location: ItemLocation,
}
#[cfg(test)]
impl TypeAliasDefinition {
    pub fn new(target: Type) -> Self {
        Self {
            target,
            attributes: Default::default(),
            location: ItemLocation::test(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl IntoIterator<Item = Attribute>) -> Self {
        self.attributes = Attributes::from_iter(attributes);
        self
    }
}

// items
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct ConstDefinition {
    pub type_: Type,
    pub expr: Expr,
    pub attributes: Attributes,
    pub location: ItemLocation,
}
#[cfg(test)]
impl ConstDefinition {
    pub fn new(type_: Type, expr: Expr) -> Self {
        Self {
            type_,
            expr,
            attributes: Default::default(),
            location: ItemLocation::test(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl IntoIterator<Item = Attribute>) -> Self {
        self.attributes = Attributes::from_iter(attributes);
        self
    }
}

// items
/// A `pub extern some_value: *mut T;` declaration. The `#[address(...)]`
/// attribute (required at the semantic layer) lives in `attributes`. Like
/// [`ConstDefinition`], this is a value item, not a type; `visibility`,
/// `name`, and `doc_comments` live on the enclosing [`ItemDefinition`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct ExternValueDefinition {
    pub type_: Type,
    pub attributes: Attributes,
    pub location: ItemLocation,
}
#[cfg(test)]
impl ExternValueDefinition {
    pub fn new(type_: Type) -> Self {
        Self {
            type_,
            attributes: Default::default(),
            location: ItemLocation::test(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl IntoIterator<Item = Attribute>) -> Self {
        self.attributes = Attributes::from_iter(attributes);
        self
    }
}

#[cfg(test)]
mod tests;
