use crate::{
    span::{HasLocation, ItemLocation},
    tokenizer::TokenKind,
};

#[cfg(test)]
use crate::span::StripLocations;

use super::{
    ParseError,
    attributes::{Attributes, Visibility},
    core::Parser,
    items::Comment,
    paths::{ItemPath, ItemPathSegment},
    types::{Ident, Type},
};

#[cfg(test)]
use super::attributes::Attribute;

#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub enum Argument {
    ConstSelf {
        location: ItemLocation,
    },
    MutSelf {
        location: ItemLocation,
    },
    Named {
        ident: Ident,
        type_: Type,
        location: ItemLocation,
    },
}
#[cfg(test)]
impl Argument {
    pub fn const_self() -> Argument {
        Argument::ConstSelf {
            location: ItemLocation::test(),
        }
    }
    pub fn mut_self() -> Argument {
        Argument::MutSelf {
            location: ItemLocation::test(),
        }
    }
    pub fn named(ident: impl Into<Ident>, type_: impl Into<Type>) -> Argument {
        let type_ = type_.into();
        Argument::Named {
            ident: ident.into(),
            location: *type_.location(),
            type_,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct Function {
    pub visibility: Visibility,
    pub name: Ident,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
    pub arguments: Vec<Argument>,
    pub return_type: Option<Type>,
    pub location: ItemLocation,
}
#[cfg(test)]
impl Function {
    pub fn new(
        (visibility, name): (Visibility, &str),
        arguments: impl IntoIterator<Item = Argument>,
    ) -> Self {
        Self {
            visibility,
            name: name.into(),
            attributes: Default::default(),
            doc_comments: vec![],
            arguments: arguments.into_iter().collect(),
            return_type: None,
            location: ItemLocation::test(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl IntoIterator<Item = Attribute>) -> Self {
        self.attributes = Attributes::from_iter(attributes);
        self
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
    pub fn with_return_type(mut self, return_type: impl Into<Type>) -> Self {
        self.return_type = Some(return_type.into());
        self
    }
}

/// Items in an impl block (preserves ordering and comments)
#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub enum ImplItem {
    Comment(Comment),
    Function(Function),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
pub struct FunctionBlock {
    pub name: Ident,
    /// Qualified path segments after the first name (e.g. `Inner` in
    /// `impl Outer::Inner`). `None` for simple `impl Foo` blocks.
    pub name_path: Option<ItemPath>,
    /// Type parameters declared on the `impl` block (e.g. `T, Y` in
    /// `impl<T, Y> Foo<T> { ... }`). Empty for non-generic impls.
    pub type_parameters: Vec<crate::parser::types::TypeParameter>,
    /// Type arguments applied to the type name (the `<T>` in `Foo<T>`).
    /// Tracked separately from `type_parameters` because not every impl
    /// param appears after the type name (e.g. method-level extras like
    /// `Y` in `impl<T, Y> Foo<T>`).
    pub type_arguments: Vec<crate::parser::types::TypeParameter>,
    pub items: Vec<ImplItem>,
    pub attributes: Attributes,
    pub location: ItemLocation,
}
#[cfg(test)]
impl StripLocations for FunctionBlock {
    fn strip_locations(&self) -> Self {
        FunctionBlock {
            name: self.name.strip_locations(),
            name_path: self.name_path.clone(),
            type_parameters: self.type_parameters.strip_locations(),
            type_arguments: self.type_arguments.strip_locations(),
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    ImplItem::Comment(_) => None, // Filter out comments
                    ImplItem::Function(f) => Some(ImplItem::Function(f.strip_locations())),
                })
                .collect(),
            attributes: self.attributes.strip_locations(),
            location: ItemLocation::test(),
        }
    }
}
#[cfg(test)]
impl FunctionBlock {
    pub fn new(name: impl Into<Ident>, functions: impl IntoIterator<Item = Function>) -> Self {
        Self {
            name: name.into(),
            name_path: None,
            type_parameters: vec![],
            type_arguments: vec![],
            items: functions.into_iter().map(ImplItem::Function).collect(),
            attributes: Default::default(),
            location: ItemLocation::test(),
        }
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
    pub fn with_type_parameters(
        mut self,
        type_parameters: impl IntoIterator<Item = crate::parser::types::TypeParameter>,
    ) -> Self {
        self.type_parameters = type_parameters.into_iter().collect();
        self
    }
}
impl FunctionBlock {
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.items.iter().filter_map(|item| match item {
            ImplItem::Function(func) => Some(func),
            _ => None,
        })
    }
}

impl Parser {
    pub(crate) fn parse_impl_block(&mut self) -> Result<FunctionBlock, ParseError> {
        let start_pos = self.current().location.span.start;
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        self.expect(TokenKind::Impl)?;
        // `impl<T1, T2> Foo<T1, T2> { ... }` — the parameter list goes on
        // the `impl` keyword. The optional `<...>` after the type name is
        // accepted but its contents are ignored (they should always match
        // the `impl<...>` parameters).
        let type_parameters = self.parse_type_parameters()?;
        let (name, _) = self.expect_ident()?;

        // Check for qualified path: impl Outer::Inner { ... }
        let name_path = if matches!(self.peek(), TokenKind::ColonColon) {
            let mut segments: Vec<ItemPathSegment> = Vec::new();
            while matches!(self.peek(), TokenKind::ColonColon) {
                self.advance(); // consume ::
                let (seg, _) = self.expect_ident()?;
                segments.push(seg.as_str().into());
            }
            Some(segments.into_iter().collect())
        } else {
            None
        };

        let type_arguments = if matches!(self.peek(), TokenKind::Lt) {
            self.parse_type_parameters()?
        } else {
            vec![]
        };
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

            let function = self.parse_function()?;
            items.push(ImplItem::Function(function));
        }

        let last_token = self.expect(TokenKind::RBrace)?;

        let location = self.item_location_from_locations(start_pos, last_token.end_location());
        Ok(FunctionBlock {
            name,
            name_path,
            type_parameters,
            type_arguments,
            items,
            attributes,
            location,
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
        let mut doc_comments = self.collect_doc_comments();
        let attributes = if matches!(self.peek(), TokenKind::Hash) {
            self.parse_attributes()?
        } else {
            Attributes::default()
        };

        // Also collect doc comments that appear after attributes
        let after_attr_doc_comments = self.collect_doc_comments();
        doc_comments.extend(after_attr_doc_comments);

        let start_pos = self.current().location.span.start;
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
        let location = self.item_location_from_locations(start_pos, end_pos);

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
                Ok(Argument::MutSelf {
                    location: tok.location,
                })
            } else {
                let tok = self.expect(TokenKind::SelfValue)?;
                Ok(Argument::ConstSelf {
                    location: tok.location,
                })
            }
        } else {
            let start_pos = self.current().location.span.start;
            let (name, _) = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let type_ = self.parse_type()?;
            let end_pos = self.current().location.span.end;
            let location = self.item_location_from_locations(start_pos, end_pos);

            Ok(Argument::Named {
                ident: name,
                type_,
                location,
            })
        }
    }
}

#[cfg(test)]
mod tests;
