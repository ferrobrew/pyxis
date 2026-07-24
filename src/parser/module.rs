#[cfg(test)]
use crate::span::StripLocations;

use crate::{
    parser::{
        ParseError,
        attributes::{Attributes, Visibility},
        core::Parser,
        external::{Splice, UseTree},
        functions::{Function, FunctionBlock},
        items::{Comment, ItemDefinition, ItemTerminator},
        types::Ident,
    },
    span::{HasLocation, ItemLocation},
    tokenizer::TokenKind,
};

#[cfg(test)]
use super::paths::ItemPath;

/// Module-level items (preserves ordering and comments)
#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub enum ModuleItem {
    Comment {
        comment: Comment,
    },
    /// Module-level inner attributes (`#![...]`), e.g. `#![rust(example_flag)]`.
    /// Kept as an item (rather than a side-field on [`Module`]) so their
    /// position relative to comments is preserved on a format round-trip.
    InnerAttributes {
        attributes: Attributes,
        location: ItemLocation,
    },
    /// A `use` import. `visibility` is [`Visibility::Public`] for `pub use`
    /// (an explicit re-export: the imported name is exported from this module
    /// as `<module>::<name>`) and [`Visibility::Private`] for a plain `use`
    /// (in scope for this module only, not re-exported).
    Use {
        tree: UseTree,
        visibility: Visibility,
        /// Optional leading `#[cfg(...)]`. A cfg-gated `use` still
        /// participates in name resolution; the cpp backend additionally
        /// promotes a cpp-active gated `use` to a forced `#include`.
        attributes: Attributes,
        location: ItemLocation,
    },
    ExternType {
        name: Ident,
        attributes: Attributes,
        doc_comments: Vec<String>,
        /// Full span (incl. doc comments / attributes), used by the formatter.
        location: ItemLocation,
        /// Position of the declaration itself, for documentation source links.
        declaration_location: ItemLocation,
    },
    Splice {
        splice: Splice,
    },
    Definition {
        definition: ItemDefinition,
    },
    Impl {
        impl_block: FunctionBlock,
    },
    Function {
        function: Function,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Module {
    pub items: Vec<ModuleItem>,
    pub doc_comments: Vec<String>,
}
impl Module {
    pub fn new() -> Self {
        Self::default()
    }
}
#[cfg(test)]
impl StripLocations for Module {
    fn strip_locations(&self) -> Self {
        Module {
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    ModuleItem::Comment { .. } => None, // Filter out comments
                    _ => Some(item.strip_locations()),
                })
                .collect(),
            doc_comments: self.doc_comments.strip_locations(),
        }
    }
}
#[cfg(test)]
impl Module {
    /// Add simple path-based use statements (convenience for tests)
    pub fn with_uses(mut self, uses: impl IntoIterator<Item = ItemPath>) -> Self {
        for path in uses.into_iter() {
            self.items.push(ModuleItem::Use {
                tree: UseTree::path(path),
                visibility: Visibility::Private,
                attributes: Attributes::default(),
                location: ItemLocation::test(),
            });
        }
        self
    }

    /// Add `pub use` re-exports (convenience for tests).
    pub fn with_pub_uses(mut self, uses: impl IntoIterator<Item = ItemPath>) -> Self {
        for path in uses.into_iter() {
            self.items.push(ModuleItem::Use {
                tree: UseTree::path(path),
                visibility: Visibility::Public,
                attributes: Attributes::default(),
                location: ItemLocation::test(),
            });
        }
        self
    }

    /// Add use statements with full UseTree support (for braced import tests)
    pub fn with_use_trees(mut self, trees: impl IntoIterator<Item = UseTree>) -> Self {
        for tree in trees.into_iter() {
            self.items.push(ModuleItem::Use {
                tree,
                visibility: Visibility::Private,
                attributes: Attributes::default(),
                location: ItemLocation::test(),
            });
        }
        self
    }

    /// Add `pub use` re-export trees with full UseTree support (for tests).
    pub fn with_pub_use_trees(mut self, trees: impl IntoIterator<Item = UseTree>) -> Self {
        for tree in trees.into_iter() {
            self.items.push(ModuleItem::Use {
                tree,
                visibility: Visibility::Public,
                attributes: Attributes::default(),
                location: ItemLocation::test(),
            });
        }
        self
    }
    pub fn with_extern_types(
        mut self,
        extern_types: impl IntoIterator<Item = (Ident, Attributes)>,
    ) -> Self {
        for (name, attributes) in extern_types.into_iter() {
            self.items.push(ModuleItem::ExternType {
                name,
                attributes,
                doc_comments: vec![],
                location: ItemLocation::test(),
                declaration_location: ItemLocation::test(),
            });
        }
        self
    }
    pub fn with_functions(mut self, functions: impl IntoIterator<Item = Function>) -> Self {
        for function in functions.into_iter() {
            self.items.push(ModuleItem::Function { function });
        }
        self
    }
    pub fn with_definitions(
        mut self,
        definitions: impl IntoIterator<Item = ItemDefinition>,
    ) -> Self {
        for definition in definitions.into_iter() {
            self.items.push(ModuleItem::Definition { definition });
        }
        self
    }
    pub fn with_impls(mut self, impls: impl IntoIterator<Item = FunctionBlock>) -> Self {
        for impl_block in impls.into_iter() {
            self.items.push(ModuleItem::Impl { impl_block });
        }
        self
    }
    pub fn with_splices(mut self, splices: impl IntoIterator<Item = Splice>) -> Self {
        for splice in splices.into_iter() {
            self.items.push(ModuleItem::Splice { splice });
        }
        self
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.items.push(ModuleItem::InnerAttributes {
            attributes: attributes.into(),
            location: ItemLocation::test(),
        });
        self
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
}
impl Module {
    pub fn uses(&self) -> impl Iterator<Item = &ModuleItem> {
        self.items
            .iter()
            .filter(|item| matches!(item, ModuleItem::Use { .. }))
    }
    pub fn extern_types(&self) -> impl Iterator<Item = &ModuleItem> {
        self.items
            .iter()
            .filter(|item| matches!(item, ModuleItem::ExternType { .. }))
    }
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Function { function } => Some(function),
            _ => None,
        })
    }
    pub fn definitions(&self) -> impl Iterator<Item = &ItemDefinition> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Definition { definition } => Some(definition),
            _ => None,
        })
    }
    pub fn impls(&self) -> impl Iterator<Item = &FunctionBlock> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Impl { impl_block } => Some(impl_block),
            _ => None,
        })
    }
    /// All module-level inner attributes (`#![...]`), flattened across any
    /// number of `#![...]` groups in the file.
    pub fn inner_attributes(&self) -> impl Iterator<Item = &crate::grammar::Attribute> {
        self.items.iter().flat_map(|item| match item {
            ModuleItem::InnerAttributes { attributes, .. } => attributes.0.iter(),
            _ => [].iter(),
        })
    }
    pub fn splices(&self) -> impl Iterator<Item = &Splice> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Splice { splice } => Some(splice),
            _ => None,
        })
    }
}

impl Parser {
    /// Skip over attributes during lookahead, returning the position after all attributes.
    /// Uses safe token access that won't panic on out-of-bounds.
    pub(crate) fn skip_attributes_lookahead(&self, start_pos: usize) -> usize {
        let mut pos = start_pos;

        // Skip past attributes
        while self
            .tokens
            .get(pos)
            .is_some_and(|t| matches!(t.kind, TokenKind::Hash))
        {
            pos += 1; // skip #
            if self
                .tokens
                .get(pos)
                .is_some_and(|t| matches!(t.kind, TokenKind::LBracket))
            {
                pos += 1; // skip [
                // Skip until matching ]
                let mut depth = 1;
                while depth > 0 {
                    match self.tokens.get(pos).map(|t| &t.kind) {
                        Some(TokenKind::LBracket) => depth += 1,
                        Some(TokenKind::RBracket) => depth -= 1,
                        None => break, // EOF reached
                        _ => {}
                    }
                    pos += 1;
                }
            }
        }

        pos
    }

    /// Get the token kind at a position, returning None if out of bounds.
    pub(crate) fn peek_at(&self, pos: usize) -> Option<&TokenKind> {
        self.tokens.get(pos).map(|t| &t.kind)
    }

    /// Skip over all comments and whitespace
    pub fn parse_module(&mut self) -> Result<Module, ParseError> {
        let mut items = Vec::new();
        let mut module_doc_comments = Vec::new();

        // Collect module-level doc comments (//!)
        while matches!(self.peek(), TokenKind::DocInner(_)) {
            if let TokenKind::DocInner(text) = &self.advance().kind {
                let content = text.strip_prefix("//!").unwrap_or(text).to_string();
                module_doc_comments.push(content);
            }
        }

        while !matches!(self.peek(), TokenKind::Eof) {
            // Collect non-doc comments (doc comments will be collected by item parsers)
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                if let Some(comment) = self.collect_comment() {
                    items.push(ModuleItem::Comment { comment });
                }
            }

            if matches!(self.peek(), TokenKind::Eof) {
                break;
            }

            // Parse module-level items
            items.push(self.parse_module_item()?);

            // Add any pending comments that were collected during parsing (e.g., inline comments after attributes)
            for comment in self.pending_comments.drain(..) {
                items.push(ModuleItem::Comment { comment });
            }

            // Collect any inline comments that appeared after the item
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                if let Some(comment) = self.collect_comment() {
                    items.push(ModuleItem::Comment { comment });
                }
            }
        }

        Ok(Module {
            items,
            doc_comments: module_doc_comments,
        })
    }

    /// Parse a single `#![...]` group into a [`ModuleItem::InnerAttributes`].
    fn parse_inner_attribute_item(&mut self) -> Result<ModuleItem, ParseError> {
        let start = self.current().location.span.start;
        self.advance(); // #
        self.advance(); // !
        self.expect(TokenKind::LBracket)?;
        let mut attrs = Vec::new();
        while !matches!(self.peek(), TokenKind::RBracket) {
            attrs.push(self.parse_attribute()?);
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            } else if !matches!(self.peek(), TokenKind::RBracket) {
                return Err(ParseError::ExpectedToken {
                    expected: vec![TokenKind::RBracket, TokenKind::Comma],
                    found: self.peek().clone(),
                    location: self.current().location,
                });
            }
        }
        let end = self.expect(TokenKind::RBracket)?.end_location();
        let location = self.item_location_from_locations(start, end);
        Ok(ModuleItem::InnerAttributes {
            attributes: Attributes(attrs),
            location,
        })
    }

    /// Parse a module-level item definition and consume its terminator.
    ///
    /// Brace-delimited items (`type Name { .. }`, `enum`, `bitflags`) are
    /// self-terminating, but value-like items (`const`, `extern`, a type alias,
    /// or an opaque `type Name`) must be terminated with `;` at module level.
    /// The item's [`ItemDefinition::terminator`] says whether it is
    /// self-terminating; the terminator is enforced here rather than inside
    /// `parse_item_definition` so that body contexts (type/enum/bitflags) can
    /// apply their own separator rules.
    fn parse_module_definition(&mut self) -> Result<ModuleItem, ParseError> {
        let definition = self.parse_item_definition()?;
        if definition.terminator() == ItemTerminator::Separated {
            self.expect(TokenKind::Semi)?;
        }
        Ok(ModuleItem::Definition { definition })
    }

    pub(crate) fn parse_module_item(&mut self) -> Result<ModuleItem, ParseError> {
        // Module-level inner attributes (`#![...]`). Detected before the
        // outer-attribute (`#[...]`) handling below via the leading `!`.
        if matches!(self.peek(), TokenKind::Hash) && matches!(self.peek_nth(1), TokenKind::Bang) {
            return self.parse_inner_attribute_item();
        }

        // Attributes can appear before any item
        let has_attributes = matches!(self.peek(), TokenKind::Hash);

        match self.peek() {
            TokenKind::Use => {
                let start_pos = self.current().location.span.start;
                self.parse_use(Visibility::Private, Attributes::default(), start_pos)
            }
            // `pub use ...;` — an explicit re-export.
            TokenKind::Pub if matches!(self.peek_nth(1), TokenKind::Use) => {
                let start_pos = self.current().location.span.start;
                self.advance(); // consume `pub`
                self.parse_use(Visibility::Public, Attributes::default(), start_pos)
            }
            // Standalone `prologue`/`epilogue` splice statements (ungated).
            TokenKind::Prologue | TokenKind::Epilogue => self
                .parse_splice()
                .map(|splice| ModuleItem::Splice { splice }),
            TokenKind::Extern if !has_attributes => {
                // Peek ahead to distinguish extern type from extern value. Extern
                // values are item definitions (`ItemDefinitionInner::ExternValue`),
                // so they route through `parse_item_definition`; extern types are
                // their own module-level construct.
                if matches!(self.peek_nth(1), TokenKind::Type) {
                    self.parse_extern_type()
                } else {
                    self.parse_module_definition()
                }
            }
            TokenKind::Hash => {
                // Attributes - need to peek ahead to see what comes after
                let mut pos = self.skip_attributes_lookahead(self.pos);

                // Skip over any comments (including doc comments) after attributes in lookahead
                while matches!(
                    self.peek_at(pos),
                    Some(
                        TokenKind::Comment(_)
                            | TokenKind::MultiLineComment(_)
                            | TokenKind::DocOuter(_)
                            | TokenKind::DocInner(_)
                    )
                ) {
                    pos += 1;
                }

                // Now check what comes after attributes (and comments)
                match self.peek_at(pos) {
                    Some(TokenKind::Extern) => {
                        // Could be extern type or extern value
                        if matches!(self.peek_at(pos + 1), Some(TokenKind::Type)) {
                            self.parse_extern_type()
                        } else {
                            self.parse_module_definition()
                        }
                    }
                    // Cfg-gated `use` — parse the attributes, then the use.
                    Some(TokenKind::Use) => {
                        let start_pos = self.current().location.span.start;
                        let attributes = self.parse_attributes()?;
                        self.parse_use(Visibility::Private, attributes, start_pos)
                    }
                    // Cfg-gated `prologue`/`epilogue` splice; `parse_splice`
                    // consumes its own leading attributes.
                    Some(TokenKind::Prologue | TokenKind::Epilogue) => self
                        .parse_splice()
                        .map(|splice| ModuleItem::Splice { splice }),
                    Some(TokenKind::Pub) => {
                        // Could be pub use, pub extern value, pub fn, or pub item definition
                        match self.peek_at(pos + 1) {
                            Some(TokenKind::Use) => {
                                let start_pos = self.current().location.span.start;
                                let attributes = self.parse_attributes()?;
                                self.advance(); // consume `pub`
                                self.parse_use(Visibility::Public, attributes, start_pos)
                            }
                            Some(TokenKind::Extern) => self.parse_module_definition(),
                            Some(TokenKind::Fn) => self
                                .parse_function()
                                .map(|function| ModuleItem::Function { function }),
                            _ => self.parse_module_definition(),
                        }
                    }
                    Some(
                        TokenKind::Type | TokenKind::Enum | TokenKind::Bitflags | TokenKind::Const,
                    ) => self.parse_module_definition(),
                    Some(TokenKind::Impl) => self
                        .parse_impl_block()
                        .map(|impl_block| ModuleItem::Impl { impl_block }),
                    Some(TokenKind::Fn) => self
                        .parse_function()
                        .map(|function| ModuleItem::Function { function }),
                    _ => {
                        // Lookahead couldn't determine item type - this often happens with
                        // malformed attributes. Let parse_item_definition handle it, which
                        // will properly parse (and error on) the attributes.
                        self.parse_module_definition()
                    }
                }
            }
            TokenKind::DocOuter(_) => {
                // Peek ahead to see what comes after doc comments
                let mut pos = self.pos;
                while matches!(self.peek_at(pos), Some(TokenKind::DocOuter(_))) {
                    pos += 1;
                }

                // Check if this is an extern type - skip any attributes first
                if matches!(self.peek_at(pos), Some(TokenKind::Hash)) {
                    pos = self.skip_attributes_lookahead(pos);
                }

                // Now check what comes after doc comments (and possibly attributes)
                if matches!(self.peek_at(pos), Some(TokenKind::Extern)) {
                    // extern type vs extern value
                    if matches!(self.peek_at(pos + 1), Some(TokenKind::Type)) {
                        self.parse_extern_type()
                    } else {
                        self.parse_module_definition()
                    }
                } else if matches!(self.peek_at(pos), Some(TokenKind::Pub))
                    && matches!(self.peek_at(pos + 1), Some(TokenKind::Extern))
                {
                    self.parse_module_definition()
                } else if matches!(self.peek_at(pos), Some(TokenKind::Fn))
                    || (matches!(self.peek_at(pos), Some(TokenKind::Pub))
                        && matches!(self.peek_at(pos + 1), Some(TokenKind::Fn)))
                {
                    self.parse_function()
                        .map(|function| ModuleItem::Function { function })
                } else {
                    self.parse_module_definition()
                }
            }
            TokenKind::Pub => {
                // Check if this is `pub fn` (freestanding function) or pub item definition
                if matches!(self.peek_nth(1), TokenKind::Fn) {
                    self.parse_function()
                        .map(|function| ModuleItem::Function { function })
                } else {
                    self.parse_module_definition()
                }
            }
            TokenKind::Type | TokenKind::Enum | TokenKind::Bitflags | TokenKind::Const => {
                self.parse_module_definition()
            }
            TokenKind::Impl => self
                .parse_impl_block()
                .map(|impl_block| ModuleItem::Impl { impl_block }),
            TokenKind::Fn => {
                // Freestanding function (private, or with attributes handled above)
                self.parse_function()
                    .map(|function| ModuleItem::Function { function })
            }
            _ => Err(ParseError::UnexpectedModuleToken {
                found: self.peek().clone(),
                location: self.current().location,
            }),
        }
    }
}

#[cfg(test)]
mod tests;
