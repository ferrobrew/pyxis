//! The public data model of resolved doc links: link targets, parsed link
//! paths, and the per-module tables backends consume.

use std::collections::BTreeMap;

use crate::{
    grammar::{ItemPath, ItemPathSegment},
    span::ItemLocation,
};

/// A resolved intra-doc link target.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DocLinkTarget {
    /// A type / enum / bitflags / type alias item.
    Item(ItemPath),
    /// A member of an item: an associated/vftable method, enum variant, or flag.
    Member {
        item: ItemPath,
        name: String,
        kind: DocLinkMemberKind,
    },
    /// A freestanding, module-level function.
    Function { module: ItemPath, name: String },
    /// A module-level extern value (global).
    ExternValue { module: ItemPath, name: String },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DocLinkMemberKind {
    Method,
    VftableMethod,
    Field,
    Variant,
    Flag,
    Constant,
    ExternValue,
}

/// A parsed intra-doc link path: an optional leading `Self` plus the remaining
/// `::`-separated segments.
///
/// This is the *only* place link text is split into segments — everything
/// downstream of [`DocLinkPath::parse`] works with the structured form, so the
/// resolution path never re-derives structure from strings.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DocLinkPath {
    /// Whether the written path began with `Self` (bare `Self` or `Self::…`).
    pub self_prefixed: bool,
    /// The path segments after any `Self` prefix.
    pub segments: Vec<ItemPathSegment>,
}

impl DocLinkPath {
    pub fn parse(text: &str) -> Self {
        let mut parts = text.split("::").peekable();
        let self_prefixed = parts.peek() == Some(&"Self");
        if self_prefixed {
            parts.next();
        }
        DocLinkPath {
            self_prefixed,
            segments: parts.map(ItemPathSegment::from).collect(),
        }
    }
}

/// A single doc link resolved to its target, alongside the exact path text
/// written in the source — the text a backend substitutes when rewriting the
/// link destination.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedDocLink {
    pub text: String,
    pub target: DocLinkTarget,
}

/// Identity of one doc block within a module, for keying resolved links.
///
/// The module's own (`//!`-style) doc block gets a dedicated variant rather
/// than being keyed by `Module::location()`: that location is a *proxy*
/// borrowed from the module's first item (see `Module::from_ast`), so using
/// it as a key would collide with that item's own doc block. Every other
/// doc-bearing node is keyed by its source location, which is unique — no two
/// distinct nodes share an identical full span.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DocBlockKey {
    /// The module's own doc block.
    Module,
    /// A doc-bearing node (item, field, function, variant, flag, …) at this
    /// source location.
    Node(ItemLocation),
}

/// Every resolved intra-doc link in one module's documentation, keyed by the
/// owning doc block. Produced once by [`resolve_all`] during semantic
/// analysis; backends look their links up here rather than re-scanning and
/// re-resolving doc text with locally-reconstructed context.
///
/// [`resolve_all`]: super::resolve_all
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ModuleDocLinks {
    pub by_block: BTreeMap<DocBlockKey, Vec<ResolvedDocLink>>,
}

impl ModuleDocLinks {
    /// The resolved links of the doc block owned by the node at `location`.
    pub fn at(&self, location: &ItemLocation) -> &[ResolvedDocLink] {
        self.by_block
            .get(&DocBlockKey::Node(*location))
            .map(Vec::as_slice)
            .unwrap_or_default()
    }

    /// The resolved links of the module's own doc block.
    pub fn module_doc(&self) -> &[ResolvedDocLink] {
        self.by_block
            .get(&DocBlockKey::Module)
            .map(Vec::as_slice)
            .unwrap_or_default()
    }

    /// Iterate every resolved link in the module.
    pub fn iter(&self) -> impl Iterator<Item = &ResolvedDocLink> {
        self.by_block.values().flatten()
    }
}

/// The resolved doc links of every module in the crate, keyed by module path.
pub type DocLinks = BTreeMap<ItemPath, ModuleDocLinks>;
