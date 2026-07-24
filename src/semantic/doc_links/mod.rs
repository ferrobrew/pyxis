//! Resolution of rustdoc-style intra-doc links embedded in doc comments, e.g.
//! `[`Type`]`, `[`Type::method`]`, `[`Self::member`]`, and the inline form
//! `` [label](Type::method) ``.
//!
//! Every link is resolved exactly once, during semantic analysis
//! ([`resolve_all`]) — validation is that pass's failure path, and the
//! resulting per-module [`ModuleDocLinks`] tables (keyed by doc block) are
//! what the backends consume to rewrite or surface links. The [`DocLinkResolver`]
//! itself is also used live by the LSP to resolve links at edit time.

mod resolver;
mod scan;
mod types;

pub use resolver::{DocLinkResolver, resolve_all};
pub use scan::{DocLinkSyntax, ScannedLink, extract_links, is_path, scan_links};
pub use types::{
    DocBlockKey, DocLinkMemberKind, DocLinkPath, DocLinkTarget, DocLinks, ModuleDocLinks,
    ResolvedDocLink,
};
