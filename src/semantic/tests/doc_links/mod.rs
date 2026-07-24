//! Tests for rustdoc-style intra-doc link resolution.

use crate::{
    grammar::test_aliases::*,
    semantic::{
        SemanticBuilder, SemanticError,
        doc_links::{DocLinkMemberKind, DocLinkSyntax, DocLinkTarget, extract_links, scan_links},
    },
};

use super::util::*;

mod extraction;
mod resolution;
mod validation;

/// Parse a written link path for `resolve()`.
pub(super) fn segs(s: &str) -> crate::semantic::doc_links::DocLinkPath {
    crate::semantic::doc_links::DocLinkPath::parse(s)
}
