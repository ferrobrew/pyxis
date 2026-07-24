use std::{collections::BTreeSet, str::FromStr};

use crate::{
    grammar::ItemPath,
    semantic::{
        TypeRegistry,
        doc_links::{DocLinkTarget, ModuleDocLinks, ResolvedDocLink},
    },
    span::ItemLocation,
};

use super::helpers::{
    doc_to_tokens, extern_value_accessor_doc_path, find_module_prefix_len, flatten_type_name,
};

/// Context for rewriting intra-doc links in emitted docs.
///
/// Each link's *resolved target* — determined once during semantic analysis
/// and stored in the module's [`ModuleDocLinks`] table — is rendered as an
/// absolute Rust path (`crate::module::Outer_Inner::member`), flattening
/// nested-item names and substituting extern-value accessors. Rewriting from
/// the target rather than the written text means the destination is always
/// what the link actually resolved to: no leaf-name rewrite maps that can
/// collide, no doc-driven `use` imports for rustdoc's benefit.
pub(super) struct DocLinkCx<'a> {
    pub(super) links: &'a ModuleDocLinks,
    pub(super) type_registry: &'a TypeRegistry,
    pub(super) module_paths: &'a BTreeSet<ItemPath>,
    /// The module being emitted; extern-value accessor paths in the same
    /// module stay relative.
    pub(super) module_path: &'a ItemPath,
    pub(super) prefix: Option<&'a ItemPath>,
    /// `crate` or `crate::<prefix>`.
    pub(super) root: String,
}

impl DocLinkCx<'_> {
    /// Doc tokens for the doc block owned by the node at `location`.
    pub(super) fn node(&self, doc: &[String], location: &ItemLocation) -> proc_macro2::TokenStream {
        doc_to_tokens(false, doc, Some((self, self.links.at(location))))
    }

    /// Doc tokens for the module's own (`//!`) doc block.
    pub(super) fn module_doc(&self, doc: &[String]) -> proc_macro2::TokenStream {
        doc_to_tokens(true, doc, Some((self, self.links.module_doc())))
    }

    /// The absolute Rust path of an item: `{root}::{module}::{FlatName}`,
    /// flattening nested-item segments (`module::Outer::Inner` →
    /// `crate::module::Outer_Inner`).
    pub(super) fn absolute_item_path(&self, path: &ItemPath) -> String {
        let module_len = find_module_prefix_len(path, self.module_paths);
        let root = &self.root;
        if path.len() > module_len + 1 {
            let flat_name = flatten_type_name(path, self.module_paths);
            let module_part: Vec<&str> = path.iter().take(module_len).map(|s| s.as_str()).collect();
            if module_part.is_empty() {
                format!("{root}::{flat_name}")
            } else {
                format!("{root}::{}::{flat_name}", module_part.join("::"))
            }
        } else {
            format!("{root}::{path}")
        }
    }

    /// Render a resolved target as the destination rustdoc should see, or
    /// `None` to leave the written link untouched (predefined types, which
    /// rustdoc resolves natively as primitives).
    fn render_target(&self, target: &DocLinkTarget) -> Option<String> {
        use crate::semantic::doc_links::DocLinkMemberKind;
        match target {
            DocLinkTarget::Item(path) => {
                let predefined = self
                    .type_registry
                    .get(path, &ItemLocation::internal())
                    .is_ok_and(|i| i.category == crate::semantic::types::ItemCategory::Predefined);
                if predefined {
                    return None;
                }
                Some(self.absolute_item_path(path))
            }
            DocLinkTarget::Member { item, name, kind } => match kind {
                DocLinkMemberKind::ExternValue => Some(self.accessor_path(item, name)),
                _ => Some(format!("{}::{name}", self.absolute_item_path(item))),
            },
            DocLinkTarget::Function { module, name } => Some(if module.is_empty() {
                format!("{}::{name}", self.root)
            } else {
                format!("{}::{module}::{name}", self.root)
            }),
            DocLinkTarget::ExternValue { module, name } => Some(self.accessor_path(module, name)),
        }
    }

    /// The rustdoc path of an extern value's `get_<name>` accessor, given the
    /// value's parent (module or type) and name.
    fn accessor_path(&self, parent: &ItemPath, name: &str) -> String {
        let value_path = parent.join(crate::grammar::ItemPathSegment::from(name));
        extern_value_accessor_doc_path(
            &value_path,
            self.module_path,
            self.module_paths,
            self.prefix,
        )
    }

    /// Rewrite every resolved link in `line` to its rendered destination.
    ///
    /// Link spans come from [`scan_links`](crate::semantic::doc_links::scan_links)
    /// (shared with the compiler and LSP) and are substituted right-to-left so
    /// earlier offsets stay valid. An inline link keeps its label and gets its
    /// destination replaced; a code shortcut becomes an inline link so its
    /// visible label survives the rewrite. Bare `[Path]` shortcuts aren't
    /// resolved by the compiler and are left alone.
    pub(super) fn rewrite_line(&self, line: &str, block: &[ResolvedDocLink]) -> String {
        use crate::semantic::doc_links::DocLinkSyntax;
        if block.is_empty() {
            return line.to_string();
        }
        let mut result = line.to_string();
        let mut scanned = crate::semantic::doc_links::scan_links(line);
        scanned.retain(|l| l.syntax != DocLinkSyntax::PlainShortcut);
        for link in scanned.into_iter().rev() {
            let Some(resolved) = block.iter().find(|r| r.text == link.path) else {
                continue;
            };
            let Some(dest) = self.render_target(&resolved.target) else {
                continue;
            };
            match link.syntax {
                DocLinkSyntax::Inline => {
                    result.replace_range(link.path_region.0..link.path_region.1, &dest);
                }
                DocLinkSyntax::CodeShortcut | DocLinkSyntax::PlainShortcut => {
                    let label = &line[link.label_region.0..link.label_region.1];
                    result.replace_range(link.link.0..link.link.1, &format!("[{label}]({dest})"));
                }
            }
        }
        result
    }
}

pub(super) fn hex_literal(value: impl Into<usize>) -> proc_macro2::Literal {
    // https://stackoverflow.com/a/78902864
    proc_macro2::Literal::from_str(&format!("0x{:X}", value.into())).unwrap()
}
