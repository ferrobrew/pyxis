use super::*;

use pyxis::semantic::doc_links::{DocLinkSyntax, DocLinkTarget, ScannedLink};

impl ServerState {
    /// Doc-comment links that reference `symbol`, returning the span of the
    /// symbol's name within each link's path (a type's name segment, or a
    /// member's leaf) so rename/find-references reach into doc comments too.
    pub(crate) fn doc_link_occurrences(
        &self,
        symbol: &Symbol,
        from_uri: &Uri,
    ) -> Vec<lsp_types::Location> {
        let name = match symbol {
            Symbol::Type(p) => match p.last() {
                Some(s) => s.as_str().to_string(),
                None => return Vec::new(),
            },
            Symbol::Member { name, .. } => name.clone(),
        };
        let matches = |target: &DocLinkTarget| match (symbol, target) {
            (Symbol::Type(p), DocLinkTarget::Item(q)) => q == p,
            (Symbol::Type(p), DocLinkTarget::Member { item, .. }) => item == p,
            (Symbol::Member { owner, name: n }, DocLinkTarget::Member { item, name: m, .. }) => {
                item == owner && m == n
            }
            _ => false,
        };

        let from_root = self
            .documents
            .get(from_uri)
            .and_then(|d| d.project_root.clone());
        let pointer_size = self.pointer_size_for(from_uri);
        let source_set = semantic::SourceSet::new(&self.db, self.sources_for(from_uri));
        let analysis = semantic::analyze(&self.db, pointer_size, source_set);
        let resolver = analysis.doc_link_resolver(&self.db);

        let mut uris: Vec<Uri> = self
            .documents
            .iter()
            .filter(|(_, d)| d.project_root == from_root)
            .map(|(u, _)| u.clone())
            .collect();
        uris.sort_by(|a, b| a.as_str().cmp(b.as_str()));

        let mut out = Vec::new();
        for uri in &uris {
            let Some(content) = self.get_content(uri) else {
                continue;
            };
            let tokens_arc = self.tokens_for(uri);
            let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);
            let scope = self.scope_for(uri);
            let doc_lines: std::collections::HashSet<usize> = tokens
                .iter()
                .filter_map(|t| match &t.kind {
                    TokenKind::DocOuter(_) => Some(t.location.span.start.line),
                    _ => None,
                })
                .collect();
            let lines: Vec<&str> = content.lines().collect();
            for &line_no in &doc_lines {
                let Some(line) = lines.get(line_no - 1) else {
                    continue;
                };
                for dl in scan_doc_links(line) {
                    if !resolver
                        .resolve(&scope, &dl.path)
                        .is_some_and(|t| matches(&t))
                    {
                        continue;
                    }
                    let mut push_region = |(region_start, region_end): (usize, usize)| {
                        for off in whole_word_offsets(&line[region_start..region_end], &name) {
                            let start = region_start + off;
                            let span = Span::new(
                                Location::new(line_no, start + 1),
                                Location::new(line_no, start + name.len() + 1),
                            );
                            out.push(lsp_types::Location {
                                uri: uri.clone(),
                                range: pyxis_span_to_lsp_range(content, &span),
                            });
                        }
                    };
                    // Always rewrite the name in the path. For an inline link
                    // the label is separate from the destination, so rewrite it
                    // too when it's an exact echo of the name (`` [`Foo`](Foo) ``)
                    // — never arbitrary prose like `[the Foo struct]`. For a
                    // shortcut the label *is* the path (already pushed), so
                    // pushing it again would double-count.
                    push_region(dl.path_region);
                    if dl.syntax == DocLinkSyntax::Inline {
                        let label = line[dl.label_region.0..dl.label_region.1]
                            .trim()
                            .trim_matches('`')
                            .trim();
                        if label == name {
                            push_region(dl.label_region);
                        }
                    }
                }
            }
        }
        out
    }

    /// textDocument/documentLink — make doc-comment cross-references
    /// (`[Foo]`, [`Foo`], `[label](path::To)`) clickable, targeting the
    /// referenced item's file and line.
    pub fn handle_document_link(&self, req: Request) -> Response {
        let links = serde_json::from_value::<DocumentLinkParams>(req.params.clone())
            .ok()
            .map(|p| self.doc_links(&p.text_document.uri))
            .unwrap_or_default();
        Response {
            id: req.id,
            result: Some(serde_json::to_value(links).unwrap()),
            error: None,
        }
    }

    fn doc_links(&self, uri: &Uri) -> Vec<DocumentLink> {
        let Some(ctx) = self.analysis_ctx(uri) else {
            return vec![];
        };
        let AnalysisCtx {
            content,
            scope,
            type_registry,
            analysis,
            tokens_arc,
            ..
        } = ctx;
        let resolver = analysis.doc_link_resolver(&self.db);
        let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);

        // Source lines (1-indexed) that are doc comments.
        let doc_lines: std::collections::HashSet<usize> = tokens
            .iter()
            .filter_map(|t| match &t.kind {
                TokenKind::DocOuter(_) => Some(t.location.span.start.line),
                _ => None,
            })
            .collect();

        let lines: Vec<&str> = content.lines().collect();
        let mut links = Vec::new();
        for &line_no in &doc_lines {
            let Some(line) = lines.get(line_no - 1) else {
                continue;
            };
            for dl in scan_doc_links(line) {
                let item_path = match resolver.resolve(&scope, &dl.path) {
                    Some(DocLinkTarget::Item(p)) => p,
                    Some(DocLinkTarget::Member { item, .. }) => item,
                    _ => continue,
                };
                let Some(rd) = self.resolved_definition(&item_path, type_registry, uri) else {
                    continue;
                };
                let span = Span::new(
                    Location::new(line_no, dl.link.0 + 1),
                    Location::new(line_no, dl.link.1 + 1),
                );
                let target = format!("{}#L{}", rd.uri.as_str(), rd.name_span.start.line)
                    .parse()
                    .unwrap_or(rd.uri);
                links.push(DocumentLink {
                    range: pyxis_span_to_lsp_range(content, &span),
                    target: Some(target),
                    tooltip: Some(item_path.to_string()),
                    data: None,
                });
            }
        }
        links
    }

    /// Locate a member (`Type::member`) referenced by a doc link: its file, the
    /// name span to jump to, and hover markdown. Covers impl methods (possibly
    /// in another file), vftable methods, and fields. `None` for members it
    /// can't pin down (callers fall back to the owning type).
    ///
    /// Returns the first declaration in deterministic (URI-sorted) order; for
    /// the *complete* set across every (possibly `#[cfg]`-gated) impl block use
    /// [`Self::resolve_doc_members`].
    pub(crate) fn resolve_doc_member(
        &self,
        item: &ItemPath,
        name: &str,
        from_uri: &Uri,
    ) -> Option<(Uri, Span, String)> {
        self.resolve_doc_members(item, name, from_uri)
            .into_iter()
            .next()
    }

    /// Every declaration of a member (`Type::member`) across the project: the
    /// method in *each* impl block (including separate `#[cfg(...)]`-gated ones,
    /// possibly in different files), the vftable method, or the field. Each entry
    /// is `(file, name span, hover markdown)`. Powers find-references / rename /
    /// highlight, which must touch *all* declarations, not just the first one
    /// (the `HashMap` iteration order made "first" nondeterministic too).
    ///
    /// Iterates files in a stable URI-sorted order so the results — and the
    /// `resolve_doc_member` "primary" derived from them — are deterministic.
    pub(crate) fn resolve_doc_members(
        &self,
        item: &ItemPath,
        name: &str,
        from_uri: &Uri,
    ) -> Vec<(Uri, Span, String)> {
        use pyxis::grammar::{ImplItem, ItemDefinitionInner};

        let from_root = self
            .documents
            .get(from_uri)
            .and_then(|d| d.project_root.clone());
        let decl_registry = self.decl_registry_for(from_uri);

        let mut uris: Vec<Uri> = self
            .documents
            .iter()
            .filter(|(_, d)| d.project_root == from_root)
            .map(|(u, _)| u.clone())
            .collect();
        uris.sort_by(|a, b| a.as_str().cmp(b.as_str()));

        let mut out: Vec<(Uri, Span, String)> = Vec::new();
        for uri in &uris {
            let Some(module) = self.get_parsed_module(uri) else {
                continue;
            };
            let tokens_arc = self.tokens_for(uri);
            let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);
            let scope = self.scope_for(uri);
            let module_path = self.module_path_for(uri);
            let fn_hit = |f: &Function| -> (Uri, Span, String) {
                let span = name_token_span(tokens, &f.location.span.start, name)
                    .unwrap_or(f.location.span);
                (uri.clone(), span, format_function_hover(f))
            };
            for node in &module.items {
                match node {
                    // Impl methods — the impl block may live in a different file,
                    // and the same method may be declared in several (e.g.
                    // `#[cfg]`-gated) impl blocks: collect every one.
                    ModuleItem::Impl { impl_block } => {
                        let target = ItemPath::from(impl_block.name.as_str());
                        if resolve_type_path(&target, &scope, decl_registry).as_ref() == Some(item)
                        {
                            for impl_item in &impl_block.items {
                                if let ImplItem::Function(f) = impl_item
                                    && f.name.as_str() == name
                                {
                                    out.push(fn_hit(f));
                                }
                            }
                        }
                    }
                    // The type's own body: vftable methods and fields.
                    ModuleItem::Definition { definition } => {
                        let def_path = match &module_path {
                            Some(m) => m.join(definition.name.as_str().into()),
                            None => ItemPath::from(definition.name.as_str()),
                        };
                        if &def_path != item {
                            continue;
                        }
                        if let ItemDefinitionInner::Type(td) = &definition.inner {
                            for statement in td.statements() {
                                match &statement.field {
                                    TypeField::Vftable(fns) => {
                                        for f in fns {
                                            if f.name.as_str() == name {
                                                out.push(fn_hit(f));
                                            }
                                        }
                                    }
                                    TypeField::Field(_, field_name, _) => {
                                        if field_name.as_str() == name {
                                            let span = name_token_span(
                                                tokens,
                                                &statement.location.span.start,
                                                name,
                                            )
                                            .unwrap_or(statement.location.span);
                                            out.push((
                                                uri.clone(),
                                                span,
                                                format!("**field** `{name}`"),
                                            ));
                                        }
                                    }
                                    TypeField::Item(_) => {
                                        // Nested items are handled at the top level;
                                        // their internal members are not searched here.
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        out
    }

    /// If `loc` sits on a doc-comment cross-reference (`[Foo]`, [`Foo`],
    /// `[label](Type::method)`), returns the whole link's source span, the
    /// resolved target's location, and hover markdown. `Type::member` links
    /// resolve to the member itself (the impl/vftable method or field), not just
    /// the owning type. Lets hover and go-to-definition follow doc links — Zed
    /// navigates them through go-to-definition, not documentLink.
    pub(crate) fn doc_link_at(
        &self,
        uri: &Uri,
        loc: &Location,
    ) -> Option<(Span, lsp_types::Location, String)> {
        let AnalysisCtx {
            content,
            scope,
            type_registry,
            decl_registry,
            analysis,
            tokens_arc,
            ..
        } = self.analysis_ctx(uri)?;
        let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);
        // Only act inside a doc comment.
        if !tokens.iter().any(|t| {
            matches!(t.kind, TokenKind::DocOuter(_)) && t.location.span.start.line == loc.line
        }) {
            return None;
        }
        let line = content.lines().nth(loc.line - 1)?;
        let col = loc.column.saturating_sub(1); // 0-indexed byte column
        let resolver = analysis.doc_link_resolver(&self.db);

        let to_type = |this: &Self, item: &ItemPath| -> Option<(lsp_types::Location, String)> {
            let rd = this.resolved_definition(item, type_registry, uri)?;
            let target_content = this.get_content(&rd.uri)?;
            let hover = this.type_hover_text(item, type_registry, decl_registry, uri)?;
            Some((
                lsp_types::Location {
                    uri: rd.uri.clone(),
                    range: pyxis_span_to_lsp_range(target_content, &rd.name_span),
                },
                hover,
            ))
        };

        for dl in scan_doc_links(line) {
            if col < dl.link.0 || col >= dl.link.1 {
                continue;
            }
            let link_span = Span::new(
                Location::new(loc.line, dl.link.0 + 1),
                Location::new(loc.line, dl.link.1 + 1),
            );
            let (location, hover) = match resolver.resolve(&scope, &dl.path)? {
                DocLinkTarget::Item(p) => to_type(self, &p)?,
                DocLinkTarget::Member { item, name, .. } => {
                    match self.resolve_doc_member(&item, &name, uri) {
                        Some((muri, mspan, mhover)) => {
                            let mcontent = self.get_content(&muri)?;
                            (
                                lsp_types::Location {
                                    uri: muri.clone(),
                                    range: pyxis_span_to_lsp_range(mcontent, &mspan),
                                },
                                mhover,
                            )
                        }
                        None => to_type(self, &item)?,
                    }
                }
                _ => return None,
            };
            return Some((link_span, location, hover));
        }
        None
    }
}

/// Find Markdown cross-reference links in one doc-comment line: `[Foo]`,
/// `` [`Foo`] ``, and `[label](path::To)`. Backed by the compiler's shared
/// [`scan_links`](pyxis::semantic::doc_links::scan_links), so the LSP and the
/// backends recognise exactly the same links (only `::`-path targets; prose and
/// URLs are skipped). Unlike the compiler, the LSP surfaces every syntactic
/// form — including bare `[Foo]` shortcuts — as navigable.
///
/// `line` is a raw source line, so its leading indentation and `///`/`//!`
/// marker are stripped before scanning: CommonMark treats a line indented ≥4
/// columns as a code block and does no inline parsing, which would suppress
/// every link on a doc comment inside a type body (the compiler avoids this by
/// scanning already-stripped doc text). Offsets are shifted back so they stay
/// in raw-line byte coordinates.
pub(crate) fn scan_doc_links(line: &str) -> Vec<ScannedLink> {
    let Some(off) = doc_content_start(line) else {
        return Vec::new();
    };
    let shift = |(a, b): (usize, usize)| (a + off, b + off);
    pyxis::semantic::doc_links::scan_links(&line[off..])
        .into_iter()
        .map(|l| ScannedLink {
            link: shift(l.link),
            path_region: shift(l.path_region),
            label_region: shift(l.label_region),
            ..l
        })
        .collect()
}

/// Byte offset where a doc comment's Markdown content begins — past leading
/// indentation and the `///` (outer) or `//!` (inner) marker. `None` if `line`
/// isn't a doc comment (no marker after its indentation).
fn doc_content_start(line: &str) -> Option<usize> {
    let indent = line.len() - line.trim_start().len();
    let rest = &line[indent..];
    if rest.starts_with("///") || rest.starts_with("//!") {
        Some(indent + 3)
    } else {
        None
    }
}

/// Byte offsets of every whole-word (identifier-boundary) occurrence of `word`
/// within `text`.
pub(crate) fn whole_word_offsets(text: &str, word: &str) -> Vec<usize> {
    if word.is_empty() {
        return Vec::new();
    }
    // Match `is_valid_identifier`'s Unicode notion of an identifier char, so
    // boundaries are correct next to non-ASCII letters (pyxis identifiers are
    // Unicode), not just ASCII.
    let is_ident = |c: char| c.is_alphanumeric() || c == '_';
    let mut out = Vec::new();
    let mut from = 0;
    while let Some(rel) = text[from..].find(word) {
        let start = from + rel;
        let before_ok = start == 0 || !text[..start].chars().next_back().is_some_and(is_ident);
        let after = start + word.len();
        let after_ok = after >= text.len() || !text[after..].chars().next().is_some_and(is_ident);
        if before_ok && after_ok {
            out.push(start);
        }
        from = start + word.len();
    }
    out
}

#[cfg(test)]
mod tests {
    use super::{scan_doc_links, whole_word_offsets};

    #[test]
    fn scans_links_on_indented_doc_lines() {
        // A doc comment inside a type body is indented; the raw line's leading
        // whitespace + `///` must not trip CommonMark's indented-code-block rule
        // (≥4 columns), which would otherwise find no links at all.
        let indented = "    /// See ([`GetRenderPassName`]) for details.";
        let links = scan_doc_links(indented);
        assert_eq!(links.len(), 1, "expected one link in {indented:?}");
        assert_eq!(links[0].path, "GetRenderPassName");
        // Offsets stay in raw-line coordinates: the code-span content sits where
        // it actually appears in the source line.
        let (s, e) = links[0].path_region;
        assert_eq!(&indented[s..e], "GetRenderPassName");

        // Same content unindented resolves to the same path (parity check).
        assert_eq!(
            scan_doc_links("/// See ([`GetRenderPassName`]).")[0].path,
            "GetRenderPassName",
        );
    }

    #[test]
    fn ignores_non_doc_lines() {
        assert!(scan_doc_links("    let x = [0];").is_empty());
    }

    #[test]
    fn whole_word_offsets_respects_ascii_boundaries() {
        // Standalone occurrence matches; substring within a larger word doesn't.
        assert_eq!(whole_word_offsets("Foo and Foobar", "Foo"), vec![0]);
        assert_eq!(whole_word_offsets("a_Foo Foo", "Foo"), vec![6]);
        assert_eq!(whole_word_offsets("Foo, Foo", "Foo"), vec![0, 5]);
    }

    #[test]
    fn whole_word_offsets_respects_unicode_boundaries() {
        // A non-ASCII letter directly abutting the word forms one identifier, so
        // the embedded occurrence must NOT match (the old ASCII-only predicate
        // treated `é`/`ä` as a boundary and rewrote the substring incorrectly).
        assert!(whole_word_offsets("café_Foo", "Foo").is_empty());
        assert!(whole_word_offsets("Fooä", "Foo").is_empty());
        assert!(whole_word_offsets("πFoo", "Foo").is_empty());
        // A standalone Unicode identifier still matches whole-word.
        assert_eq!(whole_word_offsets("café Δσ café", "café"), vec![0, 11]);
    }
}
