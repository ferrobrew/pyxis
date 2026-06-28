use super::*;

impl ServerState {
    /// Doc-comment links that reference `symbol`, returning the span of the
    /// symbol's name within each link's path (a type's name segment, or a
    /// member's leaf) so rename/find-references reach into doc comments too.
    pub(crate) fn doc_link_occurrences(
        &self,
        symbol: &Symbol,
        from_uri: &Uri,
    ) -> Vec<lsp_types::Location> {
        use pyxis::semantic::doc_links::DocLinkTarget;
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

        let uris: Vec<Uri> = self
            .documents
            .iter()
            .filter(|(_, d)| d.project_root == from_root)
            .map(|(u, _)| u.clone())
            .collect();

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
                    // Always rewrite the name in the path. Rewrite the bracket
                    // label only when it's an exact echo of the name (`[Foo]` /
                    // [`Foo`]) — never arbitrary prose like `[the Foo struct]`.
                    // (For a shortcut link the two regions coincide; guard so we
                    // don't double-count.)
                    push_region(dl.path_region);
                    if dl.label_region != dl.path_region {
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
        use pyxis::semantic::doc_links::DocLinkTarget;

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
    pub(crate) fn resolve_doc_member(
        &self,
        item: &ItemPath,
        name: &str,
        from_uri: &Uri,
    ) -> Option<(Uri, Span, String)> {
        use pyxis::grammar::{ImplItem, ItemDefinitionInner};

        let from_root = self
            .documents
            .get(from_uri)
            .and_then(|d| d.project_root.clone());
        let decl_registry = self.decl_registry_for(from_uri);

        let uris: Vec<Uri> = self
            .documents
            .iter()
            .filter(|(_, d)| d.project_root == from_root)
            .map(|(u, _)| u.clone())
            .collect();

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
                    // Impl methods — the impl block may live in a different file.
                    ModuleItem::Impl { impl_block } => {
                        let target = ItemPath::from(impl_block.name.as_str());
                        if resolve_type_path(&target, &scope, decl_registry).as_ref() == Some(item)
                        {
                            for impl_item in &impl_block.items {
                                if let ImplItem::Function(f) = impl_item
                                    && f.name.as_str() == name
                                {
                                    return Some(fn_hit(f));
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
                                                return Some(fn_hit(f));
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
                                            return Some((
                                                uri.clone(),
                                                span,
                                                format!("**field** `{name}`"),
                                            ));
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        None
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
        use pyxis::semantic::doc_links::DocLinkTarget;
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

/// A Markdown cross-reference link found in a doc-comment line.
pub(crate) struct DocLink {
    /// Byte range of the whole link (`[Foo]` / [`Foo`] / `[label](path)`).
    link: (usize, usize),
    /// Resolved path text (backticks/whitespace trimmed) — what resolves.
    path: String,
    /// Byte range of the path *text* in the line: the `(...)` content for an
    /// inline link, or the bracket content for a shortcut (`[Foo]`).
    path_region: (usize, usize),
    /// Byte range of the bracket label `[...]`. Equals `path_region` for a
    /// shortcut link. Distinguished so a rename rewrites the path but only an
    /// echoing label — never prose.
    label_region: (usize, usize),
}

/// Find Markdown cross-reference links in one doc-comment line: `[Foo]`,
/// [`Foo`], and `[label](path::To)`. Only `::`-path targets are kept (per the
/// compiler's [`is_path`](pyxis::semantic::doc_links::is_path)); prose and URLs
/// are skipped.
pub(crate) fn scan_doc_links(line: &str) -> Vec<DocLink> {
    use pyxis::semantic::doc_links::is_path;
    let mut out = Vec::new();
    let mut i = 0;
    while let Some(rel_open) = line[i..].find('[') {
        let open = i + rel_open;
        let Some(rel_close) = line[open + 1..].find(']') else {
            break;
        };
        let close = open + 1 + rel_close;
        let inner = &line[open + 1..close];
        let after = close + 1;
        let label_region = (open + 1, close);
        // `[label](path)` → path is the URL and the link extends through `)`;
        // otherwise the bracket content is the path and the link ends at `]`.
        let (path_raw, link_end, path_region) = if line[after..].starts_with('(') {
            match line[after + 1..].find(')') {
                Some(rp) => {
                    let region = (after + 1, after + 1 + rp);
                    (line[region.0..region.1].to_string(), region.1 + 1, region)
                }
                None => {
                    i = close + 1;
                    continue;
                }
            }
        } else {
            (inner.to_string(), close + 1, label_region)
        };
        let path = path_raw.trim().trim_matches('`').trim().to_string();
        // Defer to the compiler's notion of a link target so we recognise the
        // same paths it resolves (and skip prose / URLs up-front).
        if is_path(&path) {
            out.push(DocLink {
                link: (open, link_end),
                path,
                path_region,
                label_region,
            });
        }
        i = link_end;
    }
    out
}

/// Byte offsets of every whole-word (identifier-boundary) occurrence of `word`
/// within `text`.
pub(crate) fn whole_word_offsets(text: &str, word: &str) -> Vec<usize> {
    if word.is_empty() {
        return Vec::new();
    }
    let is_ident = |c: char| c.is_ascii_alphanumeric() || c == '_';
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
