use super::*;

impl ServerState {
    /// textDocument/codeAction — currently: import an unresolved type.
    #[allow(clippy::mutable_key_type)] // lsp_types::Uri key is fine here
    pub fn handle_code_action(&self, req: Request) -> Response {
        let params: CodeActionParams = match serde_json::from_value(req.params.clone()) {
            Ok(p) => p,
            Err(e) => return error_response(req.id, e),
        };
        let uri = &params.text_document.uri;

        let actions = self.import_actions(uri, params.range);
        Response {
            id: req.id,
            result: Some(serde_json::to_value(actions).unwrap()),
            error: None,
        }
    }

    /// "Import `path`" quick-fixes for an unresolved type reference under the
    /// cursor: each declared item across the project whose name matches the
    /// unresolved ident becomes one action, adding (or extending) a `use`.
    #[allow(clippy::mutable_key_type)] // lsp_types::Uri key is fine here
    fn import_actions(&self, uri: &Uri, range: Range) -> Vec<CodeActionOrCommand> {
        let Some(ctx) = self.analysis_ctx(uri) else {
            return vec![];
        };
        let AnalysisCtx {
            content,
            module,
            scope,
            type_registry,
            decl_registry,
            pointer_size,
            tokens_arc,
            ..
        } = ctx;
        let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);
        let loc = lsp_position_to_pyxis_location(content, range.start);

        // Only act on a *bare, unresolved* type reference under the cursor.
        let name = match find_reference_at(
            &module,
            &loc,
            &scope,
            decl_registry,
            tokens,
            type_registry,
            pointer_size,
        ) {
            Some(Ref::Item {
                item: None,
                module_path,
                ..
            }) if module_path.len() == 1 => module_path.last().map(|s| s.as_str().to_string()),
            _ => None,
        };
        let Some(name) = name else {
            return vec![];
        };

        let own_module = self.module_path_for(uri);

        // Declared items elsewhere in the project whose name matches — each a
        // candidate import. Deduplicate and sort for stable ordering.
        let mut candidates: Vec<ItemPath> = decl_registry
            .item_paths()
            .filter(|p| {
                p.len() > 1
                    && p.last().map(|s| s.as_str()) == Some(name.as_str())
                    && p.parent().as_ref() != own_module.as_ref()
            })
            .cloned()
            .collect();
        candidates.sort_by_key(|p| p.to_string());
        candidates.dedup();

        candidates
            .into_iter()
            .filter_map(|cand| {
                let (range, new_text) = self.import_edit(&module, content, &cand)?;
                let mut changes = HashMap::new();
                changes.insert(uri.clone(), vec![TextEdit { range, new_text }]);
                Some(CodeActionOrCommand::CodeAction(CodeAction {
                    title: format!("Import `{cand}`"),
                    kind: Some(CodeActionKind::QUICKFIX),
                    edit: Some(WorkspaceEdit {
                        changes: Some(changes),
                        document_changes: None,
                        change_annotations: None,
                    }),
                    ..Default::default()
                }))
            })
            .collect()
    }

    /// The edit that imports `candidate`: extend an existing `use` that shares
    /// the same module prefix into a braced group, else insert a new `use` line
    /// after the last existing one.
    pub(crate) fn import_edit(
        &self,
        module: &Module,
        content: &str,
        candidate: &ItemPath,
    ) -> Option<(Range, String)> {
        // Merge into the existing `use` that shares the longest prefix with the
        // candidate, re-rendering its whole path set (so nested/multi-prefix
        // groups like `use types::{math::Aabb, shared_ptr::WeakPtr};` correctly
        // absorb a `types::math::Vector3` as `…math::{Aabb, Vector3}…`).
        let mut best: Option<(usize, &ItemLocation, Vec<ItemPath>)> = None;
        for item in &module.items {
            let ModuleItem::Use { tree, location, .. } = item else {
                continue;
            };
            let flat = tree.flatten();
            if flat.iter().any(|p| p == candidate) {
                return None; // already imported
            }
            let common = flat
                .iter()
                .map(|p| common_prefix_len(candidate, p))
                .max()
                .unwrap_or(0);
            if common >= 1 && best.as_ref().is_none_or(|(c, _, _)| common > *c) {
                best = Some((common, location, flat));
            }
        }
        if let Some((_, location, mut flat)) = best {
            flat.push(candidate.clone());
            let new_text = format!("use {};", render_use_tree(&flat));
            return Some((pyxis_span_to_lsp_range(content, &location.span), new_text));
        }

        // Otherwise, a fresh `use` line after the last existing one (or the top).
        let after = module
            .items
            .iter()
            .filter_map(|item| match item {
                ModuleItem::Use { location, .. } => Some(location.span.end.line as u32),
                _ => None,
            })
            .max();
        let pos = Position {
            line: after.unwrap_or(0),
            character: 0,
        };
        Some((
            Range {
                start: pos,
                end: pos,
            },
            format!("use {candidate};\n"),
        ))
    }
}

/// Number of leading path segments `a` and `b` share.
pub(crate) fn common_prefix_len(a: &ItemPath, b: &ItemPath) -> usize {
    a.iter().zip(b.iter()).take_while(|(x, y)| x == y).count()
}

/// Render a set of full import paths as a compact `use`-tree body (the text
/// between `use ` and `;`), grouping shared prefixes: `types::math::Aabb`,
/// `types::math::Vector3` and `types::shared_ptr::WeakPtr` together render as
/// `types::{math::{Aabb, Vector3}, shared_ptr::WeakPtr}`. Paths are sorted and
/// de-duplicated for a stable result.
pub(crate) fn render_use_tree(paths: &[ItemPath]) -> String {
    let mut full: Vec<Vec<String>> = paths
        .iter()
        .map(|p| p.iter().map(|s| s.as_str().to_string()).collect())
        .collect();
    full.sort();
    full.dedup();
    let slices: Vec<&[String]> = full.iter().map(|v| v.as_slice()).collect();
    render_use_group(&slices)
}

pub(crate) fn render_use_group(paths: &[&[String]]) -> String {
    use std::collections::BTreeMap;
    let mut groups: BTreeMap<&str, Vec<&[String]>> = BTreeMap::new();
    for p in paths {
        if let Some((first, rest)) = p.split_first() {
            groups.entry(first.as_str()).or_default().push(rest);
        }
    }
    let entries: Vec<String> = groups
        .iter()
        .map(|(seg, subs)| {
            let nested: Vec<&[String]> = subs.iter().filter(|s| !s.is_empty()).copied().collect();
            if nested.is_empty() {
                seg.to_string()
            } else {
                format!("{seg}::{}", render_use_group(&nested))
            }
        })
        .collect();
    if entries.len() == 1 {
        entries.into_iter().next().unwrap()
    } else {
        format!("{{{}}}", entries.join(", "))
    }
}
