use super::*;

impl ServerState {
    /// Resolve the cursor to the canonical path of the symbol it names — a type
    /// reference, a `use` leaf, or a definition's own name. The anchor for
    /// find-references / document-highlight / rename.
    pub(crate) fn symbol_at(&self, uri: &Uri, loc: &Location) -> Option<Symbol> {
        use pyxis::grammar::{ImplItem, ItemDefinitionInner};
        let AnalysisCtx {
            module,
            scope,
            type_registry,
            decl_registry,
            pointer_size,
            tokens_arc,
            ..
        } = self.analysis_ctx(uri)?;
        let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);
        let own_module = self.module_path_for(uri);

        // 1. A type reference (type position or `use` leaf) → the type it names.
        if let Some(Ref::Item { item: Some(p), .. }) = find_reference_at(
            &module,
            loc,
            &scope,
            decl_registry,
            tokens,
            type_registry,
            pointer_size,
        ) {
            return Some(Symbol::Type(p));
        }

        // 2/3. A definition's own name (→ Type) or a member declaration within
        //      it / an impl block (→ Member).
        let on_name = |from: &Location, name: &str| {
            name_token_span(tokens, from, name).is_some_and(|s| s.contains(loc))
        };
        for item in &module.items {
            match item {
                ModuleItem::Definition { definition } => {
                    let owner = || match &own_module {
                        Some(mp) => mp.join(definition.name.as_str().into()),
                        None => ItemPath::from(definition.name.as_str()),
                    };
                    if on_name(
                        &definition.declaration_location.span.start,
                        definition.name.as_str(),
                    ) {
                        return Some(Symbol::Type(owner()));
                    }
                    if let ItemDefinitionInner::Type(td) = &definition.inner {
                        for statement in td.statements() {
                            let member = match &statement.field {
                                TypeField::Field(_, field_name, _)
                                    if on_name(
                                        &statement.location.span.start,
                                        field_name.as_str(),
                                    ) =>
                                {
                                    Some(field_name.as_str().to_string())
                                }
                                TypeField::Vftable(fns) => fns
                                    .iter()
                                    .find(|f| on_name(&f.location.span.start, f.name.as_str()))
                                    .map(|f| f.name.as_str().to_string()),
                                _ => None,
                            };
                            if let Some(name) = member {
                                return Some(Symbol::Member {
                                    owner: owner(),
                                    name,
                                });
                            }
                        }
                    }
                }
                ModuleItem::Impl { impl_block } => {
                    for impl_item in &impl_block.items {
                        if let ImplItem::Function(f) = impl_item
                            && on_name(&f.location.span.start, f.name.as_str())
                        {
                            let owner = resolve_type_path(
                                &ItemPath::from(impl_block.name.as_str()),
                                &scope,
                                decl_registry,
                            )?;
                            return Some(Symbol::Member {
                                owner,
                                name: f.name.as_str().to_string(),
                            });
                        }
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Every occurrence — definition name, type references, and `use` leaves —
    /// of the resolved symbol `target`, across the requesting file's project.
    /// The shared engine behind find-references, document-highlight, and rename.
    fn symbol_occurrences(&self, symbol: &Symbol, from_uri: &Uri) -> Vec<lsp_types::Location> {
        // A member (field/method) has no source cross-references beyond its
        // declaration plus any doc-comment links to it.
        let target = match symbol {
            Symbol::Type(p) => p,
            Symbol::Member { owner, name } => {
                // A member can be declared in several places — every impl block
                // (including `#[cfg]`-gated ones, possibly across files), the
                // vftable, or a field — so collect *all* declarations, not just
                // the first. Each gets a rename edit / reference / highlight.
                let mut out: Vec<lsp_types::Location> = self
                    .resolve_doc_members(owner, name, from_uri)
                    .into_iter()
                    .filter_map(|(uri, span, _)| {
                        let content = self.get_content(&uri)?;
                        Some(lsp_types::Location {
                            uri,
                            range: pyxis_span_to_lsp_range(content, &span),
                        })
                    })
                    .collect();
                out.extend(self.doc_link_occurrences(symbol, from_uri));
                // Guard against emitting the same span twice (a declaration that
                // also surfaced as a doc link). Counts are tiny, so an O(n²)
                // pass is fine and keeps source order stable.
                let mut seen: Vec<lsp_types::Location> = Vec::with_capacity(out.len());
                out.retain(|l| {
                    if seen.iter().any(|s| s.uri == l.uri && s.range == l.range) {
                        false
                    } else {
                        seen.push(l.clone());
                        true
                    }
                });
                return out;
            }
        };
        let mut out: Vec<lsp_types::Location> = Vec::new();
        let Some(name) = target.last().map(|s| s.as_str().to_string()) else {
            return out;
        };
        let from_root = self
            .documents
            .get(from_uri)
            .and_then(|d| d.project_root.clone());

        let pointer_size = self.pointer_size_for(from_uri);

        // Files in the same project (same relative path across projects shares a
        // module path, so we must not cross the project boundary). Sorted by URI
        // for deterministic output regardless of `HashMap` iteration order.
        let mut uris: Vec<Uri> = self
            .documents
            .iter()
            .filter(|(_, d)| d.project_root == from_root)
            .map(|(u, _)| u.clone())
            .collect();
        uris.sort_by(|a, b| a.as_str().cmp(b.as_str()));

        for uri in &uris {
            let Some(module) = self.get_parsed_module(uri) else {
                continue;
            };
            let Some(content) = self.get_content(uri) else {
                continue;
            };
            let tokens_arc = self.tokens_for(uri);
            let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);

            // References + `use`-tree leaves: from the per-file, incrementally
            // cached source map (leaf span → resolved path), filtered to target.
            if let Some(source_file) = self.documents.get(uri).map(|d| d.source_file) {
                let source_set = semantic::SourceSet::new(&self.db, self.sources_for(uri));
                let references =
                    semantic::file_type_references(&self.db, source_file, source_set, pointer_size);
                for (span, path) in references.references(&self.db).iter() {
                    if path == target {
                        out.push(lsp_types::Location {
                            uri: uri.clone(),
                            range: pyxis_span_to_lsp_range(content, span),
                        });
                    }
                }
            }

            // Definition site: the type's own name token.
            if self
                .module_path_for(uri)
                .map(|mp| mp.join(name.as_str().into()))
                .as_ref()
                == Some(target)
            {
                for def in module.definitions() {
                    if def.name.as_str() == name
                        && let Some(span) =
                            name_token_span(tokens, &def.declaration_location.span.start, &name)
                    {
                        out.push(lsp_types::Location {
                            uri: uri.clone(),
                            range: pyxis_span_to_lsp_range(content, &span),
                        });
                    }
                }
            }
        }
        out.extend(self.doc_link_occurrences(symbol, from_uri));
        out
    }

    /// The declaration site(s) of a symbol — the type's own name, or the
    /// member's declaration(s). A member can be declared in several impl blocks
    /// (incl. `#[cfg]`-gated ones), so all of them count as "the declaration"
    /// for `includeDeclaration: false`.
    fn symbol_declarations(&self, symbol: &Symbol, from_uri: &Uri) -> Vec<lsp_types::Location> {
        let spans: Vec<(Uri, Span)> = match symbol {
            Symbol::Type(p) => {
                let (type_registry, _) = self.registries_for(from_uri);
                self.resolved_definition(p, type_registry, from_uri)
                    .map(|rd| (rd.uri, rd.name_span))
                    .into_iter()
                    .collect()
            }
            Symbol::Member { owner, name } => self
                .resolve_doc_members(owner, name, from_uri)
                .into_iter()
                .map(|(uri, span, _)| (uri, span))
                .collect(),
        };
        spans
            .into_iter()
            .filter_map(|(uri, span)| {
                let content = self.get_content(&uri)?;
                Some(lsp_types::Location {
                    range: pyxis_span_to_lsp_range(content, &span),
                    uri,
                })
            })
            .collect()
    }

    /// textDocument/references
    pub fn handle_references(&self, req: Request) -> Response {
        let params: lsp_types::ReferenceParams = match serde_json::from_value(req.params.clone()) {
            Ok(p) => p,
            Err(e) => return error_response(req.id, e),
        };
        let uri = &params.text_document_position.text_document.uri;
        let include_decl = params.context.include_declaration;
        let locations = self
            .get_content(uri)
            .map(|content| {
                lsp_position_to_pyxis_location(content, params.text_document_position.position)
            })
            .and_then(|loc| self.symbol_at(uri, &loc))
            .map(|symbol| {
                let mut locs = self.symbol_occurrences(&symbol, uri);
                if !include_decl {
                    let decls = self.symbol_declarations(&symbol, uri);
                    locs.retain(|l| !decls.iter().any(|d| d.uri == l.uri && d.range == l.range));
                }
                locs
            })
            .unwrap_or_default();
        Response {
            id: req.id,
            result: Some(serde_json::to_value(locations).unwrap()),
            error: None,
        }
    }

    /// textDocument/documentHighlight — occurrences within the current file only.
    pub fn handle_document_highlight(&self, req: Request) -> Response {
        let params: TextDocumentPositionParams = match serde_json::from_value(req.params.clone()) {
            Ok(p) => p,
            Err(e) => return error_response(req.id, e),
        };
        let uri = &params.text_document.uri;
        let highlights: Vec<lsp_types::DocumentHighlight> = self
            .get_content(uri)
            .map(|content| lsp_position_to_pyxis_location(content, params.position))
            .and_then(|loc| self.symbol_at(uri, &loc))
            .map(|target| {
                self.symbol_occurrences(&target, uri)
                    .into_iter()
                    .filter(|l| &l.uri == uri)
                    .map(|l| lsp_types::DocumentHighlight {
                        range: l.range,
                        kind: Some(lsp_types::DocumentHighlightKind::TEXT),
                    })
                    .collect()
            })
            .unwrap_or_default();
        Response {
            id: req.id,
            result: Some(serde_json::to_value(highlights).unwrap()),
            error: None,
        }
    }

    /// textDocument/prepareRename — validate the cursor is on a renameable
    /// identifier (a user-defined type/reference/use-leaf, not a builtin) and
    /// return its range + current text.
    pub fn handle_prepare_rename(&self, req: Request) -> Response {
        let result = self
            .prepare_rename(&req)
            .map(|r| serde_json::to_value(r).unwrap());
        Response {
            id: req.id,
            result: Some(result.unwrap_or(serde_json::Value::Null)),
            error: None,
        }
    }

    fn prepare_rename(&self, req: &Request) -> Option<lsp_types::PrepareRenameResponse> {
        let params: TextDocumentPositionParams = serde_json::from_value(req.params.clone()).ok()?;
        let uri = &params.text_document.uri;
        let content = self.get_content(uri)?;
        let loc = lsp_position_to_pyxis_location(content, params.position);
        // Renameable iff the cursor is on a type or member symbol.
        let symbol = self.symbol_at(uri, &loc)?;

        // A builtin type resolves but must not be renamed.
        if let Some(p) = symbol.type_path()
            && self.decl_registry_for(uri).get_predefined(p).is_some()
        {
            return None;
        }

        // The identifier token under the cursor is what gets rewritten.
        let tokens_arc = self.tokens_for(uri);
        let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);
        let tok = tokens
            .iter()
            .find(|t| t.location.span.contains(&loc) && matches!(&t.kind, TokenKind::Ident(_)))?;
        let TokenKind::Ident(name) = &tok.kind else {
            return None;
        };
        Some(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
            range: pyxis_span_to_lsp_range(content, &tok.location.span),
            placeholder: name.clone(),
        })
    }

    /// textDocument/rename
    #[allow(clippy::mutable_key_type)] // lsp_types::Uri key is fine here
    pub fn handle_rename(&self, req: Request) -> Response {
        let params: lsp_types::RenameParams = match serde_json::from_value(req.params.clone()) {
            Ok(p) => p,
            Err(e) => return error_response(req.id, e),
        };

        let uri = &params.text_document_position.text_document.uri;
        let _position = params.text_document_position.position;
        let new_name = params.new_name;

        // Validate the new name
        if new_name.is_empty() || !is_valid_identifier(&new_name) {
            return Response {
                id: req.id,
                result: None,
                error: Some(lsp_server::ResponseError {
                    code: -32602, // Invalid params
                    message: format!("Invalid identifier: '{new_name}'"),
                    data: None,
                }),
            };
        }

        let Some(content) = self.get_content(uri) else {
            return Response {
                id: req.id,
                result: Some(serde_json::Value::Null),
                error: None,
            };
        };

        let loc = lsp_position_to_pyxis_location(content, params.text_document_position.position);

        // Resolve the symbol under the cursor, then rewrite every occurrence of
        // it (definition, references, `use` leaves) across the project — each
        // occurrence span is exactly the identifier token, so renaming a leaf of
        // a path leaves the rest of the path intact.
        let Some(target) = self.symbol_at(uri, &loc) else {
            return Response {
                id: req.id,
                result: Some(serde_json::to_value(WorkspaceEdit::default()).unwrap()),
                error: None,
            };
        };

        let mut edits: HashMap<Uri, Vec<TextEdit>> = HashMap::new();
        for occurrence in self.symbol_occurrences(&target, uri) {
            edits.entry(occurrence.uri).or_default().push(TextEdit {
                range: occurrence.range,
                new_text: new_name.clone(),
            });
        }

        let workspace_edit = WorkspaceEdit {
            changes: Some(edits),
            document_changes: None,
            change_annotations: None,
        };

        Response {
            id: req.id,
            result: Some(serde_json::to_value(workspace_edit).unwrap()),
            error: None,
        }
    }
}

/// Check if a string is a valid Pyxis identifier
pub(crate) fn is_valid_identifier(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let mut chars = s.chars();
    let first = chars.next().unwrap();
    if !first.is_alphabetic() && first != '_' {
        return false;
    }
    chars.all(|c| c.is_alphanumeric() || c == '_')
}

/// A reference found under the cursor. Either a *named* reference (a type/import
/// segment, resolved to a concrete item or module) or a pointer/array/unknown
/// *shell* whose pre-rendered hover markdown describes its shape.
/// What the cursor resolves to for navigation/rename: a type, or a member of
/// one (a field or method). Members have no cross-references in pyxis source
/// beyond their declaration.
pub(crate) enum Symbol {
    Type(ItemPath),
    Member { owner: ItemPath, name: String },
}

impl Symbol {
    /// The type path, if this symbol is a type (not a member). Used by features
    /// that only apply to types — go-to-implementation, type hierarchy.
    pub(crate) fn type_path(&self) -> Option<&ItemPath> {
        match self {
            Symbol::Type(p) => Some(p),
            Symbol::Member { .. } => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::state::ServerState;
    use lsp_server::{Request, RequestId};
    use lsp_types::{
        Position, RenameParams, TextDocumentIdentifier, TextDocumentPositionParams, WorkspaceEdit,
    };

    /// Regression: a method declared in *several* impl blocks (here two
    /// `#[cfg]`-gated ones) must have *every* declaration renamed — not just the
    /// first one the (previously nondeterministic) `HashMap` iteration reached.
    #[test]
    fn rename_member_touches_all_impl_declarations() {
        let src = "pub type Foo {\n    pub x: u32,\n}\n#[cfg(backend = \"rust\")]\nimpl Foo {\n    pub fn doit(&mut self);\n}\n#[cfg(backend = \"cpp\")]\nimpl Foo {\n    pub fn doit(&mut self);\n}\n";
        let st = ServerState::in_memory(&[("/p", 8, &[("m.pyxis", src)])]);
        let uri = ServerState::document_uri("/p", "m.pyxis");

        // Cursor on the first `doit` declaration (line 5, 0-indexed).
        let line = 5u32;
        let character = src
            .lines()
            .nth(line as usize)
            .unwrap()
            .find("doit")
            .unwrap() as u32;
        let req = Request::new(
            RequestId::from(1),
            "textDocument/rename".into(),
            serde_json::to_value(RenameParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position { line, character },
                },
                new_name: "did_it".into(),
                work_done_progress_params: Default::default(),
            })
            .unwrap(),
        );
        let we: WorkspaceEdit =
            serde_json::from_value(st.handle_rename(req).result.unwrap()).unwrap();
        let total: usize = we
            .changes
            .map(|c| c.values().map(|v| v.len()).sum())
            .unwrap_or(0);
        assert_eq!(
            total, 2,
            "both impl-block declarations of `doit` must be renamed"
        );
    }
}
