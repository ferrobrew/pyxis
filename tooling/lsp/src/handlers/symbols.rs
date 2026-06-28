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
                let mut out: Vec<lsp_types::Location> = self
                    .resolve_doc_member(owner, name, from_uri)
                    .and_then(|(uri, span, _)| {
                        let content = self.get_content(&uri)?;
                        Some(lsp_types::Location {
                            uri: uri.clone(),
                            range: pyxis_span_to_lsp_range(content, &span),
                        })
                    })
                    .into_iter()
                    .collect();
                out.extend(self.doc_link_occurrences(symbol, from_uri));
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
        let (type_registry, decl_registry) = self.registries_for(from_uri);

        // Files in the same project (same relative path across projects shares a
        // module path, so we must not cross the project boundary).
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
            let Some(content) = self.get_content(uri) else {
                continue;
            };
            let tokens_arc = self.tokens_for(uri);
            let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);
            let scope = self.scope_for(uri);

            // References + `use` leaves: each ident token spelled like the target,
            // confirmed by resolving it back to the target path.
            for tok in tokens.iter() {
                if !matches!(&tok.kind, TokenKind::Ident(s) if *s == name) {
                    continue;
                }
                if let Some(Ref::Item { item: Some(p), .. }) = find_reference_at(
                    &module,
                    &tok.location.span.start,
                    &scope,
                    decl_registry,
                    tokens,
                    type_registry,
                    pointer_size,
                ) && p == *target
                {
                    out.push(lsp_types::Location {
                        uri: uri.clone(),
                        range: pyxis_span_to_lsp_range(content, &tok.location.span),
                    });
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

    /// The declaration site of a symbol — the type's own name, or the member's
    /// declaration. Used to honour `includeDeclaration: false`.
    fn symbol_declaration(&self, symbol: &Symbol, from_uri: &Uri) -> Option<lsp_types::Location> {
        let (uri, span) = match symbol {
            Symbol::Type(p) => {
                let (type_registry, _) = self.registries_for(from_uri);
                let rd = self.resolved_definition(p, type_registry, from_uri)?;
                (rd.uri, rd.name_span)
            }
            Symbol::Member { owner, name } => {
                let (uri, span, _) = self.resolve_doc_member(owner, name, from_uri)?;
                (uri, span)
            }
        };
        let content = self.get_content(&uri)?;
        Some(lsp_types::Location {
            range: pyxis_span_to_lsp_range(content, &span),
            uri,
        })
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
                if !include_decl && let Some(decl) = self.symbol_declaration(&symbol, uri) {
                    locs.retain(|l| !(l.uri == decl.uri && l.range == decl.range));
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
