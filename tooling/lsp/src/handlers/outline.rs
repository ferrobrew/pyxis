use super::*;

use pyxis::grammar::ItemDefinitionInner;

impl ServerState {
    /// textDocument/documentSymbol
    pub fn handle_document_symbols(&self, req: Request) -> Response {
        let params: DocumentSymbolParams = match serde_json::from_value(req.params.clone()) {
            Ok(p) => p,
            Err(e) => return error_response(req.id, e),
        };

        let uri = &params.text_document.uri;

        let Some(content) = self.get_content(uri) else {
            return Response {
                id: req.id,
                result: Some(serde_json::Value::Null),
                error: None,
            };
        };

        let Some(module) = self.get_parsed_module(uri) else {
            return Response {
                id: req.id,
                result: Some(serde_json::Value::Null),
                error: None,
            };
        };

        let mut symbols = Vec::new();

        for item in &module.items {
            if let Some(symbol) = module_item_to_symbol(item, content) {
                symbols.push(symbol);
            }
        }

        Response {
            id: req.id,
            result: Some(serde_json::to_value(DocumentSymbolResponse::Nested(symbols)).unwrap()),
            error: None,
        }
    }

    /// workspace/symbol
    #[allow(deprecated)] // SymbolInformation::deprecated field
    pub fn handle_workspace_symbols(&self, req: Request) -> Response {
        let params: WorkspaceSymbolParams = match serde_json::from_value(req.params.clone()) {
            Ok(p) => p,
            Err(e) => return error_response(req.id, e),
        };
        // Honour the query: a case-insensitive substring filter (empty query
        // returns everything, per the LSP spec).
        let query = params.query.to_lowercase();

        // Collect matching symbols across all open documents.
        let mut symbols: Vec<SymbolInformation> = Vec::new();

        for (uri, doc) in &self.documents {
            let parsed = semantic::parse_file(&self.db, doc.source_file);
            let module = parsed.module(&self.db);
            let content = &doc.content;

            for item in &module.items {
                if let Some(symbol) = module_item_to_symbol(item, content) {
                    if !query.is_empty() && !symbol.name.to_lowercase().contains(&query) {
                        continue;
                    }
                    symbols.push(SymbolInformation {
                        name: symbol.name,
                        kind: symbol.kind,
                        tags: None,
                        deprecated: None,
                        location: lsp_types::Location {
                            uri: uri.clone(),
                            range: symbol.range,
                        },
                        container_name: None,
                    });
                }
            }
        }

        Response {
            id: req.id,
            result: Some(serde_json::to_value(symbols).unwrap()),
            error: None,
        }
    }

    /// textDocument/formatting
    pub fn handle_formatting(&self, req: Request) -> Response {
        let params: lsp_types::DocumentFormattingParams =
            match serde_json::from_value(req.params.clone()) {
                Ok(p) => p,
                Err(e) => return error_response(req.id, e),
            };

        let uri = &params.text_document.uri;

        let Some(content) = self.get_content(uri) else {
            return Response {
                id: req.id,
                result: Some(serde_json::Value::Null),
                error: None,
            };
        };

        // Parse and pretty-print
        let file_id = self
            .documents
            .get(uri)
            .map(|d| d.file_id)
            .unwrap_or(FileId::INTERNAL);

        match pyxis::parser::parse_str_with_file_id(content, file_id) {
            Ok(module) => {
                // pretty_print trims trailing whitespace; re-add a single final
                // newline so format-on-save doesn't strip the file's trailing
                // newline (which shows up as "the last line was removed").
                let formatted = format!("{}\n", pyxis::pretty_print::pretty_print(&module));

                // Return a single TextEdit replacing the entire document
                let edit = TextEdit {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 0,
                        },
                        end: Position {
                            line: u32::MAX,
                            character: 0,
                        },
                    },
                    new_text: formatted,
                };

                Response {
                    id: req.id,
                    result: Some(serde_json::to_value(vec![edit]).unwrap()),
                    error: None,
                }
            }
            Err(_) => Response {
                id: req.id,
                result: Some(serde_json::Value::Null),
                error: None,
            },
        }
    }

    /// textDocument/codeLens
    pub fn handle_code_lens(&self, req: Request) -> Response {
        let params: lsp_types::CodeLensParams = match serde_json::from_value(req.params.clone()) {
            Ok(p) => p,
            Err(e) => return error_response(req.id, e),
        };

        let uri = &params.text_document.uri;

        let Some(ctx) = self.analysis_ctx(uri) else {
            return Response {
                id: req.id,
                result: Some(serde_json::json!([])),
                error: None,
            };
        };
        let AnalysisCtx {
            content,
            module,
            type_registry,
            ..
        } = ctx;

        let mut lenses = Vec::new();

        for definition in module.definitions() {
            let path = self.definition_path(uri, definition.name.as_str());
            if let Ok(item) = type_registry.get(&path, &definition.location)
                && let Some(resolved) = item.resolved()
            {
                let range = pyxis_span_to_lsp_range(content, &definition.location.span);
                let lens = CodeLens {
                    range,
                    command: Some(lsp_types::Command {
                        title: format!("size: 0x{:X}", resolved.size),
                        command: String::new(),
                        arguments: None,
                    }),
                    data: None,
                };
                lenses.push(lens);
            }
        }

        Response {
            id: req.id,
            result: Some(serde_json::to_value(lenses).unwrap()),
            error: None,
        }
    }

    /// textDocument/inlayHint
    pub fn handle_inlay_hints(&self, req: Request) -> Response {
        let params: lsp_types::InlayHintParams = match serde_json::from_value(req.params.clone()) {
            Ok(p) => p,
            Err(e) => return error_response(req.id, e),
        };

        let uri = &params.text_document.uri;

        let Some(ctx) = self.analysis_ctx(uri) else {
            return Response {
                id: req.id,
                result: Some(serde_json::Value::Null),
                error: None,
            };
        };
        let AnalysisCtx {
            content,
            module,
            type_registry,
            ..
        } = ctx;

        let mut hints = Vec::new();

        for definition in module.definitions() {
            // For each field, show its type size as an inlay hint
            if let ItemDefinitionInner::Type(td) = &definition.inner {
                for statement in td.statements() {
                    if let TypeField::Field(_, _name, type_) = &statement.field {
                        let path = type_.as_path();
                        if let Some(path) = path {
                            // Use the field type's own location for the
                            // visibility check, not the enclosing definition.
                            let type_loc = type_.location();
                            if let Ok(item) = type_registry.get(path, type_loc)
                                && let Some(resolved) = item.resolved()
                            {
                                let range = pyxis_span_to_lsp_range(content, &type_loc.span);
                                let hint = InlayHint {
                                    position: range.end,
                                    label: InlayHintLabel::String(format!(
                                        "  // 0x{:X}",
                                        resolved.size
                                    )),
                                    kind: Some(lsp_types::InlayHintKind::TYPE),
                                    text_edits: None,
                                    tooltip: None,
                                    padding_left: None,
                                    padding_right: None,
                                    data: None,
                                };
                                hints.push(hint);
                            }
                        }
                    }
                }
            }
        }

        Response {
            id: req.id,
            result: Some(serde_json::to_value(hints).unwrap()),
            error: None,
        }
    }

    /// textDocument/semanticTokens/full — resolution-aware tokens layered over
    /// the tree-sitter grammar: every type-position identifier that resolves is
    /// a `type` (builtins flagged `defaultLibrary`), and intermediate module
    /// segments of a path are `namespace`. Unresolved/structural idents are left
    /// to the grammar.
    pub fn handle_semantic_tokens_full(&self, req: Request) -> Response {
        let data = serde_json::from_value::<lsp_types::SemanticTokensParams>(req.params.clone())
            .ok()
            .map(|p| self.semantic_tokens(&p.text_document.uri))
            .unwrap_or_default();
        let result = lsp_types::SemanticTokens {
            result_id: None,
            data,
        };
        Response {
            id: req.id,
            result: Some(serde_json::to_value(result).unwrap()),
            error: None,
        }
    }

    fn semantic_tokens(&self, uri: &Uri) -> Vec<lsp_types::SemanticToken> {
        const NAMESPACE: u32 = 0;
        const TYPE: u32 = 1;
        const DEFAULT_LIBRARY: u32 = 1; // bit 0

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

        // Collect (line, start_char, length, type, modifiers) for each
        // type-position identifier, in source order.
        let mut raw: Vec<(u32, u32, u32, u32, u32)> = Vec::new();
        for tok in tokens {
            if !matches!(&tok.kind, TokenKind::Ident(_)) {
                continue;
            }
            let classified = match find_reference_at(
                &module,
                &tok.location.span.start,
                &scope,
                decl_registry,
                tokens,
                type_registry,
                pointer_size,
            ) {
                Some(Ref::Item { item: Some(p), .. }) => {
                    if decl_registry.get_predefined(&p).is_some() {
                        Some((TYPE, DEFAULT_LIBRARY))
                    } else {
                        Some((TYPE, 0))
                    }
                }
                Some(Ref::Item {
                    item: None,
                    module_path,
                    ..
                }) if decl_registry.module_paths().any(|m| *m == module_path) => {
                    Some((NAMESPACE, 0))
                }
                _ => None,
            };
            let Some((ty, mods)) = classified else {
                continue;
            };
            let range = pyxis_span_to_lsp_range(content, &tok.location.span);
            if range.start.line != range.end.line {
                continue; // semantic tokens are single-line
            }
            raw.push((
                range.start.line,
                range.start.character,
                range.end.character - range.start.character,
                ty,
                mods,
            ));
        }
        raw.sort_by_key(|(l, c, ..)| (*l, *c));

        // Delta-encode.
        let mut data = Vec::with_capacity(raw.len());
        let (mut prev_line, mut prev_char) = (0u32, 0u32);
        for (line, ch, len, ty, mods) in raw {
            let delta_line = line - prev_line;
            let delta_start = if delta_line == 0 { ch - prev_char } else { ch };
            data.push(lsp_types::SemanticToken {
                delta_line,
                delta_start,
                length: len,
                token_type: ty,
                token_modifiers_bitset: mods,
            });
            prev_line = line;
            prev_char = ch;
        }
        data
    }

    /// textDocument/foldingRange — fold the body of each type/enum/bitflags/
    /// impl/backend block (and nested vftable blocks): from the `{` line to the
    /// `}` line, keeping the signature line visible.
    pub fn handle_folding_range(&self, req: Request) -> Response {
        let ranges = serde_json::from_value::<FoldingRangeParams>(req.params.clone())
            .ok()
            .map(|p| self.folding_ranges(&p.text_document.uri))
            .unwrap_or_default();
        Response {
            id: req.id,
            result: Some(serde_json::to_value(ranges).unwrap()),
            error: None,
        }
    }

    fn folding_ranges(&self, uri: &Uri) -> Vec<FoldingRange> {
        let Some(module) = self.get_parsed_module(uri) else {
            return vec![];
        };
        let tokens_arc = self.tokens_for(uri);
        let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);

        let mut out = Vec::new();
        let mut push = |span: Span| {
            if let Some(fr) = body_fold(tokens, &span) {
                out.push(fr);
            }
        };
        for item in &module.items {
            match item {
                ModuleItem::Definition { definition } => {
                    push(definition.location.span);
                    if let ItemDefinitionInner::Type(td) = &definition.inner {
                        for statement in td.statements() {
                            if matches!(statement.field, TypeField::Vftable(_)) {
                                push(statement.location.span);
                            }
                        }
                    }
                }
                ModuleItem::Impl { impl_block } => push(impl_block.location.span),
                ModuleItem::Backend { backend } => push(backend.location.span),
                ModuleItem::Use { location, .. } => push(location.span),
                _ => {}
            }
        }
        out
    }
}

/// Convert a ModuleItem to a DocumentSymbol
#[allow(deprecated)] // DocumentSymbol::deprecated field
pub(crate) fn module_item_to_symbol(item: &ModuleItem, source: &str) -> Option<DocumentSymbol> {
    let (name, kind, range_span) = match item {
        ModuleItem::Definition { definition } => {
            let kind = match &definition.inner {
                ItemDefinitionInner::Type(_) => SymbolKind::STRUCT,
                ItemDefinitionInner::Enum(_) => SymbolKind::ENUM,
                ItemDefinitionInner::Bitflags(_) => SymbolKind::ENUM,
                ItemDefinitionInner::TypeAlias(_) => SymbolKind::TYPE_PARAMETER,
            };
            (
                definition.name.as_str().to_string(),
                kind,
                definition.location.span,
            )
        }
        ModuleItem::Function { function } => (
            function.name.as_str().to_string(),
            SymbolKind::FUNCTION,
            function.location.span,
        ),
        ModuleItem::ExternType { name, .. } => (
            name.as_str().to_string(),
            SymbolKind::STRUCT,
            Span::synthetic(),
        ),
        ModuleItem::ExternValue { extern_value } => (
            extern_value.name.as_str().to_string(),
            SymbolKind::VARIABLE,
            extern_value.location.span,
        ),
        ModuleItem::Impl { impl_block } => {
            // Impl blocks: use the target type name
            let name = impl_block.name.as_str().to_string();
            (name, SymbolKind::OBJECT, Span::synthetic())
        }
        ModuleItem::Use { .. } => return None,
        ModuleItem::Backend { .. } => return None,
        ModuleItem::Comment { .. } => return None,
        ModuleItem::InnerAttributes { .. } => return None,
    };

    let range = pyxis_span_to_lsp_range(source, &range_span);
    Some(DocumentSymbol {
        name,
        detail: None,
        kind,
        tags: None,
        deprecated: None,
        range,
        selection_range: range,
        children: None,
    })
}

/// A folding range for the brace-delimited body within `span`: from the line of
/// the first `{` (the body opener, kept visible) to the span's closing line.
/// `None` for single-line or brace-less spans.
pub(crate) fn body_fold(tokens: &[Token], span: &Span) -> Option<FoldingRange> {
    let open = tokens
        .iter()
        .find(|t| matches!(t.kind, TokenKind::LBrace) && span.contains(&t.location.span.start))?;
    let start = open.location.span.start.line;
    let end = span.end.line;
    if end <= start {
        return None;
    }
    Some(FoldingRange {
        // Pyxis lines are 1-indexed; LSP folding ranges are 0-indexed.
        start_line: start as u32 - 1,
        end_line: end as u32 - 1,
        ..Default::default()
    })
}
