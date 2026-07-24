use super::*;

impl ServerState {
    /// textDocument/definition
    pub fn handle_definition(&self, req: Request) -> Response {
        let params: TextDocumentPositionParams = match serde_json::from_value(req.params.clone()) {
            Ok(p) => p,
            Err(e) => return error_response(req.id, e),
        };

        let uri = &params.text_document.uri;
        let position = params.position;

        let Some(ctx) = self.analysis_ctx(uri) else {
            return Response {
                id: req.id,
                result: Some(serde_json::Value::Null),
                error: None,
            };
        };
        // The compiler's cached token stream — see `handle_hover` for rationale.
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

        let loc = lsp_position_to_pyxis_location(content, position);

        // 0. A doc-comment cross-reference link → jump to the referenced member
        //    (impl/vftable method, field) or type.
        if let Some((_span, location, _hover)) = self.doc_link_at(uri, &loc) {
            return Response {
                id: req.id,
                result: Some(
                    serde_json::to_value(lsp_types::GotoDefinitionResponse::Scalar(location))
                        .unwrap(),
                ),
                error: None,
            };
        }

        // 1. Cursor on a type or import reference (e.g. `Camera` in
        //    `pub field: Camera`, `*mut Camera`, or a name in a `use`
        //    statement). For an FQN like `a::b::C`, the individual segment under
        //    the cursor is resolved: the leaf jumps to the type, earlier
        //    segments to their module's file. This must take priority over the
        //    enclosing-definition check below, since a field's type span is
        //    contained within its parent definition's span. A pointer/array
        //    *shell* has no definition, so it falls through to a null result.
        if let Some(Ref::Item {
            item, module_path, ..
        }) = find_reference_at(
            &module,
            &loc,
            &scope,
            decl_registry,
            tokens,
            type_registry,
            pointer_size,
        ) {
            // a) Concrete item (type/extern/predefined) → jump to its name.
            if let Some(item_path) = &item
                && let Some(rd) = self.resolved_definition(item_path, type_registry, uri)
                && let Some(target_content) = self.get_content(&rd.uri)
            {
                let range = pyxis_span_to_lsp_range(target_content, &rd.name_span);
                let location = lsp_types::Location { uri: rd.uri, range };
                return Response {
                    id: req.id,
                    result: Some(
                        serde_json::to_value(lsp_types::GotoDefinitionResponse::Scalar(location))
                            .unwrap(),
                    ),
                    error: None,
                };
            }
            // b) Module segment → jump to the top of its file.
            if let Some(target_uri) = self.module_uri(&module_path, uri) {
                let location = lsp_types::Location {
                    uri: target_uri,
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 0,
                        },
                        end: Position {
                            line: 0,
                            character: 0,
                        },
                    },
                };
                return Response {
                    id: req.id,
                    result: Some(
                        serde_json::to_value(lsp_types::GotoDefinitionResponse::Scalar(location))
                            .unwrap(),
                    ),
                    error: None,
                };
            }
        }

        // 2. Cursor on a definition's own name → jump to itself (scoped to the
        //    name, not the whole declaration).
        for definition in module.definitions() {
            if let Some(span) = name_token_span(
                tokens,
                &definition.declaration_location.span.start,
                definition.name.as_str(),
            ) && span.contains(&loc)
            {
                let range = pyxis_span_to_lsp_range(content, &span);
                let location = lsp_types::Location {
                    uri: uri.clone(),
                    range,
                };
                return Response {
                    id: req.id,
                    result: Some(
                        serde_json::to_value(lsp_types::GotoDefinitionResponse::Scalar(location))
                            .unwrap(),
                    ),
                    error: None,
                };
            }
        }

        Response {
            id: req.id,
            result: Some(serde_json::Value::Null),
            error: None,
        }
    }

    /// textDocument/implementation — from a type (its name or any reference),
    /// the `impl` block(s) targeting it across the project.
    pub fn handle_implementation(&self, req: Request) -> Response {
        let locations = serde_json::from_value::<TextDocumentPositionParams>(req.params.clone())
            .ok()
            .and_then(|p| {
                let uri = p.text_document.uri;
                let content = self.get_content(&uri)?;
                let loc = lsp_position_to_pyxis_location(content, p.position);
                let symbol = self.symbol_at(&uri, &loc)?;
                Some(self.impl_locations(symbol.type_path()?, &uri))
            })
            .unwrap_or_default();
        Response {
            id: req.id,
            result: Some(serde_json::to_value(locations).unwrap()),
            error: None,
        }
    }

    /// Locations of every `impl` block whose target resolves to `target`,
    /// within the requesting file's project.
    fn impl_locations(&self, target: &ItemPath, from_uri: &Uri) -> Vec<lsp_types::Location> {
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

        let mut out = Vec::new();
        for uri in &uris {
            let Some(module) = self.get_parsed_module(uri) else {
                continue;
            };
            let Some(content) = self.get_content(uri) else {
                continue;
            };
            let scope = self.scope_for(uri);
            let tokens_arc = self.tokens_for(uri);
            let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);
            for item in &module.items {
                if let ModuleItem::Impl { impl_block } = item {
                    let name_path = ItemPath::from(impl_block.name.as_str());
                    if resolve_type_path(&name_path, &scope, decl_registry).as_ref() == Some(target)
                    {
                        let span = name_token_span(
                            tokens,
                            &impl_block.location.span.start,
                            impl_block.name.as_str(),
                        )
                        .unwrap_or(impl_block.location.span);
                        out.push(lsp_types::Location {
                            uri: uri.clone(),
                            range: pyxis_span_to_lsp_range(content, &span),
                        });
                    }
                }
            }
        }
        out
    }
}
