use super::*;

impl ServerState {
    /// textDocument/hover
    pub fn handle_hover(&self, req: Request) -> Response {
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
        // The compiler's cached token stream — span helpers locate identifiers
        // by matching real tokens rather than scanning source text, so names in
        // comments / splices (which lex as comment/string tokens) are ignored.
        let AnalysisCtx {
            content,
            module,
            scope,
            type_registry,
            decl_registry,
            pointer_size,
            source_set,
            tokens_arc,
            ..
        } = ctx;
        let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);

        let loc = lsp_position_to_pyxis_location(content, position);

        // 0. A doc-comment cross-reference link → describe the referenced member
        //    (or type), over the whole link.
        if let Some((span, _location, hover)) = self.doc_link_at(uri, &loc) {
            return hover_response(req.id, hover, content, &span);
        }

        // 1. Cursor on a type or import reference (e.g. a field's type, or a
        //    segment of a `use`/FQN path) → hover the *referenced* item, not the
        //    enclosing definition. For an intermediate FQN segment, show the
        //    module it names. A pointer/array/unknown *shell* (rather than the
        //    pointee/element) describes the shape — at every type position.
        if let Some(reference) = find_reference_at(
            &module,
            &loc,
            &scope,
            decl_registry,
            tokens,
            type_registry,
            pointer_size,
        ) {
            match reference {
                Ref::Item {
                    item,
                    module_path,
                    span,
                } => {
                    let hover = item
                        .as_ref()
                        .and_then(|item_path| {
                            self.type_hover_text(item_path, type_registry, decl_registry, uri)
                        })
                        .or_else(|| {
                            self.module_uri(&module_path, uri)
                                .map(|_| format!("**module** `{module_path}`"))
                        });
                    if let Some(value) = hover {
                        return hover_response(req.id, value, content, &span);
                    }
                }
                Ref::Shell { md, span } => {
                    return hover_response(req.id, md, content, &span);
                }
            }
        }

        // 2. An attribute → describe the attribute itself, not the item it's
        //    attached to. Must precede the structural checks below, since an
        //    attribute's span sits inside its field/type's span.
        if let Some((attribute, span)) = attribute_at(&module, &loc) {
            return hover_response(
                req.id,
                format_attribute_hover(attribute, &span, content),
                content,
                &span,
            );
        }

        // 3. Structural elements — scoped tightly to what's under the cursor
        //    rather than the whole enclosing definition:
        //    - a definition's own name → the type (size/align/fields);
        //    - a field name → the field (type + attributes + size);
        //    - a vftable entry or an impl method → its signature.
        for item in &module.items {
            match item {
                ModuleItem::Definition { definition } => {
                    // The definition's own name.
                    if let Some(span) = name_token_span(
                        tokens,
                        &definition.declaration_location.span.start,
                        definition.name.as_str(),
                    ) && span.contains(&loc)
                    {
                        let item_path = self.definition_path(uri, definition.name.as_str());
                        let resolved = resolve_item(&self.db, source_set, pointer_size, item_path);
                        let resolved_item = resolved.item(&self.db);
                        let value = if let Some(rs) = resolved_item.resolved() {
                            format_type_hover_with_size(definition, rs.size, rs.alignment)
                        } else {
                            format_type_hover(definition)
                        };
                        return hover_response(req.id, value, content, &span);
                    }
                    // Fields and vftable entries of a type definition.
                    if let pyxis::grammar::ItemDefinitionInner::Type(td) = &definition.inner {
                        for statement in td.statements() {
                            if !statement.location.span.contains(&loc) {
                                continue;
                            }
                            match &statement.field {
                                TypeField::Field(vis, name, type_) => {
                                    // The pointer/array shell and the pointee/
                                    // element are both handled as references in
                                    // branch 1; here we only describe the field.
                                    let span = name_token_span(
                                        tokens,
                                        &statement.location.span.start,
                                        name.as_str(),
                                    )
                                    .unwrap_or(statement.location.span);
                                    let size = type_size_of(
                                        type_,
                                        type_registry,
                                        &scope,
                                        decl_registry,
                                        pointer_size,
                                    );
                                    // Offset within the parent type's resolved
                                    // layout. The parent is resolved via
                                    // resolve_item (analyze()'s registry leaves
                                    // composite types unresolved).
                                    let parent_path =
                                        self.definition_path(uri, definition.name.as_str());
                                    let parent = resolve_item(
                                        &self.db,
                                        source_set,
                                        pointer_size,
                                        parent_path,
                                    );
                                    let offset = parent.item(&self.db).resolved().and_then(|rs| {
                                        field_offset(rs, name.as_str(), type_registry)
                                    });
                                    let value = format_field_hover(
                                        vis,
                                        name,
                                        type_,
                                        &statement.attributes,
                                        size,
                                        offset,
                                    );
                                    return hover_response(req.id, value, content, &span);
                                }
                                TypeField::Vftable(fns) => {
                                    for f in fns {
                                        if !f.location.span.contains(&loc) {
                                            continue;
                                        }
                                        // Arg/return *types* (including pointer/
                                        // array shells) are handled in branch 1.
                                        // An argument name…
                                        if let Some((value, span)) = named_arg_hover(
                                            f,
                                            &loc,
                                            tokens,
                                            type_registry,
                                            &scope,
                                            decl_registry,
                                            pointer_size,
                                        ) {
                                            return hover_response(req.id, value, content, &span);
                                        }
                                        // …`self`, resolving to the owning type…
                                        if let Some(span) = self_arg_span(f, &loc) {
                                            let owner =
                                                self.definition_path(uri, definition.name.as_str());
                                            if let Some(value) = self.type_hover_text(
                                                &owner,
                                                type_registry,
                                                decl_registry,
                                                uri,
                                            ) {
                                                return hover_response(
                                                    req.id, value, content, &span,
                                                );
                                            }
                                        }
                                        // …or the function itself, annotated with
                                        // its vftable slot index and byte offset.
                                        let span = name_token_span(
                                            tokens,
                                            &f.location.span.start,
                                            f.name.as_str(),
                                        )
                                        .unwrap_or(f.location.span);
                                        let index = vftable_index_of(fns, f);
                                        return hover_response(
                                            req.id,
                                            format_vftable_fn_hover(f, index, pointer_size),
                                            content,
                                            &span,
                                        );
                                    }
                                    // Cursor on the `vftable` keyword → describe
                                    // the generated vtable struct (the resolved
                                    // count includes inherited entries).
                                    if let Some(span) = name_token_span(
                                        tokens,
                                        &statement.location.span.start,
                                        "vftable",
                                    ) && span.contains(&loc)
                                    {
                                        let owner =
                                            self.definition_path(uri, definition.name.as_str());
                                        let resolved =
                                            resolve_item(&self.db, source_set, pointer_size, owner);
                                        let count = match resolved.item(&self.db).resolved() {
                                                Some(rs) => match &rs.inner {
                                                    pyxis::semantic::types::ItemDefinitionInner::Type(td) => td
                                                        .vftable
                                                        .as_ref()
                                                        .map(|v| v.functions.len())
                                                        .unwrap_or(fns.len()),
                                                    _ => fns.len(),
                                                },
                                                None => fns.len(),
                                            };
                                        let md = format!(
                                            "**vftable** of `{}`\n\nGenerates a vtable struct with `{count}` virtual function(s).",
                                            definition.name.as_str(),
                                        );
                                        return hover_response(req.id, md, content, &span);
                                    }
                                }
                            }
                        }
                    }
                    // Enum / bitflags variants → the variant and its value.
                    if let pyxis::grammar::ItemDefinitionInner::Enum(e) = &definition.inner {
                        for statement in e.statements() {
                            if statement.location.span.contains(&loc) {
                                let span = name_token_span(
                                    tokens,
                                    &statement.location.span.start,
                                    statement.name.as_str(),
                                )
                                .unwrap_or(statement.location.span);
                                let value = self.variant_value(
                                    uri,
                                    definition,
                                    statement.name.as_str(),
                                    source_set,
                                    pointer_size,
                                );
                                let md = format_variant_hover(
                                    "variant",
                                    statement.name.as_str(),
                                    value,
                                    &statement.attributes,
                                    &statement.doc_comments,
                                );
                                return hover_response(req.id, md, content, &span);
                            }
                        }
                    }
                    if let pyxis::grammar::ItemDefinitionInner::Bitflags(b) = &definition.inner {
                        for statement in b.statements() {
                            if statement.location.span.contains(&loc) {
                                let span = name_token_span(
                                    tokens,
                                    &statement.location.span.start,
                                    statement.name.as_str(),
                                )
                                .unwrap_or(statement.location.span);
                                let value = self.variant_value(
                                    uri,
                                    definition,
                                    statement.name.as_str(),
                                    source_set,
                                    pointer_size,
                                );
                                let md = format_variant_hover(
                                    "flag",
                                    statement.name.as_str(),
                                    value,
                                    &statement.attributes,
                                    &statement.doc_comments,
                                );
                                return hover_response(req.id, md, content, &span);
                            }
                        }
                    }
                    // Deliberately no "anywhere else in the body" fallback: a
                    // hover must never highlight a token the cursor isn't on, so
                    // blank space / braces / keywords resolve to nothing rather
                    // than the (distant) type name.
                }
                // Impl methods (including `#[cfg(...)]`-gated blocks).
                ModuleItem::Impl { impl_block } => {
                    for impl_item in &impl_block.items {
                        if let pyxis::grammar::ImplItem::Function(f) = impl_item {
                            if !f.location.span.contains(&loc) {
                                continue;
                            }
                            // Arg/return *types* (including pointer/array shells)
                            // are handled in branch 1.
                            // An argument name…
                            if let Some((value, span)) = named_arg_hover(
                                f,
                                &loc,
                                tokens,
                                type_registry,
                                &scope,
                                decl_registry,
                                pointer_size,
                            ) {
                                return hover_response(req.id, value, content, &span);
                            }
                            // …`self`, resolving to the impl target type…
                            if let Some(span) = self_arg_span(f, &loc) {
                                let owner = ItemPath::from(impl_block.name.as_str());
                                if let Some(resolved) =
                                    resolve_type_path(&owner, &scope, decl_registry)
                                    && let Some(value) = self.type_hover_text(
                                        &resolved,
                                        type_registry,
                                        decl_registry,
                                        uri,
                                    )
                                {
                                    return hover_response(req.id, value, content, &span);
                                }
                            }
                            // …or the function itself.
                            let span =
                                name_token_span(tokens, &f.location.span.start, f.name.as_str())
                                    .unwrap_or(f.location.span);
                            return hover_response(
                                req.id,
                                format_function_hover(f),
                                content,
                                &span,
                            );
                        }
                    }
                }
                // Free functions: argument names and the function itself.
                ModuleItem::Function { function } if function.location.span.contains(&loc) => {
                    // Arg/return types (including shells) → branch 1.
                    if let Some((value, span)) = named_arg_hover(
                        function,
                        &loc,
                        tokens,
                        type_registry,
                        &scope,
                        decl_registry,
                        pointer_size,
                    ) {
                        return hover_response(req.id, value, content, &span);
                    }
                    let span = name_token_span(
                        tokens,
                        &function.location.span.start,
                        function.name.as_str(),
                    )
                    .unwrap_or(function.location.span);
                    return hover_response(req.id, format_function_hover(function), content, &span);
                }
                // Extern values: `extern name: Type;` — the value's own name.
                ModuleItem::ExternValue { extern_value } => {
                    if let Some(span) = name_token_span(
                        tokens,
                        &extern_value.location.span.start,
                        extern_value.name.as_str(),
                    ) && span.contains(&loc)
                    {
                        let mut md = format!(
                            "**extern value** `{}`\n\n```pyxis\n{}: {}\n```\n",
                            extern_value.name.as_str(),
                            extern_value.name.as_str(),
                            extern_value.type_,
                        );
                        if let Some(size) = type_size_of(
                            &extern_value.type_,
                            type_registry,
                            &scope,
                            decl_registry,
                            pointer_size,
                        ) {
                            push_facts(&mut md, &[("type size", fmt_bytes(size))]);
                        }
                        return hover_response(req.id, md, content, &span);
                    }
                }
                // Extern types: `extern type Name;` — the declared name.
                ModuleItem::ExternType { name, location, .. } => {
                    if let Some(span) = name_token_span(tokens, &location.span.start, name.as_str())
                        && span.contains(&loc)
                    {
                        let path = match self.module_path_for(uri) {
                            Some(mp) => mp.join(name.as_str().into()),
                            None => ItemPath::from(name.as_str()),
                        };
                        let value = self
                            .type_hover_text(&path, type_registry, decl_registry, uri)
                            .unwrap_or_else(|| format!("**extern type** `{}`", name.as_str()));
                        return hover_response(req.id, value, content, &span);
                    }
                }
                // Backend keywords (cpp/rust/prologue/epilogue/definition/for).
                ModuleItem::Backend { backend } => {
                    if backend.location.span.contains(&loc)
                        && let Some((value, span)) = backend_term_at(tokens, backend, &loc)
                    {
                        return hover_response(req.id, value, content, &span);
                    }
                }
                _ => {}
            }
        }

        Response {
            id: req.id,
            result: Some(serde_json::Value::Null),
            error: None,
        }
    }

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

    /// textDocument/prepareTypeHierarchy — anchor the hierarchy on the type
    /// under the cursor.
    pub fn handle_prepare_type_hierarchy(&self, req: Request) -> Response {
        let items = (|| {
            let params: TypeHierarchyPrepareParams =
                serde_json::from_value(req.params.clone()).ok()?;
            let uri = params.text_document_position_params.text_document.uri;
            let content = self.get_content(&uri)?;
            let loc = lsp_position_to_pyxis_location(
                content,
                params.text_document_position_params.position,
            );
            let symbol = self.symbol_at(&uri, &loc)?;
            let (type_registry, decl_registry) = self.registries_for(&uri);
            let item =
                self.type_hierarchy_item(symbol.type_path()?, &uri, type_registry, decl_registry)?;
            Some(vec![item])
        })();
        Response {
            id: req.id,
            result: Some(serde_json::to_value(items).unwrap()),
            error: None,
        }
    }

    /// typeHierarchy/supertypes — a type's base classes (its `#[base]` fields).
    pub fn handle_type_hierarchy_supertypes(&self, req: Request) -> Response {
        let items = serde_json::from_value::<TypeHierarchySupertypesParams>(req.params.clone())
            .ok()
            .map(|p| self.related_types(&p.item, true))
            .unwrap_or_default();
        Response {
            id: req.id,
            result: Some(serde_json::to_value(items).unwrap()),
            error: None,
        }
    }

    /// typeHierarchy/subtypes — types that declare this one as a `#[base]`.
    pub fn handle_type_hierarchy_subtypes(&self, req: Request) -> Response {
        let items = serde_json::from_value::<TypeHierarchySubtypesParams>(req.params.clone())
            .ok()
            .map(|p| self.related_types(&p.item, false))
            .unwrap_or_default();
        Response {
            id: req.id,
            result: Some(serde_json::to_value(items).unwrap()),
            error: None,
        }
    }

    /// Build a TypeHierarchyItem for a resolved type path; the path round-trips
    /// through `data` so supertypes/subtypes can resume from it.
    fn type_hierarchy_item(
        &self,
        path: &ItemPath,
        from_uri: &Uri,
        type_registry: &TypeRegistry,
        decl_registry: &DeclarationRegistry,
    ) -> Option<TypeHierarchyItem> {
        use pyxis::grammar::ItemDefinitionInner;
        let rd = self.resolved_definition(path, type_registry, from_uri)?;
        let content = self.get_content(&rd.uri)?;
        let kind = match decl_registry.get_definition(path).map(|d| &d.inner) {
            Some(ItemDefinitionInner::Enum(_) | ItemDefinitionInner::Bitflags(_)) => {
                SymbolKind::ENUM
            }
            _ => SymbolKind::STRUCT,
        };
        Some(TypeHierarchyItem {
            name: path.last()?.as_str().to_string(),
            kind,
            tags: None,
            detail: Some(path.to_string()),
            uri: rd.uri.clone(),
            range: pyxis_span_to_lsp_range(content, &rd.def.location.span),
            selection_range: pyxis_span_to_lsp_range(content, &rd.name_span),
            data: Some(serde_json::Value::String(path.to_string())),
        })
    }

    /// Supertypes (bases) or subtypes (derivers) of the type named by `item`,
    /// via `#[base]` fields.
    fn related_types(&self, item: &TypeHierarchyItem, supertypes: bool) -> Vec<TypeHierarchyItem> {
        let Some(path) = item
            .data
            .as_ref()
            .and_then(|d| d.as_str())
            .map(ItemPath::from)
        else {
            return vec![];
        };
        let (type_registry, decl_registry) = self.registries_for(&item.uri);

        let mut out = Vec::new();
        if supertypes {
            // `path`'s own `#[base]` fields.
            if let Some(bases) = base_field_targets(&path, decl_registry) {
                for base in bases {
                    if let Some(it) =
                        self.type_hierarchy_item(&base, &item.uri, type_registry, decl_registry)
                    {
                        out.push(it);
                    }
                }
            }
        } else {
            // Every project type that lists `path` among its `#[base]` fields.
            for cand in decl_registry.item_paths() {
                if base_field_targets(cand, decl_registry)
                    .is_some_and(|bases| bases.contains(&path))
                    && let Some(it) =
                        self.type_hierarchy_item(cand, &item.uri, type_registry, decl_registry)
                {
                    out.push(it);
                }
            }
        }
        out
    }
}

/// Build a hover Response with markdown content and a highlight range.
pub(crate) fn hover_response(
    id: lsp_server::RequestId,
    value: String,
    content: &str,
    span: &Span,
) -> Response {
    Response {
        id,
        result: Some(
            serde_json::to_value(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value,
                }),
                range: Some(pyxis_span_to_lsp_range(content, span)),
            })
            .unwrap(),
        ),
        error: None,
    }
}

/// Whether an attribute list contains `#[base]`.
pub(crate) fn has_base_attribute(attrs: &Attributes) -> bool {
    attrs
        .iter()
        .any(|a| matches!(a, Attribute::Ident { ident, .. } if ident.as_str() == "base"))
}

/// The resolved type paths of a type's `#[base]` fields (its base classes).
/// `None` if `path` isn't a declared type; `Some(vec![])` for a type with no
/// bases.
pub(crate) fn base_field_targets(
    path: &ItemPath,
    decl_registry: &DeclarationRegistry,
) -> Option<Vec<ItemPath>> {
    use pyxis::grammar::ItemDefinitionInner;
    let def = decl_registry.get_definition(path)?;
    let ItemDefinitionInner::Type(td) = &def.inner else {
        return Some(vec![]);
    };
    let scope = decl_registry.get_scope(path).unwrap_or(&[]);
    let mut out = Vec::new();
    for st in td.statements() {
        if let TypeField::Field(_, _, type_) = &st.field
            && has_base_attribute(&st.attributes)
            && let Some(p) = type_
                .as_path()
                .and_then(|tp| resolve_type_path(tp, scope, decl_registry))
        {
            out.push(p);
        }
    }
    Some(out)
}
