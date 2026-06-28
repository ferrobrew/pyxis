//! LSP request handlers: hover, definition, completion, symbols, formatting, code lens, inlay hints, rename.

use std::collections::HashMap;

use lsp_server::{Request, Response};
use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeLens, CompletionItem,
    CompletionItemKind, CompletionParams, DocumentLink, DocumentLinkParams, DocumentSymbol,
    DocumentSymbolParams, DocumentSymbolResponse, FoldingRange, FoldingRangeParams, Hover,
    HoverContents, InlayHint, InlayHintLabel, MarkupContent, MarkupKind, Position, Range,
    SymbolInformation, SymbolKind, TextDocumentPositionParams, TextEdit, TypeHierarchyItem,
    TypeHierarchyPrepareParams, TypeHierarchySubtypesParams, TypeHierarchySupertypesParams, Uri,
    WorkspaceEdit, WorkspaceSymbolParams,
};

use pyxis::grammar::{
    Attribute, Attributes, Function, ItemDefinition, ItemPath, Module, ModuleItem, Type, TypeField,
    UseTree, Visibility,
};
use pyxis::semantic;
use pyxis::semantic::declaration_registry::DeclarationRegistry;
use pyxis::semantic::type_registry::TypeRegistry;
use pyxis::span::{FileId, HasLocation, ItemLocation, Location, Span};
use pyxis::tokenizer::{Token, TokenKind};

use crate::span::{lsp_position_to_pyxis_location, pyxis_span_to_lsp_range};
use crate::state::ServerState;

use pyxis::semantic::resolve_item;

impl ServerState {
    /// textDocument/hover
    pub fn handle_hover(&self, req: Request) -> Response {
        let params: TextDocumentPositionParams = match serde_json::from_value(req.params.clone()) {
            Ok(p) => p,
            Err(e) => return error_response(req.id, e),
        };

        let uri = &params.text_document.uri;
        let position = params.position;

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

        let loc = lsp_position_to_pyxis_location(content, position);

        // The compiler's cached token stream — span helpers locate identifiers
        // by matching real tokens rather than scanning source text, so names in
        // comments / splices (which lex as comment/string tokens) are ignored.
        let tokens_arc = self.tokens_for(uri);
        let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);

        let scope = self.scope_for(uri);
        let sources = self.sources_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, sources);
        let analysis = semantic::analyze(&self.db, self.pointer_size_for(uri), source_set);
        let type_registry = analysis.type_registry(&self.db);
        let decl_set =
            semantic::collect_declarations(&self.db, source_set, self.pointer_size_for(uri));
        let decl_registry = decl_set.registry(&self.db);

        let pointer_size = self.pointer_size_for(uri);

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
                        let item_path = match self.module_path_for(uri) {
                            Some(mp) => mp.join(definition.name.as_str().into()),
                            None => ItemPath::from(definition.name.as_str()),
                        };
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
                                    let parent_path = match self.module_path_for(uri) {
                                        Some(mp) => mp.join(definition.name.as_str().into()),
                                        None => ItemPath::from(definition.name.as_str()),
                                    };
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
                                            let owner = match self.module_path_for(uri) {
                                                Some(mp) => {
                                                    mp.join(definition.name.as_str().into())
                                                }
                                                None => ItemPath::from(definition.name.as_str()),
                                            };
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
                                        let owner = match self.module_path_for(uri) {
                                            Some(mp) => mp.join(definition.name.as_str().into()),
                                            None => ItemPath::from(definition.name.as_str()),
                                        };
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

        let loc = lsp_position_to_pyxis_location(content, position);

        // The compiler's cached token stream — see `handle_hover` for rationale.
        let tokens_arc = self.tokens_for(uri);
        let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);

        let scope = self.scope_for(uri);

        // Run analyze to get the type registry with resolved items, and the
        // declaration registry for name resolution.
        let sources = self.sources_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, sources);
        let analysis = semantic::analyze(&self.db, self.pointer_size_for(uri), source_set);
        let type_registry = analysis.type_registry(&self.db);
        let decl_set =
            semantic::collect_declarations(&self.db, source_set, self.pointer_size_for(uri));
        let decl_registry = decl_set.registry(&self.db);

        let pointer_size = self.pointer_size_for(uri);

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

    /// textDocument/completion
    pub fn handle_completion(&self, req: Request) -> Response {
        let uri = serde_json::from_value::<CompletionParams>(req.params.clone())
            .ok()
            .map(|p| p.text_document_position.text_document.uri);

        let mut items: Vec<CompletionItem> = [
            "pub", "type", "enum", "bitflags", "impl", "fn", "extern", "use", "backend", "vftable",
            "const", "mut", "as", "prologue", "epilogue", "self", "Self",
        ]
        .iter()
        .map(|kw| CompletionItem {
            label: kw.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            ..Default::default()
        })
        .collect();

        if let Some(uri) = uri.as_ref() {
            items.extend(self.type_completions(uri));
        }

        Response {
            id: req.id,
            result: Some(serde_json::to_value(items).unwrap()),
            error: None,
        }
    }

    /// Type-name completions for a document: builtins, in-scope user types
    /// (no edit needed), and out-of-scope project types (each carrying a
    /// `use`-import edit applied on accept). The client filters by typed prefix.
    fn type_completions(&self, uri: &Uri) -> Vec<CompletionItem> {
        use pyxis::semantic::types::PredefinedItem;

        let Some(content) = self.get_content(uri) else {
            return vec![];
        };
        let Some(module) = self.get_parsed_module(uri) else {
            return vec![];
        };
        let scope = self.scope_for(uri);
        let pointer_size = self.pointer_size_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, self.sources_for(uri));
        let decl_set = semantic::collect_declarations(&self.db, source_set, pointer_size);
        let decl_registry = decl_set.registry(&self.db);
        let own_module = self.module_path_for(uri);

        let mut items = Vec::new();
        let mut in_scope_names: std::collections::HashSet<String> =
            std::collections::HashSet::new();

        // Builtins — always usable.
        for predefined in PredefinedItem::ALL {
            in_scope_names.insert(predefined.name().to_string());
            items.push(CompletionItem {
                label: predefined.name().to_string(),
                kind: Some(CompletionItemKind::STRUCT),
                detail: Some("builtin".to_string()),
                ..Default::default()
            });
        }

        // `use`-imported types — usable as their leaf name.
        for path in &scope {
            if Some(path) == own_module.as_ref() {
                continue;
            }
            if let Some(leaf) = path.last()
                && in_scope_names.insert(leaf.as_str().to_string())
            {
                items.push(self.type_item(leaf.as_str(), &path.to_string(), decl_registry, path));
            }
        }

        // Same-module types — usable without an import.
        for path in decl_registry.item_paths() {
            if path.parent().as_ref() == own_module.as_ref()
                && let Some(leaf) = path.last()
                && in_scope_names.insert(leaf.as_str().to_string())
            {
                items.push(self.type_item(leaf.as_str(), "this module", decl_registry, path));
            }
        }

        // Out-of-scope project types — offer with an import edit, unless a
        // same-named type is already usable.
        for path in decl_registry.item_paths() {
            let Some(leaf) = path.last() else { continue };
            let name = leaf.as_str();
            if path.parent().as_ref() == own_module.as_ref()
                || scope.iter().any(|s| s == path)
                || in_scope_names.contains(name)
            {
                continue;
            }
            let mut item = self.type_item(name, &path.to_string(), decl_registry, path);
            if let Some((range, new_text)) = self.import_edit(&module, content, path) {
                item.additional_text_edits = Some(vec![TextEdit { range, new_text }]);
            }
            items.push(item);
        }

        items
    }

    /// A completion item for a declared type, with a kind reflecting whether
    /// it's a type/enum/bitflags.
    fn type_item(
        &self,
        label: &str,
        detail: &str,
        decl_registry: &DeclarationRegistry,
        path: &ItemPath,
    ) -> CompletionItem {
        use pyxis::grammar::ItemDefinitionInner;
        let kind = match decl_registry.get_definition(path).map(|d| &d.inner) {
            Some(ItemDefinitionInner::Enum(_) | ItemDefinitionInner::Bitflags(_)) => {
                CompletionItemKind::ENUM
            }
            _ => CompletionItemKind::STRUCT,
        };
        CompletionItem {
            label: label.to_string(),
            kind: Some(kind),
            detail: Some(detail.to_string()),
            ..Default::default()
        }
    }

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
        let _params: WorkspaceSymbolParams = match serde_json::from_value(req.params.clone()) {
            Ok(p) => p,
            Err(e) => return error_response(req.id, e),
        };

        // Collect all symbols across all open documents
        let mut symbols: Vec<SymbolInformation> = Vec::new();

        for (uri, doc) in &self.documents {
            let parsed = semantic::parse_file(&self.db, doc.source_file);
            let module = parsed.module(&self.db);
            let content = &doc.content;

            for item in &module.items {
                if let Some(symbol) = module_item_to_symbol(item, content) {
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

        let Some(content) = self.get_content(uri) else {
            return Response {
                id: req.id,
                result: Some(serde_json::json!([])),
                error: None,
            };
        };

        let Some(module) = self.get_parsed_module(uri) else {
            return Response {
                id: req.id,
                result: Some(serde_json::json!([])),
                error: None,
            };
        };

        let sources = self.sources_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, sources);
        let analysis = semantic::analyze(&self.db, self.pointer_size_for(uri), source_set);
        let type_registry = analysis.type_registry(&self.db);

        let mut lenses = Vec::new();

        for definition in module.definitions() {
            let path = match self.module_path_for(uri) {
                Some(mp) => mp.join(definition.name.as_str().into()),
                None => ItemPath::from(definition.name.as_str()),
            };
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

        let sources = self.sources_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, sources);
        let analysis = semantic::analyze(&self.db, self.pointer_size_for(uri), source_set);
        let type_registry = analysis.type_registry(&self.db);

        let mut hints = Vec::new();

        for definition in module.definitions() {
            // For each field, show its type size as an inlay hint
            if let pyxis::grammar::ItemDefinitionInner::Type(td) = &definition.inner {
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

    /// Resolve the cursor to the canonical path of the symbol it names — a type
    /// reference, a `use` leaf, or a definition's own name. The anchor for
    /// find-references / document-highlight / rename.
    fn symbol_at(&self, uri: &Uri, loc: &Location) -> Option<Symbol> {
        use pyxis::grammar::{ImplItem, ItemDefinitionInner};
        let module = self.get_parsed_module(uri)?;
        let tokens_arc = self.tokens_for(uri);
        let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);
        let scope = self.scope_for(uri);
        let pointer_size = self.pointer_size_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, self.sources_for(uri));
        let analysis = semantic::analyze(&self.db, pointer_size, source_set);
        let type_registry = analysis.type_registry(&self.db);
        let decl_set = semantic::collect_declarations(&self.db, source_set, pointer_size);
        let decl_registry = decl_set.registry(&self.db);
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
        // declaration — return just that.
        let target = match symbol {
            Symbol::Type(p) => p,
            Symbol::Member { owner, name } => {
                return self
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
        let sources = self.sources_for(from_uri);
        let source_set = semantic::SourceSet::new(&self.db, sources);
        let analysis = semantic::analyze(&self.db, pointer_size, source_set);
        let type_registry = analysis.type_registry(&self.db);
        let decl_set = semantic::collect_declarations(&self.db, source_set, pointer_size);
        let decl_registry = decl_set.registry(&self.db);

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

        let Some(content) = self.get_content(uri) else {
            return vec![];
        };
        let scope = self.scope_for(uri);
        let pointer_size = self.pointer_size_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, self.sources_for(uri));
        let analysis = semantic::analyze(&self.db, pointer_size, source_set);
        let type_registry = analysis.type_registry(&self.db);
        let resolver = analysis.doc_link_resolver(&self.db);
        let tokens_arc = self.tokens_for(uri);
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
            for (start_byte, end_byte, path_str) in scan_doc_links(line) {
                let item_path = match resolver.resolve(&scope, &path_str) {
                    Some(DocLinkTarget::Item(p)) => p,
                    Some(DocLinkTarget::Member { item, .. }) => item,
                    _ => continue,
                };
                let Some(rd) = self.resolved_definition(&item_path, type_registry, uri) else {
                    continue;
                };
                let span = Span::new(
                    Location::new(line_no, start_byte + 1),
                    Location::new(line_no, end_byte + 1),
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
    fn resolve_doc_member(
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
        let pointer_size = self.pointer_size_for(from_uri);
        let source_set = semantic::SourceSet::new(&self.db, self.sources_for(from_uri));
        let decl_set = semantic::collect_declarations(&self.db, source_set, pointer_size);
        let decl_registry = decl_set.registry(&self.db);

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
    fn doc_link_at(
        &self,
        uri: &Uri,
        loc: &Location,
    ) -> Option<(Span, lsp_types::Location, String)> {
        use pyxis::semantic::doc_links::DocLinkTarget;
        let content = self.get_content(uri)?;
        let tokens_arc = self.tokens_for(uri);
        let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);
        // Only act inside a doc comment.
        if !tokens.iter().any(|t| {
            matches!(t.kind, TokenKind::DocOuter(_)) && t.location.span.start.line == loc.line
        }) {
            return None;
        }
        let line = content.lines().nth(loc.line - 1)?;
        let col = loc.column.saturating_sub(1); // 0-indexed byte column
        let scope = self.scope_for(uri);
        let pointer_size = self.pointer_size_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, self.sources_for(uri));
        let analysis = semantic::analyze(&self.db, pointer_size, source_set);
        let resolver = analysis.doc_link_resolver(&self.db);
        let type_registry = analysis.type_registry(&self.db);
        let decl_set = semantic::collect_declarations(&self.db, source_set, pointer_size);
        let decl_registry = decl_set.registry(&self.db);

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

        for (start_byte, end_byte, path_str) in scan_doc_links(line) {
            if col < start_byte || col >= end_byte {
                continue;
            }
            let link_span = Span::new(
                Location::new(loc.line, start_byte + 1),
                Location::new(loc.line, end_byte + 1),
            );
            let (location, hover) = match resolver.resolve(&scope, &path_str)? {
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
        let pointer_size = self.pointer_size_for(from_uri);
        let source_set = semantic::SourceSet::new(&self.db, self.sources_for(from_uri));
        let decl_set = semantic::collect_declarations(&self.db, source_set, pointer_size);
        let decl_registry = decl_set.registry(&self.db);

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

        let Some(content) = self.get_content(uri) else {
            return vec![];
        };
        let Some(module) = self.get_parsed_module(uri) else {
            return vec![];
        };
        let scope = self.scope_for(uri);
        let pointer_size = self.pointer_size_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, self.sources_for(uri));
        let analysis = semantic::analyze(&self.db, pointer_size, source_set);
        let type_registry = analysis.type_registry(&self.db);
        let decl_set = semantic::collect_declarations(&self.db, source_set, pointer_size);
        let decl_registry = decl_set.registry(&self.db);
        let tokens_arc = self.tokens_for(uri);
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
                    if let pyxis::grammar::ItemDefinitionInner::Type(td) = &definition.inner {
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

    /// Project type/decl registries for a file (shared analysis setup).
    fn registries_for(
        &self,
        uri: &Uri,
    ) -> (
        &std::sync::Arc<TypeRegistry>,
        &std::sync::Arc<DeclarationRegistry>,
    ) {
        let pointer_size = self.pointer_size_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, self.sources_for(uri));
        let analysis = semantic::analyze(&self.db, pointer_size, source_set);
        let decl_set = semantic::collect_declarations(&self.db, source_set, pointer_size);
        (
            analysis.type_registry(&self.db),
            decl_set.registry(&self.db),
        )
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
        let Some(content) = self.get_content(uri) else {
            return vec![];
        };
        let Some(module) = self.get_parsed_module(uri) else {
            return vec![];
        };
        let loc = lsp_position_to_pyxis_location(content, range.start);
        let tokens_arc = self.tokens_for(uri);
        let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);
        let scope = self.scope_for(uri);
        let pointer_size = self.pointer_size_for(uri);
        let sources = self.sources_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, sources);
        let analysis = semantic::analyze(&self.db, pointer_size, source_set);
        let type_registry = analysis.type_registry(&self.db);
        let decl_set = semantic::collect_declarations(&self.db, source_set, pointer_size);
        let decl_registry = decl_set.registry(&self.db);

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
    fn import_edit(
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
            let ModuleItem::Use { tree, location } = item else {
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
        if let Some(p) = symbol.type_path() {
            let pointer_size = self.pointer_size_for(uri);
            let source_set = semantic::SourceSet::new(&self.db, self.sources_for(uri));
            let decl_set = semantic::collect_declarations(&self.db, source_set, pointer_size);
            if decl_set.registry(&self.db).get_predefined(p).is_some() {
                return None;
            }
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

    /// Build the name-resolution scope for a document: its own module path plus
    /// every `use`-imported path. This mirrors `DeclarationRegistry::register_module`
    /// — `resolve_name` matches imported names by their full paths' last segment,
    /// so a single-segment type reference like `EventHandler` only resolves when
    /// the corresponding `use game::event_handler::EventHandler;` path is present
    /// in the scope.
    fn scope_for(&self, uri: &Uri) -> Vec<ItemPath> {
        let Some(module_path) = self.module_path_for(uri) else {
            return vec![];
        };
        let mut scope = vec![module_path];
        if let Some(module) = self.get_parsed_module(uri) {
            for item in &module.items {
                if let ModuleItem::Use { tree, .. } = item {
                    scope.extend(tree.flatten());
                }
            }
        }
        scope
    }

    /// Resolve an item path to its grammar definition by locating it in its
    /// module's source file. Deliberately does NOT depend on the type registry
    /// for *finding* the definition — types with semantic errors (e.g. a
    /// mid-edit `#[size]` mismatch, common in RE work) drop out of the registry,
    /// but their declaration is still in the file, so hover/go-to-def keep
    /// working. The registry is only consulted for size/alignment, which is
    /// legitimately absent when the type doesn't resolve.
    fn resolved_definition(
        &self,
        resolved_path: &ItemPath,
        type_registry: &TypeRegistry,
        from_uri: &Uri,
    ) -> Option<ResolvedDefinition> {
        let module_path = resolved_path.parent()?;
        let uri = self.module_uri(&module_path, from_uri)?;
        let tokens_arc = self.tokens_for(&uri)?;
        let module = self.get_parsed_module(&uri)?;
        let target_name = resolved_path.last()?.as_str();
        let def = module
            .definitions()
            .find(|d| d.name.as_str() == target_name)
            .cloned()?;
        let name_span = name_token_span(
            &tokens_arc,
            &def.declaration_location.span.start,
            target_name,
        )
        .unwrap_or(def.location.span);
        let size_align = type_registry
            .get(resolved_path, &ItemLocation::internal())
            .ok()
            .and_then(|i| i.resolved())
            .map(|r| (r.size, r.alignment));
        Some(ResolvedDefinition {
            def,
            uri,
            name_span,
            size_align,
        })
    }

    /// Resolved value of an enum variant / bitflags flag named `variant_name`
    /// within `definition` (auto-incremented values are only known post-resolve).
    fn variant_value(
        &self,
        uri: &Uri,
        definition: &ItemDefinition,
        variant_name: &str,
        source_set: semantic::SourceSet,
        pointer_size: usize,
    ) -> Option<i128> {
        let path = match self.module_path_for(uri) {
            Some(mp) => mp.join(definition.name.as_str().into()),
            None => ItemPath::from(definition.name.as_str()),
        };
        let resolved = resolve_item(&self.db, source_set, pointer_size, path);
        match &resolved.item(&self.db).resolved()?.inner {
            pyxis::semantic::types::ItemDefinitionInner::Enum(e) => e
                .variants
                .iter()
                .find(|v| v.name == variant_name)
                .map(|v| v.value as i128),
            pyxis::semantic::types::ItemDefinitionInner::Bitflags(b) => b
                .flags
                .iter()
                .find(|f| f.name == variant_name)
                .map(|f| f.value as i128),
            _ => None,
        }
    }

    /// Hover markdown for a resolved item path: a user-defined type located in
    /// its source file, or a predefined/extern builtin.
    fn type_hover_text(
        &self,
        item_path: &ItemPath,
        type_registry: &TypeRegistry,
        decl_registry: &DeclarationRegistry,
        from_uri: &Uri,
    ) -> Option<String> {
        self.resolved_definition(item_path, type_registry, from_uri)
            .map(|rd| match rd.size_align {
                Some((size, alignment)) => format_type_hover_with_size(&rd.def, size, alignment),
                None => format_type_hover(&rd.def),
            })
            .or_else(|| builtin_hover(item_path, decl_registry))
    }

    /// Find the document URI whose module path equals `module_path`, scoped to
    /// the same project as `from_uri`. Different projects can have files at the
    /// same relative path (e.g. `game_objects/physics_game_object.pyxis` in both
    /// JustCause2 and MadMax), hence the same module path — so we must resolve
    /// within the requesting file's project rather than matching globally.
    fn module_uri(&self, module_path: &ItemPath, from_uri: &Uri) -> Option<Uri> {
        let from_root = self
            .documents
            .get(from_uri)
            .and_then(|d| d.project_root.clone());
        self.documents
            .keys()
            .find(|uri| {
                self.documents
                    .get(*uri)
                    .and_then(|d| d.project_root.clone())
                    == from_root
                    && self.module_path_for(uri).as_ref() == Some(module_path)
            })
            .cloned()
    }
}

/// A type/item definition located in its source file.
struct ResolvedDefinition {
    def: ItemDefinition,
    /// URI of the file the definition lives in.
    uri: Uri,
    /// Span of the definition's name (the go-to-definition target).
    name_span: Span,
    /// Size/alignment, if the type resolved cleanly.
    size_align: Option<(usize, usize)>,
}

/// Hover for a predefined (`bool`, `u32`, …) or extern type — these have no
/// source definition but a known size/alignment.
fn builtin_hover(path: &ItemPath, decl_registry: &DeclarationRegistry) -> Option<String> {
    let (kind, size, alignment) = if let Some(p) = decl_registry.get_predefined(path) {
        ("builtin", p.size, p.alignment)
    } else if let Some(e) = decl_registry.get_extern_type(path) {
        ("extern type", e.size, e.alignment)
    } else {
        return None;
    };
    let mut md = format!("**{kind}** `{path}`\n");
    push_facts(
        &mut md,
        &[("size", fmt_bytes(size)), ("align", fmt_bytes(alignment))],
    );
    Some(md)
}

/// Render a byte quantity as `` `0x10` (16) `` for hover facts.
fn fmt_bytes(n: usize) -> String {
    format!("`0x{n:X}` ({n})")
}

/// Join labelled facts into a single markdown line so they don't collapse onto
/// each other (a lone `\n` is not a line break in markdown). rust-analyzer
/// renders the same kind of `size = …, align = …` brief; we use ` | ` so the
/// facts read as a compact, scannable strip.
fn facts_line(facts: &[(&str, String)]) -> String {
    facts
        .iter()
        .map(|(label, value)| format!("{label} {value}"))
        .collect::<Vec<_>>()
        .join("  |  ")
}

/// Append a facts line as its own paragraph (blank line before).
fn push_facts(md: &mut String, facts: &[(&str, String)]) {
    if !facts.is_empty() {
        md.push_str(&format!("\n{}\n", facts_line(facts)));
    }
}

/// Format a type definition for hover display with size and alignment
fn format_type_hover_with_size(
    definition: &ItemDefinition,
    size: usize,
    alignment: usize,
) -> String {
    let mut md = format_type_hover(definition);
    push_facts(
        &mut md,
        &[("size", fmt_bytes(size)), ("align", fmt_bytes(alignment))],
    );
    md
}

/// Format a type definition for hover display
fn format_type_hover(definition: &ItemDefinition) -> String {
    let name = definition.name.as_str();
    let kind = match &definition.inner {
        pyxis::grammar::ItemDefinitionInner::Type(_) => "type",
        pyxis::grammar::ItemDefinitionInner::Enum(_) => "enum",
        pyxis::grammar::ItemDefinitionInner::Bitflags(_) => "bitflags",
        pyxis::grammar::ItemDefinitionInner::TypeAlias(_) => "type alias",
    };
    let mut md = format!("**{}** `{}`\n\n", kind, name);

    if !definition.doc_comments.is_empty() {
        md.push_str(&definition.doc_comments.join("\n"));
        md.push_str("\n\n");
    }

    if let pyxis::grammar::ItemDefinitionInner::Type(td) = &definition.inner {
        md.push_str("**Fields:**\n");
        for statement in td.statements() {
            if let TypeField::Field(vis, name, type_) = &statement.field {
                let vis_str = if matches!(vis, Visibility::Public) {
                    "pub "
                } else {
                    ""
                };
                md.push_str(&format!("- `{}{}: {}`\n", vis_str, name, type_));
            }
        }
    }

    md
}

/// Convert a ModuleItem to a DocumentSymbol
#[allow(deprecated)] // DocumentSymbol::deprecated field
fn module_item_to_symbol(item: &ModuleItem, source: &str) -> Option<DocumentSymbol> {
    let (name, kind, range_span) = match item {
        ModuleItem::Definition { definition } => {
            let kind = match &definition.inner {
                pyxis::grammar::ItemDefinitionInner::Type(_) => SymbolKind::STRUCT,
                pyxis::grammar::ItemDefinitionInner::Enum(_) => SymbolKind::ENUM,
                pyxis::grammar::ItemDefinitionInner::Bitflags(_) => SymbolKind::ENUM,
                pyxis::grammar::ItemDefinitionInner::TypeAlias(_) => SymbolKind::TYPE_PARAMETER,
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

/// Check if a string is a valid Pyxis identifier
fn is_valid_identifier(s: &str) -> bool {
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

/// Build a hover Response with markdown content and a highlight range.
fn hover_response(
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

fn error_response(id: lsp_server::RequestId, e: serde_json::Error) -> Response {
    Response {
        id,
        result: None,
        error: Some(lsp_server::ResponseError {
            code: 0,
            message: e.to_string(),
            data: None,
        }),
    }
}

/// A reference found under the cursor. Either a *named* reference (a type/import
/// segment, resolved to a concrete item or module) or a pointer/array/unknown
/// *shell* whose pre-rendered hover markdown describes its shape.
/// What the cursor resolves to for navigation/rename: a type, or a member of
/// one (a field or method). Members have no cross-references in pyxis source
/// beyond their declaration.
enum Symbol {
    Type(ItemPath),
    Member { owner: ItemPath, name: String },
}

impl Symbol {
    /// The type path, if this symbol is a type (not a member). Used by features
    /// that only apply to types — go-to-implementation, type hierarchy.
    fn type_path(&self) -> Option<&ItemPath> {
        match self {
            Symbol::Type(p) => Some(p),
            Symbol::Member { .. } => None,
        }
    }
}

enum Ref {
    Item {
        /// Resolved concrete item path, if this segment names a type/extern/predefined.
        item: Option<ItemPath>,
        /// Absolute path of this segment — used as a module target when `item`
        /// is `None` (e.g. the `game::event_handler` part of an FQN reference).
        module_path: ItemPath,
        /// Source span of the *segment* under the cursor (hover-range highlight).
        span: Span,
    },
    Shell {
        /// Pre-rendered hover markdown for the shell.
        md: String,
        /// Span of the shell type (hover-range highlight). Shells have no
        /// definition, so go-to-def ignores this variant.
        span: Span,
    },
}

/// The result of locating the cursor within a grammar `Type`: either on a named
/// type identifier (the leaf to resolve) or on a pointer/array/unknown shell.
enum TypeHit<'a> {
    Ident(&'a ItemPath, Span),
    Shell(&'a Type),
}

/// Find a type or import reference at the given location in a parsed module.
///
/// Walks `use` statements and type positions (field types, enum/bitflags base
/// types, type-alias targets, vftable signatures) looking for the AST node
/// whose span contains the cursor. For multi-segment paths (`a::b::C`), the
/// *individual* segment under the cursor is resolved independently: the leaf
/// resolves to a type, earlier segments to their module namespaces.
///
/// Resolution itself comes from the Salsa-computed `DeclarationRegistry` —
/// this function only does the position→node matching, which is an LSP
/// concern (the compiler never works with cursor positions).
fn find_reference_at(
    module: &Module,
    loc: &Location,
    scope: &[ItemPath],
    decl_registry: &DeclarationRegistry,
    tokens: &[Token],
    type_registry: &TypeRegistry,
    pointer_size: usize,
) -> Option<Ref> {
    let resolve = |raw: &ItemPath, full_span: Span| -> Ref {
        let (sub_path, seg_span) =
            segment_at(raw, &full_span, loc, tokens).unwrap_or((raw.clone(), full_span));
        let item = resolve_type_path(&sub_path, scope, decl_registry);
        Ref::Item {
            item,
            module_path: sub_path,
            span: seg_span,
        }
    };
    // For `use` trees the segment is already resolved by use_tree_reference, so
    // build the reference directly without re-running segment_at.
    let make_ref = |seg_path: ItemPath, span: Span| -> Ref {
        let item = resolve_type_path(&seg_path, scope, decl_registry);
        Ref::Item {
            item,
            module_path: seg_path,
            span,
        }
    };
    // Turn a located type-position hit into a reference: a named ident resolves
    // like any other path; a pointer/array/unknown shell renders its shape.
    let from_hit = |hit: TypeHit| -> Ref {
        match hit {
            TypeHit::Ident(path, span) => resolve(path, span),
            TypeHit::Shell(type_) => Ref::Shell {
                md: shell_hover_md(type_, type_registry, scope, decl_registry, pointer_size),
                span: type_.location().span,
            },
        }
    };

    use pyxis::grammar::ImplItem;

    for item in &module.items {
        match item {
            // `use` statements: cursor on an imported path.
            ModuleItem::Use { tree, .. } => {
                if let Some((path, span)) = use_tree_reference(tree, loc, tokens) {
                    return Some(make_ref(path, span));
                }
            }
            // `backend` blocks carry their own `use`-style dependency list, and
            // prologue/epilogue splices can be attributed `for <Type>`.
            ModuleItem::Backend { backend } => {
                for tree in &backend.uses {
                    if let Some((path, span)) = use_tree_reference(tree, loc, tokens) {
                        return Some(make_ref(path, span));
                    }
                }
                for for_type in [&backend.prologue.for_type, &backend.epilogue.for_type]
                    .into_iter()
                    .flatten()
                {
                    if let Some(span) =
                        find_for_path_span(tokens, &backend.location.span, for_type, loc)
                    {
                        return Some(resolve(for_type, span));
                    }
                }
            }
            // `impl` blocks (including `#[cfg(...)]`-gated ones): the target
            // type name and every function signature's types.
            ModuleItem::Impl { impl_block } => {
                if let Some(span) = name_token_span(
                    tokens,
                    &impl_block.location.span.start,
                    impl_block.name.as_str(),
                ) && span.contains(loc)
                {
                    let raw = ItemPath::from(impl_block.name.as_str());
                    return Some(resolve(&raw, span));
                }
                for impl_item in &impl_block.items {
                    if let ImplItem::Function(f) = impl_item
                        && let Some(hit) = fn_signature_type_ref(f, loc)
                    {
                        return Some(from_hit(hit));
                    }
                }
            }
            // Free functions: argument and return types.
            ModuleItem::Function { function } => {
                if let Some(hit) = fn_signature_type_ref(function, loc) {
                    return Some(from_hit(hit));
                }
            }
            // Extern values: `extern name: Type;` — the referenced type.
            ModuleItem::ExternValue { extern_value } => {
                if let Some(hit) = type_hit_at(&extern_value.type_, loc) {
                    return Some(from_hit(hit));
                }
            }
            _ => {}
        }
    }

    // Type positions inside definitions.
    for definition in module.definitions() {
        if let Some(hit) = find_type_ref_in_definition(definition, loc) {
            return Some(from_hit(hit));
        }
    }

    None
}

/// Given a `::`-separated path as written in `source`, its full span, and a
/// cursor location, return the path truncated to the segment under the cursor
/// (an absolute prefix for FQN paths) plus that segment's own span. Paths are
/// always written on a single line.
fn segment_at(
    path: &ItemPath,
    span: &Span,
    loc: &Location,
    tokens: &[Token],
) -> Option<(ItemPath, Span)> {
    // The path's identifier tokens, in order, within the path's span (the `::`
    // separators lex as their own `ColonColon` tokens and are skipped).
    let idents = tokens
        .iter()
        .filter(|t| matches!(t.kind, TokenKind::Ident(_)) && span.contains(&t.location.span.start));
    for (i, ident) in idents.enumerate() {
        if ident.location.span.contains(loc) {
            let count = (i + 1).min(path.len());
            let prefix: ItemPath = path.iter().take(count).cloned().collect();
            return Some((prefix, ident.location.span));
        }
    }
    None
}

/// Find a type hit (argument or return type) under the cursor in a function
/// signature — a named ident or a pointer/array/unknown shell.
fn fn_signature_type_ref<'a>(f: &'a Function, loc: &Location) -> Option<TypeHit<'a>> {
    use pyxis::grammar::Argument;
    for arg in &f.arguments {
        if let Argument::Named { type_, .. } = arg
            && let Some(found) = type_hit_at(type_, loc)
        {
            return Some(found);
        }
    }
    if let Some(ret) = &f.return_type
        && let Some(found) = type_hit_at(ret, loc)
    {
        return Some(found);
    }
    None
}

fn find_type_ref_in_definition<'a>(
    definition: &'a ItemDefinition,
    loc: &Location,
) -> Option<TypeHit<'a>> {
    use pyxis::grammar::{Argument, ItemDefinitionInner};

    match &definition.inner {
        ItemDefinitionInner::Type(td) => {
            for statement in td.statements() {
                match &statement.field {
                    TypeField::Field(_, _, type_) => {
                        if let Some(found) = type_hit_at(type_, loc) {
                            return Some(found);
                        }
                    }
                    TypeField::Vftable(fns) => {
                        for sig in fns {
                            for arg in &sig.arguments {
                                if let Argument::Named { type_, .. } = arg
                                    && let Some(found) = type_hit_at(type_, loc)
                                {
                                    return Some(found);
                                }
                            }
                            if let Some(ret) = &sig.return_type
                                && let Some(found) = type_hit_at(ret, loc)
                            {
                                return Some(found);
                            }
                        }
                    }
                }
            }
        }
        ItemDefinitionInner::Enum(e) => return type_hit_at(&e.type_, loc),
        ItemDefinitionInner::Bitflags(b) => return type_hit_at(&b.type_, loc),
        ItemDefinitionInner::TypeAlias(ta) => return type_hit_at(&ta.target, loc),
    }

    None
}

/// Resolve the import-path segment under the cursor within a `use` tree.
///
/// Returns the segment's full path (prefix-joined for grouped imports) and the
/// segment's span. Handles both the imported leaves and the shared prefix of a
/// braced group, resolving the specific `::`-separated segment the cursor is on
/// — so each ident of `types::shared_ptr::{SharedPtr, WeakPtr}` navigates
/// independently. The returned path is already truncated to the segment, so
/// callers must NOT run `segment_at` on it again.
fn use_tree_reference(
    tree: &UseTree,
    loc: &Location,
    tokens: &[Token],
) -> Option<(ItemPath, Span)> {
    match tree {
        UseTree::Path { path, location } => {
            if !location.span.contains(loc) {
                return None;
            }
            segment_at(path, &location.span, loc, tokens)
        }
        UseTree::Group {
            prefix,
            items,
            location,
        } => {
            // NB: a group's span starts at `{`, so the shared prefix that
            // precedes it is *outside* this span — don't early-return on it.
            // A leaf import — prepend the group's prefix to the matched segment.
            for item in items {
                if let Some((sub, span)) = use_tree_reference(item, loc, tokens) {
                    let full: ItemPath = prefix.iter().chain(sub.iter()).cloned().collect();
                    return Some((full, span));
                }
            }
            // The shared prefix (`a::b` in `a::b::{C, D}`). The group's span
            // starts at `{`, so the prefix tokens precede it — they're the
            // contiguous run of Ident/:: tokens immediately before the brace.
            if !prefix.is_empty() {
                let brace = location.span.start;
                if let Some(brace_idx) = tokens.iter().position(|t| t.location.span.start >= brace)
                {
                    let mut start_idx = brace_idx;
                    while start_idx > 0
                        && matches!(
                            tokens[start_idx - 1].kind,
                            TokenKind::Ident(_) | TokenKind::ColonColon
                        )
                    {
                        start_idx -= 1;
                    }
                    if start_idx < brace_idx {
                        let prefix_span = Span::new(
                            tokens[start_idx].location.span.start,
                            tokens[brace_idx - 1].location.span.end,
                        );
                        if prefix_span.contains(loc) {
                            return segment_at(prefix, &prefix_span, loc, tokens);
                        }
                    }
                }
            }
            None
        }
    }
}

/// Recursively locate the cursor within a grammar `Type`. Returns whether the
/// cursor is on a named type identifier (the leaf to resolve — checking the
/// narrower generic args first) or on a pointer/array/unknown *shell* (the
/// pointer/array/unknown wrapping, rather than its inner pointee/element). This
/// single walk drives both type-reference resolution and shell-shape hovers at
/// every type position.
fn type_hit_at<'a>(type_: &'a Type, loc: &Location) -> Option<TypeHit<'a>> {
    if !type_.location().span.contains(loc) {
        return None;
    }
    match type_ {
        Type::Ident {
            path, generic_args, ..
        } => {
            for arg in generic_args {
                if let Some(found) = type_hit_at(arg, loc) {
                    return Some(found);
                }
            }
            Some(TypeHit::Ident(path, type_.location().span))
        }
        Type::ConstPointer { pointee, .. } | Type::MutPointer { pointee, .. } => {
            if pointee.location().span.contains(loc) {
                type_hit_at(pointee, loc)
            } else {
                Some(TypeHit::Shell(type_))
            }
        }
        Type::Array { element, .. } => {
            if element.location().span.contains(loc) {
                type_hit_at(element, loc)
            } else {
                Some(TypeHit::Shell(type_))
            }
        }
        Type::Unknown { .. } => Some(TypeHit::Shell(type_)),
    }
}

/// Resolve a type path (from the grammar AST) to a full `ItemPath` using the
/// declaration registry's scope resolution (handles use-statements, relative
/// paths, and absolute paths). This is the Salsa-computed resolution — we're
/// just calling it, not reimplementing it.
fn resolve_type_path(
    path: &ItemPath,
    scope: &[ItemPath],
    decl_registry: &DeclarationRegistry,
) -> Option<ItemPath> {
    use pyxis::semantic::declaration_registry::NameResolution;

    // For multi-segment paths (e.g. `types::math::Vector2`), the path is
    // likely absolute — check it directly against the registry first.
    if path.len() > 1 && decl_registry.contains(path) {
        return Some(path.clone());
    }

    // Single-segment or unresolved multi-segment: resolve the leaf name
    // through the scope (handles use-statements and relative resolution).
    let name = path.last()?.as_str();
    match decl_registry.resolve_name(scope, name) {
        NameResolution::Found(p)
        | NameResolution::FoundPredefined(p)
        | NameResolution::FoundExtern(p) => Some(p),
        NameResolution::NotFound => None,
    }
}

/// Find the span of the identifier `name` at or after `from` in the token
/// stream.
///
/// Grammar `Ident`s don't carry their own span, so name spans (type names,
/// field names, function names) are recovered from the compiler's cached token
/// stream: the first token at/after the declaration's start point whose text
/// matches `name`. The `"vftable"` keyword lexes as `TokenKind::Vftable` rather
/// than an `Ident`, so it's matched specially. Working off real tokens (rather
/// than a text scan) means occurrences inside comments or `r#"…"#` splices —
/// which lex as comment/string tokens, not `Ident`s — are correctly ignored.
fn name_token_span(tokens: &[Token], from: &Location, name: &str) -> Option<Span> {
    if name.is_empty() {
        return None;
    }
    tokens
        .iter()
        .find(|t| {
            t.location.span.start >= *from
                && match &t.kind {
                    TokenKind::Vftable => name == "vftable",
                    TokenKind::Ident(s) => s == name,
                    _ => false,
                }
        })
        .map(|t| t.location.span)
}

/// Find the span of a `for <path>` clause (in a backend prologue/epilogue)
/// whose path matches `path` and whose span contains `loc`. The `for_type`
/// clause has no recorded span, so we recover it from the token stream: the
/// `for` keyword (which lexes as `Ident("for")`) within the backend block,
/// followed by the run of `Ident`/`::` tokens that make up the path.
fn find_for_path_span(
    tokens: &[Token],
    block_span: &Span,
    path: &ItemPath,
    loc: &Location,
) -> Option<Span> {
    for (i, t) in tokens.iter().enumerate() {
        if !block_span.contains(&t.location.span.start) {
            continue;
        }
        if !matches!(&t.kind, TokenKind::Ident(s) if s == "for") {
            continue;
        }
        // The path is the run of Ident/:: tokens immediately following `for`.
        let run_start = i + 1;
        let mut run_end = run_start;
        while run_end < tokens.len()
            && matches!(
                tokens[run_end].kind,
                TokenKind::Ident(_) | TokenKind::ColonColon
            )
        {
            run_end += 1;
        }
        let run = &tokens[run_start..run_end];
        let (Some(first), Some(last)) = (run.first(), run.last()) else {
            continue;
        };
        let span = Span::new(first.location.span.start, last.location.span.end);
        if span.contains(loc) && run_matches_path(run, path) {
            return Some(span);
        }
    }
    None
}

/// Whether the `Ident` tokens of `run` (ignoring the interleaved `::`) name
/// exactly the segments of `path`.
fn run_matches_path(run: &[Token], path: &ItemPath) -> bool {
    let idents: Vec<&str> = run
        .iter()
        .filter_map(|t| match &t.kind {
            TokenKind::Ident(s) => Some(s.as_str()),
            _ => None,
        })
        .collect();
    idents.len() == path.len()
        && idents
            .iter()
            .zip(path.iter())
            .all(|(seg, expected)| *seg == expected.as_str())
}

/// Describe a backend keyword (`cpp`/`rust`/`prologue`/`epilogue`/`definition`/
/// `uses`/`for`) under the cursor. The grammar AST carries no spans for these,
/// so we tokenize and match the token directly. A `r#"…"#` splice lexes as a
/// single token, so keywords inside the spliced code can never match.
fn backend_term_at(
    tokens: &[pyxis::tokenizer::Token],
    backend: &pyxis::grammar::Backend,
    loc: &Location,
) -> Option<(String, Span)> {
    use pyxis::tokenizer::TokenKind;
    let name = backend.name.name();
    let token = tokens.iter().find(|t| t.location.span.contains(loc))?;
    let desc: String = match &token.kind {
        TokenKind::Backend => format!("The `{name}` backend block."),
        TokenKind::Prologue => "Splice emitted *before* this backend's generated output.".into(),
        TokenKind::Epilogue => "Splice emitted *after* this backend's generated output.".into(),
        TokenKind::Ident(s) if s == name => {
            format!("The `{name}` backend — code emitted by the {name} generator.")
        }
        TokenKind::Ident(s) if s == "for" => "Attributes this splice to a specific type.".into(),
        TokenKind::Ident(s) if s == "definition" => {
            "Targets the source/definition file rather than the header.".into()
        }
        TokenKind::Ident(s) if s == "uses" => {
            "Declares other-module items this backend block depends on.".into()
        }
        _ => return None,
    };
    Some((format!("**backend**\n\n{desc}"), token.location.span))
}

/// Render a definition/field's attributes compactly (e.g. `#[base] #[cfg(...)]`).
fn render_attributes(attributes: &Attributes) -> String {
    attributes
        .0
        .iter()
        .map(|a| format!("`{}`", render_attribute(a)))
        .collect::<Vec<_>>()
        .join(" ")
}

/// The attribute name (`size`, `cfg`, `base`, …).
fn attribute_name(attribute: &Attribute) -> &str {
    match attribute {
        Attribute::Ident { ident, .. } => ident.as_str(),
        Attribute::Function { name, .. } => name.as_str(),
        Attribute::Assign { name, .. } => name.as_str(),
        Attribute::Cfg { .. } => "cfg",
    }
}

/// A one-line description of a known Pyxis attribute.
fn attribute_description(name: &str) -> Option<&'static str> {
    Some(match name {
        "size" => "Asserts/overrides the type's total size in bytes.",
        "align" => "Overrides the type's alignment in bytes.",
        "packed" => "Removes inter-field padding (alignment 1).",
        "base" => "Marks the field as a base class, inlined at the start of the type.",
        "index" => "Pins a vftable entry to a specific slot index.",
        "address" => "Pins the item to a fixed absolute address.",
        "singleton" => "Marks the type as a singleton living at a fixed address.",
        "copyable" => "Marks the type as trivially copyable.",
        "cloneable" => "Marks the type as cloneable.",
        "defaultable" | "default" => "Marks the type/variant as the default.",
        "cfg" => "Conditional-compilation predicate; each backend evaluates it independently.",
        "calling_convention" => "Sets the function's calling convention.",
        _ => return None,
    })
}

/// The source text covered by `span` (single line only).
fn span_text(content: &str, span: &Span) -> Option<String> {
    if span.start.line != span.end.line {
        return None;
    }
    let line = content.lines().nth(span.start.line.saturating_sub(1))?;
    let lo = span.start.column.saturating_sub(1);
    let hi = span.end.column.saturating_sub(1).min(line.len());
    line.get(lo..hi).map(str::to_string)
}

/// Find an attribute whose span contains `loc`, anywhere in the module (type /
/// field / vftable / enum-variant / impl / function attributes).
fn attribute_at<'a>(module: &'a Module, loc: &Location) -> Option<(&'a Attribute, Span)> {
    use pyxis::grammar::{ImplItem, ItemDefinitionInner};
    let find = |attrs: &'a Attributes| {
        attrs
            .0
            .iter()
            .find(|a| a.location().span.contains(loc))
            .map(|a| (a, a.location().span))
    };
    for item in &module.items {
        match item {
            ModuleItem::Definition { definition } => {
                let inner_attrs = match &definition.inner {
                    ItemDefinitionInner::Type(td) => &td.attributes,
                    ItemDefinitionInner::Enum(e) => &e.attributes,
                    ItemDefinitionInner::Bitflags(b) => &b.attributes,
                    ItemDefinitionInner::TypeAlias(ta) => &ta.attributes,
                };
                if let Some(hit) = find(inner_attrs) {
                    return Some(hit);
                }
                match &definition.inner {
                    ItemDefinitionInner::Type(td) => {
                        for s in td.statements() {
                            if let Some(hit) = find(&s.attributes) {
                                return Some(hit);
                            }
                            if let TypeField::Vftable(fns) = &s.field {
                                for f in fns {
                                    if let Some(hit) = find(&f.attributes) {
                                        return Some(hit);
                                    }
                                }
                            }
                        }
                    }
                    ItemDefinitionInner::Enum(e) => {
                        for s in e.statements() {
                            if let Some(hit) = find(&s.attributes) {
                                return Some(hit);
                            }
                        }
                    }
                    ItemDefinitionInner::Bitflags(b) => {
                        for s in b.statements() {
                            if let Some(hit) = find(&s.attributes) {
                                return Some(hit);
                            }
                        }
                    }
                    ItemDefinitionInner::TypeAlias(_) => {}
                }
            }
            ModuleItem::Impl { impl_block } => {
                if let Some(hit) = find(&impl_block.attributes) {
                    return Some(hit);
                }
                for it in &impl_block.items {
                    if let ImplItem::Function(f) = it
                        && let Some(hit) = find(&f.attributes)
                    {
                        return Some(hit);
                    }
                }
            }
            ModuleItem::Function { function } => {
                if let Some(hit) = find(&function.attributes) {
                    return Some(hit);
                }
            }
            ModuleItem::ExternValue { extern_value } => {
                if let Some(hit) = find(&extern_value.attributes) {
                    return Some(hit);
                }
            }
            ModuleItem::ExternType { attributes, .. } => {
                if let Some(hit) = find(attributes) {
                    return Some(hit);
                }
            }
            _ => {}
        }
    }
    None
}

/// Hover markdown for an attribute under the cursor.
fn format_attribute_hover(attribute: &Attribute, span: &Span, content: &str) -> String {
    // The attribute span covers the inner content (`size(0x10)`); re-wrap it as
    // `#[…]` so the hover shows the attribute as written.
    let src = span_text(content, span)
        .map(|s| format!("#[{s}]"))
        .unwrap_or_else(|| render_attribute(attribute));
    let mut md = format!("**attribute**\n\n```pyxis\n{src}\n```\n");
    if let Some(desc) = attribute_description(attribute_name(attribute)) {
        md.push_str(&format!("\n{desc}\n"));
    }
    md
}

/// Render a single attribute as Pyxis source (without code fencing).
fn render_attribute(attribute: &Attribute) -> String {
    match attribute {
        Attribute::Ident { ident, .. } => format!("#[{}]", ident.as_str()),
        Attribute::Function { name, .. } => format!("#[{}(…)]", name.as_str()),
        Attribute::Assign { name, .. } => format!("#[{} = …]", name.as_str()),
        Attribute::Cfg { .. } => "#[cfg(…)]".to_string(),
    }
}

/// Render a function signature as Pyxis source (e.g. `pub fn foo(&mut self, x: u32) -> bool`).
fn render_fn_signature(f: &Function) -> String {
    use pyxis::grammar::Argument;
    let mut s = String::new();
    if matches!(f.visibility, Visibility::Public) {
        s.push_str("pub ");
    }
    s.push_str("fn ");
    s.push_str(f.name.as_str());
    s.push('(');
    let args: Vec<String> = f
        .arguments
        .iter()
        .map(|arg| match arg {
            Argument::ConstSelf { .. } => "&self".to_string(),
            Argument::MutSelf { .. } => "&mut self".to_string(),
            Argument::Named { ident, type_, .. } => format!("{}: {}", ident.as_str(), type_),
        })
        .collect();
    s.push_str(&args.join(", "));
    s.push(')');
    if let Some(ret) = &f.return_type {
        s.push_str(&format!(" -> {ret}"));
    }
    s
}

/// If the cursor is on a named argument of `f`, produce its hover (name, type,
/// type size) scoped to the argument name. The argument *type* is handled by
/// `find_reference_at`, so this only fires on the name.
fn named_arg_hover(
    f: &Function,
    loc: &Location,
    tokens: &[Token],
    type_registry: &TypeRegistry,
    scope: &[ItemPath],
    decl_registry: &DeclarationRegistry,
    pointer_size: usize,
) -> Option<(String, Span)> {
    use pyxis::grammar::Argument;
    for arg in &f.arguments {
        if let Argument::Named {
            ident,
            type_,
            location,
        } = arg
            && location.span.contains(loc)
        {
            let span = name_token_span(tokens, &location.span.start, ident.as_str())
                .unwrap_or(location.span);
            let mut md = format!("**arg** `{}`\n\n", ident.as_str());
            md.push_str(&format!("```pyxis\n{}: {}\n```\n", ident.as_str(), type_));
            if let Some(size) =
                type_size_of(type_, type_registry, scope, decl_registry, pointer_size)
            {
                push_facts(&mut md, &[("type size", fmt_bytes(size))]);
            }
            return Some((md, span));
        }
    }
    None
}

/// The span of a `self`/`&self`/`&mut self` receiver of `f` if the cursor is on
/// it (so a `self` hover can show the containing type, scoped to `self`).
fn self_arg_span(f: &Function, loc: &Location) -> Option<Span> {
    use pyxis::grammar::Argument;
    for arg in &f.arguments {
        match arg {
            Argument::ConstSelf { location } | Argument::MutSelf { location }
                if location.span.contains(loc) =>
            {
                return Some(location.span);
            }
            _ => {}
        }
    }
    None
}

/// Hover markdown for an enum variant / bitflags flag, including its value.
fn format_variant_hover(
    kind: &str,
    name: &str,
    value: Option<i128>,
    attributes: &Attributes,
    doc: &[String],
) -> String {
    let mut md = format!("**{kind}** `{name}`\n");
    if !doc.is_empty() {
        md.push_str(&format!("\n{}\n", doc.join("\n")));
    }
    let attrs = render_attributes(attributes);
    if !attrs.is_empty() {
        md.push_str(&format!("\n**Attributes:** {attrs}\n"));
    }
    if let Some(v) = value {
        let value = if v >= 0 {
            format!("`{v}` (`0x{v:X}`)")
        } else {
            format!("`{v}`")
        };
        push_facts(&mut md, &[("value", value)]);
    }
    md
}

/// Hover markdown for a function (vftable entry or impl method).
fn format_function_hover(f: &Function) -> String {
    let mut md = format!("**fn** `{}`\n\n", f.name.as_str());
    md.push_str(&format!("```pyxis\n{}\n```\n", render_fn_signature(f)));
    let attrs = render_attributes(&f.attributes);
    if !attrs.is_empty() {
        md.push_str(&format!("\n**Attributes:** {attrs}\n"));
    }
    if !f.doc_comments.is_empty() {
        md.push_str(&format!("\n{}\n", f.doc_comments.join("\n")));
    }
    md
}

/// Whether an attribute list contains `#[base]`.
fn has_base_attribute(attrs: &Attributes) -> bool {
    attrs
        .iter()
        .any(|a| matches!(a, Attribute::Ident { ident, .. } if ident.as_str() == "base"))
}

/// The resolved type paths of a type's `#[base]` fields (its base classes).
/// `None` if `path` isn't a declared type; `Some(vec![])` for a type with no
/// bases.
fn base_field_targets(
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

/// A folding range for the brace-delimited body within `span`: from the line of
/// the first `{` (the body opener, kept visible) to the span's closing line.
/// `None` for single-line or brace-less spans.
fn body_fold(tokens: &[Token], span: &Span) -> Option<FoldingRange> {
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

/// Find Markdown cross-reference links in one doc-comment line, returning
/// `(link_start_byte, link_end_byte, path)` for each: `[Foo]`, [`Foo`] and
/// `[label](path::To)`. The span covers the *entire* link (brackets, label, and
/// any `(target)`); the path is what resolves. Non-path targets (e.g. URLs)
/// simply fail to resolve later.
fn scan_doc_links(line: &str) -> Vec<(usize, usize, String)> {
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
        // `[label](path)` → path is the URL and the link extends through `)`;
        // otherwise the bracket content is the path and the link ends at `]`.
        let (path_raw, link_end) = if line[after..].starts_with('(') {
            match line[after + 1..].find(')') {
                Some(rp) => (
                    line[after + 1..after + 1 + rp].to_string(),
                    after + 1 + rp + 1,
                ),
                None => {
                    i = close + 1;
                    continue;
                }
            }
        } else {
            (inner.to_string(), close + 1)
        };
        let path = path_raw.trim().trim_matches('`').trim().to_string();
        if !path.is_empty() {
            out.push((open, link_end, path));
        }
        i = link_end;
    }
    out
}

/// Number of leading path segments `a` and `b` share.
fn common_prefix_len(a: &ItemPath, b: &ItemPath) -> usize {
    a.iter().zip(b.iter()).take_while(|(x, y)| x == y).count()
}

/// Render a set of full import paths as a compact `use`-tree body (the text
/// between `use ` and `;`), grouping shared prefixes: `types::math::Aabb`,
/// `types::math::Vector3` and `types::shared_ptr::WeakPtr` together render as
/// `types::{math::{Aabb, Vector3}, shared_ptr::WeakPtr}`. Paths are sorted and
/// de-duplicated for a stable result.
fn render_use_tree(paths: &[ItemPath]) -> String {
    let mut full: Vec<Vec<String>> = paths
        .iter()
        .map(|p| p.iter().map(|s| s.as_str().to_string()).collect())
        .collect();
    full.sort();
    full.dedup();
    let slices: Vec<&[String]> = full.iter().map(|v| v.as_slice()).collect();
    render_use_group(&slices)
}

fn render_use_group(paths: &[&[String]]) -> String {
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

/// An explicit `#[index(N)]` on a vftable function, if present.
fn explicit_vftable_index(f: &Function) -> Option<usize> {
    f.attributes.iter().find_map(|attr| match attr {
        Attribute::Function { name, items, .. } if name.as_str() == "index" => items
            .exprs()
            .next()
            .and_then(|e| e.int_literal())
            .map(|v| v as usize),
        _ => None,
    })
}

/// The slot index of `target` within a vftable's function list. Indices run
/// sequentially but an `#[index(N)]` resets the running counter (the compiler
/// pads the gap with `_vfunc_*` entries), so this mirrors that assignment.
fn vftable_index_of(fns: &[Function], target: &Function) -> usize {
    let mut idx = 0;
    for func in fns {
        if let Some(n) = explicit_vftable_index(func) {
            idx = n;
        }
        if std::ptr::eq(func, target) {
            return idx;
        }
        idx += 1;
    }
    idx
}

/// Hover for a vftable entry: the function signature plus its slot index and
/// byte offset from the base of the generated vftable struct.
fn format_vftable_fn_hover(f: &Function, index: usize, pointer_size: usize) -> String {
    let mut md = format_function_hover(f);
    push_facts(
        &mut md,
        &[
            ("index", format!("`{index}`")),
            ("vftable offset", fmt_bytes(index * pointer_size)),
        ],
    );
    md
}

/// Hover markdown for a struct field.
fn format_field_hover(
    vis: &Visibility,
    name: &pyxis::grammar::Ident,
    type_: &Type,
    attributes: &Attributes,
    type_size: Option<usize>,
    offset: Option<usize>,
) -> String {
    let vis_str = if matches!(vis, Visibility::Public) {
        "pub "
    } else {
        ""
    };
    // The signature line already names the field, so no separate header.
    let mut md = format!("```pyxis\n{}{}: {}\n```\n", vis_str, name.as_str(), type_);
    let attrs = render_attributes(attributes);
    if !attrs.is_empty() {
        md.push_str(&format!("\n**Attributes:** {attrs}\n"));
    }
    let mut facts = Vec::new();
    if let Some(offset) = offset {
        facts.push(("offset", format!("`0x{offset:X}` ({offset})")));
    }
    if let Some(size) = type_size {
        facts.push(("type size", fmt_bytes(size)));
    }
    push_facts(&mut md, &facts);
    md
}

/// Compute a field's byte offset within its resolved parent type by summing the
/// sizes of preceding layout regions. The resolver inserts explicit padding
/// regions, so the running total is the true offset.
fn field_offset(
    parent_resolved: &pyxis::semantic::types::ItemStateResolved,
    field_name: &str,
    type_registry: &TypeRegistry,
) -> Option<usize> {
    let pyxis::semantic::types::ItemDefinitionInner::Type(td) = &parent_resolved.inner else {
        return None;
    };
    let mut offset = 0usize;
    for region in &td.regions {
        if region.name.as_deref() == Some(field_name) {
            return Some(offset);
        }
        offset += region.size(type_registry)?;
    }
    None
}

/// Render hover markdown for a pointer/array/unknown *shell* type. The caller
/// (via `type_hit_at`) has already determined the cursor is on this exact type's
/// shell, not its inner pointee/element. The output is intentionally identical
/// to the legacy `type_shell_at` so snapshots/tests don't move.
fn shell_hover_md(
    type_: &Type,
    type_registry: &TypeRegistry,
    scope: &[ItemPath],
    decl_registry: &DeclarationRegistry,
    pointer_size: usize,
) -> String {
    match type_ {
        Type::ConstPointer { pointee, .. } => {
            let mut md = format!("**pointer** `{type_}`\n\npoints to `{pointee}` (const)\n");
            push_facts(&mut md, &[("size", fmt_bytes(pointer_size))]);
            md
        }
        Type::MutPointer { pointee, .. } => {
            let mut md = format!("**pointer** `{type_}`\n\npoints to `{pointee}` (mut)\n");
            push_facts(&mut md, &[("size", fmt_bytes(pointer_size))]);
            md
        }
        Type::Array { element, size, .. } => {
            let mut md = format!("**array** `{type_}`\n\n`{size}` × `{element}`\n");
            let mut facts = Vec::new();
            if let Some(s) = type_size_of(type_, type_registry, scope, decl_registry, pointer_size)
            {
                facts.push(("size", fmt_bytes(s)));
            }
            if let Some(a) = type_align_of(type_, type_registry, scope, decl_registry, pointer_size)
            {
                facts.push(("align", fmt_bytes(a)));
            }
            push_facts(&mut md, &facts);
            md
        }
        Type::Unknown { size, .. } => {
            let mut md = format!("**unknown** `{type_}`\n");
            push_facts(&mut md, &[("size", fmt_bytes(*size))]);
            md
        }
        // type_hit_at only yields Shell for pointer/array/unknown.
        Type::Ident { .. } => String::new(),
    }
}

/// Best-effort alignment of a type: pointer → pointer size, array → element
/// alignment, named type → its resolved alignment.
fn type_align_of(
    type_: &Type,
    type_registry: &TypeRegistry,
    scope: &[ItemPath],
    decl_registry: &DeclarationRegistry,
    pointer_size: usize,
) -> Option<usize> {
    match type_ {
        Type::ConstPointer { .. } | Type::MutPointer { .. } => Some(pointer_size),
        Type::Array { element, .. } => {
            type_align_of(element, type_registry, scope, decl_registry, pointer_size)
        }
        Type::Unknown { .. } => None,
        Type::Ident { path, .. } => {
            let resolved = resolve_type_path(path, scope, decl_registry)?;
            type_registry
                .get(&resolved, &ItemLocation::internal())
                .ok()?
                .resolved()
                .map(|r| r.alignment)
        }
    }
}

/// Best-effort size of a field type: pointer → pointer size, array →
/// element × count, `unknown<N>` → N, named type → its resolved size.
fn type_size_of(
    type_: &Type,
    type_registry: &TypeRegistry,
    scope: &[ItemPath],
    decl_registry: &DeclarationRegistry,
    pointer_size: usize,
) -> Option<usize> {
    match type_ {
        Type::ConstPointer { .. } | Type::MutPointer { .. } => Some(pointer_size),
        Type::Array { element, size, .. } => {
            type_size_of(element, type_registry, scope, decl_registry, pointer_size)
                .map(|s| s * size)
        }
        Type::Unknown { size, .. } => Some(*size),
        Type::Ident { path, .. } => {
            let resolved = resolve_type_path(path, scope, decl_registry)?;
            type_registry
                .get(&resolved, &ItemLocation::internal())
                .ok()?
                .resolved()
                .map(|r| r.size)
        }
    }
}
