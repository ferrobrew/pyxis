//! LSP request handlers: hover, definition, completion, symbols, formatting, code lens, inlay hints, rename.

use std::collections::HashMap;

use lsp_server::{Request, Response};
use lsp_types::{
    CodeLens, CompletionItem, CompletionItemKind, DocumentSymbol, DocumentSymbolParams,
    DocumentSymbolResponse, Hover, HoverContents, InlayHint, InlayHintLabel,
    MarkupContent, MarkupKind, Position, Range, SymbolInformation, SymbolKind, Uri,
    TextDocumentPositionParams, TextEdit, WorkspaceEdit, WorkspaceSymbolParams,
};

use pyxis::grammar::{ItemPath, ModuleItem};
use pyxis::semantic;
use pyxis::span::{FileId, HasLocation};

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

        let scope = self.scope_for(uri);
        let sources = self.sources_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, sources);
        let analysis = semantic::analyze(&self.db, self.pointer_size_for(uri), source_set);
        let type_registry = analysis.type_registry(&self.db);
        let decl_set = semantic::collect_declarations(&self.db, source_set, self.pointer_size_for(uri));
        let decl_registry = decl_set.registry(&self.db);

        // 1. Cursor on a type or import reference (e.g. a field's type, or a
        //    segment of a `use`/FQN path) → hover the *referenced* item, not the
        //    enclosing definition. For an intermediate FQN segment, show the
        //    module it names.
        if let Some(reference) = find_reference_at(&module, &loc, &scope, decl_registry, content) {
            let hover = reference
                .item
                .as_ref()
                .and_then(|item_path| self.type_hover_text(item_path, type_registry, decl_registry))
                .or_else(|| {
                    self.module_uri(&reference.module_path)
                        .map(|_| format!("**module** `{}`", reference.module_path))
                });
            if let Some(value) = hover {
                return Response {
                    id: req.id,
                    result: Some(serde_json::to_value(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value,
                        }),
                        range: Some(pyxis_span_to_lsp_range(content, &reference.span)),
                    }).unwrap()),
                    error: None,
                };
            }
        }

        // 2. Structural elements — scoped tightly to what's under the cursor
        //    rather than the whole enclosing definition:
        //    - a definition's own name → the type (size/align/fields);
        //    - a field name → the field (type + attributes + size);
        //    - a vftable entry or an impl method → its signature.
        let pointer_size = self.pointer_size_for(uri);
        for item in &module.items {
            match item {
                pyxis::grammar::ModuleItem::Definition { definition } => {
                    // The definition's own name.
                    if let Some(span) = name_span_after(
                        content,
                        &definition.declaration_location.span.start,
                        definition.name.as_str(),
                    ) {
                        if span.contains(&loc) {
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
                    }
                    // Fields and vftable entries of a type definition.
                    if let pyxis::grammar::ItemDefinitionInner::Type(td) = &definition.inner {
                        for statement in td.statements() {
                            if !statement.location.span.contains(&loc) {
                                continue;
                            }
                            match &statement.field {
                                pyxis::grammar::TypeField::Field(vis, name, type_) => {
                                    let span = name_span_after(
                                        content,
                                        &statement.location.span.start,
                                        name.as_str(),
                                    )
                                    .unwrap_or(statement.location.span);
                                    let size = type_size_of(
                                        type_, type_registry, &scope, decl_registry, pointer_size,
                                    );
                                    // Offset within the parent type's resolved
                                    // layout. The parent is resolved via
                                    // resolve_item (analyze()'s registry leaves
                                    // composite types unresolved).
                                    let parent_path = match self.module_path_for(uri) {
                                        Some(mp) => mp.join(definition.name.as_str().into()),
                                        None => ItemPath::from(definition.name.as_str()),
                                    };
                                    let parent = resolve_item(&self.db, source_set, pointer_size, parent_path);
                                    let offset = parent
                                        .item(&self.db)
                                        .resolved()
                                        .and_then(|rs| field_offset(rs, name.as_str(), type_registry));
                                    let value = format_field_hover(
                                        vis, name, type_, &statement.attributes, size, offset,
                                    );
                                    return hover_response(req.id, value, content, &span);
                                }
                                pyxis::grammar::TypeField::Vftable(fns) => {
                                    for f in fns {
                                        if !f.location.span.contains(&loc) {
                                            continue;
                                        }
                                        // An argument name…
                                        if let Some((value, span)) = named_arg_hover(
                                            f, &loc, content, type_registry, &scope,
                                            decl_registry, pointer_size,
                                        ) {
                                            return hover_response(req.id, value, content, &span);
                                        }
                                        // …`self`, resolving to the owning type…
                                        if let Some(span) = self_arg_span(f, &loc) {
                                            let owner = match self.module_path_for(uri) {
                                                Some(mp) => mp.join(definition.name.as_str().into()),
                                                None => ItemPath::from(definition.name.as_str()),
                                            };
                                            if let Some(value) =
                                                self.type_hover_text(&owner, type_registry, decl_registry)
                                            {
                                                return hover_response(req.id, value, content, &span);
                                            }
                                        }
                                        // …or the function itself.
                                        let span = name_span_after(
                                            content, &f.location.span.start, f.name.as_str(),
                                        )
                                        .unwrap_or(f.location.span);
                                        return hover_response(
                                            req.id, format_function_hover(f), content, &span,
                                        );
                                    }
                                }
                            }
                        }
                    }
                    // Enum / bitflags variants → the variant and its value.
                    if let pyxis::grammar::ItemDefinitionInner::Enum(e) = &definition.inner {
                        for statement in e.statements() {
                            if statement.location.span.contains(&loc) {
                                let span = name_span_after(
                                    content, &statement.location.span.start, statement.name.as_str(),
                                )
                                .unwrap_or(statement.location.span);
                                let value = self
                                    .variant_value(uri, definition, statement.name.as_str(), source_set, pointer_size);
                                let md = format_variant_hover(
                                    "variant", statement.name.as_str(), value,
                                    &statement.attributes, &statement.doc_comments,
                                );
                                return hover_response(req.id, md, content, &span);
                            }
                        }
                    }
                    if let pyxis::grammar::ItemDefinitionInner::Bitflags(b) = &definition.inner {
                        for statement in b.statements() {
                            if statement.location.span.contains(&loc) {
                                let span = name_span_after(
                                    content, &statement.location.span.start, statement.name.as_str(),
                                )
                                .unwrap_or(statement.location.span);
                                let value = self
                                    .variant_value(uri, definition, statement.name.as_str(), source_set, pointer_size);
                                let md = format_variant_hover(
                                    "flag", statement.name.as_str(), value,
                                    &statement.attributes, &statement.doc_comments,
                                );
                                return hover_response(req.id, md, content, &span);
                            }
                        }
                    }
                    // Anywhere else inside the definition's body (blank lines,
                    // the base type) → show the containing type, but keep the
                    // highlight scoped to its name rather than the whole
                    // definition.
                    if definition.location.span.contains(&loc) {
                        if let Some(span) = name_span_after(
                            content,
                            &definition.declaration_location.span.start,
                            definition.name.as_str(),
                        ) {
                            let item_path = match self.module_path_for(uri) {
                                Some(mp) => mp.join(definition.name.as_str().into()),
                                None => ItemPath::from(definition.name.as_str()),
                            };
                            let resolved = resolve_item(&self.db, source_set, pointer_size, item_path);
                            let value = if let Some(rs) = resolved.item(&self.db).resolved() {
                                format_type_hover_with_size(definition, rs.size, rs.alignment)
                            } else {
                                format_type_hover(definition)
                            };
                            return hover_response(req.id, value, content, &span);
                        }
                    }
                }
                // Impl methods (including `#[cfg(...)]`-gated blocks).
                pyxis::grammar::ModuleItem::Impl { impl_block } => {
                    for impl_item in &impl_block.items {
                        if let pyxis::grammar::ImplItem::Function(f) = impl_item {
                            if !f.location.span.contains(&loc) {
                                continue;
                            }
                            // An argument name…
                            if let Some((value, span)) = named_arg_hover(
                                f, &loc, content, type_registry, &scope, decl_registry, pointer_size,
                            ) {
                                return hover_response(req.id, value, content, &span);
                            }
                            // …`self`, resolving to the impl target type…
                            if let Some(span) = self_arg_span(f, &loc) {
                                let owner = ItemPath::from(impl_block.name.as_str());
                                if let Some(resolved) =
                                    resolve_type_path(&owner, &scope, decl_registry)
                                {
                                    if let Some(value) =
                                        self.type_hover_text(&resolved, type_registry, decl_registry)
                                    {
                                        return hover_response(req.id, value, content, &span);
                                    }
                                }
                            }
                            // …or the function itself.
                            let span = name_span_after(
                                content, &f.location.span.start, f.name.as_str(),
                            )
                            .unwrap_or(f.location.span);
                            return hover_response(
                                req.id, format_function_hover(f), content, &span,
                            );
                        }
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

        let scope = self.scope_for(uri);

        // Run analyze to get the type registry with resolved items, and the
        // declaration registry for name resolution.
        let sources = self.sources_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, sources);
        let analysis = semantic::analyze(&self.db, self.pointer_size_for(uri), source_set);
        let type_registry = analysis.type_registry(&self.db);
        let decl_set = semantic::collect_declarations(&self.db, source_set, self.pointer_size_for(uri));
        let decl_registry = decl_set.registry(&self.db);

        // 1. Cursor on a type or import reference (e.g. `Camera` in
        //    `pub field: Camera`, `*mut Camera`, or a name in a `use`
        //    statement). For an FQN like `a::b::C`, the individual segment under
        //    the cursor is resolved: the leaf jumps to the type, earlier
        //    segments to their module's file. This must take priority over the
        //    enclosing-definition check below, since a field's type span is
        //    contained within its parent definition's span.
        if let Some(reference) = find_reference_at(&module, &loc, &scope, decl_registry, content) {
            // a) Concrete item (type/extern/predefined) → jump to its name.
            if let Some(item_path) = &reference.item {
                if let Some(rd) = self.resolved_definition(item_path, type_registry) {
                    if let Some(target_content) = self.get_content(&rd.uri) {
                        let range = pyxis_span_to_lsp_range(target_content, &rd.name_span);
                        let location = lsp_types::Location { uri: rd.uri, range };
                        return Response {
                            id: req.id,
                            result: Some(serde_json::to_value(lsp_types::GotoDefinitionResponse::Scalar(location)).unwrap()),
                            error: None,
                        };
                    }
                }
            }
            // b) Module segment → jump to the top of its file.
            if let Some(target_uri) = self.module_uri(&reference.module_path) {
                let location = lsp_types::Location {
                    uri: target_uri,
                    range: Range {
                        start: Position { line: 0, character: 0 },
                        end: Position { line: 0, character: 0 },
                    },
                };
                return Response {
                    id: req.id,
                    result: Some(serde_json::to_value(lsp_types::GotoDefinitionResponse::Scalar(location)).unwrap()),
                    error: None,
                };
            }
        }

        // 2. Cursor on a definition's own name → jump to itself (scoped to the
        //    name, not the whole declaration).
        for definition in module.definitions() {
            if let Some(span) = name_span_after(
                content,
                &definition.declaration_location.span.start,
                definition.name.as_str(),
            ) {
                if span.contains(&loc) {
                    let range = pyxis_span_to_lsp_range(content, &span);
                    let location = lsp_types::Location { uri: uri.clone(), range };
                    return Response {
                        id: req.id,
                        result: Some(serde_json::to_value(lsp_types::GotoDefinitionResponse::Scalar(location)).unwrap()),
                        error: None,
                    };
                }
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
        let keywords = vec![
            "pub", "type", "enum", "bitflags", "impl", "fn", "extern", "use", "backend",
            "vftable", "const", "mut", "as", "prologue", "epilogue", "self", "Self",
        ];

        let items: Vec<CompletionItem> = keywords
            .iter()
            .map(|kw| CompletionItem {
                label: kw.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            })
            .collect();

        Response {
            id: req.id,
            result: Some(serde_json::to_value(items).unwrap()),
            error: None,
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
            result: Some(
                serde_json::to_value(DocumentSymbolResponse::Nested(symbols)).unwrap(),
            ),
            error: None,
        }
    }

    /// workspace/symbol
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
                let formatted = pyxis::pretty_print::pretty_print(&module);

                // Return a single TextEdit replacing the entire document
                let edit = TextEdit {
                    range: Range {
                        start: Position { line: 0, character: 0 },
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
            if let Ok(item) = type_registry.get(&path, &definition.location) {
                if let Some(resolved) = item.resolved() {
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
                    if let pyxis::grammar::TypeField::Field(_, name, type_) = &statement.field {
                        let path = type_.as_path();
                        if let Some(path) = path {
                            // Use the field type's own location for the
                            // visibility check, not the enclosing definition.
                            let type_loc = type_.location();
                            if let Ok(item) = type_registry.get(path, &type_loc) {
                                if let Some(resolved) = item.resolved() {
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
        }

        Response {
            id: req.id,
            result: Some(serde_json::to_value(hints).unwrap()),
            error: None,
        }
    }

    /// textDocument/rename
    pub fn handle_rename(&self, req: Request) -> Response {
        let params: lsp_types::RenameParams = match serde_json::from_value(req.params.clone()) {
            Ok(p) => p,
            Err(e) => return error_response(req.id, e),
        };

        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
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

        let Some(module) = self.get_parsed_module(uri) else {
            return Response {
                id: req.id,
                result: Some(serde_json::Value::Null),
                error: None,
            };
        };

        let loc = lsp_position_to_pyxis_location(content, params.text_document_position.position);

        // Find the item at the cursor
        let mut target_name: Option<String> = None;
        for definition in module.definitions() {
            if definition.location.span.contains(&loc)
                || definition.declaration_location.span.contains(&loc)
            {
                target_name = Some(definition.name.as_str().to_string());
                break;
            }
        }

        let Some(target_name) = target_name else {
            return Response {
                id: req.id,
                result: Some(serde_json::to_value(WorkspaceEdit::default()).unwrap()),
                error: None,
            };
        };

        // Find all occurrences of the target name in all open documents
        let mut edits: HashMap<Uri, Vec<TextEdit>> = HashMap::new();

        for (doc_uri, doc) in &self.documents {
            let parsed = semantic::parse_file(&self.db, doc.source_file);
            let module = parsed.module(&self.db);
            let content = &doc.content;

            let mut doc_edits = Vec::new();

            // Find the definition
            for definition in module.definitions() {
                if definition.name.as_str() == target_name {
                    // The name follows the keyword; compute its span
                    let name_loc = definition.declaration_location.span;
                    let range = pyxis_span_to_lsp_range(content, &name_loc);
                    doc_edits.push(TextEdit {
                        range,
                        new_text: new_name.clone(),
                    });
                }
            }

            // Find type references in fields
            for definition in module.definitions() {
                if let pyxis::grammar::ItemDefinitionInner::Type(td) = &definition.inner {
                    for statement in td.statements() {
                        if let pyxis::grammar::TypeField::Field(_, _, type_) = &statement.field {
                            if let Some(path) = type_.as_path() {
                                if path.last().map(|s| s.as_str()) == Some(target_name.as_str()) {
                                    let loc = type_.location();
                                    let range = pyxis_span_to_lsp_range(content, &loc.span);
                                    doc_edits.push(TextEdit {
                                        range,
                                        new_text: new_name.clone(),
                                    });
                                }
                            }
                        }
                    }
                }
            }

            if !doc_edits.is_empty() {
                edits.insert(doc_uri.clone(), doc_edits);
            }
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
                if let pyxis::grammar::ModuleItem::Use { tree, .. } = item {
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
        type_registry: &pyxis::semantic::type_registry::TypeRegistry,
    ) -> Option<ResolvedDefinition> {
        let module_path = resolved_path.parent()?;
        let uri = self.module_uri(&module_path)?;
        let content = self.get_content(&uri)?.to_string();
        let module = self.get_parsed_module(&uri)?;
        let target_name = resolved_path.last()?.as_str();
        let def = module
            .definitions()
            .find(|d| d.name.as_str() == target_name)
            .cloned()?;
        let name_span =
            name_span_after(&content, &def.declaration_location.span.start, target_name)
                .unwrap_or(def.location.span);
        let size_align = type_registry
            .get(resolved_path, &pyxis::span::ItemLocation::internal())
            .ok()
            .and_then(|i| i.resolved())
            .map(|r| (r.size, r.alignment));
        Some(ResolvedDefinition { def, uri, name_span, size_align })
    }

    /// Resolved value of an enum variant / bitflags flag named `variant_name`
    /// within `definition` (auto-incremented values are only known post-resolve).
    fn variant_value(
        &self,
        uri: &Uri,
        definition: &pyxis::grammar::ItemDefinition,
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
        type_registry: &pyxis::semantic::type_registry::TypeRegistry,
        decl_registry: &pyxis::semantic::declaration_registry::DeclarationRegistry,
    ) -> Option<String> {
        self.resolved_definition(item_path, type_registry)
            .map(|rd| match rd.size_align {
                Some((size, alignment)) => format_type_hover_with_size(&rd.def, size, alignment),
                None => format_type_hover(&rd.def),
            })
            .or_else(|| builtin_hover(item_path, decl_registry))
    }

    /// Find the document URI whose module path equals `module_path` (used to
    /// navigate to an intermediate module segment of an FQN reference).
    fn module_uri(&self, module_path: &ItemPath) -> Option<Uri> {
        self.documents
            .keys()
            .find(|uri| self.module_path_for(uri).as_ref() == Some(module_path))
            .cloned()
    }
}

/// A type/item definition located in its source file.
struct ResolvedDefinition {
    def: pyxis::grammar::ItemDefinition,
    /// URI of the file the definition lives in.
    uri: Uri,
    /// Span of the definition's name (the go-to-definition target).
    name_span: pyxis::span::Span,
    /// Size/alignment, if the type resolved cleanly.
    size_align: Option<(usize, usize)>,
}

/// Hover for a predefined (`bool`, `u32`, …) or extern type — these have no
/// source definition but a known size/alignment.
fn builtin_hover(
    path: &ItemPath,
    decl_registry: &pyxis::semantic::declaration_registry::DeclarationRegistry,
) -> Option<String> {
    let (kind, size, alignment) = if let Some(p) = decl_registry.get_predefined(path) {
        ("builtin", p.size, p.alignment)
    } else if let Some(e) = decl_registry.get_extern_type(path) {
        ("extern type", e.size, e.alignment)
    } else {
        return None;
    };
    let mut md = format!("**{kind}** `{path}`\n");
    push_facts(&mut md, &[("size", fmt_bytes(size)), ("align", fmt_bytes(alignment))]);
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
    definition: &pyxis::grammar::ItemDefinition,
    size: usize,
    alignment: usize,
) -> String {
    let mut md = format_type_hover(definition);
    push_facts(&mut md, &[("size", fmt_bytes(size)), ("align", fmt_bytes(alignment))]);
    md
}

/// Format a type definition for hover display
fn format_type_hover(definition: &pyxis::grammar::ItemDefinition) -> String {
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
            if let pyxis::grammar::TypeField::Field(vis, name, type_) = &statement.field {
                let vis_str = if matches!(vis, pyxis::grammar::Visibility::Public) {
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
            pyxis::span::Span::synthetic(),
        ),
        ModuleItem::ExternValue { extern_value } => (
            extern_value.name.as_str().to_string(),
            SymbolKind::VARIABLE,
            extern_value.location.span,
        ),
        ModuleItem::Impl { impl_block } => {
            // Impl blocks: use the target type name
            let name = impl_block.name.as_str().to_string();
            (name, SymbolKind::OBJECT, pyxis::span::Span::synthetic())
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
    span: &pyxis::span::Span,
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

/// A type or import reference found under the cursor: the resolved `ItemPath`
/// of the referenced item, plus the source span of the reference itself (used
/// to highlight the hover range).
struct Reference {
    /// Source span of the *segment* under the cursor (for hover-range highlight).
    span: pyxis::span::Span,
    /// Resolved concrete item path, if this segment names a type/extern/predefined.
    item: Option<ItemPath>,
    /// Absolute path of this segment — used as a module target when `item` is
    /// `None` (e.g. the `game::event_handler` part of an FQN reference).
    module_path: ItemPath,
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
    module: &pyxis::grammar::Module,
    loc: &pyxis::span::Location,
    scope: &[ItemPath],
    decl_registry: &pyxis::semantic::declaration_registry::DeclarationRegistry,
    content: &str,
) -> Option<Reference> {
    let resolve = |raw: &ItemPath, full_span: pyxis::span::Span| -> Reference {
        let (sub_path, seg_span) =
            segment_at(raw, &full_span, loc, content).unwrap_or((raw.clone(), full_span));
        let item = resolve_type_path(&sub_path, scope, decl_registry);
        Reference { span: seg_span, item, module_path: sub_path }
    };
    // For `use` trees the segment is already resolved by use_tree_reference, so
    // build the reference directly without re-running segment_at.
    let make_ref = |seg_path: ItemPath, span: pyxis::span::Span| -> Reference {
        let item = resolve_type_path(&seg_path, scope, decl_registry);
        Reference { span, item, module_path: seg_path }
    };

    use pyxis::grammar::{Argument, ImplItem, ModuleItem};

    for item in &module.items {
        match item {
            // `use` statements: cursor on an imported path.
            ModuleItem::Use { tree, .. } => {
                if let Some((path, span)) = use_tree_reference(tree, loc, content) {
                    return Some(make_ref(path, span));
                }
            }
            // `backend` blocks carry their own `use`-style dependency list, and
            // prologue/epilogue splices can be attributed `for <Type>`.
            ModuleItem::Backend { backend } => {
                for tree in &backend.uses {
                    if let Some((path, span)) = use_tree_reference(tree, loc, content) {
                        return Some(make_ref(path, span));
                    }
                }
                for for_type in [&backend.prologue.for_type, &backend.epilogue.for_type]
                    .into_iter()
                    .flatten()
                {
                    if let Some(span) =
                        find_for_path_span(content, &backend.location.span, for_type, loc)
                    {
                        return Some(resolve(for_type, span));
                    }
                }
            }
            // `impl` blocks (including `#[cfg(...)]`-gated ones): the target
            // type name and every function signature's types.
            ModuleItem::Impl { impl_block } => {
                if let Some(span) =
                    name_span_after(content, &impl_block.location.span.start, impl_block.name.as_str())
                {
                    if span.contains(loc) {
                        let raw = ItemPath::from(impl_block.name.as_str());
                        return Some(resolve(&raw, span));
                    }
                }
                for impl_item in &impl_block.items {
                    if let ImplItem::Function(f) = impl_item {
                        for arg in &f.arguments {
                            if let Argument::Named { type_, .. } = arg {
                                if let Some((path, span)) = find_type_in_grammar_type(type_, loc) {
                                    return Some(resolve(path, span));
                                }
                            }
                        }
                        if let Some(ret) = &f.return_type {
                            if let Some((path, span)) = find_type_in_grammar_type(ret, loc) {
                                return Some(resolve(path, span));
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    // Type positions inside definitions.
    for definition in module.definitions() {
        if let Some((path, span)) = find_type_ref_in_definition(definition, loc) {
            return Some(resolve(path, span));
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
    span: &pyxis::span::Span,
    loc: &pyxis::span::Location,
    source: &str,
) -> Option<(ItemPath, pyxis::span::Span)> {
    use pyxis::span::{Location, Span};
    let line = span.start.line;
    if loc.line != line || span.end.line != line {
        return None;
    }
    let line_str = source.lines().nth(line.saturating_sub(1))?;
    let lo = span.start.column.saturating_sub(1);
    let hi = span.end.column.saturating_sub(1).min(line_str.len());
    if lo >= hi {
        return None;
    }
    let text = &line_str[lo..hi];

    let mut col = span.start.column; // 1-indexed byte column of the segment start
    for (i, part) in text.split("::").enumerate() {
        let seg_start = col;
        let seg_end = col + part.len(); // exclusive
        if loc.column >= seg_start && loc.column < seg_end {
            let count = (i + 1).min(path.len());
            let prefix: ItemPath = path.iter().take(count).cloned().collect();
            let seg_span = Span::new(
                Location::new(line, seg_start),
                Location::new(line, seg_end),
            );
            return Some((prefix, seg_span));
        }
        col = seg_end + 2; // skip the "::"
    }
    None
}

/// Search a single definition's type positions for a type reference whose span
/// contains `loc`. Returns the matched (unresolved) `ItemPath` and its span.
fn find_type_ref_in_definition<'a>(
    definition: &'a pyxis::grammar::ItemDefinition,
    loc: &pyxis::span::Location,
) -> Option<(&'a ItemPath, pyxis::span::Span)> {
    use pyxis::grammar::{Argument, ItemDefinitionInner, TypeField};

    match &definition.inner {
        ItemDefinitionInner::Type(td) => {
            for statement in td.statements() {
                match &statement.field {
                    TypeField::Field(_, _, type_) => {
                        if let Some(found) = find_type_in_grammar_type(type_, loc) {
                            return Some(found);
                        }
                    }
                    TypeField::Vftable(fns) => {
                        for sig in fns {
                            for arg in &sig.arguments {
                                if let Argument::Named { type_, .. } = arg {
                                    if let Some(found) = find_type_in_grammar_type(type_, loc) {
                                        return Some(found);
                                    }
                                }
                            }
                            if let Some(ret) = &sig.return_type {
                                if let Some(found) = find_type_in_grammar_type(ret, loc) {
                                    return Some(found);
                                }
                            }
                        }
                    }
                }
            }
        }
        ItemDefinitionInner::Enum(e) => return find_type_in_grammar_type(&e.type_, loc),
        ItemDefinitionInner::Bitflags(b) => return find_type_in_grammar_type(&b.type_, loc),
        ItemDefinitionInner::TypeAlias(ta) => return find_type_in_grammar_type(&ta.target, loc),
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
    tree: &pyxis::grammar::UseTree,
    loc: &pyxis::span::Location,
    content: &str,
) -> Option<(ItemPath, pyxis::span::Span)> {
    use pyxis::grammar::UseTree;
    use pyxis::span::{Location, Span};
    match tree {
        UseTree::Path { path, location } => {
            if !location.span.contains(loc) {
                return None;
            }
            segment_at(path, &location.span, loc, content)
        }
        UseTree::Group { prefix, items, location } => {
            // NB: a group's span starts at `{`, so the shared prefix that
            // precedes it is *outside* this span — don't early-return on it.
            // A leaf import — prepend the group's prefix to the matched segment.
            for item in items {
                if let Some((sub, span)) = use_tree_reference(item, loc, content) {
                    let full: ItemPath = prefix.iter().chain(sub.iter()).cloned().collect();
                    return Some((full, span));
                }
            }
            // The shared prefix (`a::b` in `a::b::{C, D}`). The group's span
            // starts at `{`, so the prefix text precedes it on the same line —
            // find it by scanning back from the brace.
            if !prefix.is_empty() {
                let start = location.span.start;
                if let Some(line) = content.lines().nth(start.line.saturating_sub(1)) {
                    let brace = start.column.saturating_sub(1).min(line.len());
                    let prefix_str = prefix.to_string();
                    if let Some(pos) = line[..brace].rfind(&prefix_str) {
                        let col = pos + 1;
                        let prefix_span = Span::new(
                            Location::new(start.line, col),
                            Location::new(start.line, col + prefix_str.len()),
                        );
                        if prefix_span.contains(loc) {
                            return segment_at(prefix, &prefix_span, loc, content);
                        }
                    }
                }
            }
            None
        }
    }
}

/// Recursively search a grammar `Type` for an `Ident` whose location contains
/// `loc`. Returns the `ItemPath` of the matching type reference and its span.
fn find_type_in_grammar_type<'a>(
    type_: &'a pyxis::grammar::Type,
    loc: &pyxis::span::Location,
) -> Option<(&'a ItemPath, pyxis::span::Span)> {
    use pyxis::grammar::Type;
    if !type_.location().span.contains(loc) {
        return None;
    }
    match type_ {
        Type::Ident { path, generic_args, .. } => {
            // Check generic args first (they're narrower spans)
            for arg in generic_args {
                if let Some(found) = find_type_in_grammar_type(arg, loc) {
                    return Some(found);
                }
            }
            Some((path, type_.location().span))
        }
        Type::ConstPointer { pointee, .. } | Type::MutPointer { pointee, .. } => {
            find_type_in_grammar_type(pointee, loc)
        }
        Type::Array { element, .. } => find_type_in_grammar_type(element, loc),
        Type::Unknown { .. } => None,
    }
}

/// Resolve a type path (from the grammar AST) to a full `ItemPath` using the
/// declaration registry's scope resolution (handles use-statements, relative
/// paths, and absolute paths). This is the Salsa-computed resolution — we're
/// just calling it, not reimplementing it.
fn resolve_type_path(
    path: &ItemPath,
    scope: &[ItemPath],
    decl_registry: &pyxis::semantic::declaration_registry::DeclarationRegistry,
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

fn is_ident_byte(b: u8) -> bool {
    b == b'_' || b.is_ascii_alphanumeric()
}

/// Find the span of the identifier `name` at or after `from` in `source`.
///
/// Grammar `Ident`s don't carry their own span, so name spans (type names,
/// field names, function names) are recovered by searching forward from the
/// declaration's start point for the whole-word occurrence of the name.
fn name_span_after(
    source: &str,
    from: &pyxis::span::Location,
    name: &str,
) -> Option<pyxis::span::Span> {
    use pyxis::span::{Location, Span};
    if name.is_empty() {
        return None;
    }
    let lines: Vec<&str> = source.lines().collect();
    let start_line = from.line.saturating_sub(1);
    for (li, line) in lines.iter().enumerate().skip(start_line) {
        let from_byte = if li == start_line {
            from.column.saturating_sub(1)
        } else {
            0
        };
        if from_byte > line.len() {
            continue;
        }
        let bytes = line.as_bytes();
        let mut search = from_byte;
        while let Some(rel) = line[search..].find(name) {
            let abs = search + rel;
            let before_ok = abs == 0 || !is_ident_byte(bytes[abs - 1]);
            let after = abs + name.len();
            let after_ok = after >= bytes.len() || !is_ident_byte(bytes[after]);
            if before_ok && after_ok {
                let col = abs + 1; // 1-indexed byte column
                return Some(Span::new(
                    Location::new(li + 1, col),
                    Location::new(li + 1, col + name.len()),
                ));
            }
            search = abs + 1;
        }
    }
    None
}

/// Find the span of a `for <path>` clause (in a backend prologue/epilogue)
/// whose path matches `path` and whose span contains `loc`. The `for_type`
/// clause has no recorded span, so we recover it by scanning the backend
/// block's source for `for <path>`.
fn find_for_path_span(
    content: &str,
    block_span: &pyxis::span::Span,
    path: &ItemPath,
    loc: &pyxis::span::Location,
) -> Option<pyxis::span::Span> {
    use pyxis::span::{Location, Span};
    let path_str = path.to_string();
    let lines: Vec<&str> = content.lines().collect();
    let lo = block_span.start.line.saturating_sub(1);
    let hi = block_span.end.line.saturating_sub(1).min(lines.len().saturating_sub(1));
    for li in lo..=hi {
        let line = lines[li];
        let bytes = line.as_bytes();
        let mut search = 0;
        while let Some(rel) = line[search..].find("for ") {
            let after = search + rel + 4; // first byte of the path after "for "
            if line[after..].starts_with(&path_str) {
                let end = after + path_str.len();
                let boundary_ok = bytes.get(end).is_none_or(|&b| !is_ident_byte(b) && b != b':');
                if boundary_ok {
                    let span = Span::new(
                        Location::new(li + 1, after + 1),
                        Location::new(li + 1, end + 1),
                    );
                    if span.contains(loc) {
                        return Some(span);
                    }
                }
            }
            search = after;
        }
    }
    None
}

/// Render a definition/field's attributes compactly (e.g. `#[base] #[cfg(...)]`).
fn render_attributes(attributes: &pyxis::grammar::Attributes) -> String {
    attributes
        .0
        .iter()
        .map(|a| format!("`{}`", render_attribute(a)))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Render a single attribute as Pyxis source (without code fencing).
fn render_attribute(attribute: &pyxis::grammar::Attribute) -> String {
    use pyxis::grammar::Attribute;
    match attribute {
        Attribute::Ident { ident, .. } => format!("#[{}]", ident.as_str()),
        Attribute::Function { name, .. } => format!("#[{}(…)]", name.as_str()),
        Attribute::Assign { name, .. } => format!("#[{} = …]", name.as_str()),
        Attribute::Cfg { .. } => "#[cfg(…)]".to_string(),
    }
}

/// Render a function signature as Pyxis source (e.g. `pub fn foo(&mut self, x: u32) -> bool`).
fn render_fn_signature(f: &pyxis::grammar::Function) -> String {
    use pyxis::grammar::{Argument, Visibility};
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
    f: &pyxis::grammar::Function,
    loc: &pyxis::span::Location,
    content: &str,
    type_registry: &pyxis::semantic::type_registry::TypeRegistry,
    scope: &[ItemPath],
    decl_registry: &pyxis::semantic::declaration_registry::DeclarationRegistry,
    pointer_size: usize,
) -> Option<(String, pyxis::span::Span)> {
    use pyxis::grammar::Argument;
    for arg in &f.arguments {
        if let Argument::Named { ident, type_, location } = arg {
            if location.span.contains(loc) {
                let span = name_span_after(content, &location.span.start, ident.as_str())
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
    }
    None
}

/// The span of a `self`/`&self`/`&mut self` receiver of `f` if the cursor is on
/// it (so a `self` hover can show the containing type, scoped to `self`).
fn self_arg_span(
    f: &pyxis::grammar::Function,
    loc: &pyxis::span::Location,
) -> Option<pyxis::span::Span> {
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
    attributes: &pyxis::grammar::Attributes,
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
fn format_function_hover(f: &pyxis::grammar::Function) -> String {
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

/// Hover markdown for a struct field.
fn format_field_hover(
    vis: &pyxis::grammar::Visibility,
    name: &pyxis::grammar::Ident,
    type_: &pyxis::grammar::Type,
    attributes: &pyxis::grammar::Attributes,
    type_size: Option<usize>,
    offset: Option<usize>,
) -> String {
    let vis_str = if matches!(vis, pyxis::grammar::Visibility::Public) {
        "pub "
    } else {
        ""
    };
    let mut md = format!("**field** `{}`\n\n", name.as_str());
    md.push_str(&format!("```pyxis\n{}{}: {}\n```\n", vis_str, name.as_str(), type_));
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
    type_registry: &pyxis::semantic::type_registry::TypeRegistry,
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

/// Best-effort size of a field type: pointer → pointer size, array →
/// element × count, `unknown<N>` → N, named type → its resolved size.
fn type_size_of(
    type_: &pyxis::grammar::Type,
    type_registry: &pyxis::semantic::type_registry::TypeRegistry,
    scope: &[ItemPath],
    decl_registry: &pyxis::semantic::declaration_registry::DeclarationRegistry,
    pointer_size: usize,
) -> Option<usize> {
    use pyxis::grammar::Type;
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
                .get(&resolved, &pyxis::span::ItemLocation::internal())
                .ok()?
                .resolved()
                .map(|r| r.size)
        }
    }
}
