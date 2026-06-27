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

        // Find the item at the cursor position
        for definition in module.definitions() {
            if definition.location.span.contains(&loc) {
                // Try to get resolved info (size/alignment) via resolve_item.
                // Build the full module-qualified item path (e.g.
                // "world::weather::Weather") from the document's module path.
                let sources = self.sources_for(uri);
                let source_set = semantic::SourceSet::new(&self.db, sources);
                let item_path = match self.module_path_for(uri) {
                    Some(mp) => mp.join(definition.name.as_str().into()),
                    None => ItemPath::from(definition.name.as_str()),
                };
                let resolved = resolve_item(&self.db, source_set, self.pointer_size_for(uri), item_path);
                let resolved_item = resolved.item(&self.db);

                let hover = if let Some(resolved_state) = resolved_item.resolved() {
                    format_type_hover_with_size(&definition, resolved_state.size, resolved_state.alignment)
                } else {
                    format_type_hover(&definition)
                };
                return Response {
                    id: req.id,
                    result: Some(serde_json::to_value(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: hover,
                        }),
                        range: Some(pyxis_span_to_lsp_range(content, &definition.location.span)),
                    }).unwrap()),
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

        // First: check if the cursor is on a definition itself
        for definition in module.definitions() {
            if definition.location.span.contains(&loc)
                || definition.declaration_location.span.contains(&loc)
            {
                let range = pyxis_span_to_lsp_range(content, &definition.declaration_location.span);
                let location = lsp_types::Location {
                    uri: uri.clone(),
                    range,
                };
                return Response {
                    id: req.id,
                    result: Some(serde_json::to_value(lsp_types::GotoDefinitionResponse::Scalar(location)).unwrap()),
                    error: None,
                };
            }
        }

        // Second: check if the cursor is on a type reference (e.g. `Camera`
        // in `pub field: Camera` or `*mut Camera`). Walk the AST to find a
        // Type whose location contains the cursor, resolve it to a full
        // ItemPath via the declaration registry, then look up the definition's
        // declaration_location in the type registry.
        let module_path = self.module_path_for(uri);
        let scope: Vec<ItemPath> = match &module_path {
            Some(mp) => vec![mp.clone()],
            None => vec![],
        };

        // Run analyze to get the type registry with resolved items
        let sources = self.sources_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, sources);
        let analysis = semantic::analyze(&self.db, self.pointer_size_for(uri), source_set);
        let type_registry = analysis.type_registry(&self.db);

        // Get the declaration registry for name resolution
        let decl_set = semantic::collect_declarations(&self.db, source_set, self.pointer_size_for(uri));
        let decl_registry = decl_set.registry(&self.db);

        // Find a type reference at the cursor position
        if let Some(resolved_path) = find_type_reference_at(&module, &loc, &scope, decl_registry) {
            if let Ok(item) = type_registry.get(&resolved_path, &pyxis::span::ItemLocation::internal()) {
                let def_loc = &item.declaration_location;
                if let Some(target_uri) = self.file_id_to_uri(&def_loc.file_id) {
                    let Some(target_content) = self.get_content(&target_uri) else {
                        return Response {
                            id: req.id,
                            result: Some(serde_json::Value::Null),
                            error: None,
                        };
                    };
                    let range = pyxis_span_to_lsp_range(&target_content, &def_loc.span);
                    let location = lsp_types::Location {
                        uri: target_uri,
                        range,
                    };
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
}

/// Format a type definition for hover display with size and alignment
fn format_type_hover_with_size(
    definition: &pyxis::grammar::ItemDefinition,
    size: usize,
    alignment: usize,
) -> String {
    let mut md = format_type_hover(definition);
    md.push_str(&format!("\n**Size:** `0x{:X}` ({}) bytes\n", size, size));
    md.push_str(&format!("**Alignment:** `0x{:X}` ({}) bytes\n", alignment, alignment));
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
                md.push_str(&format!("- {}{}: {}\n", vis_str, name, type_));
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

/// Find a type reference at the given location in a parsed module.
///
/// Walks the grammar AST looking for `Type::Ident` nodes whose span contains
/// the cursor position. When found, resolves the type name through the
/// declaration registry (which handles scope/use-statement resolution) to
/// get the full `ItemPath`.
///
/// Resolution itself comes from the Salsa-computed `DeclarationRegistry` —
/// this function only does the position→node matching, which is an LSP
/// concern (the compiler never works with cursor positions).
fn find_type_reference_at(
    module: &pyxis::grammar::Module,
    loc: &pyxis::span::Location,
    scope: &[ItemPath],
    decl_registry: &pyxis::semantic::declaration_registry::DeclarationRegistry,
) -> Option<ItemPath> {
    for definition in module.definitions() {
        // Walk type definition fields
        if let pyxis::grammar::ItemDefinitionInner::Type(td) = &definition.inner {
            for statement in td.statements() {
                if let pyxis::grammar::TypeField::Field(_, _, type_) = &statement.field {
                    if let Some(path) = find_type_in_grammar_type(type_, loc) {
                        return resolve_type_path(path, scope, decl_registry);
                    }
                }
            }
        }

        // Walk enum/bitflags base types
        if let pyxis::grammar::ItemDefinitionInner::Enum(e) = &definition.inner {
            if let Some(path) = find_type_in_grammar_type(&e.type_, loc) {
                return resolve_type_path(path, scope, decl_registry);
            }
        }
        if let pyxis::grammar::ItemDefinitionInner::Bitflags(b) = &definition.inner {
            if let Some(path) = find_type_in_grammar_type(&b.type_, loc) {
                return resolve_type_path(path, scope, decl_registry);
            }
        }

        // Walk type alias target types
        if let pyxis::grammar::ItemDefinitionInner::TypeAlias(ta) = &definition.inner {
            if let Some(path) = find_type_in_grammar_type(&ta.target, loc) {
                return resolve_type_path(path, scope, decl_registry);
            }
        }

        // Walk vftable function signatures (params + return types)
        if let pyxis::grammar::ItemDefinitionInner::Type(td) = &definition.inner {
            for statement in td.statements() {
                if let pyxis::grammar::TypeField::Vftable(fns) = &statement.field {
                    for sig in fns {
                        for arg in &sig.arguments {
                            if let pyxis::grammar::Argument::Named { type_, .. } = arg {
                                if let Some(path) = find_type_in_grammar_type(type_, loc) {
                                    return resolve_type_path(path, scope, decl_registry);
                                }
                            }
                        }
                        if let Some(ret) = &sig.return_type {
                            if let Some(path) = find_type_in_grammar_type(ret, loc) {
                                return resolve_type_path(path, scope, decl_registry);
                            }
                        }
                    }
                }
            }
        }
    }

    None
}

/// Recursively search a grammar `Type` for an `Ident` whose location contains
/// `loc`. Returns the `ItemPath` of the matching type reference.
fn find_type_in_grammar_type<'a>(
    type_: &'a pyxis::grammar::Type,
    loc: &pyxis::span::Location,
) -> Option<&'a ItemPath> {
    use pyxis::grammar::Type;
    if !type_.location().span.contains(loc) {
        return None;
    }
    match type_ {
        Type::Ident { path, generic_args, .. } => {
            // Check generic args first (they're narrower spans)
            for arg in generic_args {
                if let Some(p) = find_type_in_grammar_type(arg, loc) {
                    return Some(p);
                }
            }
            Some(path)
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
