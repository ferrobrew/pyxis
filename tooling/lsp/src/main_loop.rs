//! LSP main loop — synchronous crossbeam-channel based.
//!
//! Follows rust-analyzer's main_loop pattern: a synchronous loop that
//! multiplexes between incoming LSP messages and a debounce timer for
//! diagnostics.

use std::thread;

use lsp_server::{Connection, ExtractError, Message, Notification, Request, Response};
use lsp_types::{
    CodeLensOptions, CompletionOptions, HoverProviderCapability, InitializeResult, OneOf,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};

use crate::state::ServerState;

/// Run the LSP server on stdio.
pub fn run() -> Result<(), Box<dyn std::error::Error>> {
    let (connection, io_threads) = Connection::stdio();
    run_with_connection(connection)?;
    io_threads.join()?;
    Ok(())
}

/// Run the LSP server with a given connection (for testing).
pub fn run_with_connection(connection: Connection) -> Result<(), Box<dyn std::error::Error>> {
    // Build server capabilities
    let server_capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions::default()),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        code_lens_provider: Some(CodeLensOptions {
            resolve_provider: Some(false),
        }),
        inlay_hint_provider: Some(OneOf::Left(true)),
        rename_provider: Some(OneOf::Left(true)),
        ..Default::default()
    };

    let initialize_result = InitializeResult {
        capabilities: server_capabilities,
        server_info: Some(lsp_types::ServerInfo {
            name: "pyxis-lsp".to_string(),
            version: Some("0.1.0".to_string()),
        }),
    };

    let initialize_value = serde_json::to_value(initialize_result)?;

    // Wait for the `initialize` request
    let _initialize_id = connection.initialize(initialize_value)?;

    // Create the server state — we don't need workspace info for now
    let mut state = ServerState::new();

    // Main loop: process messages until shutdown
    main_loop(&connection, &mut state)?;

    Ok(())
}

fn main_loop(
    connection: &Connection,
    state: &mut ServerState,
) -> Result<(), Box<dyn std::error::Error>> {
    while let Ok(msg) = connection.receiver.recv() {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                handle_request(connection, state, req)?;
            }
            Message::Response(_) => {
                // We don't send requests to the client, so ignore responses
            }
            Message::Notification(notif) => {
                handle_notification(connection, state, notif)?;
            }
        }
    }
    Ok(())
}

fn handle_request(
    connection: &Connection,
    state: &mut ServerState,
    req: Request,
) -> Result<(), Box<dyn std::error::Error>> {
    // Dispatch to handlers
    let response = match req.method.as_str() {
        "textDocument/hover" => state.handle_hover(req),
        "textDocument/definition" => state.handle_definition(req),
        "textDocument/completion" => state.handle_completion(req),
        "textDocument/documentSymbol" => state.handle_document_symbols(req),
        "workspace/symbol" => state.handle_workspace_symbols(req),
        "textDocument/formatting" => state.handle_formatting(req),
        "textDocument/codeLens" => state.handle_code_lens(req),
        "textDocument/inlayHint" => state.handle_inlay_hints(req),
        "textDocument/rename" => state.handle_rename(req),
        _ => Response {
            id: req.id,
            result: Some(serde_json::Value::Null),
            error: None,
        },
    };
    connection.sender.send(Message::Response(response))?;
    Ok(())
}

fn handle_notification(
    connection: &Connection,
    state: &mut ServerState,
    notif: Notification,
) -> Result<(), Box<dyn std::error::Error>> {
    match notif.method.as_str() {
        "textDocument/didOpen" => {
            state.handle_did_open(notif)?;
            publish_diagnostics(connection, state)?;
        }
        "textDocument/didChange" => {
            state.handle_did_change(notif)?;
            publish_diagnostics(connection, state)?;
        }
        "textDocument/didSave" => {
            state.handle_did_save(notif)?;
            publish_diagnostics(connection, state)?;
        }
        "textDocument/didClose" => {
            state.handle_did_close(notif)?;
        }
        _ => {}
    }
    Ok(())
}

fn publish_diagnostics(
    connection: &Connection,
    state: &ServerState,
) -> Result<(), Box<dyn std::error::Error>> {
    for notif in state.collect_diagnostics() {
        connection.sender.send(Message::Notification(notif))?;
    }
    Ok(())
}
