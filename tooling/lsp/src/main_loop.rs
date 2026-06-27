//! LSP main loop — synchronous crossbeam-channel based.
//!
//! Follows rust-analyzer's main_loop pattern: a synchronous loop that
//! multiplexes between incoming LSP messages and a debounce timer for
//! diagnostics.

use std::time::Duration;

use crossbeam_channel::{after, never, select};
use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{
    CodeLensOptions, CompletionOptions, HoverProviderCapability, InitializeResult, OneOf,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};

use crate::state::ServerState;

/// Debounce window for diagnostics (ms). After a didChange, diagnostics
/// are not published until this many ms pass without further changes.
const DEBOUNCE_MS: u64 = 500;

/// Run the LSP server on stdio.
pub fn run() -> Result<(), Box<dyn std::error::Error>> {
    let (connection, io_threads) = Connection::stdio();
    run_with_connection(connection)?;
    io_threads.join()?;
    Ok(())
}

/// Run the LSP server with a given connection (for testing).
pub fn run_with_connection(connection: Connection) -> Result<(), Box<dyn std::error::Error>> {
    let server_capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            lsp_types::TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                will_save: None,
                will_save_wait_until: None,
                // We ignore the save payload (didChange keeps content in sync),
                // so don't ask the client to send the full text on every save.
                save: Some(lsp_types::TextDocumentSyncSaveOptions::SaveOptions(
                    lsp_types::SaveOptions {
                        include_text: Some(false),
                    },
                )),
            },
        )),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions::default()),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        document_highlight_provider: Some(OneOf::Left(true)),
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

    // NOTE: do NOT use `connection.initialize()` here — it wraps its argument
    // in `{ "capabilities": <arg> }`, so passing a full InitializeResult would
    // double-nest the capabilities (`result.capabilities.capabilities`) and the
    // client would see the server as having no capabilities at all. Drive the
    // handshake manually so the InitializeResult (incl. serverInfo) is sent verbatim.
    let initialize_value = serde_json::to_value(initialize_result)?;
    let (initialize_id, initialize_params) = connection.initialize_start()?;
    connection.initialize_finish(initialize_id, initialize_value)?;

    let mut state = ServerState::new(&initialize_params)?;
    main_loop(&connection, &mut state)?;

    Ok(())
}

fn main_loop(
    connection: &Connection,
    state: &mut ServerState,
) -> Result<(), Box<dyn std::error::Error>> {
    // Whether there's a pending diagnostics request after a didChange.
    let mut pending_diagnostics = false;

    loop {
        let timeout = if pending_diagnostics {
            after(Duration::from_millis(DEBOUNCE_MS))
        } else {
            never()
        };

        select! {
            recv(connection.receiver) -> msg => {
                let Ok(msg) = msg else {
                    // Channel closed — client disconnected
                    break;
                };
                match msg {
                    Message::Request(req) => {
                        if connection.handle_shutdown(&req)? {
                            return Ok(());
                        }
                        handle_request(connection, state, req)?;
                    }
                    Message::Response(_) => {}
                    Message::Notification(notif) => {
                        let action = handle_notification(connection, state, notif)?;
                        match action {
                            NotificationAction::DiagnosticsPublished => {
                                pending_diagnostics = false;
                            }
                            NotificationAction::DebounceDiagnostics => {
                                pending_diagnostics = true;
                            }
                            NotificationAction::None => {}
                        }
                    }
                }
            }
            recv(timeout) -> _ => {
                // Debounce timer fired — publish diagnostics now
                publish_diagnostics(connection, state)?;
                pending_diagnostics = false;
            }
        }
    }
    Ok(())
}

/// What the main loop should do after handling a notification.
enum NotificationAction {
    /// Diagnostics were already published (didOpen, didSave).
    DiagnosticsPublished,
    /// Diagnostics should be debounced (didChange).
    DebounceDiagnostics,
    /// No diagnostic action needed (didClose, unknown).
    None,
}

fn handle_request(
    connection: &Connection,
    state: &mut ServerState,
    req: Request,
) -> Result<(), Box<dyn std::error::Error>> {
    let response = match req.method.as_str() {
        "textDocument/hover" => state.handle_hover(req),
        "textDocument/definition" => state.handle_definition(req),
        "textDocument/references" => state.handle_references(req),
        "textDocument/documentHighlight" => state.handle_document_highlight(req),
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
) -> Result<NotificationAction, Box<dyn std::error::Error>> {
    let action = match notif.method.as_str() {
        "textDocument/didOpen" => {
            state.handle_did_open(notif)?;
            publish_diagnostics(connection, state)?;
            NotificationAction::DiagnosticsPublished
        }
        "textDocument/didChange" => {
            state.handle_did_change(notif)?;
            // Debounce: don't publish immediately, wait for more changes
            NotificationAction::DebounceDiagnostics
        }
        "textDocument/didSave" => {
            state.handle_did_save(notif)?;
            // Immediate: bypass debounce
            publish_diagnostics(connection, state)?;
            NotificationAction::DiagnosticsPublished
        }
        "textDocument/didClose" => {
            state.handle_did_close(notif)?;
            NotificationAction::None
        }
        _ => NotificationAction::None,
    };
    Ok(action)
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
