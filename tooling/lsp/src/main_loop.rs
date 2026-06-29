//! LSP main loop — synchronous crossbeam-channel based.
//!
//! Follows rust-analyzer's main_loop pattern: a synchronous loop that
//! multiplexes between incoming LSP messages and a debounce timer for
//! diagnostics.

use std::{
    panic::AssertUnwindSafe,
    time::{Duration, Instant},
};

use crossbeam_channel::{after, never, select};
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, Response, ResponseError};
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
        implementation_provider: Some(lsp_types::ImplementationProviderCapability::Simple(true)),
        completion_provider: Some(CompletionOptions::default()),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        document_highlight_provider: Some(OneOf::Left(true)),
        code_action_provider: Some(lsp_types::CodeActionProviderCapability::Simple(true)),
        semantic_tokens_provider: Some(
            lsp_types::SemanticTokensServerCapabilities::SemanticTokensOptions(
                lsp_types::SemanticTokensOptions {
                    legend: lsp_types::SemanticTokensLegend {
                        // Order must match the indices emitted in semantic_tokens().
                        token_types: vec![
                            lsp_types::SemanticTokenType::NAMESPACE,
                            lsp_types::SemanticTokenType::TYPE,
                        ],
                        token_modifiers: vec![lsp_types::SemanticTokenModifier::DEFAULT_LIBRARY],
                    },
                    full: Some(lsp_types::SemanticTokensFullOptions::Bool(true)),
                    range: Some(false),
                    work_done_progress_options: Default::default(),
                },
            ),
        ),
        document_link_provider: Some(lsp_types::DocumentLinkOptions {
            resolve_provider: Some(false),
            work_done_progress_options: Default::default(),
        }),
        document_formatting_provider: Some(OneOf::Left(true)),
        folding_range_provider: Some(lsp_types::FoldingRangeProviderCapability::Simple(true)),
        code_lens_provider: Some(CodeLensOptions {
            resolve_provider: Some(false),
        }),
        inlay_hint_provider: Some(OneOf::Left(true)),
        rename_provider: Some(OneOf::Right(lsp_types::RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: Default::default(),
        })),
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

    // Dynamic registrations: file watchers (so on-disk edits reach us) and type
    // hierarchy (which lsp_types has no static ServerCapabilities field for).
    // Only registered if the client advertised dynamic-registration support.
    let client_capabilities: lsp_types::ClientCapabilities = initialize_params
        .get("capabilities")
        .cloned()
        .and_then(|c| serde_json::from_value(c).ok())
        .unwrap_or_default();
    register_dynamic_capabilities(&connection, &client_capabilities)?;

    let mut state = ServerState::new(&initialize_params)?;
    main_loop(&connection, &mut state)?;

    Ok(())
}

fn main_loop(
    connection: &Connection,
    state: &mut ServerState,
) -> Result<(), Box<dyn std::error::Error>> {
    // Deadline at which debounced diagnostics should be published, anchored to a
    // fixed Instant set when a didChange arrives. Computing the timeout from this
    // stored Instant (rather than a fresh `after(DEBOUNCE_MS)` every iteration)
    // means unrelated messages — hover, inlayHint, codeLens — no longer restart
    // the window and delay the post-edit publish past DEBOUNCE_MS.
    let mut deadline: Option<Instant> = None;

    loop {
        // Recompute the remaining time from the stored deadline each iteration,
        // so non-change messages don't push it out.
        let timeout = match deadline {
            Some(at) => after(at.saturating_duration_since(Instant::now())),
            None => never(),
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
                                deadline = None;
                            }
                            NotificationAction::DebounceDiagnostics => {
                                // Trailing-edge debounce: publish DEBOUNCE_MS after
                                // the latest change, so reset the deadline on each
                                // change (but always relative to a stored Instant).
                                deadline =
                                    Some(Instant::now() + Duration::from_millis(DEBOUNCE_MS));
                            }
                            NotificationAction::None => {}
                        }
                    }
                }
            }
            recv(timeout) -> _ => {
                // Debounce deadline reached — publish diagnostics now
                publish_diagnostics(connection, state)?;
                deadline = None;
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
    // Preserve the id/method before dispatch so a panicking handler can still be
    // answered. A handler panic must not unwind through main_loop and kill the
    // server, and every request must still get exactly one response.
    let id = req.id.clone();
    let method = req.method.clone();

    let response = match std::panic::catch_unwind(AssertUnwindSafe(|| dispatch_request(state, req)))
    {
        Ok(response) => response,
        Err(panic) => {
            let detail = panic_message(panic.as_ref());
            eprintln!("pyxis-lsp: request handler for `{method}` panicked: {detail}");
            Response {
                id,
                result: None,
                error: Some(ResponseError {
                    code: ErrorCode::InternalError as i32,
                    message: format!("internal error handling `{method}`: {detail}"),
                    data: None,
                }),
            }
        }
    };
    connection.sender.send(Message::Response(response))?;
    Ok(())
}

/// Dispatch a request to its handler. Factored out of [`handle_request`] so the
/// whole dispatch can run inside `catch_unwind`.
fn dispatch_request(state: &mut ServerState, req: Request) -> Response {
    match req.method.as_str() {
        "textDocument/hover" => state.handle_hover(req),
        "textDocument/definition" => state.handle_definition(req),
        "textDocument/implementation" => state.handle_implementation(req),
        "textDocument/references" => state.handle_references(req),
        "textDocument/documentHighlight" => state.handle_document_highlight(req),
        "textDocument/documentLink" => state.handle_document_link(req),
        "textDocument/semanticTokens/full" => state.handle_semantic_tokens_full(req),
        "textDocument/foldingRange" => state.handle_folding_range(req),
        "textDocument/prepareTypeHierarchy" => state.handle_prepare_type_hierarchy(req),
        "typeHierarchy/supertypes" => state.handle_type_hierarchy_supertypes(req),
        "typeHierarchy/subtypes" => state.handle_type_hierarchy_subtypes(req),
        "textDocument/codeAction" => state.handle_code_action(req),
        "textDocument/completion" => state.handle_completion(req),
        "textDocument/documentSymbol" => state.handle_document_symbols(req),
        "workspace/symbol" => state.handle_workspace_symbols(req),
        "textDocument/formatting" => state.handle_formatting(req),
        "textDocument/codeLens" => state.handle_code_lens(req),
        "textDocument/inlayHint" => state.handle_inlay_hints(req),
        "textDocument/prepareRename" => state.handle_prepare_rename(req),
        "textDocument/rename" => state.handle_rename(req),
        _ => Response {
            id: req.id,
            result: Some(serde_json::Value::Null),
            error: None,
        },
    }
}

/// Extract a human-readable message from a `catch_unwind` panic payload.
fn panic_message(panic: &(dyn std::any::Any + Send)) -> String {
    if let Some(s) = panic.downcast_ref::<&str>() {
        (*s).to_string()
    } else if let Some(s) = panic.downcast_ref::<String>() {
        s.clone()
    } else {
        "unknown panic".to_string()
    }
}

/// Register dynamic capabilities: file watchers for `.pyxis`/`pyxis.toml` (so
/// on-disk edits arrive as `workspace/didChangeWatchedFiles`) and type
/// hierarchy (no static `ServerCapabilities` field exists for it in lsp_types).
fn register_dynamic_capabilities(
    connection: &Connection,
    caps: &lsp_types::ClientCapabilities,
) -> Result<(), Box<dyn std::error::Error>> {
    use lsp_types::{
        DidChangeWatchedFilesRegistrationOptions, FileSystemWatcher, GlobPattern, Registration,
        RegistrationParams, TextDocumentRegistrationOptions, TypeHierarchyRegistrationOptions,
    };

    let mut registrations = Vec::new();

    let watch_supported = caps
        .workspace
        .as_ref()
        .and_then(|w| w.did_change_watched_files.as_ref())
        .and_then(|d| d.dynamic_registration)
        .unwrap_or(false);
    if watch_supported {
        let watcher = |glob: &str| FileSystemWatcher {
            glob_pattern: GlobPattern::String(glob.to_string()),
            kind: None,
        };
        registrations.push(Registration {
            id: "pyxis-watch-files".to_string(),
            method: "workspace/didChangeWatchedFiles".to_string(),
            register_options: Some(serde_json::to_value(
                DidChangeWatchedFilesRegistrationOptions {
                    watchers: vec![watcher("**/*.pyxis"), watcher("**/pyxis.toml")],
                },
            )?),
        });
    }

    let type_hierarchy_supported = caps
        .text_document
        .as_ref()
        .and_then(|t| t.type_hierarchy.as_ref())
        .and_then(|d| d.dynamic_registration)
        .unwrap_or(false);
    if type_hierarchy_supported {
        registrations.push(Registration {
            id: "pyxis-type-hierarchy".to_string(),
            method: "textDocument/prepareTypeHierarchy".to_string(),
            register_options: Some(serde_json::to_value(TypeHierarchyRegistrationOptions {
                text_document_registration_options: TextDocumentRegistrationOptions {
                    document_selector: None,
                },
                type_hierarchy_options: Default::default(),
                static_registration_options: Default::default(),
            })?),
        });
    }

    if registrations.is_empty() {
        return Ok(());
    }
    let req = Request::new(
        lsp_server::RequestId::from("pyxis/registerCapabilities".to_string()),
        "client/registerCapability".to_string(),
        RegistrationParams { registrations },
    );
    connection.sender.send(Message::Request(req))?;
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
        "workspace/didChangeWatchedFiles" => {
            state.handle_did_change_watched_files(notif)?;
            // Re-publish for all tracked files so cross-file effects show up.
            publish_diagnostics(connection, state)?;
            NotificationAction::DiagnosticsPublished
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
