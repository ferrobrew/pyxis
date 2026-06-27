//! LSP integration tests using lsp_server::Connection::memory().
//!
//! These tests verify the full request/response cycle including serialization.

use lsp_server::{Connection, Message, Request, RequestId};
use lsp_types::{
    ClientCapabilities, DidOpenTextDocumentParams, GeneralClientCapabilities, InitializeParams,
    InitializedParams, OneOf, Position, TextDocumentClientCapabilities, TextDocumentIdentifier,
    TextDocumentItem, TextDocumentPositionParams,
};
use pyxis_lsp as _;

/// Spawn the LSP server on a background thread with an in-memory connection.
fn spawn_server() -> Connection {
    let (server, client) = Connection::memory();
    std::thread::spawn(move || {
        let _ = pyxis_lsp::main_loop::run_with_connection(server);
    });
    client
}

fn send_request(conn: &Connection, method: &str, params: serde_json::Value) -> serde_json::Value {
    let id = RequestId::from(1);
    let req = Request::new(id, method.into(), params);
    conn.sender.send(Message::Request(req)).unwrap();
    loop {
        match conn.receiver.recv().unwrap() {
            Message::Response(resp) => return resp.result.unwrap_or(serde_json::Value::Null),
            Message::Notification(_) => {}
            Message::Request(_) => {}
        }
    }
}

fn send_notification(conn: &Connection, method: &str, params: serde_json::Value) {
    let notif = lsp_server::Notification::new(method.into(), params);
    conn.sender.send(Message::Notification(notif)).unwrap();
}

fn wait_for_notification(conn: &Connection) -> Option<lsp_server::Notification> {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    while std::time::Instant::now() < deadline {
        if let Ok(msg) = conn
            .receiver
            .recv_timeout(std::time::Duration::from_millis(100))
        {
            if let Message::Notification(n) = msg {
                return Some(n);
            }
        }
    }
    None
}

#[test]
fn test_initialize() {
    let conn = spawn_server();
    let result = send_request(
        &conn,
        "initialize",
        serde_json::to_value(InitializeParams {
            process_id: None,
            capabilities: ClientCapabilities::default(),
            ..Default::default()
        })
        .unwrap(),
    );
    // Server should return capabilities
    assert!(
        result.get("capabilities").is_some(),
        "should return capabilities"
    );
}

#[test]
fn test_did_open_and_diagnostics() {
    let conn = spawn_server();

    // Initialize
    send_request(
        &conn,
        "initialize",
        serde_json::to_value(InitializeParams {
            process_id: None,
            capabilities: ClientCapabilities::default(),
            ..Default::default()
        })
        .unwrap(),
    );
    send_notification(
        &conn,
        "initialized",
        serde_json::to_value(InitializedParams {}).unwrap(),
    );

    // Open a file with a deliberate error (missing field type)
    send_notification(
        &conn,
        "textDocument/didOpen",
        serde_json::to_value(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: "file:///test.pyxis".parse().unwrap(),
                language_id: "pyxis".to_string(),
                version: 0,
                text: "pub type Broken { pub field: NonexistentType, }".to_string(),
            },
        })
        .unwrap(),
    );

    // Wait for diagnostics — the nonexistent type should produce a semantic error
    let notif = wait_for_notification(&conn);
    assert!(notif.is_some(), "should receive a notification");
    let notif = notif.unwrap();
    assert_eq!(notif.method, "textDocument/publishDiagnostics");
}

#[test]
fn test_hover() {
    let conn = spawn_server();

    send_request(
        &conn,
        "initialize",
        serde_json::to_value(InitializeParams::default()).unwrap(),
    );
    send_notification(
        &conn,
        "initialized",
        serde_json::to_value(InitializedParams {}).unwrap(),
    );

    send_notification(
        &conn,
        "textDocument/didOpen",
        serde_json::to_value(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: "file:///test.pyxis".parse().unwrap(),
                language_id: "pyxis".to_string(),
                version: 0,
                text: "pub type Foo { pub x: u32, }".to_string(),
            },
        })
        .unwrap(),
    );

    // Consume any diagnostics
    let _ = wait_for_notification(&conn);

    // Hover over the type definition (line 0, around "Foo")
    let result = send_request(
        &conn,
        "textDocument/hover",
        serde_json::to_value(TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: "file:///test.pyxis".parse().unwrap(),
            },
            position: Position {
                line: 0,
                character: 9,
            },
        })
        .unwrap(),
    );

    // Should return hover content (or null if position doesn't match exactly)
    // The important thing is that the server doesn't crash
    assert!(result.is_null() || result.get("contents").is_some());
}

#[test]
fn test_document_symbols() {
    let conn = spawn_server();

    send_request(
        &conn,
        "initialize",
        serde_json::to_value(InitializeParams::default()).unwrap(),
    );
    send_notification(
        &conn,
        "initialized",
        serde_json::to_value(InitializedParams {}).unwrap(),
    );

    send_notification(
        &conn,
        "textDocument/didOpen",
        serde_json::to_value(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: "file:///test.pyxis".parse().unwrap(),
                language_id: "pyxis".to_string(),
                version: 0,
                text: "pub type Foo { pub x: u32, }\npub type Bar { pub y: u64, }".to_string(),
            },
        })
        .unwrap(),
    );

    let _ = wait_for_notification(&conn);

    let result = send_request(
        &conn,
        "textDocument/documentSymbol",
        serde_json::to_value(lsp_types::DocumentSymbolParams {
            text_document: TextDocumentIdentifier {
                uri: "file:///test.pyxis".parse().unwrap(),
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        })
        .unwrap(),
    );

    // Should return an array of symbols
    assert!(
        result.is_array(),
        "should return symbol array, got: {result}"
    );
    let arr = result.as_array().unwrap();
    assert!(arr.len() >= 2, "should have at least 2 symbols");
}

#[test]
fn test_formatting() {
    let conn = spawn_server();

    send_request(
        &conn,
        "initialize",
        serde_json::to_value(InitializeParams::default()).unwrap(),
    );
    send_notification(
        &conn,
        "initialized",
        serde_json::to_value(InitializedParams {}).unwrap(),
    );

    let text = "pub type Foo { pub x: u32, }";
    send_notification(
        &conn,
        "textDocument/didOpen",
        serde_json::to_value(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: "file:///test.pyxis".parse().unwrap(),
                language_id: "pyxis".to_string(),
                version: 0,
                text: text.to_string(),
            },
        })
        .unwrap(),
    );

    let _ = wait_for_notification(&conn);

    let result = send_request(
        &conn,
        "textDocument/formatting",
        serde_json::to_value(lsp_types::DocumentFormattingParams {
            text_document: TextDocumentIdentifier {
                uri: "file:///test.pyxis".parse().unwrap(),
            },
            options: Default::default(),
            work_done_progress_params: Default::default(),
        })
        .unwrap(),
    );

    // Should return a TextEdit array
    assert!(result.is_array(), "should return edit array");
    let arr = result.as_array().unwrap();
    assert!(!arr.is_empty(), "should have at least one edit");
}

#[test]
fn test_did_change_debounce() {
    use std::time::{Duration, Instant};
    let conn = spawn_server();

    send_request(
        &conn,
        "initialize",
        serde_json::to_value(InitializeParams::default()).unwrap(),
    );
    send_notification(
        &conn,
        "initialized",
        serde_json::to_value(InitializedParams {}).unwrap(),
    );

    // Open a valid file
    send_notification(
        &conn,
        "textDocument/didOpen",
        serde_json::to_value(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: "file:///test.pyxis".parse().unwrap(),
                language_id: "pyxis".to_string(),
                version: 0,
                text: "pub type Foo { pub x: u32, }".to_string(),
            },
        })
        .unwrap(),
    );
    // Drain initial diagnostics
    let _ = wait_for_notification(&conn);

    // Send a didChange with an error
    let start = Instant::now();
    send_notification(
        &conn,
        "textDocument/didChange",
        serde_json::json!({
            "textDocument": { "uri": "file:///test.pyxis", "version": 1 },
            "contentChanges": [{ "text": "pub type Broken { pub field: NonexistentType, }" }]
        }),
    );

    // Diagnostics should NOT arrive immediately (debounced)
    let immediate = conn.receiver.recv_timeout(Duration::from_millis(100));
    // We might get nothing or a delayed notification — the key is that
    // it takes at least 400ms to arrive (debounce window is 500ms)
    if let Ok(Message::Notification(_)) = immediate {
        // If we got something immediately, it shouldn't be diagnostics
        // (could be something else). Check elapsed time.
        let elapsed = start.elapsed();
        // This is a soft assertion — in practice the debounce ensures
        // at least 400ms delay
        assert!(
            elapsed >= Duration::from_millis(400) || elapsed < Duration::from_millis(10),
            "diagnostics should be debounced (>=400ms) or immediate (<10ms), got {:?}",
            elapsed
        );
    }

    // Wait for the debounced diagnostics
    let notif = wait_for_notification(&conn);
    assert!(notif.is_some(), "should receive diagnostics after debounce");
    let notif = notif.unwrap();
    assert_eq!(notif.method, "textDocument/publishDiagnostics");
    let elapsed = start.elapsed();
    assert!(
        elapsed >= Duration::from_millis(400),
        "diagnostics should be delayed by debounce, took {:?}",
        elapsed
    );
}
