//! Snapshot tests for LSP features using expect_test.
//!
//! Run `UPDATE_EXPECT=1 cargo test -p pyxis-lsp --test snapshots` to update.

use expect_test::expect;
use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::{
    ClientCapabilities, DidOpenTextDocumentParams, DocumentFormattingParams, DocumentSymbolParams,
    FormattingOptions, InitializeParams, InitializedParams, Position, TextDocumentIdentifier,
    TextDocumentItem, TextDocumentPositionParams,
};

fn spawn_server() -> Connection {
    let (server, client) = Connection::memory();
    std::thread::spawn(move || {
        let _ = pyxis_lsp::main_loop::run_with_connection(server);
    });
    client
}

fn init_server(conn: &Connection) {
    let req = Request::new(
        RequestId::from(1),
        "initialize".into(),
        serde_json::to_value(InitializeParams {
            capabilities: ClientCapabilities::default(),
            ..Default::default()
        })
        .unwrap(),
    );
    conn.sender.send(Message::Request(req)).unwrap();
    // Wait for response
    loop {
        if let Ok(Message::Response(_)) = conn.receiver.recv() {
            break;
        }
    }
    // Send initialized notification
    let notif = Notification::new(
        "initialized".into(),
        serde_json::to_value(InitializedParams {}).unwrap(),
    );
    conn.sender.send(Message::Notification(notif)).unwrap();
}

fn open_file(conn: &Connection, uri: &str, text: &str) {
    let notif = Notification::new(
        "textDocument/didOpen".into(),
        serde_json::to_value(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.parse().unwrap(),
                language_id: "pyxis".to_string(),
                version: 0,
                text: text.to_string(),
            },
        })
        .unwrap(),
    );
    conn.sender.send(Message::Notification(notif)).unwrap();
    // Drain diagnostics
    let _ = conn
        .receiver
        .recv_timeout(std::time::Duration::from_millis(200));
}

fn send_request(
    conn: &Connection,
    id: i32,
    method: &str,
    params: serde_json::Value,
) -> serde_json::Value {
    let req = Request::new(RequestId::from(id), method.into(), params);
    conn.sender.send(Message::Request(req)).unwrap();
    loop {
        match conn.receiver.recv().unwrap() {
            Message::Response(resp) => return resp.result.unwrap_or(serde_json::Value::Null),
            Message::Notification(_) => {}
            Message::Request(_) => {}
        }
    }
}

const TEST_URI: &str = "file:///test.pyxis";
const TEST_CODE: &str = r#"pub type Foo {
    pub x: u32,
    pub y: u64,
}

pub enum Color: u32 {
    Red = 0,
    Green = 1,
    Blue = 2,
}

pub fn freestanding() -> u32;
"#;

#[test]
fn snapshot_hover() {
    let conn = spawn_server();
    init_server(&conn);
    open_file(&conn, TEST_URI, TEST_CODE);

    let result = send_request(
        &conn,
        2,
        "textDocument/hover",
        serde_json::to_value(TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: TEST_URI.parse().unwrap(),
            },
            position: Position {
                line: 0,
                character: 9,
            },
        })
        .unwrap(),
    );

    let formatted = serde_json::to_string_pretty(&result).unwrap();
    expect![[r#"
        {
          "contents": {
            "kind": "markdown",
            "value": "**type** `Foo`\n\n**Fields:**\n- `pub x: u32`\n- `pub y: u64`\n"
          },
          "range": {
            "end": {
              "character": 12,
              "line": 0
            },
            "start": {
              "character": 9,
              "line": 0
            }
          }
        }"#]]
    .assert_eq(&formatted);
}

#[test]
fn snapshot_completion() {
    let conn = spawn_server();
    init_server(&conn);
    open_file(&conn, TEST_URI, TEST_CODE);

    let result = send_request(
        &conn,
        2,
        "textDocument/completion",
        serde_json::to_value(TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: TEST_URI.parse().unwrap(),
            },
            position: Position {
                line: 0,
                character: 0,
            },
        })
        .unwrap(),
    );

    let formatted = serde_json::to_string_pretty(&result).unwrap();
    expect![[r#"
        [
          {
            "kind": 14,
            "label": "pub"
          },
          {
            "kind": 14,
            "label": "type"
          },
          {
            "kind": 14,
            "label": "enum"
          },
          {
            "kind": 14,
            "label": "bitflags"
          },
          {
            "kind": 14,
            "label": "impl"
          },
          {
            "kind": 14,
            "label": "fn"
          },
          {
            "kind": 14,
            "label": "extern"
          },
          {
            "kind": 14,
            "label": "use"
          },
          {
            "kind": 14,
            "label": "backend"
          },
          {
            "kind": 14,
            "label": "vftable"
          },
          {
            "kind": 14,
            "label": "const"
          },
          {
            "kind": 14,
            "label": "mut"
          },
          {
            "kind": 14,
            "label": "as"
          },
          {
            "kind": 14,
            "label": "prologue"
          },
          {
            "kind": 14,
            "label": "epilogue"
          },
          {
            "kind": 14,
            "label": "self"
          },
          {
            "kind": 14,
            "label": "Self"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "void"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "bool"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "u8"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "u16"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "u32"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "u64"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "u128"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "i8"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "i16"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "i32"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "i64"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "i128"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "f32"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "f64"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "c_char"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "AtomicBool"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "AtomicU8"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "AtomicU16"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "AtomicU32"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "AtomicU64"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "AtomicI8"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "AtomicI16"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "AtomicI32"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "AtomicI64"
          },
          {
            "detail": "builtin",
            "kind": 22,
            "label": "str"
          },
          {
            "detail": "this module",
            "kind": 13,
            "label": "Color"
          },
          {
            "detail": "this module",
            "kind": 22,
            "label": "Foo"
          }
        ]"#]]
    .assert_eq(&formatted);
}

#[test]
fn snapshot_document_symbols() {
    let conn = spawn_server();
    init_server(&conn);
    open_file(&conn, TEST_URI, TEST_CODE);

    let result = send_request(
        &conn,
        2,
        "textDocument/documentSymbol",
        serde_json::to_value(DocumentSymbolParams {
            text_document: TextDocumentIdentifier {
                uri: TEST_URI.parse().unwrap(),
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        })
        .unwrap(),
    );

    let formatted = serde_json::to_string_pretty(&result).unwrap();
    expect![[r#"
        [
          {
            "kind": 23,
            "name": "Foo",
            "range": {
              "end": {
                "character": 1,
                "line": 3
              },
              "start": {
                "character": 0,
                "line": 0
              }
            },
            "selectionRange": {
              "end": {
                "character": 1,
                "line": 3
              },
              "start": {
                "character": 0,
                "line": 0
              }
            }
          },
          {
            "kind": 10,
            "name": "Color",
            "range": {
              "end": {
                "character": 1,
                "line": 9
              },
              "start": {
                "character": 0,
                "line": 5
              }
            },
            "selectionRange": {
              "end": {
                "character": 1,
                "line": 9
              },
              "start": {
                "character": 0,
                "line": 5
              }
            }
          },
          {
            "kind": 12,
            "name": "freestanding",
            "range": {
              "end": {
                "character": 29,
                "line": 11
              },
              "start": {
                "character": 0,
                "line": 11
              }
            },
            "selectionRange": {
              "end": {
                "character": 29,
                "line": 11
              },
              "start": {
                "character": 0,
                "line": 11
              }
            }
          }
        ]"#]]
    .assert_eq(&formatted);
}

#[test]
fn snapshot_formatting() {
    let conn = spawn_server();
    init_server(&conn);
    open_file(&conn, TEST_URI, TEST_CODE);

    let result = send_request(
        &conn,
        2,
        "textDocument/formatting",
        serde_json::to_value(DocumentFormattingParams {
            text_document: TextDocumentIdentifier {
                uri: TEST_URI.parse().unwrap(),
            },
            options: FormattingOptions::default(),
            work_done_progress_params: Default::default(),
        })
        .unwrap(),
    );

    let formatted = serde_json::to_string_pretty(&result).unwrap();
    expect![[r#"
        [
          {
            "newText": "pub type Foo {\n    pub x: u32,\n    pub y: u64,\n}\n\npub enum Color: u32 {\n    Red = 0,\n    Green = 1,\n    Blue = 2,\n}\n\npub fn freestanding() -> u32;\n",
            "range": {
              "end": {
                "character": 0,
                "line": 4294967295
              },
              "start": {
                "character": 0,
                "line": 0
              }
            }
          }
        ]"#]].assert_eq(&formatted);
}

#[test]
fn snapshot_code_lens() {
    let conn = spawn_server();
    init_server(&conn);
    open_file(&conn, TEST_URI, TEST_CODE);

    let result = send_request(
        &conn,
        2,
        "textDocument/codeLens",
        serde_json::to_value(lsp_types::CodeLensParams {
            text_document: TextDocumentIdentifier {
                uri: TEST_URI.parse().unwrap(),
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        })
        .unwrap(),
    );

    let formatted = serde_json::to_string_pretty(&result).unwrap();
    expect![[r#"
        [
          {
            "command": {
              "command": "",
              "title": "size: 0x4"
            },
            "range": {
              "end": {
                "character": 1,
                "line": 9
              },
              "start": {
                "character": 0,
                "line": 5
              }
            }
          }
        ]"#]]
    .assert_eq(&formatted);
}

#[test]
fn snapshot_diagnostics() {
    let conn = spawn_server();
    init_server(&conn);

    // Open a file with a deliberate error
    let notif = Notification::new(
        "textDocument/didOpen".into(),
        serde_json::to_value(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: TEST_URI.parse().unwrap(),
                language_id: "pyxis".to_string(),
                version: 0,
                text: "pub type Broken { pub field: NonexistentType, }".to_string(),
            },
        })
        .unwrap(),
    );
    conn.sender.send(Message::Notification(notif)).unwrap();

    // Wait for diagnostics
    let notif = conn
        .receiver
        .recv_timeout(std::time::Duration::from_secs(5))
        .unwrap();
    let notif: Notification = match notif {
        Message::Notification(n) => n,
        _ => panic!("expected notification"),
    };

    let formatted = serde_json::to_string_pretty(&notif.params).unwrap();
    expect![[r#"
        {
          "diagnostics": [
            {
              "message": "1:30: Type not found: `NonexistentType`",
              "range": {
                "end": {
                  "character": 44,
                  "line": 0
                },
                "start": {
                  "character": 29,
                  "line": 0
                }
              },
              "severity": 1,
              "source": "pyxis"
            }
          ],
          "uri": "file:///test.pyxis"
        }"#]]
    .assert_eq(&formatted);
}

#[test]
fn snapshot_rename_invalid() {
    let conn = spawn_server();
    init_server(&conn);
    open_file(&conn, TEST_URI, TEST_CODE);

    let result = send_request(
        &conn,
        2,
        "textDocument/rename",
        serde_json::to_value(lsp_types::RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: TEST_URI.parse().unwrap(),
                },
                position: Position {
                    line: 0,
                    character: 9,
                },
            },
            new_name: "123invalid".to_string(),
            work_done_progress_params: Default::default(),
        })
        .unwrap(),
    );

    // Should return an error for invalid identifier
    let formatted = serde_json::to_string_pretty(&result).unwrap();
    expect!["null"].assert_eq(&formatted);
}
