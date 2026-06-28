//! Regression test for issue 4: saving a corrupt .pyxis file must NOT clear
//! the parse-error diagnostic that typing produced.

use lsp_server::Notification;
use lsp_types::{
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem,
    VersionedTextDocumentIdentifier,
};
use pyxis_lsp::state::ServerState;

const URI: &str = "file:///test.pyxis";

fn notif<T: serde::Serialize>(method: &str, params: T) -> Notification {
    Notification::new(method.into(), serde_json::to_value(params).unwrap())
}

/// Does the collected diagnostics set contain a non-empty diagnostic list for our URI?
fn has_error(notifs: &[Notification]) -> bool {
    for n in notifs {
        if n.method != "textDocument/publishDiagnostics" {
            continue;
        }
        let p: lsp_types::PublishDiagnosticsParams =
            serde_json::from_value(n.params.clone()).unwrap();
        if p.uri.as_str() == URI && !p.diagnostics.is_empty() {
            return true;
        }
    }
    false
}

#[test]
fn save_does_not_clear_parse_error() {
    let mut state = ServerState::in_memory(&[]);

    let uri: lsp_types::Uri = URI.parse().unwrap();

    // 1. Open a valid file.
    state
        .handle_did_open(notif(
            "textDocument/didOpen",
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "pyxis".into(),
                    version: 0,
                    text: "type Foo { pub x: u32, }".into(),
                },
            },
        ))
        .unwrap();
    let diags = state.collect_diagnostics();
    assert!(
        !has_error(&diags),
        "valid file should have no error: {diags:?}"
    );

    // 2. Type a corruption that produces a parse error.
    let corrupt = "type Foo { pub x: u32".to_string();
    state
        .handle_did_change(notif(
            "textDocument/didChange",
            DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: 1,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: corrupt.clone(),
                }],
            },
        ))
        .unwrap();
    let diags = state.collect_diagnostics();
    assert!(
        has_error(&diags),
        "typed corruption should show parse error: {diags:?}"
    );

    // 3. Save. Crucially, the `text` included on the save lags / differs from
    //    the live buffer — here it carries the older *clean* content (modelling
    //    a client that sends a stale or on-disk snapshot in didSave). The live
    //    buffer (synced via didChange) is still corrupt, so the parse error must
    //    remain. Before the fix, handle_did_save overwrote the synced content
    //    with this stale text and the error wrongly vanished.
    let stale_clean = "type Foo { pub x: u32, }".to_string();
    state
        .handle_did_save(notif(
            "textDocument/didSave",
            DidSaveTextDocumentParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                text: Some(stale_clean),
            },
        ))
        .unwrap();
    let diags = state.collect_diagnostics();
    assert!(
        has_error(&diags),
        "saving must NOT clear the parse error of the live (corrupt) buffer: {diags:?}"
    );

    // 4. Saving with matching corrupt text must also keep the error.
    state
        .handle_did_save(notif(
            "textDocument/didSave",
            DidSaveTextDocumentParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                text: Some(corrupt.clone()),
            },
        ))
        .unwrap();
    let diags = state.collect_diagnostics();
    assert!(
        has_error(&diags),
        "saving corrupt file must NOT clear the parse error: {diags:?}"
    );
}
