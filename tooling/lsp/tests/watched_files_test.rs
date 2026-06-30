//! File-watching (workspace/didChangeWatchedFiles) is inherently coupled to the
//! real filesystem — the handler re-reads changed files from disk — so unlike
//! the in-memory handler tests this one writes to a temp dir.

use lsp_server::{Notification, Request, RequestId};
use lsp_types::{
    CompletionParams, FileChangeType, FileEvent, Position, TextDocumentIdentifier,
    TextDocumentPositionParams,
};
use pyxis_lsp::state::ServerState;

fn completion_labels(s: &ServerState, uri: &lsp_types::Uri) -> Vec<String> {
    let params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 0,
                character: 0,
            },
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: None,
    };
    let r = Request::new(
        RequestId::from(1),
        "textDocument/completion".into(),
        serde_json::to_value(params).unwrap(),
    );
    let items: Vec<serde_json::Value> =
        serde_json::from_value(s.handle_completion(r).result.unwrap()).unwrap();
    items
        .iter()
        .map(|i| i["label"].as_str().unwrap().to_string())
        .collect()
}

fn notify_watched(state: &mut ServerState, path: &std::path::Path, typ: FileChangeType) {
    let uri: lsp_types::Uri = format!("file://{}", path.display()).parse().unwrap();
    let notif = Notification::new(
        "workspace/didChangeWatchedFiles".into(),
        lsp_types::DidChangeWatchedFilesParams {
            changes: vec![FileEvent { uri, typ }],
        },
    );
    state.handle_did_change_watched_files(notif).unwrap();
}

#[test]
fn watched_files_pick_up_on_disk_changes() {
    let base = std::env::temp_dir().join(format!("pyxis-watched-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    std::fs::create_dir_all(&base).unwrap();
    std::fs::write(
        base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    )
    .unwrap();
    std::fs::write(base.join("a.pyxis"), "pub type Foo {\n    pub x: u64,\n}\n").unwrap();

    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let mut st = ServerState::new(&init).unwrap();
    let a: lsp_types::Uri = format!("file://{}", base.join("a.pyxis").display())
        .parse()
        .unwrap();

    assert!(!completion_labels(&st, &a).contains(&"Bar".to_string()));

    // CHANGED: a type added to an existing file on disk is picked up.
    std::fs::write(
        base.join("a.pyxis"),
        "pub type Foo {\n    pub x: u64,\n}\npub type Bar {\n    pub y: u64,\n}\n",
    )
    .unwrap();
    notify_watched(&mut st, &base.join("a.pyxis"), FileChangeType::CHANGED);
    assert!(
        completion_labels(&st, &a).contains(&"Bar".to_string()),
        "CHANGED"
    );

    // CREATED: a brand-new file joins its project (visible cross-file).
    std::fs::write(base.join("b.pyxis"), "pub type Baz {\n    pub z: u64,\n}\n").unwrap();
    notify_watched(&mut st, &base.join("b.pyxis"), FileChangeType::CREATED);
    assert!(
        completion_labels(&st, &a).contains(&"Baz".to_string()),
        "CREATED"
    );

    // DELETED: the file drops out.
    let _ = std::fs::remove_file(base.join("b.pyxis"));
    notify_watched(&mut st, &base.join("b.pyxis"), FileChangeType::DELETED);
    assert!(
        !completion_labels(&st, &a).contains(&"Baz".to_string()),
        "DELETED"
    );

    let _ = std::fs::remove_dir_all(&base);
}
