//! Regression tests for hover / go-to-definition on field types and FQN paths.
//!
//! Covers:
//! - a field type imported via `use` (single segment) resolving to the type
//!   (not the enclosing struct);
//! - each segment of a fully-qualified path (`a::b::C`) resolving independently
//!   (leaf → type, earlier segments → their module files).

use lsp_server::{Request, RequestId};
use lsp_types::{Position, TextDocumentIdentifier, TextDocumentPositionParams};
use pyxis_lsp::state::ServerState;

fn def(state: &ServerState, uri: &lsp_types::Uri, line: u32, ch: u32) -> serde_json::Value {
    request(state, "textDocument/definition", uri, line, ch)
}
fn hover(state: &ServerState, uri: &lsp_types::Uri, line: u32, ch: u32) -> serde_json::Value {
    request(state, "textDocument/hover", uri, line, ch)
}
fn request(
    state: &ServerState,
    method: &str,
    uri: &lsp_types::Uri,
    line: u32,
    ch: u32,
) -> serde_json::Value {
    let r = Request::new(
        RequestId::from(1),
        method.into(),
        serde_json::to_value(TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line,
                character: ch,
            },
        })
        .unwrap(),
    );
    let resp = if method == "textDocument/hover" {
        state.handle_hover(r)
    } else {
        state.handle_definition(r)
    };
    resp.result.unwrap_or(serde_json::Value::Null)
}

fn def_uri(v: &serde_json::Value) -> Option<&str> {
    v.get("uri").and_then(|u| u.as_str())
}
fn hover_text(v: &serde_json::Value) -> Option<&str> {
    v.get("contents")?.get("value")?.as_str()
}

const ROOT: &str = "/proj";
const EVENT_HANDLER: (&str, &str) = (
    "game/event_handler.pyxis",
    "pub type EventHandler {\n    pub id: u32,\n}\n",
);
const MOD: (&str, &str) = ("game/mod.pyxis", "// game module root\n");

#[test]
fn field_type_via_use_resolves_to_type() {
    let player = "use game::event_handler::EventHandler;\n\npub type Player {\n    pub event_handler: EventHandler,\n}\n";
    let st = ServerState::in_memory(&[(ROOT, 8, &[EVENT_HANDLER, MOD, ("player.pyxis", player)])]);
    let uri = ServerState::document_uri(ROOT, "player.pyxis");

    let line = player.lines().nth(3).unwrap();
    let ty = line.find("EventHandler").unwrap() as u32 + 2;

    let d = def(&st, &uri, 3, ty);
    assert_eq!(
        def_uri(&d),
        Some(ServerState::document_uri(ROOT, "game/event_handler.pyxis").as_str()),
        "field type should jump to EventHandler's file, got {d}"
    );

    let h = hover(&st, &uri, 3, ty);
    assert!(
        hover_text(&h).unwrap_or("").contains("`EventHandler`"),
        "hover should describe EventHandler, got {h}"
    );
}

#[test]
fn fqn_segments_resolve_independently() {
    let main = "pub type Game {\n    pub handler: game::event_handler::EventHandler,\n}\n";
    let st = ServerState::in_memory(&[(ROOT, 8, &[EVENT_HANDLER, MOD, ("main.pyxis", main)])]);
    let uri = ServerState::document_uri(ROOT, "main.pyxis");

    let l = main.lines().nth(1).unwrap();
    let game = l.find("game").unwrap() as u32 + 1;
    let evh = l.find("event_handler").unwrap() as u32 + 1;
    let eh = l.find("EventHandler").unwrap() as u32 + 1;

    assert_eq!(
        def_uri(&def(&st, &uri, 1, game)),
        Some(ServerState::document_uri(ROOT, "game/mod.pyxis").as_str()),
        "`game` segment should jump to game/mod.pyxis"
    );
    assert_eq!(
        def_uri(&def(&st, &uri, 1, evh)),
        Some(ServerState::document_uri(ROOT, "game/event_handler.pyxis").as_str()),
        "`event_handler` segment should jump to its module file"
    );
    assert_eq!(
        def_uri(&def(&st, &uri, 1, eh)),
        Some(ServerState::document_uri(ROOT, "game/event_handler.pyxis").as_str()),
        "`EventHandler` segment should jump to the type's file"
    );

    assert!(
        hover_text(&hover(&st, &uri, 1, game))
            .unwrap_or("")
            .contains("module"),
        "hover on `game` should describe a module"
    );
    assert!(
        hover_text(&hover(&st, &uri, 1, eh))
            .unwrap_or("")
            .contains("`EventHandler`"),
        "hover on `EventHandler` should describe the type"
    );
}

#[test]
fn braced_import_segments_resolve() {
    let st = ServerState::in_memory(&[(
        ROOT,
        8,
        &[
            (
                "types/shared_ptr.pyxis",
                "pub type SharedPtr {\n    pub p: u64,\n}\npub type WeakPtr {\n    pub w: u64,\n}\n",
            ),
            ("x.pyxis", "use types::shared_ptr::{SharedPtr, WeakPtr};\n"),
        ],
    )]);
    let uri = ServerState::document_uri(ROOT, "x.pyxis");
    let l = "use types::shared_ptr::{SharedPtr, WeakPtr};";
    let sp = l.find("SharedPtr").unwrap() as u32 + 1;
    let wp = l.find("WeakPtr").unwrap() as u32 + 1;
    let target = ServerState::document_uri(ROOT, "types/shared_ptr.pyxis");
    assert_eq!(
        def_uri(&def(&st, &uri, 0, sp)),
        Some(target.as_str()),
        "braced leaf SharedPtr should resolve to its file"
    );
    assert_eq!(
        def_uri(&def(&st, &uri, 0, wp)),
        Some(target.as_str()),
        "braced leaf WeakPtr should resolve to its file"
    );
    // shared prefix segment navigates to its module
    let sh = l.find("shared_ptr").unwrap() as u32 + 1;
    assert!(
        hover(&st, &uri, 0, sh).get("contents").is_some(),
        "braced-import prefix segment should resolve"
    );
}
