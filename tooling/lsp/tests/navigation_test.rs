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

fn write(path: &std::path::Path, contents: &str) {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).unwrap();
    }
    std::fs::write(path, contents).unwrap();
}

fn def(state: &ServerState, uri: &lsp_types::Uri, line: u32, ch: u32) -> serde_json::Value {
    request(state, "textDocument/definition", uri, line, ch)
}
fn hover(state: &ServerState, uri: &lsp_types::Uri, line: u32, ch: u32) -> serde_json::Value {
    request(state, "textDocument/hover", uri, line, ch)
}
fn request(state: &ServerState, method: &str, uri: &lsp_types::Uri, line: u32, ch: u32) -> serde_json::Value {
    let r = Request::new(
        RequestId::from(1),
        method.into(),
        serde_json::to_value(TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position { line, character: ch },
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

struct Project {
    state: ServerState,
    base: std::path::PathBuf,
}
impl Project {
    fn uri(&self, rel: &str) -> lsp_types::Uri {
        format!("file://{}", self.base.join(rel).display()).parse().unwrap()
    }
}

fn setup(tag: &str) -> Project {
    let base = std::env::temp_dir().join(format!("pyxis-nav-{}-{}", tag, std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(&base.join("pyxis.toml"), "[project]\nname = \"t\"\npointer_size = 8\n");
    write(&base.join("game/event_handler.pyxis"), "pub type EventHandler {\n    pub id: u32,\n}\n");
    write(&base.join("game/mod.pyxis"), "// game module root\n");
    let init = serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let state = ServerState::new(&init).unwrap();
    Project { state, base }
}

#[test]
fn field_type_via_use_resolves_to_type() {
    let mut p = setup("use");
    let player = "use game::event_handler::EventHandler;\n\npub type Player {\n    pub event_handler: EventHandler,\n}\n";
    write(&p.base.join("player.pyxis"), player);
    // Re-discover the new file by re-creating state (discovery happens in new()).
    let init = serde_json::json!({ "rootUri": format!("file://{}", p.base.display()), "capabilities": {} });
    p.state = ServerState::new(&init).unwrap();
    let uri = p.uri("player.pyxis");

    let line = player.lines().nth(3).unwrap();
    let ty = line.find("EventHandler").unwrap() as u32 + 2;

    let d = def(&p.state, &uri, 3, ty);
    assert_eq!(def_uri(&d), Some(p.uri("game/event_handler.pyxis").as_str()),
        "field type should jump to EventHandler's file, got {d}");

    let h = hover(&p.state, &uri, 3, ty);
    assert!(hover_text(&h).unwrap_or("").contains("`EventHandler`"),
        "hover should describe EventHandler, got {h}");
}

#[test]
fn fqn_segments_resolve_independently() {
    let mut p = setup("fqn");
    let main = "pub type Game {\n    pub handler: game::event_handler::EventHandler,\n}\n";
    write(&p.base.join("main.pyxis"), main);
    let init = serde_json::json!({ "rootUri": format!("file://{}", p.base.display()), "capabilities": {} });
    p.state = ServerState::new(&init).unwrap();
    let uri = p.uri("main.pyxis");

    let l = main.lines().nth(1).unwrap();
    let game = l.find("game").unwrap() as u32 + 1;
    let evh = l.find("event_handler").unwrap() as u32 + 1;
    let eh = l.find("EventHandler").unwrap() as u32 + 1;

    assert_eq!(def_uri(&def(&p.state, &uri, 1, game)), Some(p.uri("game/mod.pyxis").as_str()),
        "`game` segment should jump to game/mod.pyxis");
    assert_eq!(def_uri(&def(&p.state, &uri, 1, evh)), Some(p.uri("game/event_handler.pyxis").as_str()),
        "`event_handler` segment should jump to its module file");
    assert_eq!(def_uri(&def(&p.state, &uri, 1, eh)), Some(p.uri("game/event_handler.pyxis").as_str()),
        "`EventHandler` segment should jump to the type's file");

    assert!(hover_text(&hover(&p.state, &uri, 1, game)).unwrap_or("").contains("module"),
        "hover on `game` should describe a module");
    assert!(hover_text(&hover(&p.state, &uri, 1, eh)).unwrap_or("").contains("`EventHandler`"),
        "hover on `EventHandler` should describe the type");
}
