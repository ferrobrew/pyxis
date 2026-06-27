//! Regression tests for precise, structure-aware hover/go-to-definition:
//! type names, fields, vftable entries, impl methods, impl targets, backend
//! `use`s — and robustness when a type has a semantic error (mid-edit `#[size]`).

use lsp_server::{Request, RequestId};
use lsp_types::{Position, TextDocumentIdentifier, TextDocumentPositionParams};
use pyxis_lsp::state::ServerState;

fn write(p: &std::path::Path, c: &str) {
    if let Some(d) = p.parent() { std::fs::create_dir_all(d).unwrap(); }
    std::fs::write(p, c).unwrap();
}
fn hover_text(s: &ServerState, u: &lsp_types::Uri, line: u32, ch: u32) -> String {
    let r = Request::new(RequestId::from(1), "textDocument/hover".into(),
        serde_json::to_value(TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: u.clone() },
            position: Position { line, character: ch },
        }).unwrap());
    let v = s.handle_hover(r).result.unwrap_or(serde_json::Value::Null);
    v.get("contents").and_then(|c| c.get("value")).and_then(|x| x.as_str()).unwrap_or("").to_string()
}

const SRC: &str = r#"use types::math::Matrix4;

backend cpp {
    use types::math::Matrix4;
}

#[size(0x18)]
pub type GameObject {
    vftable {
        pub fn destructor(&mut self);
    },
    #[base]
    pub transform: Matrix4,
    pub object_id: u32,
}

#[cfg(backend = "cpp")]
impl GameObject {
    pub fn release(&mut self) -> Matrix4;
}
"#;

fn project() -> (ServerState, lsp_types::Uri) {
    let base = std::env::temp_dir().join(format!("pyxis-structs-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(&base.join("pyxis.toml"), "[project]\nname = \"t\"\npointer_size = 8\n");
    write(&base.join("types/math.pyxis"), "pub type Matrix4 {\n    pub data: [f32; 16],\n}\n");
    write(&base.join("game.pyxis"), SRC);
    let init = serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let state = ServerState::new(&init).unwrap();
    let uri = format!("file://{}", base.join("game.pyxis").display()).parse().unwrap();
    (state, uri)
}

/// (line, char) of the first occurrence of `needle` at/after `from_line`.
fn at(needle: &str, from_line: usize) -> (u32, u32) {
    for (i, l) in SRC.lines().enumerate().skip(from_line) {
        if let Some(c) = l.find(needle) { return (i as u32, c as u32 + 1); }
    }
    panic!("not found: {needle}");
}

#[test]
fn structural_hovers() {
    let (st, uri) = project();

    let (l, c) = at("GameObject", 7); // the type name
    assert!(hover_text(&st, &uri, l, c).contains("**type** `GameObject`"));

    let (l, c) = at("transform", 0); // field name
    let h = hover_text(&st, &uri, l, c);
    assert!(h.contains("**field** `transform`"), "got {h}");
    assert!(h.contains("Matrix4"), "field hover shows its type: {h}");

    let (l, c) = at("destructor", 0); // vftable fn
    assert!(hover_text(&st, &uri, l, c).contains("**fn** `destructor`"));

    let (l, c) = at("Matrix4", 12); // field type
    assert!(hover_text(&st, &uri, l, c).contains("**type** `Matrix4`"));

    let (l, c) = at("Matrix4", 2); // type inside a backend block's `use`
    assert!(hover_text(&st, &uri, l, c).contains("**type** `Matrix4`"));

    let (l, c) = at("release", 0); // impl method (in a cfg-gated block)
    assert!(hover_text(&st, &uri, l, c).contains("**fn** `release`"));

    let (l, c) = at("GameObject", 17); // impl target
    assert!(hover_text(&st, &uri, l, c).contains("**type** `GameObject`"));
}

#[test]
fn references_resolve_despite_size_error() {
    // #[size(0x18)] is wrong (real size differs) → GameObject has a semantic
    // error and drops from the type registry, but hover/nav must still work.
    let (st, uri) = project();
    let (l, c) = at("GameObject", 17); // impl target of the errored type
    assert!(hover_text(&st, &uri, l, c).contains("**type** `GameObject`"),
        "references to error-state types must still resolve");
}

#[test]
fn field_hover_shows_offset() {
    // All-u64 fields lay out with no implicit padding, so the type resolves
    // and field offsets are available.
    let base = std::env::temp_dir().join(format!("pyxis-off-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(&base.join("pyxis.toml"), "[project]\nname = \"t\"\npointer_size = 8\n");
    let src = "pub type S {\n    pub a: u64,\n    pub b: u64,\n    pub c: u64,\n}\n";
    write(&base.join("s.pyxis"), src);
    let init = serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("s.pyxis").display()).parse().unwrap();
    let c = src.lines().nth(2).unwrap().find('b').unwrap() as u32 + 1;
    let h = hover_text(&st, &uri, 2, c);
    assert!(h.contains("offset `0x8`"), "field b should be at offset 0x8: {h}");
}

#[test]
fn backend_for_type_navigates() {
    let base = std::env::temp_dir().join(format!("pyxis-bf-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(&base.join("pyxis.toml"), "[project]\nname = \"t\"\npointer_size = 8\n");
    write(&base.join("types/shared.pyxis"), "pub type SharedPtr {\n    pub ptr: u64,\n}\n");
    let main = "use types::shared::SharedPtr;\n\nbackend cpp {\n    epilogue for SharedPtr r#\"// code\"#;\n}\n";
    write(&base.join("m.pyxis"), main);
    let init = serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display()).parse().unwrap();
    let c = main.lines().nth(3).unwrap().find("SharedPtr").unwrap() as u32 + 2;
    assert!(hover_text(&st, &uri, 3, c).contains("**type** `SharedPtr`"),
        "hovering the `for <Type>` target should describe the type");
}

#[test]
fn predefined_field_type_hovers_the_type() {
    let base = std::env::temp_dir().join(format!("pyxis-bi-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(&base.join("pyxis.toml"), "[project]\nname = \"t\"\npointer_size = 8\n");
    let src = "pub type S {\n    pub flag: bool,\n}\n";
    write(&base.join("s.pyxis"), src);
    let init = serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("s.pyxis").display()).parse().unwrap();
    let c = src.lines().nth(1).unwrap().find("bool").unwrap() as u32 + 1;
    let h = hover_text(&st, &uri, 1, c);
    assert!(h.contains("**builtin** `bool`"), "hovering bool should describe bool, not the field: {h}");
}
