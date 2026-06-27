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
    // Unique per call: `structural_hovers` and `references_resolve_despite_size_error`
    // both call this and run in parallel, so a dir keyed only by pid would race
    // (one test's remove_dir_all + rewrite clobbering the other's file scan).
    static COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    let n = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let base = std::env::temp_dir().join(format!("pyxis-structs-{}-{}", std::process::id(), n));
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
    assert!(h.contains("pub transform: Matrix4"), "field hover shows its signature: {h}");

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

#[test]
fn function_args_and_self() {
    let base = std::env::temp_dir().join(format!("pyxis-arg-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(&base.join("pyxis.toml"), "[project]\nname = \"t\"\npointer_size = 8\n");
    let src = "pub type Foo {\n    pub x: u64,\n    vftable {\n        pub fn doit(&mut self, count: u32);\n    },\n}\n";
    write(&base.join("foo.pyxis"), src);
    let init = serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("foo.pyxis").display()).parse().unwrap();
    let l = src.lines().nth(3).unwrap();
    assert!(hover_text(&st, &uri, 3, l.find("count").unwrap() as u32 + 1).contains("**arg** `count`"));
    assert!(hover_text(&st, &uri, 3, l.find("self").unwrap() as u32 + 1).contains("**type** `Foo`"));
}

#[test]
fn enum_variant_shows_value() {
    let base = std::env::temp_dir().join(format!("pyxis-en-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(&base.join("pyxis.toml"), "[project]\nname = \"t\"\npointer_size = 8\n");
    let src = "pub enum E: u32 {\n    A,\n    B = 5,\n    C,\n}\n";
    write(&base.join("e.pyxis"), src);
    let init = serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("e.pyxis").display()).parse().unwrap();
    // auto-incremented after B=5 → C=6
    let cc = src.lines().nth(3).unwrap().find('C').unwrap() as u32;
    let h = hover_text(&st, &uri, 3, cc);
    assert!(h.contains("**variant** `C`"), "should describe the variant, not the enum: {h}");
    assert!(h.contains("value `6`"), "auto-incremented value should be 6: {h}");
}

#[test]
fn attribute_hover_describes_attribute() {
    let base = std::env::temp_dir().join(format!("pyxis-attr-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(&base.join("pyxis.toml"), "[project]\nname = \"t\"\npointer_size = 8\n");
    let src = "#[size(0x10)]\npub type Foo {\n    #[base]\n    pub p: u64,\n}\n";
    write(&base.join("foo.pyxis"), src);
    let init = serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("foo.pyxis").display()).parse().unwrap();
    let h = hover_text(&st, &uri, 0, 3); // #[size(0x10)]
    assert!(h.contains("**attribute**") && h.contains("#[size(0x10)]"), "got {h}");
    let b = hover_text(&st, &uri, 2, 7); // #[base]
    assert!(b.contains("**attribute**") && b.contains("base class"), "got {b}");
}

#[test]
fn free_functions_hover() {
    let base = std::env::temp_dir().join(format!("pyxis-ff-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(&base.join("pyxis.toml"), "[project]\nname = \"t\"\npointer_size = 8\n");
    let src = "pub type T {\n    pub x: u64,\n}\nfn free_fn(item: *const T) -> bool;\n";
    write(&base.join("m.pyxis"), src);
    let init = serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display()).parse().unwrap();
    let l = src.lines().nth(3).unwrap();
    assert!(hover_text(&st, &uri, 3, l.find("free_fn").unwrap() as u32 + 1).contains("**fn** `free_fn`"));
    assert!(hover_text(&st, &uri, 3, l.find("item").unwrap() as u32 + 1).contains("**arg** `item`"));
    assert!(hover_text(&st, &uri, 3, l.find('T').unwrap() as u32).contains("**type** `T`"));
}

#[test]
fn extern_value_and_type_hover() {
    let base = std::env::temp_dir().join(format!("pyxis-ex-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(&base.join("pyxis.toml"), "[project]\nname = \"t\"\npointer_size = 8\n");
    // extern value references a type defined later (forward ref).
    let src = "#[address(0x100)]\nextern foo: Bar;\n\npub type Bar {\n    pub x: u64,\n}\n";
    write(&base.join("m.pyxis"), src);
    let init = serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display()).parse().unwrap();
    let l = src.lines().nth(1).unwrap();
    assert!(hover_text(&st, &uri, 1, l.find("foo").unwrap() as u32 + 1).contains("**extern value** `foo`"));
    assert!(hover_text(&st, &uri, 1, l.find("Bar").unwrap() as u32 + 1).contains("**type** `Bar`"),
        "forward-referenced extern type should resolve");
}

#[test]
fn pointer_and_array_shells() {
    let base = std::env::temp_dir().join(format!("pyxis-pa-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(&base.join("pyxis.toml"), "[project]\nname = \"t\"\npointer_size = 8\n");
    let src = "pub type Foo {\n    pub x: u64,\n}\npub type T {\n    pub p: *mut Foo,\n    pub arr: [Foo; 4],\n}\n";
    write(&base.join("m.pyxis"), src);
    let init = serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display()).parse().unwrap();
    let l4 = src.lines().nth(4).unwrap();
    assert!(hover_text(&st, &uri, 4, l4.find('*').unwrap() as u32).contains("**pointer**"));
    assert!(hover_text(&st, &uri, 4, l4.find("Foo").unwrap() as u32).contains("**type** `Foo`"));
    let l5 = src.lines().nth(5).unwrap();
    assert!(hover_text(&st, &uri, 5, l5.find('[').unwrap() as u32).contains("**array**"));
    assert!(hover_text(&st, &uri, 5, l5.find("Foo").unwrap() as u32).contains("**type** `Foo`"));
}

#[test]
fn backend_terms_hover() {
    let base = std::env::temp_dir().join(format!("pyxis-bt-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(&base.join("pyxis.toml"), "[project]\nname = \"t\"\npointer_size = 8\n");
    let src = "pub type Foo {\n    pub x: u64,\n}\nbackend rust {\n    epilogue for Foo r#\"\n    for x in 0..3 {}\n\"#;\n}\n";
    write(&base.join("m.pyxis"), src);
    let init = serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display()).parse().unwrap();
    assert!(hover_text(&st, &uri, 3, src.lines().nth(3).unwrap().find("rust").unwrap() as u32).contains("`rust` backend"));
    assert!(hover_text(&st, &uri, 4, src.lines().nth(4).unwrap().find("epilogue").unwrap() as u32).contains("**backend**"));
    // `for` inside the spliced code must NOT be treated as a backend keyword
    assert!(hover_text(&st, &uri, 5, src.lines().nth(5).unwrap().find("for").unwrap() as u32).is_empty()
        || !hover_text(&st, &uri, 5, src.lines().nth(5).unwrap().find("for").unwrap() as u32).contains("**backend**"));
}

#[test]
fn vftable_keyword_describes_struct() {
    let base = std::env::temp_dir().join(format!("pyxis-vf-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(&base.join("pyxis.toml"), "[project]\nname = \"t\"\npointer_size = 8\n");
    let src = "pub type Foo {\n    vftable {\n        pub fn a(&mut self);\n        pub fn b(&mut self);\n    },\n}\n";
    write(&base.join("m.pyxis"), src);
    let init = serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display()).parse().unwrap();
    let c = src.lines().nth(1).unwrap().find("vftable").unwrap() as u32 + 2;
    let h = hover_text(&st, &uri, 1, c);
    assert!(h.contains("**vftable**") && h.contains("`2` virtual"), "got {h}");
}
