//! Regression tests for precise, structure-aware hover/go-to-definition:
//! type names, fields, vftable entries, impl methods, impl targets, backend
//! `use`s — and robustness when a type has a semantic error (mid-edit `#[size]`).

use lsp_server::{Request, RequestId};
use lsp_types::{Position, TextDocumentIdentifier, TextDocumentPositionParams};
use pyxis_lsp::state::ServerState;

fn write(p: &std::path::Path, c: &str) {
    if let Some(d) = p.parent() {
        std::fs::create_dir_all(d).unwrap();
    }
    std::fs::write(p, c).unwrap();
}
fn hover_text(s: &ServerState, u: &lsp_types::Uri, line: u32, ch: u32) -> String {
    let r = Request::new(
        RequestId::from(1),
        "textDocument/hover".into(),
        serde_json::to_value(TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: u.clone() },
            position: Position {
                line,
                character: ch,
            },
        })
        .unwrap(),
    );
    let v = s.handle_hover(r).result.unwrap_or(serde_json::Value::Null);
    v.get("contents")
        .and_then(|c| c.get("value"))
        .and_then(|x| x.as_str())
        .unwrap_or("")
        .to_string()
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
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    write(
        &base.join("types/math.pyxis"),
        "pub type Matrix4 {\n    pub data: [f32; 16],\n}\n",
    );
    write(&base.join("game.pyxis"), SRC);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let state = ServerState::new(&init).unwrap();
    let uri = format!("file://{}", base.join("game.pyxis").display())
        .parse()
        .unwrap();
    (state, uri)
}

/// (line, char) of the first occurrence of `needle` at/after `from_line`.
fn at(needle: &str, from_line: usize) -> (u32, u32) {
    for (i, l) in SRC.lines().enumerate().skip(from_line) {
        if let Some(c) = l.find(needle) {
            return (i as u32, c as u32 + 1);
        }
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
    assert!(
        h.contains("pub transform: Matrix4"),
        "field hover shows its signature: {h}"
    );

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
    assert!(
        hover_text(&st, &uri, l, c).contains("**type** `GameObject`"),
        "references to error-state types must still resolve"
    );
}

#[test]
fn field_hover_shows_offset() {
    // All-u64 fields lay out with no implicit padding, so the type resolves
    // and field offsets are available.
    let base = std::env::temp_dir().join(format!("pyxis-off-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src = "pub type S {\n    pub a: u64,\n    pub b: u64,\n    pub c: u64,\n}\n";
    write(&base.join("s.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("s.pyxis").display())
        .parse()
        .unwrap();
    let c = src.lines().nth(2).unwrap().find('b').unwrap() as u32 + 1;
    let h = hover_text(&st, &uri, 2, c);
    assert!(
        h.contains("offset `0x8`"),
        "field b should be at offset 0x8: {h}"
    );
}

#[test]
fn backend_for_type_navigates() {
    let base = std::env::temp_dir().join(format!("pyxis-bf-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    write(
        &base.join("types/shared.pyxis"),
        "pub type SharedPtr {\n    pub ptr: u64,\n}\n",
    );
    let main = "use types::shared::SharedPtr;\n\nbackend cpp {\n    epilogue for SharedPtr r#\"// code\"#;\n}\n";
    write(&base.join("m.pyxis"), main);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display())
        .parse()
        .unwrap();
    let c = main.lines().nth(3).unwrap().find("SharedPtr").unwrap() as u32 + 2;
    assert!(
        hover_text(&st, &uri, 3, c).contains("**type** `SharedPtr`"),
        "hovering the `for <Type>` target should describe the type"
    );
}

#[test]
fn predefined_field_type_hovers_the_type() {
    let base = std::env::temp_dir().join(format!("pyxis-bi-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src = "pub type S {\n    pub flag: bool,\n}\n";
    write(&base.join("s.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("s.pyxis").display())
        .parse()
        .unwrap();
    let c = src.lines().nth(1).unwrap().find("bool").unwrap() as u32 + 1;
    let h = hover_text(&st, &uri, 1, c);
    assert!(
        h.contains("**builtin** `bool`"),
        "hovering bool should describe bool, not the field: {h}"
    );
}

#[test]
fn function_args_and_self() {
    let base = std::env::temp_dir().join(format!("pyxis-arg-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src = "pub type Foo {\n    pub x: u64,\n    vftable {\n        pub fn doit(&mut self, count: u32);\n    },\n}\n";
    write(&base.join("foo.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("foo.pyxis").display())
        .parse()
        .unwrap();
    let l = src.lines().nth(3).unwrap();
    assert!(
        hover_text(&st, &uri, 3, l.find("count").unwrap() as u32 + 1).contains("**arg** `count`")
    );
    assert!(
        hover_text(&st, &uri, 3, l.find("self").unwrap() as u32 + 1).contains("**type** `Foo`")
    );
}

#[test]
fn enum_variant_shows_value() {
    let base = std::env::temp_dir().join(format!("pyxis-en-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src = "pub enum E: u32 {\n    A,\n    B = 5,\n    C,\n}\n";
    write(&base.join("e.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("e.pyxis").display())
        .parse()
        .unwrap();
    // auto-incremented after B=5 → C=6
    let cc = src.lines().nth(3).unwrap().find('C').unwrap() as u32;
    let h = hover_text(&st, &uri, 3, cc);
    assert!(
        h.contains("**variant** `C`"),
        "should describe the variant, not the enum: {h}"
    );
    assert!(
        h.contains("value `6`"),
        "auto-incremented value should be 6: {h}"
    );
}

#[test]
fn attribute_hover_describes_attribute() {
    let base = std::env::temp_dir().join(format!("pyxis-attr-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src = "#[size(0x10)]\npub type Foo {\n    #[base]\n    pub p: u64,\n}\n";
    write(&base.join("foo.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("foo.pyxis").display())
        .parse()
        .unwrap();
    let h = hover_text(&st, &uri, 0, 3); // #[size(0x10)]
    assert!(
        h.contains("**attribute**") && h.contains("#[size(0x10)]"),
        "got {h}"
    );
    let b = hover_text(&st, &uri, 2, 7); // #[base]
    assert!(
        b.contains("**attribute**") && b.contains("base class"),
        "got {b}"
    );
}

#[test]
fn free_functions_hover() {
    let base = std::env::temp_dir().join(format!("pyxis-ff-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src = "pub type T {\n    pub x: u64,\n}\nfn free_fn(item: *const T) -> bool;\n";
    write(&base.join("m.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display())
        .parse()
        .unwrap();
    let l = src.lines().nth(3).unwrap();
    assert!(
        hover_text(&st, &uri, 3, l.find("free_fn").unwrap() as u32 + 1)
            .contains("**fn** `free_fn`")
    );
    assert!(
        hover_text(&st, &uri, 3, l.find("item").unwrap() as u32 + 1).contains("**arg** `item`")
    );
    assert!(hover_text(&st, &uri, 3, l.find('T').unwrap() as u32).contains("**type** `T`"));
}

#[test]
fn extern_value_and_type_hover() {
    let base = std::env::temp_dir().join(format!("pyxis-ex-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    // extern value references a type defined later (forward ref).
    let src = "#[address(0x100)]\nextern foo: Bar;\n\npub type Bar {\n    pub x: u64,\n}\n";
    write(&base.join("m.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display())
        .parse()
        .unwrap();
    let l = src.lines().nth(1).unwrap();
    assert!(
        hover_text(&st, &uri, 1, l.find("foo").unwrap() as u32 + 1)
            .contains("**extern value** `foo`")
    );
    assert!(
        hover_text(&st, &uri, 1, l.find("Bar").unwrap() as u32 + 1).contains("**type** `Bar`"),
        "forward-referenced extern type should resolve"
    );
}

#[test]
fn pointer_and_array_shells() {
    let base = std::env::temp_dir().join(format!("pyxis-pa-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src = "pub type Foo {\n    pub x: u64,\n}\npub type T {\n    pub p: *mut Foo,\n    pub arr: [Foo; 4],\n}\n";
    write(&base.join("m.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display())
        .parse()
        .unwrap();
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
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src = "pub type Foo {\n    pub x: u64,\n}\nbackend rust {\n    epilogue for Foo r#\"\n    for x in 0..3 {}\n\"#;\n}\n";
    write(&base.join("m.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display())
        .parse()
        .unwrap();
    assert!(
        hover_text(
            &st,
            &uri,
            3,
            src.lines().nth(3).unwrap().find("rust").unwrap() as u32
        )
        .contains("`rust` backend")
    );
    assert!(
        hover_text(
            &st,
            &uri,
            4,
            src.lines().nth(4).unwrap().find("epilogue").unwrap() as u32
        )
        .contains("**backend**")
    );
    // `for` inside the spliced code must NOT be treated as a backend keyword
    assert!(
        hover_text(
            &st,
            &uri,
            5,
            src.lines().nth(5).unwrap().find("for").unwrap() as u32
        )
        .is_empty()
            || !hover_text(
                &st,
                &uri,
                5,
                src.lines().nth(5).unwrap().find("for").unwrap() as u32
            )
            .contains("**backend**")
    );
}

#[test]
fn vftable_keyword_describes_struct() {
    let base = std::env::temp_dir().join(format!("pyxis-vf-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src = "pub type Foo {\n    vftable {\n        pub fn a(&mut self);\n        pub fn b(&mut self);\n    },\n}\n";
    write(&base.join("m.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display())
        .parse()
        .unwrap();
    let c = src.lines().nth(1).unwrap().find("vftable").unwrap() as u32 + 2;
    let h = hover_text(&st, &uri, 1, c);
    assert!(
        h.contains("**vftable**") && h.contains("`2` virtual"),
        "got {h}"
    );
}

#[test]
fn pointer_shell_in_function_signature() {
    let base = std::env::temp_dir().join(format!("pyxis-cps-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src = "pub type Foo {\n    vftable {\n        pub fn f(&mut self, mat: *const f32) -> *const u32;\n    },\n}\n";
    write(&base.join("m.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display())
        .parse()
        .unwrap();
    let l = src.lines().nth(2).unwrap();
    // the `*const` of the argument and of the return type both describe the pointer
    assert!(hover_text(&st, &uri, 2, l.find("*const f32").unwrap() as u32).contains("**pointer**"));
    assert!(hover_text(&st, &uri, 2, l.find("*const u32").unwrap() as u32).contains("**pointer**"));
    assert!(hover_text(&st, &uri, 2, l.find("f32").unwrap() as u32).contains("**builtin** `f32`"));
}

#[test]
fn shell_in_type_alias_target() {
    // Hovering the `*const` shell of a type-alias target describes the pointer,
    // while the pointee still resolves to its type.
    let base = std::env::temp_dir().join(format!("pyxis-alias-shell-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src = "pub type Foo {\n    pub x: u64,\n}\npub type Alias = *const Foo;\n";
    write(&base.join("m.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display())
        .parse()
        .unwrap();
    let l = src.lines().nth(3).unwrap();
    assert!(
        hover_text(&st, &uri, 3, l.find('*').unwrap() as u32).contains("**pointer**"),
        "alias target pointer shell describes the pointer"
    );
    assert!(
        hover_text(&st, &uri, 3, l.find("Foo").unwrap() as u32).contains("**type** `Foo`"),
        "alias target pointee still resolves"
    );
}

#[test]
fn shell_in_extern_value_type() {
    // Hovering the `*const` shell of an extern value's type describes the pointer.
    let base = std::env::temp_dir().join(format!("pyxis-extern-shell-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src = "pub type Foo {\n    pub x: u64,\n}\n#[address(0x100)]\nextern p: *const Foo;\n";
    write(&base.join("m.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display())
        .parse()
        .unwrap();
    let l = src.lines().nth(4).unwrap();
    assert!(
        hover_text(&st, &uri, 4, l.find('*').unwrap() as u32).contains("**pointer**"),
        "extern value pointer shell describes the pointer"
    );
    assert!(
        hover_text(&st, &uri, 4, l.find("Foo").unwrap() as u32).contains("**type** `Foo`"),
        "extern value pointee still resolves"
    );
}

#[test]
fn shell_in_generic_argument() {
    // Hovering the `*const` shell *inside* a generic argument describes the
    // inner pointer, not the outer generic type.
    let base = std::env::temp_dir().join(format!("pyxis-generic-shell-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src = "pub type Foo {\n    pub x: u64,\n}\npub type SharedPtr<T> {\n    pub ptr: *mut T,\n}\npub type Holder {\n    pub h: SharedPtr<*const Foo>,\n}\n";
    write(&base.join("m.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display())
        .parse()
        .unwrap();
    let l = src.lines().nth(7).unwrap(); // `    pub h: SharedPtr<*const Foo>,`
    // The `*const` inside the generic arg is a pointer shell, not SharedPtr.
    let star = l.find('*').unwrap() as u32;
    let h = hover_text(&st, &uri, 7, star);
    assert!(
        h.contains("**pointer**"),
        "inner generic-arg pointer shell describes the pointer: {h}"
    );
    // The outer generic name still resolves to its type.
    assert!(
        hover_text(&st, &uri, 7, l.find("SharedPtr").unwrap() as u32)
            .contains("**type** `SharedPtr`"),
        "outer generic still resolves"
    );
    // The innermost pointee still resolves to its type.
    assert!(
        hover_text(&st, &uri, 7, l.find("Foo").unwrap() as u32).contains("**type** `Foo`"),
        "generic-arg pointee still resolves"
    );
}

#[test]
fn attribute_hover_on_free_function_and_extern() {
    let base = std::env::temp_dir().join(format!("pyxis-fa-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src =
        "#[address(0x100)]\nfn do_thing(x: u32) -> bool;\n\n#[address(0x200)]\nextern foo: u32;\n";
    write(&base.join("m.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display())
        .parse()
        .unwrap();
    assert!(
        hover_text(&st, &uri, 0, 3).contains("**attribute**"),
        "free-function attribute"
    );
    assert!(
        hover_text(&st, &uri, 3, 3).contains("**attribute**"),
        "extern value attribute"
    );
}

fn def_uri(s: &ServerState, u: &lsp_types::Uri, line: u32, ch: u32) -> Option<String> {
    let r = Request::new(
        RequestId::from(1),
        "textDocument/definition".into(),
        serde_json::to_value(TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: u.clone() },
            position: Position {
                line,
                character: ch,
            },
        })
        .unwrap(),
    );
    let v = s
        .handle_definition(r)
        .result
        .unwrap_or(serde_json::Value::Null);
    v.get("uri").and_then(|x| x.as_str()).map(|s| s.to_string())
}

// Two+ projects under one workspace root can have files at the SAME relative
// path (e.g. `world/shared.pyxis`), hence the same module path. Resolution must
// stay within the requesting file's project, not match a same-named file in a
// sibling project (the JustCause2/MadMax `physics_game_object` bug).
#[test]
fn cross_project_resolution_stays_in_project() {
    let base = std::env::temp_dir().join(format!("pyxis-xproj-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    // projA defines Foo at world/shared.pyxis; the decoys have world/shared.pyxis
    // too, with the same module path but WITHOUT Foo.
    write(
        &base.join("projA/pyxis.toml"),
        "[project]\nname = \"a\"\npointer_size = 8\n",
    );
    write(
        &base.join("projA/world/shared.pyxis"),
        "pub type Foo {\n    pub x: u64,\n}\n",
    );
    write(
        &base.join("projA/consumer.pyxis"),
        "use world::shared::Foo;\n\npub type C {\n    pub f: Foo,\n}\n",
    );
    for decoy in ["projB", "projC", "projD"] {
        write(
            &base.join(decoy).join("pyxis.toml"),
            "[project]\nname = \"d\"\npointer_size = 8\n",
        );
        write(
            &base.join(decoy).join("world/shared.pyxis"),
            "pub type Bar {\n    pub y: u64,\n}\n",
        );
    }

    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let consumer: lsp_types::Uri =
        format!("file://{}", base.join("projA/consumer.pyxis").display())
            .parse()
            .unwrap();
    // `pub f: Foo,` is line 3 (0-indexed).
    let src = std::fs::read_to_string(base.join("projA/consumer.pyxis")).unwrap();
    let col = src.lines().nth(3).unwrap().find("Foo").unwrap() as u32;

    // Resolves to projA's Foo — would be <none> if it picked a decoy's shared.pyxis.
    assert!(
        hover_text(&st, &consumer, 3, col).contains("**type** `Foo`"),
        "hover should resolve Foo within projA"
    );
    let def = def_uri(&st, &consumer, 3, col).expect("definition");
    assert!(
        def.contains("/projA/world/shared.pyxis"),
        "def should be projA's shared.pyxis, got {def}"
    );
    assert!(
        !def.contains("/projB/") && !def.contains("/projC/") && !def.contains("/projD/"),
        "def must not cross into a sibling project, got {def}"
    );
}

fn references(
    s: &ServerState,
    u: &lsp_types::Uri,
    line: u32,
    ch: u32,
    include_decl: bool,
) -> Vec<lsp_types::Location> {
    let params = lsp_types::ReferenceParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: u.clone() },
            position: Position {
                line,
                character: ch,
            },
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: lsp_types::ReferenceContext {
            include_declaration: include_decl,
        },
    };
    let r = Request::new(
        RequestId::from(1),
        "textDocument/references".into(),
        serde_json::to_value(params).unwrap(),
    );
    serde_json::from_value(s.handle_references(r).result.unwrap()).unwrap()
}

// A two-file project: `world/shared.pyxis` defines Foo; `consumer.pyxis` imports
// and uses it twice. Exercises the shared symbol-occurrences engine.
fn occ_project() -> (ServerState, lsp_types::Uri, lsp_types::Uri, u32) {
    let base = std::env::temp_dir().join(format!(
        "pyxis-occ-{}-{:?}",
        std::process::id(),
        std::thread::current().id()
    ));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"o\"\npointer_size = 8\n",
    );
    write(
        &base.join("world/shared.pyxis"),
        "pub type Foo {\n    pub x: u64,\n}\n",
    );
    write(
        &base.join("consumer.pyxis"),
        "use world::shared::Foo;\n\npub type C {\n    pub f: Foo,\n    pub g: Foo,\n}\n",
    );
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let consumer: lsp_types::Uri = format!("file://{}", base.join("consumer.pyxis").display())
        .parse()
        .unwrap();
    let shared: lsp_types::Uri = format!("file://{}", base.join("world/shared.pyxis").display())
        .parse()
        .unwrap();
    // column of the first `Foo` field reference (line 3: "    pub f: Foo,")
    let col = "    pub f: ".len() as u32;
    (st, consumer, shared, col)
}

#[test]
fn find_references_spans_definition_and_uses() {
    let (st, consumer, shared, col) = occ_project();
    // Invoke on the field reference; expect: definition + use leaf + 2 field refs.
    let locs = references(&st, &consumer, 3, col, true);
    assert_eq!(locs.len(), 4, "got {locs:#?}");
    assert!(
        locs.iter().any(|l| l.uri == shared),
        "must include the definition file"
    );
    assert_eq!(
        locs.iter().filter(|l| l.uri == consumer).count(),
        3,
        "use leaf + 2 fields"
    );
    // Excluding the declaration drops the definition-file occurrence.
    let no_decl = references(&st, &consumer, 3, col, false);
    assert_eq!(no_decl.len(), 3);
    assert!(no_decl.iter().all(|l| l.uri == consumer));
}

#[test]
fn document_highlight_is_current_file_only() {
    let (st, consumer, _shared, col) = occ_project();
    let params = TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
            uri: consumer.clone(),
        },
        position: Position {
            line: 3,
            character: col,
        },
    };
    let r = Request::new(
        RequestId::from(1),
        "textDocument/documentHighlight".into(),
        serde_json::to_value(params).unwrap(),
    );
    let hl: Vec<lsp_types::DocumentHighlight> =
        serde_json::from_value(st.handle_document_highlight(r).result.unwrap()).unwrap();
    // use leaf + 2 field refs, all in consumer.pyxis (not the definition file).
    assert_eq!(hl.len(), 3, "got {hl:#?}");
}

#[test]
#[allow(clippy::mutable_key_type)] // lsp_types::Uri key is fine here
fn rename_rewrites_every_occurrence() {
    let (st, consumer, shared, col) = occ_project();
    let params = lsp_types::RenameParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: consumer.clone(),
            },
            position: Position {
                line: 3,
                character: col,
            },
        },
        new_name: "Bar".to_string(),
        work_done_progress_params: Default::default(),
    };
    let r = Request::new(
        RequestId::from(1),
        "textDocument/rename".into(),
        serde_json::to_value(params).unwrap(),
    );
    let we: lsp_types::WorkspaceEdit =
        serde_json::from_value(st.handle_rename(r).result.unwrap()).unwrap();
    let changes = we.changes.unwrap();
    // Edits across both files: the definition + the use leaf + 2 field refs.
    let total: usize = changes.values().map(|v| v.len()).sum();
    assert_eq!(total, 4, "got {changes:#?}");
    assert!(changes[&shared].iter().all(|e| e.new_text == "Bar"));
    assert!(changes.contains_key(&consumer) && changes.contains_key(&shared));
}

#[test]
fn vftable_function_hover_shows_index_and_offset() {
    let base = std::env::temp_dir().join(format!("pyxis-vfi-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    let src = "pub type Foo {\n    vftable {\n        pub fn a(&mut self);\n        pub fn b(&mut self);\n        #[index(5)]\n        pub fn c(&mut self);\n    },\n}\n";
    write(&base.join("m.pyxis"), src);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("m.pyxis").display())
        .parse()
        .unwrap();
    let col = |l: usize, n: &str| src.lines().nth(l).unwrap().find(n).unwrap() as u32;
    let a = hover_text(&st, &uri, 2, col(2, "a"));
    assert!(
        a.contains("index `0`") && a.contains("vftable offset `0x0`"),
        "{a}"
    );
    let b = hover_text(&st, &uri, 3, col(3, "b"));
    assert!(
        b.contains("index `1`") && b.contains("vftable offset `0x8`"),
        "{b}"
    );
    // #[index(5)] resets the running counter → offset 5 * 8 = 0x28.
    let c = hover_text(&st, &uri, 5, col(5, "c"));
    assert!(
        c.contains("index `5`") && c.contains("vftable offset `0x28`"),
        "{c}"
    );
}

fn import_actions(
    s: &ServerState,
    u: &lsp_types::Uri,
    line: u32,
    ch: u32,
) -> Vec<serde_json::Value> {
    let params = lsp_types::CodeActionParams {
        text_document: TextDocumentIdentifier { uri: u.clone() },
        range: Position {
            line,
            character: ch,
        }
        .into_range(),
        context: lsp_types::CodeActionContext::default(),
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };
    let r = Request::new(
        RequestId::from(1),
        "textDocument/codeAction".into(),
        serde_json::to_value(params).unwrap(),
    );
    serde_json::from_value(s.handle_code_action(r).result.unwrap()).unwrap()
}
trait IntoRange {
    fn into_range(self) -> lsp_types::Range;
}
impl IntoRange for Position {
    fn into_range(self) -> lsp_types::Range {
        lsp_types::Range {
            start: self,
            end: self,
        }
    }
}
fn action_new_text(a: &serde_json::Value) -> String {
    a["edit"]["changes"]
        .as_object()
        .unwrap()
        .values()
        .next()
        .unwrap()[0]["newText"]
        .as_str()
        .unwrap()
        .to_string()
}

fn import_project() -> (ServerState, lsp_types::Uri, String) {
    // Unique per call — the import tests run in parallel and would otherwise
    // race on a shared pid-keyed dir.
    static COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    let n = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let base = std::env::temp_dir().join(format!("pyxis-imp-{}-{}", std::process::id(), n));
    let _ = std::fs::remove_dir_all(&base);
    write(
        &base.join("pyxis.toml"),
        "[project]\nname = \"t\"\npointer_size = 8\n",
    );
    write(
        &base.join("rendering/render_block.pyxis"),
        "pub type RenderBlock {\n    pub x: u64,\n}\npub type GenericRenderBlock {\n    pub y: u64,\n}\n",
    );
    write(
        &base.join("gui/widget.pyxis"),
        "pub type Widget {\n    pub z: u64,\n}\n",
    );
    let consumer = "use rendering::render_block::RenderBlock;\n\npub type C {\n    pub a: RenderBlock,\n    pub b: GenericRenderBlock,\n    pub c: Widget,\n}\n";
    write(&base.join("consumer.pyxis"), consumer);
    let init =
        serde_json::json!({ "rootUri": format!("file://{}", base.display()), "capabilities": {} });
    let st = ServerState::new(&init).unwrap();
    let uri: lsp_types::Uri = format!("file://{}", base.join("consumer.pyxis").display())
        .parse()
        .unwrap();
    (st, uri, consumer.to_string())
}

#[test]
fn auto_import_extends_matching_use() {
    let (st, uri, src) = import_project();
    let col = src
        .lines()
        .nth(4)
        .unwrap()
        .find("GenericRenderBlock")
        .unwrap() as u32;
    let acts = import_actions(&st, &uri, 4, col);
    let a = acts
        .iter()
        .find(|a| a["title"].as_str().unwrap().contains("GenericRenderBlock"))
        .expect("import action");
    // The existing `use rendering::render_block::RenderBlock;` is folded into a group.
    assert_eq!(
        action_new_text(a),
        "use rendering::render_block::{RenderBlock, GenericRenderBlock};"
    );
}

#[test]
fn auto_import_adds_new_use_when_no_prefix_matches() {
    let (st, uri, src) = import_project();
    let col = src.lines().nth(5).unwrap().find("Widget").unwrap() as u32;
    let acts = import_actions(&st, &uri, 5, col);
    let a = acts
        .iter()
        .find(|a| a["title"].as_str().unwrap().contains("Widget"))
        .expect("import action");
    assert_eq!(action_new_text(a), "use gui::widget::Widget;\n");
}

#[test]
fn no_import_action_for_resolved_type() {
    let (st, uri, src) = import_project();
    // RenderBlock IS imported → no import action offered.
    let col = src.lines().nth(3).unwrap().find("RenderBlock").unwrap() as u32;
    assert!(import_actions(&st, &uri, 3, col).is_empty());
}
