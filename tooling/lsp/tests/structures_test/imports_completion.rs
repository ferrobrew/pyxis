use pyxis_lsp::state::ServerState;

use crate::helpers::*;

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
    // The existing `use rendering::render_block::RenderBlock;` is folded into a
    // group; entries are sorted.
    assert_eq!(
        action_new_text(a),
        "use rendering::render_block::{GenericRenderBlock, RenderBlock};"
    );
}

#[test]
fn auto_import_merges_into_nested_group() {
    // A multi-prefix group must absorb the new type under its own sub-group,
    // not spawn a duplicate `use` line.
    for existing in [
        "use types::{math::Aabb, shared_ptr::WeakPtr};",
        "use types::{math::{Aabb}, shared_ptr::WeakPtr};",
    ] {
        let consumer = format!("{existing}\n\npub type C {{\n    pub v: Vector3,\n}}\n");
        let st = ServerState::in_memory(&[(
            "/proj",
            8,
            &[
                (
                    "types/math.pyxis",
                    "pub type Aabb {\n    pub a: u64,\n}\npub type Vector3 {\n    pub b: u64,\n}\n",
                ),
                (
                    "types/shared_ptr.pyxis",
                    "pub type WeakPtr {\n    pub c: u64,\n}\n",
                ),
                ("consumer.pyxis", &consumer),
            ],
        )]);
        let uri = ServerState::document_uri("/proj", "consumer.pyxis");
        let col = consumer.lines().nth(3).unwrap().find("Vector3").unwrap() as u32;
        let acts = import_actions(&st, &uri, 3, col);
        let a = acts
            .iter()
            .find(|a| a["title"].as_str().unwrap().contains("Vector3"))
            .expect("import action");
        assert_eq!(
            action_new_text(a),
            "use types::{math::{Aabb, Vector3}, shared_ptr::WeakPtr};",
            "from existing: {existing}"
        );
    }
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

#[test]
fn completion_offers_types_and_imports() {
    let st = ServerState::in_memory(&[(
        "/p",
        8,
        &[
            (
                "types/math.pyxis",
                "pub type Vector3 {\n    pub x: u32,\n}\npub type Aabb {\n    pub y: u32,\n}\n",
            ),
            ("flags.pyxis", "pub enum Color: u32 {\n    Red,\n}\n"),
            (
                "consumer.pyxis",
                "use types::math::Vector3;\n\npub type Local {\n    pub a: u32,\n}\npub type C {\n    pub v: Vector3,\n}\n",
            ),
        ],
    )]);
    let uri = ServerState::document_uri("/p", "consumer.pyxis");
    let items = completions(&st, &uri, 6, 11);
    let get = |label: &str| {
        items
            .iter()
            .find(|i| i["label"] == label)
            .unwrap_or_else(|| panic!("missing {label}"))
    };
    let edit_of = |i: &serde_json::Value| {
        i.get("additionalTextEdits")
            .and_then(|e| e[0]["newText"].as_str().map(str::to_string))
    };

    // keyword + builtin are present
    assert!(items.iter().any(|i| i["label"] == "type"));
    assert!(edit_of(get("u32")).is_none());
    // in-scope (imported) and same-module types carry no edit
    assert!(edit_of(get("Vector3")).is_none());
    assert_eq!(get("Local")["detail"], "this module");
    // out-of-scope type merges into the existing `use types::math::Vector3;`
    assert_eq!(
        edit_of(get("Aabb")).as_deref(),
        Some("use types::math::{Aabb, Vector3};")
    );
    // out-of-scope enum gets a new `use` and an ENUM kind
    assert_eq!(
        edit_of(get("Color")).as_deref(),
        Some("use flags::Color;\n")
    );
    assert_eq!(get("Color")["kind"], serde_json::json!(13)); // CompletionItemKind::ENUM
}
