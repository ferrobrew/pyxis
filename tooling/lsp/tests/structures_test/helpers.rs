//! Shared fixtures and request-building helpers for the structures_test suite.

use lsp_server::{Request, RequestId};
use lsp_types::{Position, TextDocumentIdentifier, TextDocumentPositionParams};
use pyxis_lsp::state::ServerState;

pub(crate) fn hover_text(s: &ServerState, u: &lsp_types::Uri, line: u32, ch: u32) -> String {
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

pub(crate) const SRC: &str = r#"use types::math::Matrix4;

// cpp include hint for the epilogue below
#[cfg(backend = "cpp")]
use types::math::Matrix4;

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

pub(crate) fn project() -> (ServerState, lsp_types::Uri) {
    let state = ServerState::in_memory(&[(
        "/proj",
        8,
        &[
            (
                "types/math.pyxis",
                "pub type Matrix4 {\n    pub data: [f32; 16],\n}\n",
            ),
            ("game.pyxis", SRC),
        ],
    )]);
    let uri = ServerState::document_uri("/proj", "game.pyxis");
    (state, uri)
}

/// (line, char) of the first occurrence of `needle` at/after `from_line`.
pub(crate) fn at(needle: &str, from_line: usize) -> (u32, u32) {
    for (i, l) in SRC.lines().enumerate().skip(from_line) {
        if let Some(c) = l.find(needle) {
            return (i as u32, c as u32 + 1);
        }
    }
    panic!("not found: {needle}");
}

pub(crate) fn def_uri(s: &ServerState, u: &lsp_types::Uri, line: u32, ch: u32) -> Option<String> {
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

pub(crate) fn references(
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
pub(crate) fn occ_project() -> (ServerState, lsp_types::Uri, lsp_types::Uri, u32) {
    let st = ServerState::in_memory(&[(
        "/proj",
        8,
        &[
            ("world/shared.pyxis", "pub type Foo {\n    pub x: u64,\n}\n"),
            (
                "consumer.pyxis",
                "use world::shared::Foo;\n\npub type C {\n    pub f: Foo,\n    pub g: Foo,\n}\n",
            ),
        ],
    )]);
    let consumer = ServerState::document_uri("/proj", "consumer.pyxis");
    let shared = ServerState::document_uri("/proj", "world/shared.pyxis");
    // column of the first `Foo` field reference (line 3: "    pub f: Foo,")
    let col = "    pub f: ".len() as u32;
    (st, consumer, shared, col)
}

pub(crate) fn import_actions(
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
pub(crate) fn action_new_text(a: &serde_json::Value) -> String {
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

pub(crate) fn import_project() -> (ServerState, lsp_types::Uri, String) {
    let consumer = "use rendering::render_block::RenderBlock;\n\npub type C {\n    pub a: RenderBlock,\n    pub b: GenericRenderBlock,\n    pub c: Widget,\n}\n";
    let st = ServerState::in_memory(&[(
        "/proj",
        8,
        &[
            (
                "rendering/render_block.pyxis",
                "pub type RenderBlock {\n    pub x: u64,\n}\npub type GenericRenderBlock {\n    pub y: u64,\n}\n",
            ),
            (
                "gui/widget.pyxis",
                "pub type Widget {\n    pub z: u64,\n}\n",
            ),
            ("consumer.pyxis", consumer),
        ],
    )]);
    let uri = ServerState::document_uri("/proj", "consumer.pyxis");
    (st, uri, consumer.to_string())
}

pub(crate) fn completions(
    s: &ServerState,
    u: &lsp_types::Uri,
    line: u32,
    ch: u32,
) -> Vec<serde_json::Value> {
    let params = lsp_types::CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: u.clone() },
            position: Position {
                line,
                character: ch,
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
    serde_json::from_value(s.handle_completion(r).result.unwrap()).unwrap()
}
