use lsp_server::{Request, RequestId};
use lsp_types::{Position, TextDocumentIdentifier, TextDocumentPositionParams};
use pyxis_lsp::state::ServerState;

use crate::helpers::*;

#[test]
fn document_link_resolves_doc_cross_references() {
    let src = "/// See [Bar] and [the docs](Bar).\npub type Foo {\n    pub x: u64,\n}\npub type Bar {\n    pub y: u64,\n}\n";
    let st = ServerState::in_memory(&[("/p", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/p", "m.pyxis");
    let params = lsp_types::DocumentLinkParams {
        text_document: TextDocumentIdentifier { uri },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };
    let r = Request::new(
        RequestId::from(1),
        "textDocument/documentLink".into(),
        serde_json::to_value(params).unwrap(),
    );
    let links: Vec<serde_json::Value> =
        serde_json::from_value(st.handle_document_link(r).result.unwrap()).unwrap();
    // both [Bar] and the (Bar) target resolve to Bar's definition (line 5, 1-indexed).
    assert_eq!(links.len(), 2, "{links:#?}");
    for l in &links {
        assert!(l["target"].as_str().unwrap().ends_with("m.pyxis#L5"), "{l}");
        assert_eq!(l["tooltip"], "m::Bar");
        assert_eq!(l["range"]["start"]["line"], 0);
    }
}

#[test]
fn document_link_resolves_to_function() {
    // A doc link to a freestanding function should be clickable, targeting the
    // function's declaration line — not just types/members.
    let src = "/// See [`GetName`].\npub type Foo {\n    pub x: u64,\n}\n\n#[address(0x456)]\npub fn GetName();\n";
    let st = ServerState::in_memory(&[("/p", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/p", "m.pyxis");
    let params = lsp_types::DocumentLinkParams {
        text_document: TextDocumentIdentifier { uri },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };
    let r = Request::new(
        RequestId::from(1),
        "textDocument/documentLink".into(),
        serde_json::to_value(params).unwrap(),
    );
    let links: Vec<serde_json::Value> =
        serde_json::from_value(st.handle_document_link(r).result.unwrap()).unwrap();
    assert_eq!(links.len(), 1, "{links:#?}");
    let l = &links[0];
    // `pub fn GetName();` is on line 7 (1-indexed).
    assert!(l["target"].as_str().unwrap().ends_with("m.pyxis#L7"), "{l}");
    assert_eq!(l["tooltip"], "m::GetName");
    assert_eq!(l["range"]["start"]["line"], 0);
}

#[test]
fn document_link_to_member_targets_the_member() {
    // A `Type::method` doc link should target the method's own line, matching
    // hover / go-to-definition — not the enclosing type's line.
    let src = "/// See [`Foo::bar`].\npub type Foo {\n    pub x: u64,\n}\nimpl Foo {\n    #[address(0x1)]\n    pub fn bar(&self);\n}\n";
    let st = ServerState::in_memory(&[("/p", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/p", "m.pyxis");
    let params = lsp_types::DocumentLinkParams {
        text_document: TextDocumentIdentifier { uri },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };
    let r = Request::new(
        RequestId::from(1),
        "textDocument/documentLink".into(),
        serde_json::to_value(params).unwrap(),
    );
    let links: Vec<serde_json::Value> =
        serde_json::from_value(st.handle_document_link(r).result.unwrap()).unwrap();
    assert_eq!(links.len(), 1, "{links:#?}");
    let l = &links[0];
    // `pub fn bar(&self);` is on line 7 (1-indexed); the type Foo is on line 2.
    assert!(l["target"].as_str().unwrap().ends_with("m.pyxis#L7"), "{l}");
    assert_eq!(l["tooltip"], "m::Foo::bar");
}

#[test]
fn implementation_finds_impl_blocks_across_files() {
    let st = ServerState::in_memory(&[(
        "/p",
        8,
        &[
            (
                "game.pyxis",
                "pub type GameObject {\n    pub x: u64,\n}\nimpl GameObject {\n    pub fn a(&mut self);\n}\n",
            ),
            (
                "other.pyxis",
                "use game::GameObject;\n\nimpl GameObject {\n    pub fn b(&mut self);\n}\n",
            ),
        ],
    )]);
    let uri = ServerState::document_uri("/p", "game.pyxis");
    let r = Request::new(
        RequestId::from(1),
        "textDocument/implementation".into(),
        serde_json::to_value(TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position: Position {
                line: 0,
                character: 10,
            }, // on `GameObject`
        })
        .unwrap(),
    );
    let locs: Vec<lsp_types::Location> =
        serde_json::from_value(st.handle_implementation(r).result.unwrap()).unwrap();
    assert_eq!(locs.len(), 2, "{locs:#?}");
    assert!(
        locs.iter()
            .any(|l| l.uri.as_str().ends_with("game.pyxis") && l.range.start.line == 3)
    );
    assert!(
        locs.iter()
            .any(|l| l.uri.as_str().ends_with("other.pyxis") && l.range.start.line == 2)
    );
}

#[test]
fn semantic_tokens_classify_types_namespaces_builtins() {
    let st = ServerState::in_memory(&[(
        "/p",
        8,
        &[
            (
                "types/math.pyxis",
                "pub type Vector3 {\n    pub x: u32,\n}\n",
            ),
            (
                "m.pyxis",
                "use types::math::Vector3;\n\npub type Foo {\n    pub v: Vector3,\n    pub n: u32,\n}\n",
            ),
        ],
    )]);
    let uri = ServerState::document_uri("/p", "m.pyxis");
    let params = lsp_types::SemanticTokensParams {
        text_document: TextDocumentIdentifier { uri },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };
    let r = Request::new(
        RequestId::from(1),
        "textDocument/semanticTokens/full".into(),
        serde_json::to_value(params).unwrap(),
    );
    let v = st.handle_semantic_tokens_full(r).result.unwrap();
    let data = v["data"].as_array().unwrap();
    // decode to (line, char, len, type, mods)
    let (mut line, mut ch) = (0i64, 0i64);
    let mut toks = Vec::new();
    for t in data.chunks(5) {
        let dl = t[0].as_i64().unwrap();
        line += dl;
        if dl != 0 {
            ch = 0;
        }
        ch += t[1].as_i64().unwrap();
        toks.push((
            line,
            ch,
            t[2].as_i64().unwrap(),
            t[3].as_i64().unwrap(),
            t[4].as_i64().unwrap(),
        ));
    }
    // namespace=0, type=1; defaultLibrary modifier = bit 0
    // `math` segment of the use path → namespace
    assert!(
        toks.iter()
            .any(|&(l, _, len, ty, _)| l == 0 && len == 4 && ty == 0),
        "math namespace: {toks:?}"
    );
    // Vector3 field type → type, no modifier
    assert!(
        toks.iter()
            .any(|&(l, _, len, ty, m)| l == 3 && len == 7 && ty == 1 && m == 0),
        "Vector3 type: {toks:?}"
    );
    // u32 builtin → type + defaultLibrary
    assert!(
        toks.iter()
            .any(|&(l, _, len, ty, m)| l == 4 && len == 3 && ty == 1 && m == 1),
        "u32 builtin: {toks:?}"
    );
}

#[test]
fn folding_ranges_cover_bodies_and_use_groups() {
    let src = "use types::math::{\n    Vector3,\n    Aabb,\n};\n\npub type Foo {\n    vftable {\n        pub fn a(&mut self);\n    },\n    pub x: u64,\n}\nimpl Foo {\n    pub fn b(&mut self);\n}\n";
    let st = ServerState::in_memory(&[("/p", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/p", "m.pyxis");
    let r = Request::new(
        RequestId::from(1),
        "textDocument/foldingRange".into(),
        serde_json::to_value(lsp_types::FoldingRangeParams {
            text_document: TextDocumentIdentifier { uri },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        })
        .unwrap(),
    );
    let ranges: Vec<serde_json::Value> =
        serde_json::from_value(st.handle_folding_range(r).result.unwrap()).unwrap();
    let has = |s: i64, e: i64| {
        ranges
            .iter()
            .any(|x| x["startLine"] == s && x["endLine"] == e)
    };
    assert!(has(0, 3), "multi-line use group: {ranges:?}");
    assert!(has(5, 10), "type body");
    assert!(has(6, 8), "nested vftable block");
    assert!(has(11, 13), "impl body");
}

#[test]
fn type_hierarchy_via_base_fields() {
    let src = "pub type PfxGameObject {\n    pub a: u64,\n}\npub type GameObject {\n    pub b: u64,\n}\npub type PhysicsGameObject {\n    #[base]\n    pub pfx: PfxGameObject,\n    #[base]\n    pub go: GameObject,\n}\n";
    let st = ServerState::in_memory(&[("/p", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/p", "m.pyxis");
    let prepare = |line: u32| -> Vec<lsp_types::TypeHierarchyItem> {
        let r = Request::new(
            RequestId::from(1),
            "textDocument/prepareTypeHierarchy".into(),
            serde_json::json!({"textDocument": {"uri": uri.as_str()}, "position": {"line": line, "character": 11}}),
        );
        serde_json::from_value(st.handle_prepare_type_hierarchy(r).result.unwrap()).unwrap()
    };
    let names = |v: serde_json::Value| -> Vec<String> {
        let items: Vec<serde_json::Value> = serde_json::from_value(v).unwrap();
        items
            .iter()
            .map(|i| i["name"].as_str().unwrap().to_string())
            .collect()
    };

    let physics = prepare(6)
        .into_iter()
        .next()
        .expect("prepare PhysicsGameObject");
    assert_eq!(physics.name, "PhysicsGameObject");
    // supertypes = the #[base] fields, in declaration order
    let sup = Request::new(
        RequestId::from(2),
        "typeHierarchy/supertypes".into(),
        serde_json::json!({"item": physics}),
    );
    assert_eq!(
        names(st.handle_type_hierarchy_supertypes(sup).result.unwrap()),
        ["PfxGameObject", "GameObject"]
    );
    // subtypes of a base = types that list it as #[base]
    let pfx = prepare(0)
        .into_iter()
        .next()
        .expect("prepare PfxGameObject");
    let sub = Request::new(
        RequestId::from(3),
        "typeHierarchy/subtypes".into(),
        serde_json::json!({"item": pfx}),
    );
    assert_eq!(
        names(st.handle_type_hierarchy_subtypes(sub).result.unwrap()),
        ["PhysicsGameObject"]
    );
}

#[test]
fn doc_links_navigate_via_hover_and_definition() {
    // Doc-comment cross-references resolve through hover + go-to-definition,
    // since Zed (and most editors) navigate doc links that way, not documentLink.
    let src = "/// See [Bar] and [docs](Bar).\npub type Foo {\n    pub x: u64,\n}\npub type Bar {\n    pub y: u64,\n}\n";
    let st = ServerState::in_memory(&[("/p", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/p", "m.pyxis");
    let in_bracket = src.lines().next().unwrap().find("Bar").unwrap() as u32 + 1;

    assert!(
        hover_text(&st, &uri, 0, in_bracket).contains("**type** `Bar`"),
        "hover on a doc link"
    );
    let def = def_uri(&st, &uri, 0, in_bracket);
    assert!(def.is_some(), "go-to-def on a doc link resolves");
    // the `[docs](Bar)` form resolves via its label (`docs`), the clickable part
    let in_label = src.lines().next().unwrap().find("docs").unwrap() as u32 + 1;
    assert!(
        hover_text(&st, &uri, 0, in_label).contains("**type** `Bar`"),
        "hover on [label](Bar) label"
    );
}

#[test]
fn doc_links_to_impl_methods_resolve_despite_project_errors() {
    // Regression: a doc link to an impl method (Type::method) must resolve even
    // when another type in the project has a semantic error — analyze() bails
    // out early there, and previously built the doc-link resolver without the
    // associated-function merge, breaking every impl-method link project-wide.
    let m = "/// [`Add`](RBILists::Add) appends.\npub type RBILists {\n    pub x: u64,\n}\nimpl RBILists {\n    #[address(0x1000)]\n    pub fn Add(&mut self);\n}\n";
    let errored = "#[size(0x99)]\npub type Broken {\n    pub a: u64,\n}\n";
    let st = ServerState::in_memory(&[("/p", 8, &[("m.pyxis", m), ("bad.pyxis", errored)])]);
    let uri = ServerState::document_uri("/p", "m.pyxis");
    let col = m.lines().next().unwrap().find("Add").unwrap() as u32 + 1;
    // Resolves to the impl method itself (not just the owning type), even with
    // an errored sibling.
    assert!(
        hover_text(&st, &uri, 0, col).contains("**fn** `Add`"),
        "impl-method doc link must resolve to the method despite an errored sibling"
    );
    let def = def_uri(&st, &uri, 0, col).expect("go-to-def");
    assert!(
        def.ends_with("m.pyxis"),
        "jumps to where Add is defined: {def}"
    );
}

#[test]
fn doc_link_covers_whole_link_and_targets_member() {
    let m = "/// see [`Add`](Foo::Add) here.\npub type Foo {\n    pub x: u64,\n}\nimpl Foo {\n    #[address(0x10)]\n    pub fn Add(&mut self);\n}\n";
    let st = ServerState::in_memory(&[("/p", 8, &[("m.pyxis", m)])]);
    let uri = ServerState::document_uri("/p", "m.pyxis");
    let line = m.lines().next().unwrap();
    // hover/def land on the whole link: the `[` at the start and inside `(Foo::Add)` both work
    let open = line.find("[`Add`]").unwrap() as u32;
    let in_path = line.find("Foo::Add").unwrap() as u32 + 1;
    assert!(
        hover_text(&st, &uri, 0, open).contains("**fn** `Add`"),
        "hover at link start"
    );
    assert!(
        hover_text(&st, &uri, 0, in_path).contains("**fn** `Add`"),
        "hover inside (target)"
    );
    assert!(
        def_uri(&st, &uri, 0, in_path).is_some(),
        "def from inside (target)"
    );
}

#[test]
fn doc_links_resolve_self_prefixed_paths() {
    // `Self::` doc links (issue #114) resolve against the enclosing type in
    // the editor too: hover and go-to-definition work on a field doc's
    // [`Self::member`] link, and inside an impl block's method docs.
    let src = "pub type Foo {\n    /// Pairs with [`Self::y`].\n    pub x: u64,\n    pub y: u64,\n}\nimpl Foo {\n    /// Uses [`Self::x`].\n    #[address(0x10)]\n    pub fn go(&mut self);\n}\n";
    let st = ServerState::in_memory(&[("/p", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/p", "m.pyxis");

    let field_col = src.lines().nth(1).unwrap().find("Self::y").unwrap() as u32 + 1;
    assert!(
        hover_text(&st, &uri, 1, field_col).contains("`y`"),
        "hover on [`Self::y`] in a field doc resolves to the sibling field"
    );
    assert!(
        def_uri(&st, &uri, 1, field_col).is_some(),
        "go-to-def on [`Self::y`] resolves"
    );

    let impl_col = src.lines().nth(6).unwrap().find("Self::x").unwrap() as u32 + 1;
    assert!(
        hover_text(&st, &uri, 6, impl_col).contains("`x`"),
        "hover on [`Self::x`] in an impl method doc resolves to the field"
    );
}
