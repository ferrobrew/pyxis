use lsp_server::{Request, RequestId};
use lsp_types::{Position, TextDocumentIdentifier, TextDocumentPositionParams};
use pyxis_lsp::state::ServerState;

use crate::helpers::*;

// Two+ projects under one workspace root can have files at the SAME relative
// path (e.g. `world/shared.pyxis`), hence the same module path. Resolution must
// stay within the requesting file's project, not match a same-named file in a
// sibling project (the JustCause2/MadMax `physics_game_object` bug).
#[test]
fn cross_project_resolution_stays_in_project() {
    // projA defines Foo at world/shared.pyxis; the decoys have world/shared.pyxis
    // too, with the same module path but WITHOUT Foo.
    let consumer_src = "use world::shared::Foo;\n\npub type C {\n    pub f: Foo,\n}\n";
    let decoy: &[(&str, &str)] = &[("world/shared.pyxis", "pub type Bar {\n    pub y: u64,\n}\n")];
    let st = ServerState::in_memory(&[
        (
            "/projA",
            8,
            &[
                ("world/shared.pyxis", "pub type Foo {\n    pub x: u64,\n}\n"),
                ("consumer.pyxis", consumer_src),
            ],
        ),
        ("/projB", 8, decoy),
        ("/projC", 8, decoy),
        ("/projD", 8, decoy),
    ]);
    let consumer = ServerState::document_uri("/projA", "consumer.pyxis");
    // `pub f: Foo,` is line 3 (0-indexed).
    let col = consumer_src.lines().nth(3).unwrap().find("Foo").unwrap() as u32;

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
fn prepare_rename_validates_target() {
    let src = "pub type Foo {\n    pub v: Foo,\n    pub n: u32,\n}\n";
    let st = ServerState::in_memory(&[("/p", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/p", "m.pyxis");
    let pr = |l: u32, c: u32| {
        let r = Request::new(
            RequestId::from(1),
            "textDocument/prepareRename".into(),
            serde_json::to_value(TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: Position {
                    line: l,
                    character: c,
                },
            })
            .unwrap(),
        );
        st.handle_prepare_rename(r).result.unwrap()
    };
    // definition name and a reference are renameable, with the identifier range + placeholder
    let def = pr(0, 10);
    assert_eq!(def["placeholder"], "Foo");
    assert_eq!(def["range"]["start"]["character"], 9);
    assert_eq!(pr(1, 11)["placeholder"], "Foo");
    // builtins and non-identifier positions are not renameable
    assert!(pr(2, 11).is_null(), "u32 builtin must not be renameable");
    assert!(pr(1, 2).is_null(), "whitespace must not be renameable");
}

#[test]
fn rename_members_fields_and_methods() {
    let src = "pub type Foo {\n    pub bar: u64,\n    vftable {\n        pub fn vf(&mut self);\n    },\n}\nimpl Foo {\n    pub fn doit(&mut self);\n}\n";
    let st = ServerState::in_memory(&[("/p", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/p", "m.pyxis");
    let col = |l: usize, n: &str| src.lines().nth(l).unwrap().find(n).unwrap() as u32;
    let edits = |l: u32, c: u32, name: &str| -> usize {
        let r = Request::new(
            RequestId::from(1),
            "textDocument/rename".into(),
            serde_json::to_value(lsp_types::RenameParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position {
                        line: l,
                        character: c,
                    },
                },
                new_name: name.to_string(),
                work_done_progress_params: Default::default(),
            })
            .unwrap(),
        );
        let we: lsp_types::WorkspaceEdit =
            serde_json::from_value(st.handle_rename(r).result.unwrap()).unwrap();
        we.changes
            .map(|c| c.values().map(|v| v.len()).sum())
            .unwrap_or(0)
    };
    let prepare = |l: u32, c: u32| {
        let r = Request::new(
            RequestId::from(1),
            "textDocument/prepareRename".into(),
            serde_json::to_value(TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: Position {
                    line: l,
                    character: c,
                },
            })
            .unwrap(),
        );
        st.handle_prepare_rename(r).result.unwrap()
    };

    // A field is renameable (prepare returns its name) and rewrites its single declaration.
    assert_eq!(prepare(1, col(1, "bar"))["placeholder"], "bar");
    assert_eq!(edits(1, col(1, "bar"), "baz"), 1, "field rename");
    // vftable and impl methods too.
    assert!(!prepare(3, col(3, "vf")).is_null());
    assert_eq!(edits(3, col(3, "vf"), "vf2"), 1, "vftable method rename");
    assert!(!prepare(7, col(7, "doit")).is_null());
    assert_eq!(edits(7, col(7, "doit"), "doit2"), 1, "impl method rename");
}

#[test]
fn rename_reaches_doc_comment_links() {
    let src = "/// See [Foo], [`DoDraw`](Foo::DoDraw) and [`Add`](Foo::Add).\npub type Foo {\n    vftable {\n        pub fn DoDraw(&mut self);\n    },\n}\nimpl Foo {\n    #[address(0x10)] pub fn Add(&mut self);\n}\n";
    let st = ServerState::in_memory(&[("/p", 8, &[("a.pyxis", src)])]);
    let uri = ServerState::document_uri("/p", "a.pyxis");
    let line = |n: &str| src.lines().position(|l| l.contains(n)).unwrap() as u32;
    let col = |l: u32, n: &str| src.lines().nth(l as usize).unwrap().find(n).unwrap() as u32;
    let count = |l: u32, c: u32, name: &str| -> usize {
        let r = Request::new(
            RequestId::from(1),
            "textDocument/rename".into(),
            serde_json::to_value(lsp_types::RenameParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position {
                        line: l,
                        character: c,
                    },
                },
                new_name: name.to_string(),
                work_done_progress_params: Default::default(),
            })
            .unwrap(),
        );
        let we: lsp_types::WorkspaceEdit =
            serde_json::from_value(st.handle_rename(r).result.unwrap()).unwrap();
        we.changes
            .map(|c| c.values().map(|v| v.len()).sum())
            .unwrap_or(0)
    };
    // Type: def + impl target + three doc-link refs ([Foo], Foo::DoDraw, Foo::Add).
    let lf = line("pub type");
    assert_eq!(
        count(lf, col(lf, "Foo"), "Bar"),
        5,
        "type rename reaches doc links"
    );
    // Member: declaration + the doc-link's label and path.
    let ld = line("pub fn DoDraw");
    assert_eq!(
        count(ld, col(ld, "DoDraw"), "DoDraw2"),
        3,
        "vftable rename reaches doc links"
    );
    let la = line("pub fn Add");
    assert_eq!(
        count(la, col(la, "Add"), "Add2"),
        3,
        "impl method rename reaches doc links"
    );
}

#[test]
fn rename_does_not_corrupt_doc_link_prose_labels() {
    // A label that merely *contains* the name (prose) must not be rewritten;
    // only the (path) and an exact-echo label should change.
    let src = "/// [the Foo struct](Foo) and [`Foo`](Foo).\npub type Foo {\n    pub x: u64,\n}\n";
    let st = ServerState::in_memory(&[("/p", 8, &[("a.pyxis", src)])]);
    let uri = ServerState::document_uri("/p", "a.pyxis");
    let lf = src.lines().position(|l| l.contains("pub type")).unwrap() as u32;
    let col = src.lines().nth(lf as usize).unwrap().find("Foo").unwrap() as u32;
    let r = Request::new(
        RequestId::from(1),
        "textDocument/rename".into(),
        serde_json::to_value(lsp_types::RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: Position {
                    line: lf,
                    character: col,
                },
            },
            new_name: "Bar".to_string(),
            work_done_progress_params: Default::default(),
        })
        .unwrap(),
    );
    let we: lsp_types::WorkspaceEdit =
        serde_json::from_value(st.handle_rename(r).result.unwrap()).unwrap();
    let cols: Vec<u32> = we
        .changes
        .unwrap()
        .values()
        .flatten()
        .filter(|e| e.range.start.line == 0)
        .map(|e| e.range.start.character)
        .collect();
    // The "Foo" inside "the Foo struct" is at column 9 — it must NOT be edited.
    assert!(
        !cols.contains(&9),
        "prose label 'the Foo struct' must not be rewritten"
    );
}

#[test]
#[allow(clippy::mutable_key_type)] // lsp_types::Uri map key is fine here
fn rename_type_updates_use_leaf_and_field_across_files() {
    // Exercises the source-map-backed symbol_occurrences: a use-statement leaf
    // and a field reference in another file must both be renamed.
    let st = ServerState::in_memory(&[(
        "/p",
        8,
        &[
            ("a.pyxis", "pub type Foo {\n    pub x: u64,\n}\n"),
            (
                "b.pyxis",
                "use a::Foo;\npub type Bar {\n    pub f: Foo,\n}\n",
            ),
        ],
    )]);
    let a = ServerState::document_uri("/p", "a.pyxis");
    let col = "pub type ".len() as u32; // the `Foo` in a.pyxis
    let r = Request::new(
        RequestId::from(1),
        "textDocument/rename".into(),
        serde_json::to_value(lsp_types::RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: a.clone() },
                position: Position {
                    line: 0,
                    character: col,
                },
            },
            new_name: "Renamed".to_string(),
            work_done_progress_params: Default::default(),
        })
        .unwrap(),
    );
    let we: lsp_types::WorkspaceEdit =
        serde_json::from_value(st.handle_rename(r).result.unwrap()).unwrap();
    let changes = we.changes.unwrap();
    let total: usize = changes.values().map(|v| v.len()).sum();
    // Definition (a.pyxis) + use leaf (b.pyxis) + field type (b.pyxis) = 3.
    assert_eq!(
        total, 3,
        "rename should touch def + use leaf + field ref; got {total}: {changes:?}"
    );
    let b = ServerState::document_uri("/p", "b.pyxis");
    assert_eq!(
        changes.get(&b).map(|v| v.len()),
        Some(2),
        "both b.pyxis occurrences"
    );
}

#[test]
#[allow(clippy::mutable_key_type)] // lsp_types::Uri map key is fine here
fn rename_type_updates_splice_for_clause() {
    // Regression guard: a type referenced in a splice `for <Type>` clause must
    // be renamed (source map must cover splices, like find_reference_at).
    let src = "pub type Widget {\n    pub id: u32,\n}\n#[cfg(backend = \"rust\")]\nepilogue for Widget r#\"x\"#;\n";
    let st = ServerState::in_memory(&[("/p", 8, &[("a.pyxis", src)])]);
    let uri = ServerState::document_uri("/p", "a.pyxis");
    let col = "pub type ".len() as u32;
    let r = Request::new(
        RequestId::from(1),
        "textDocument/rename".into(),
        serde_json::to_value(lsp_types::RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: Position {
                    line: 0,
                    character: col,
                },
            },
            new_name: "Gadget".to_string(),
            work_done_progress_params: Default::default(),
        })
        .unwrap(),
    );
    let we: lsp_types::WorkspaceEdit =
        serde_json::from_value(st.handle_rename(r).result.unwrap()).unwrap();
    let total: usize = we.changes.unwrap().values().map(|v| v.len()).sum();
    // Definition + the `for Widget` clause = 2.
    assert_eq!(
        total, 2,
        "rename should update def + splice `for` clause; got {total}"
    );
}
