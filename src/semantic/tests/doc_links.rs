//! Tests for rustdoc-style intra-doc link resolution.

use crate::{
    grammar::test_aliases::*,
    semantic::{
        SemanticBuilder, SemanticError,
        doc_links::{DocLinkMemberKind, DocLinkSyntax, DocLinkTarget, extract_links, scan_links},
    },
};

use super::util::*;
use pretty_assertions::assert_eq;

#[test]
fn extracts_shortcut_and_inline_links() {
    let doc = vec![
        " See [`Foo`] and [`Bar::baz`].".to_string(),
        " Also [the thing](Qux::quux) but not [external](https://example.com).".to_string(),
        " A code-labelled inline link [`Update`](Mode::Update) too.".to_string(),
        " And plain [text] is ignored.".to_string(),
    ];
    assert_eq!(
        extract_links(&doc),
        vec!["Foo", "Bar::baz", "Qux::quux", "Mode::Update"]
    );
}

#[test]
fn ignores_brackets_inside_code_spans() {
    // `[first, last)` is a code span — its `[` must not consume the `]`
    // from the real link [`Target`].
    let doc = vec![" Half-open range `[first, last)`: walks the [`Target`] list.".to_string()];
    assert_eq!(extract_links(&doc), vec!["Target"]);
}

#[test]
fn resolves_every_link_form() {
    // A module with one of each kind of link target: a type (with a method and
    // a field), an enum variant, a bitflags flag, a freestanding function, and
    // an extern value.
    let module =
        M::new()
            .with_definitions([
                ID::new(
                    (V::Public, "Target"),
                    TD::new([TS::field((V::Public, "m_value"), T::ident("u32"))
                        .with_attributes([A::address(0)])])
                    .with_attributes([A::size(4), A::align(4)]),
                ),
                ID::new(
                    (V::Public, "Mode"),
                    ED::new(T::ident("u32"), [ES::field("VarA")], []),
                ),
                ID::new(
                    (V::Public, "Flags"),
                    BFD::new(T::ident("u32"), [BFS::field("FlagX", int_literal(1))], []),
                ),
            ])
            .with_impls([FB::new(
                "Target",
                [F::new((V::Public, "do_it"), [Ar::const_self()])
                    .with_attributes([A::address(0x10)])],
            )])
            .with_functions([F::new((V::Public, "helper"), []).with_attributes([A::address(0x30)])])
            .with_extern_values([EV::new(
                V::Public,
                "global",
                T::ident("u32").mut_pointer(),
                [A::address(0x20)],
            )]);

    let state = build_state(&module, &IP::from("test")).unwrap();
    let scope = state.modules().get(&IP::from("test")).unwrap().scope();
    let resolver = state.doc_link_resolver();

    let member = |item: &str, name: &str, kind| DocLinkTarget::Member {
        item: IP::from(item),
        name: name.to_string(),
        kind,
    };

    assert_eq!(
        resolver.resolve(&scope, "Target"),
        Some(DocLinkTarget::Item(IP::from("test::Target")))
    );
    assert_eq!(
        resolver.resolve(&scope, "Target::do_it"),
        Some(member("test::Target", "do_it", DocLinkMemberKind::Method))
    );
    assert_eq!(
        resolver.resolve(&scope, "Target::m_value"),
        Some(member("test::Target", "m_value", DocLinkMemberKind::Field))
    );
    assert_eq!(
        resolver.resolve(&scope, "Mode::VarA"),
        Some(member("test::Mode", "VarA", DocLinkMemberKind::Variant))
    );
    assert_eq!(
        resolver.resolve(&scope, "Flags::FlagX"),
        Some(member("test::Flags", "FlagX", DocLinkMemberKind::Flag))
    );
    assert_eq!(
        resolver.resolve(&scope, "helper"),
        Some(DocLinkTarget::Function {
            module: IP::from("test"),
            name: "helper".to_string(),
        })
    );
    assert_eq!(
        resolver.resolve(&scope, "global"),
        Some(DocLinkTarget::ExternValue {
            module: IP::from("test"),
            name: "global".to_string(),
        })
    );
    assert_eq!(resolver.resolve(&scope, "Nonexistent"), None);
    assert_eq!(resolver.resolve(&scope, "Target::missing"), None);
}

#[test]
fn resolves_nested_constant_as_member() {
    // A constant nested inside a type resolves as a `Constant` *member* of its
    // parent (issue #103), not as a freestanding item: the Rust backend emits
    // it as an associated const, so a `Type::CONST` link imports the parent
    // type and rewrites to the associated-const form rather than importing a
    // nonexistent flattened free item.
    let module = M::new().with_definitions([ID::new(
        (V::Public, "Player"),
        TD::new([
            TS::item(ID::new(
                (V::Public, "STARTING_GOLD"),
                CD::new(T::ident("u32"), int_literal(500)),
            )),
            TS::field((V::Public, "health"), T::ident("i32")).with_attributes([A::address(0)]),
        ])
        .with_attributes([A::size(4), A::align(4)]),
    )]);

    let state = build_state(&module, &IP::from("test")).unwrap();
    let scope = state.modules().get(&IP::from("test")).unwrap().scope();
    let resolver = state.doc_link_resolver();

    assert_eq!(
        resolver.resolve(&scope, "Player::STARTING_GOLD"),
        Some(DocLinkTarget::Member {
            item: IP::from("test::Player"),
            name: "STARTING_GOLD".to_string(),
            kind: DocLinkMemberKind::Constant,
        })
    );
    // A nested constant has no freestanding path to link to by bare name, so it
    // must not resolve as an item (which would emit an unresolvable import).
    assert_eq!(resolver.resolve(&scope, "STARTING_GOLD"), None);
    assert_eq!(resolver.resolve(&scope, "Player::MISSING"), None);
}

#[test]
fn scan_links_tags_syntax_with_precise_regions() {
    // The shared scanner backs the compiler, the Rust backend's link rewriting,
    // and the LSP, so each link's syntax and `path_region` must be exact.
    let line = "a [`Foo::bar`] b [Baz] c [lbl](Qux::quux) d";
    let links = scan_links(line);

    let summary: Vec<(DocLinkSyntax, &str)> =
        links.iter().map(|l| (l.syntax, l.path.as_str())).collect();
    assert_eq!(
        summary,
        vec![
            (DocLinkSyntax::CodeShortcut, "Foo::bar"),
            (DocLinkSyntax::PlainShortcut, "Baz"),
            (DocLinkSyntax::Inline, "Qux::quux"),
        ]
    );

    // `path_region` slices to exactly the path text (no backticks / label).
    for l in &links {
        assert_eq!(&line[l.path_region.0..l.path_region.1], l.path);
    }
    // An inline link's label region is the bracket text, distinct from its dest.
    let inline = links
        .iter()
        .find(|l| l.syntax == DocLinkSyntax::Inline)
        .unwrap();
    assert_eq!(&line[inline.label_region.0..inline.label_region.1], "lbl");
}

#[test]
fn resolves_a_type_in_a_sibling_module() {
    // `Other` lives in a sibling module and isn't imported; a bare-name link
    // still resolves to it crate-wide (the Rust backend imports it).
    let other = M::new().with_definitions([ID::new(
        (V::Public, "Other"),
        TD::new([]).with_attributes([A::size(0), A::align(1)]),
    )]);
    let referencing = M::new();

    let mut builder = SemanticBuilder::new(pointer_size());
    builder.add_module(&other, &IP::from("other")).unwrap();
    builder
        .add_module(&referencing, &IP::from("referencing"))
        .unwrap();
    let state = builder.build().unwrap();

    let scope = state
        .modules()
        .get(&IP::from("referencing"))
        .unwrap()
        .scope();
    assert_eq!(
        state.doc_link_resolver().resolve(&scope, "Other"),
        Some(DocLinkTarget::Item(IP::from("other::Other")))
    );
}

#[test]
fn errors_on_unresolved_doc_link() {
    let module = M::new().with_functions([F::new((V::Public, "f"), [])
        .with_attributes([A::address(0x40)])
        .with_doc_comments(vec![" See [`Nonexistent`] for details.".to_string()])]);

    let err = build_state(&module, &IP::from("test")).unwrap_err();
    assert!(
        matches!(&err, SemanticError::DocLinkNotFound { path, .. } if path == "Nonexistent"),
        "unexpected error: {err:?}"
    );
}
