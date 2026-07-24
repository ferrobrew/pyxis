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

/// Parse a written link path for `resolve()`.
fn segs(s: &str) -> crate::semantic::doc_links::DocLinkPath {
    crate::semantic::doc_links::DocLinkPath::parse(s)
}

#[test]
fn extracts_shortcut_and_inline_links() {
    let doc = vec![
        " See [`Foo`] and [`Bar::baz`].".to_string(),
        " Also [the thing](Qux::quux) but not [external](https://example.com).".to_string(),
        " A code-labelled inline link [`Update`](Mode::Update) too.".to_string(),
        " And plain [text] is ignored.".to_string(),
    ];
    let texts: Vec<String> = extract_links(&doc)
        .into_iter()
        .map(|(text, _)| text)
        .collect();
    assert_eq!(texts, vec!["Foo", "Bar::baz", "Qux::quux", "Mode::Update"]);
}

#[test]
fn ignores_brackets_inside_code_spans() {
    // `[first, last)` is a code span — its `[` must not consume the `]`
    // from the real link [`Target`].
    let doc = vec![" Half-open range `[first, last)`: walks the [`Target`] list.".to_string()];
    let texts: Vec<String> = extract_links(&doc)
        .into_iter()
        .map(|(text, _)| text)
        .collect();
    assert_eq!(texts, vec!["Target"]);
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
            .with_definitions([ID::new(
                (V::Public, "global"),
                EVD::new(T::ident("u32").mut_pointer()).with_attributes([A::address(0x20)]),
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
        resolver.resolve(&scope, &segs("Target"), None),
        Some(DocLinkTarget::Item(IP::from("test::Target")))
    );
    assert_eq!(
        resolver.resolve(&scope, &segs("Target::do_it"), None),
        Some(member("test::Target", "do_it", DocLinkMemberKind::Method))
    );
    assert_eq!(
        resolver.resolve(&scope, &segs("Target::m_value"), None),
        Some(member("test::Target", "m_value", DocLinkMemberKind::Field))
    );
    assert_eq!(
        resolver.resolve(&scope, &segs("Mode::VarA"), None),
        Some(member("test::Mode", "VarA", DocLinkMemberKind::Variant))
    );
    assert_eq!(
        resolver.resolve(&scope, &segs("Flags::FlagX"), None),
        Some(member("test::Flags", "FlagX", DocLinkMemberKind::Flag))
    );
    assert_eq!(
        resolver.resolve(&scope, &segs("helper"), None),
        Some(DocLinkTarget::Function {
            module: IP::from("test"),
            name: "helper".to_string(),
        })
    );
    assert_eq!(
        resolver.resolve(&scope, &segs("global"), None),
        Some(DocLinkTarget::ExternValue {
            module: IP::from("test"),
            name: "global".to_string(),
        })
    );
    assert_eq!(resolver.resolve(&scope, &segs("Nonexistent"), None), None);
    assert_eq!(
        resolver.resolve(&scope, &segs("Target::missing"), None),
        None
    );
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
        resolver.resolve(&scope, &segs("Player::STARTING_GOLD"), None),
        Some(DocLinkTarget::Member {
            item: IP::from("test::Player"),
            name: "STARTING_GOLD".to_string(),
            kind: DocLinkMemberKind::Constant,
        })
    );
    // A nested constant has no freestanding path to link to by bare name, so it
    // must not resolve as an item (which would emit an unresolvable import).
    assert_eq!(resolver.resolve(&scope, &segs("STARTING_GOLD"), None), None);
    assert_eq!(
        resolver.resolve(&scope, &segs("Player::MISSING"), None),
        None
    );
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
        state
            .doc_link_resolver()
            .resolve(&scope, &segs("Other"), None),
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

// --- Self:: resolution tests ---

#[test]
fn resolves_self_member() {
    // A type with a field and method. `Self::field` and `Self::method` resolve
    // as members of the enclosing type.
    let module =
        M::new()
            .with_definitions([ID::new(
                (V::Public, "Widget"),
                TD::new([TS::field((V::Public, "m_value"), T::ident("u32"))
                    .with_attributes([A::address(0)])])
                .with_attributes([A::size(4), A::align(4)]),
            )])
            .with_impls([FB::new(
                "Widget",
                [F::new((V::Public, "do_it"), [Ar::const_self()])
                    .with_attributes([A::address(0x10)])],
            )]);

    let state = build_state(&module, &IP::from("test")).unwrap();
    let scope = state.modules().get(&IP::from("test")).unwrap().scope();
    let resolver = state.doc_link_resolver();
    let enclosing = IP::from("test::Widget");

    assert_eq!(
        resolver.resolve(&scope, &segs("Self::m_value"), Some(&enclosing)),
        Some(DocLinkTarget::Member {
            item: IP::from("test::Widget"),
            name: "m_value".to_string(),
            kind: DocLinkMemberKind::Field,
        })
    );
    assert_eq!(
        resolver.resolve(&scope, &segs("Self::do_it"), Some(&enclosing)),
        Some(DocLinkTarget::Member {
            item: IP::from("test::Widget"),
            name: "do_it".to_string(),
            kind: DocLinkMemberKind::Method,
        })
    );
}

#[test]
fn resolves_self_member_without_enclosing_type() {
    // `Self::member` at module scope (no enclosing type) returns None.
    let module = M::new().with_definitions([ID::new(
        (V::Public, "Widget"),
        TD::new([
            TS::field((V::Public, "m_value"), T::ident("u32")).with_attributes([A::address(0)])
        ])
        .with_attributes([A::size(4), A::align(4)]),
    )]);

    let state = build_state(&module, &IP::from("test")).unwrap();
    let scope = state.modules().get(&IP::from("test")).unwrap().scope();
    let resolver = state.doc_link_resolver();

    assert_eq!(resolver.resolve(&scope, &segs("Self::m_value"), None), None);
}

#[test]
fn resolves_self_item() {
    // Bare `Self` resolves to the enclosing type as a whole item.
    let module = M::new().with_definitions([ID::new(
        (V::Public, "Widget"),
        TD::new([
            TS::field((V::Public, "m_value"), T::ident("u32")).with_attributes([A::address(0)])
        ])
        .with_attributes([A::size(4), A::align(4)]),
    )]);

    let state = build_state(&module, &IP::from("test")).unwrap();
    let scope = state.modules().get(&IP::from("test")).unwrap().scope();
    let resolver = state.doc_link_resolver();
    let enclosing = IP::from("test::Widget");

    assert_eq!(
        resolver.resolve(&scope, &segs("Self"), Some(&enclosing)),
        Some(DocLinkTarget::Item(IP::from("test::Widget")))
    );
    // `Self` with no enclosing type returns None.
    assert_eq!(resolver.resolve(&scope, &segs("Self"), None), None);
}

#[test]
fn resolves_self_nested_type_member() {
    // A type with a nested type that has a member. `Self::NestedType::member`
    // resolves as a member of the nested type.
    let module = M::new().with_definitions([ID::new(
        (V::Public, "Outer"),
        TD::new([
            TS::item(ID::new(
                (V::Public, "Inner"),
                TD::new([TS::field((V::Public, "inner_field"), T::ident("u32"))
                    .with_attributes([A::address(0)])])
                .with_attributes([A::size(4), A::align(4)]),
            )),
            TS::field((V::Public, "outer_field"), T::ident("u32")).with_attributes([A::address(0)]),
        ])
        .with_attributes([A::size(8), A::align(4)]),
    )]);

    let state = build_state(&module, &IP::from("test")).unwrap();
    let scope = state.modules().get(&IP::from("test")).unwrap().scope();
    let resolver = state.doc_link_resolver();
    let enclosing = IP::from("test::Outer");

    assert_eq!(
        resolver.resolve(&scope, &segs("Self::Inner::inner_field"), Some(&enclosing)),
        Some(DocLinkTarget::Member {
            item: IP::from("test::Outer::Inner"),
            name: "inner_field".to_string(),
            kind: DocLinkMemberKind::Field,
        })
    );
}

// --- Module-qualified function/extern-value tests ---

#[test]
fn resolves_qualified_function() {
    // A function in module `a` referenced from module `b` via `a::func`.
    let module_a = M::new().with_functions([
        F::new((V::Public, "shared_func"), []).with_attributes([A::address(0x10)])
    ]);
    let module_b = M::new();

    let mut builder = SemanticBuilder::new(pointer_size());
    builder.add_module(&module_a, &IP::from("a")).unwrap();
    builder.add_module(&module_b, &IP::from("b")).unwrap();
    let state = builder.build().unwrap();

    let scope = state.modules().get(&IP::from("b")).unwrap().scope();
    let resolver = state.doc_link_resolver();

    assert_eq!(
        resolver.resolve(&scope, &segs("a::shared_func"), None),
        Some(DocLinkTarget::Function {
            module: IP::from("a"),
            name: "shared_func".to_string(),
        })
    );
}

#[test]
fn resolves_qualified_extern_value() {
    // An extern value in module `a` referenced from module `b` via `a::global`.
    let module_a = M::new().with_definitions([ID::new(
        (V::Public, "global"),
        EVD::new(T::ident("u32").mut_pointer()).with_attributes([A::address(0x20)]),
    )]);
    let module_b = M::new();

    let mut builder = SemanticBuilder::new(pointer_size());
    builder.add_module(&module_a, &IP::from("a")).unwrap();
    builder.add_module(&module_b, &IP::from("b")).unwrap();
    let state = builder.build().unwrap();

    let scope = state.modules().get(&IP::from("b")).unwrap().scope();
    let resolver = state.doc_link_resolver();

    assert_eq!(
        resolver.resolve(&scope, &segs("a::global"), None),
        Some(DocLinkTarget::ExternValue {
            module: IP::from("a"),
            name: "global".to_string(),
        })
    );
}

#[test]
fn resolves_qualified_function_not_found() {
    // Module doesn't exist or function doesn't exist in that module.
    let module_a = M::new().with_functions([
        F::new((V::Public, "shared_func"), []).with_attributes([A::address(0x10)])
    ]);
    let module_b = M::new();

    let mut builder = SemanticBuilder::new(pointer_size());
    builder.add_module(&module_a, &IP::from("a")).unwrap();
    builder.add_module(&module_b, &IP::from("b")).unwrap();
    let state = builder.build().unwrap();

    let scope = state.modules().get(&IP::from("b")).unwrap().scope();
    let resolver = state.doc_link_resolver();

    // Nonexistent module
    assert_eq!(
        resolver.resolve(&scope, &segs("nonexistent::func"), None),
        None
    );
    // Existing module, nonexistent function
    assert_eq!(
        resolver.resolve(&scope, &segs("a::missing_func"), None),
        None
    );
}

// --- validate() Self:: tests ---

#[test]
fn validate_accepts_self_doc_link() {
    // A type with a valid `Self::` doc-link in its doc comment should pass
    // validation without error.
    let module =
        M::new()
            .with_definitions([ID::new(
                (V::Public, "Widget"),
                TD::new([TS::field((V::Public, "m_value"), T::ident("u32"))
                    .with_attributes([A::address(0)])])
                .with_attributes([A::size(4), A::align(4)]),
            )
            .with_doc_comments(vec![" See [`Self::m_value`] for the value.".to_string()])])
            .with_impls([FB::new(
                "Widget",
                [F::new((V::Public, "do_it"), [Ar::const_self()])
                    .with_attributes([A::address(0x10)])],
            )]);

    // Should succeed — no DocLinkNotFound error.
    build_state(&module, &IP::from("test")).unwrap();
}

#[test]
fn validate_accepts_self_in_nested_item() {
    // A type with a nested enum whose variant has a `Self::` doc-link should
    // pass validation.
    let module = M::new().with_definitions([ID::new(
        (V::Public, "Outer"),
        TD::new([
            TS::item(ID::new(
                (V::Public, "Inner"),
                ED::new(
                    T::ident("u32"),
                    [ES::field("VariantA")
                        .with_doc_comments(vec![" See [`Self::VariantA`].".to_string()])],
                    [],
                ),
            )),
            TS::field((V::Public, "outer_field"), T::ident("u32")).with_attributes([A::address(0)]),
        ])
        .with_attributes([A::size(8), A::align(4)]),
    )]);

    // Should succeed — `Self::VariantA` inside the nested enum refers to
    // the nested enum's own variant.
    build_state(&module, &IP::from("test")).unwrap();
}

#[test]
fn validate_rejects_invalid_link_in_nested_item() {
    // A type with a nested enum whose variant has an invalid doc-link should
    // fail validation.
    let module = M::new().with_definitions([ID::new(
        (V::Public, "Outer"),
        TD::new([
            TS::item(ID::new(
                (V::Public, "Inner"),
                ED::new(
                    T::ident("u32"),
                    [ES::field("VariantA")
                        .with_doc_comments(vec![" See [`Nonexistent`].".to_string()])],
                    [],
                ),
            )),
            TS::field((V::Public, "outer_field"), T::ident("u32")).with_attributes([A::address(0)]),
        ])
        .with_attributes([A::size(8), A::align(4)]),
    )]);

    let err = build_state(&module, &IP::from("test")).unwrap_err();
    assert!(
        matches!(&err, SemanticError::DocLinkNotFound { path, .. } if path == "Nonexistent"),
        "unexpected error: {err:?}"
    );
}

// --- DocLinkPath parsing ---

#[test]
fn parses_doc_link_paths() {
    use crate::{grammar::ItemPathSegment, semantic::doc_links::DocLinkPath};
    let seg = ItemPathSegment::from;

    assert_eq!(
        DocLinkPath::parse("Foo"),
        DocLinkPath {
            self_prefixed: false,
            segments: vec![seg("Foo")],
        }
    );
    assert_eq!(
        DocLinkPath::parse("a::b::c"),
        DocLinkPath {
            self_prefixed: false,
            segments: vec![seg("a"), seg("b"), seg("c")],
        }
    );
    assert_eq!(
        DocLinkPath::parse("Self"),
        DocLinkPath {
            self_prefixed: true,
            segments: vec![],
        }
    );
    assert_eq!(
        DocLinkPath::parse("Self::member"),
        DocLinkPath {
            self_prefixed: true,
            segments: vec![seg("member")],
        }
    );
    // `Self` only counts as a prefix in leading position.
    assert_eq!(
        DocLinkPath::parse("Foo::Self"),
        DocLinkPath {
            self_prefixed: false,
            segments: vec![seg("Foo"), seg("Self")],
        }
    );
}

// --- Enclosing-type semantics of the doc walk ---

#[test]
fn resolves_self_in_nested_constant_doc_as_parent() {
    // A constant nested in a type emits as an associated const inside the
    // parent's `impl` block, where rustdoc resolves `Self` as the parent type —
    // so `Self::health` in its doc must resolve to the parent's field.
    let module = M::new().with_definitions([ID::new(
        (V::Public, "Player"),
        TD::new([
            TS::item(
                ID::new(
                    (V::Public, "STARTING_GOLD"),
                    CD::new(T::ident("u32"), int_literal(500)),
                )
                .with_doc_comments(vec![" Initial value of [`Self::health`].".to_string()]),
            ),
            TS::field((V::Public, "health"), T::ident("i32")).with_attributes([A::address(0)]),
        ])
        .with_attributes([A::size(4), A::align(4)]),
    )]);

    let state = build_state(&module, &IP::from("test")).unwrap();
    let module_links = state.module_doc_links(&IP::from("test"));
    let link = module_links
        .iter()
        .find(|l| l.text == "Self::health")
        .expect("Self::health link recorded");
    assert_eq!(
        link.target,
        DocLinkTarget::Member {
            item: IP::from("test::Player"),
            name: "health".to_string(),
            kind: DocLinkMemberKind::Field,
        }
    );
}

#[test]
fn validate_rejects_self_on_module_level_constant() {
    // A module-level constant's emitted docs have no `Self` to refer to, so a
    // `Self` link in them must fail validation rather than silently producing
    // a link rustdoc can't resolve.
    let module = M::new().with_definitions([ID::new(
        (V::Public, "MAX_PLAYERS"),
        CD::new(T::ident("u32"), int_literal(8)),
    )
    .with_doc_comments(vec![" See [`Self`].".to_string()])]);

    let err = build_state(&module, &IP::from("test")).unwrap_err();
    assert!(
        matches!(&err, SemanticError::DocLinkNotFound { path, .. } if path == "Self"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn validate_rejects_self_on_type_alias() {
    let module =
        M::new().with_definitions([ID::new((V::Public, "Handle"), TAD::new(T::ident("u32")))
            .with_doc_comments(vec![" See [`Self`].".to_string()])]);

    let err = build_state(&module, &IP::from("test")).unwrap_err();
    assert!(
        matches!(&err, SemanticError::DocLinkNotFound { path, .. } if path == "Self"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn validate_rejects_invalid_link_in_nested_type_field_doc() {
    // Docs on a *nested type's* fields are part of the walk too — a bad link
    // there must fail validation just like one on a top-level type's field.
    let module = M::new().with_definitions([ID::new(
        (V::Public, "Outer"),
        TD::new([
            TS::item(ID::new(
                (V::Public, "Inner"),
                TD::new([TS::field((V::Public, "inner_field"), T::ident("u32"))
                    .with_attributes([A::address(0)])
                    .with_doc_comments(vec![" See [`Nonexistent`].".to_string()])])
                .with_attributes([A::size(4), A::align(4)]),
            )),
            TS::field((V::Public, "outer_field"), T::ident("u32")).with_attributes([A::address(0)]),
        ])
        .with_attributes([A::size(8), A::align(4)]),
    )]);

    let err = build_state(&module, &IP::from("test")).unwrap_err();
    assert!(
        matches!(&err, SemanticError::DocLinkNotFound { path, .. } if path == "Nonexistent"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn resolves_self_in_nested_type_field_doc() {
    // `Self::` in a nested type's field doc refers to the nested type itself.
    let module = M::new().with_definitions([ID::new(
        (V::Public, "Outer"),
        TD::new([
            TS::item(ID::new(
                (V::Public, "Inner"),
                TD::new([
                    TS::field((V::Public, "first"), T::ident("u32"))
                        .with_attributes([A::address(0)])
                        .with_doc_comments(vec![" Pairs with [`Self::second`].".to_string()]),
                    TS::field((V::Public, "second"), T::ident("u32"))
                        .with_attributes([A::address(4)]),
                ])
                .with_attributes([A::size(8), A::align(4)]),
            )),
            TS::field((V::Public, "outer_field"), T::ident("u32")).with_attributes([A::address(0)]),
        ])
        .with_attributes([A::size(12), A::align(4)]),
    )]);

    let state = build_state(&module, &IP::from("test")).unwrap();
    let link = state
        .module_doc_links(&IP::from("test"))
        .iter()
        .find(|l| l.text == "Self::second")
        .expect("Self::second link recorded");
    assert_eq!(
        link.target,
        DocLinkTarget::Member {
            item: IP::from("test::Outer::Inner"),
            name: "second".to_string(),
            kind: DocLinkMemberKind::Field,
        }
    );
}

#[test]
fn prefers_same_module_candidate_from_type_scoped_docs() {
    // Two modules each define a type named `Context`; only module `b`'s has
    // the field. A doc on a member of another type in `b` resolves under the
    // *augmented* type scope (the type's own path is prepended for nested-item
    // references), so the resolver must still anchor same-module preference at
    // the module — not at whatever sits first in the scope list. Regression:
    // the alphabetically-first `a::Context` (no such field) was picked and the
    // link failed to resolve.
    let module_a = M::new().with_definitions([ID::new(
        (V::Public, "Context"),
        TD::new([
            TS::field((V::Public, "unrelated"), T::ident("u32")).with_attributes([A::address(0)])
        ])
        .with_attributes([A::size(4), A::align(4)]),
    )]);
    let module_b = M::new()
        .with_definitions([
            ID::new(
                (V::Public, "Context"),
                TD::new([TS::field((V::Public, "latch"), T::ident("u32"))
                    .with_attributes([A::address(0)])])
                .with_attributes([A::size(4), A::align(4)]),
            ),
            ID::new(
                (V::Public, "Handle"),
                TD::new([TS::field((V::Public, "data"), T::ident("u32"))
                    .with_attributes([A::address(0)])])
                .with_attributes([A::size(4), A::align(4)]),
            ),
        ])
        .with_impls([FB::new(
            "Handle",
            [F::new((V::Public, "consume"), [Ar::mut_self()])
                .with_attributes([A::address(0x10)])
                .with_doc_comments(vec![
                    " Short-circuits on [`latch`](Context::latch).".to_string(),
                ])],
        )]);

    let mut builder = SemanticBuilder::new(pointer_size());
    builder.add_module(&module_a, &IP::from("a")).unwrap();
    builder.add_module(&module_b, &IP::from("b")).unwrap();
    let state = builder
        .build()
        .expect("Context::latch resolves to b::Context");

    let link = state
        .module_doc_links(&IP::from("b"))
        .iter()
        .find(|l| l.text == "Context::latch")
        .expect("link recorded");
    assert_eq!(
        link.target,
        DocLinkTarget::Member {
            item: IP::from("b::Context"),
            name: "latch".to_string(),
            kind: DocLinkMemberKind::Field,
        }
    );
}
