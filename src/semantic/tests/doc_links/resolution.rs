//! Tests for resolving doc links to their targets.

use super::*;
use pretty_assertions::assert_eq;

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
