//! Tests for doc link validation during semantic analysis.

use super::*;
use pretty_assertions::assert_eq;

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
