//! Tests for freestanding function resolution.

use crate::{
    grammar::test_aliases::*,
    semantic::{error::SemanticError, types::test_aliases::*},
    span::{ItemLocation, StripLocations},
};

use super::util::*;
use pretty_assertions::assert_eq;

#[test]
fn can_resolve_freestanding_functions() {
    let ast = M::new()
        .with_definitions([ID::new(
            (V::Public, "TestEnum"),
            ED::new(T::ident("u32"), [ES::field("None")], []),
        )])
        .with_impls([FB::new(
            "TestEnum",
            [F::new((V::Public, "test"), []).with_attributes([A::address(0x123)])],
        )])
        .with_functions([
            F::new((V::Public, "freestanding"), []).with_attributes([A::address(0x456)]),
            F::new(
                (V::Private, "another_freestanding"),
                [Ar::named("arg1", T::ident("i32"))],
            )
            .with_attributes([A::address(0x789)])
            .with_return_type(T::ident("i32")),
        ]);

    let test_path = IP::from("test");
    let state = build_state(&ast, &test_path).unwrap();
    let module = state.modules().get(&test_path).unwrap();

    assert_eq!(module.functions().len(), 2);

    // Check first freestanding function
    let func1 = &module.functions()[0];
    assert_eq!(func1.name, "freestanding");
    assert_eq!(func1.visibility, SV::Public);
    assert_eq!(func1.arguments.len(), 0);
    assert_eq!(func1.return_type, None);
    assert_eq!(func1.calling_convention, SCC::System);
    assert_eq!(func1.body, SFB::Address { address: 0x456 });

    // Check second freestanding function
    let func2 = &module.functions()[1];
    assert_eq!(func2.name, "another_freestanding");
    assert_eq!(func2.visibility, SV::Private);
    assert_eq!(func2.arguments.len(), 1);
    assert_eq!(
        func2.arguments[0].strip_locations(),
        SAr::field("arg1", ST::raw("i32")).strip_locations()
    );
    assert_eq!(func2.return_type, Some(ST::raw("i32")));
    assert_eq!(func2.calling_convention, SCC::System);
    assert_eq!(func2.body, SFB::Address { address: 0x789 });
}

#[test]
fn freestanding_function_without_address_fails() {
    // Freestanding function without #[address(...)] should fail
    let ast = M::new().with_functions([F::new((V::Public, "test"), [])]);

    assert_ast_produces_exact_error(
        ast,
        SemanticError::FunctionMissingImplementation {
            function_name: "test".to_string(),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn private_freestanding_function_without_address_fails() {
    // Private freestanding function without #[address(...)] should also fail
    let ast = M::new().with_functions([F::new((V::Private, "internal_func"), [])]);

    assert_ast_produces_exact_error(
        ast,
        SemanticError::FunctionMissingImplementation {
            function_name: "internal_func".to_string(),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn freestanding_function_with_args_but_no_address_fails() {
    // Even with arguments, missing address should fail
    let ast = M::new().with_functions([F::new(
        (V::Public, "compute"),
        [Ar::named("value", T::ident("i32"))],
    )
    .with_return_type(T::ident("i32"))]);

    assert_ast_produces_exact_error(
        ast,
        SemanticError::FunctionMissingImplementation {
            function_name: "compute".to_string(),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn external_body_function_form_is_rejected() {
    // `#[external_body(...)]` is a typo for the bare ident
    // `#[external_body]`; the function-style form should hard-error
    // rather than silently fall through as an unknown attribute.
    let ast = M::new().with_functions([F::new((V::Public, "make"), [])
        .with_return_type(T::ident("i32"))
        .with_attributes([A::external_body_function_form()])]);

    assert_ast_produces_exact_error(
        ast,
        SemanticError::AttributeWrongForm {
            attribute_name: crate::semantic::error::AttributeName::ExternalBody,
            expected: "the bare ident `#[external_body]` (no arguments)".into(),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn external_body_assign_form_is_rejected() {
    // `#[external_body = ...]` is also wrong — only the bare ident
    // form is accepted.
    let ast = M::new().with_functions([F::new((V::Public, "make"), [])
        .with_return_type(T::ident("i32"))
        .with_attributes([A::external_body_assign_form()])]);

    assert_ast_produces_exact_error(
        ast,
        SemanticError::AttributeWrongForm {
            attribute_name: crate::semantic::error::AttributeName::ExternalBody,
            expected: "the bare ident `#[external_body]` (no value)".into(),
            location: ItemLocation::test(),
        },
    );
}

// --- calling_convention attribute -----------------------------------
//
// `#[calling_convention(...)]` takes a bare ident (not a string — see
// ferrobrew/pyxis#91). The codegen corpus covers the happy path
// (`#[calling_convention(C)]`); these tests pin down the two failure
// modes so the exact error shape is visible.

#[test]
fn calling_convention_string_form_is_rejected() {
    // The pre-#91 string form is no longer accepted: an `Expr::Ident` is
    // expected, so a string literal yields `InvalidAttributeValue`.
    let text = r#"#[calling_convention("cdecl"), address(0x1)] pub fn f();"#;
    let ast = crate::parser::parse_str_for_tests(text).unwrap();
    let err = build_state(&ast, &IP::from("test")).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        SemanticError::InvalidAttributeValue {
            attribute_name: crate::semantic::error::AttributeName::CallingConvention,
            expected_type: std::any::type_name::<SCC>().into(),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn calling_convention_unknown_ident_is_rejected() {
    // A well-formed ident that isn't one of the seven accepted ABIs yields
    // `InvalidCallingConvention`, naming both the bad value and the function.
    let text = r#"#[calling_convention(bogus), address(0x1)] pub fn f();"#;
    let ast = crate::parser::parse_str_for_tests(text).unwrap();
    let err = build_state(&ast, &IP::from("test")).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        SemanticError::InvalidCallingConvention {
            convention: "bogus".to_string(),
            function_name: "f".to_string(),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

// --- cfg-aware duplicate-method dedup -------------------------------
//
// Two `impl` blocks may declare a method with the same name when their
// cfg predicates are mutually exclusive (e.g. one per backend), so each
// backend surfaces its own declaration. This is what lets `#[external_body]`
// declarations for the same method coexist across backends.

#[test]
fn cfg_disjoint_same_name_methods_allowed() {
    let text = r#"
        type Foo { x: u32 }
        #[cfg(backend = "cpp")]
        impl Foo {
            #[external_body]
            pub fn bar(&self) -> u32;
        }
        #[cfg(backend = "rust")]
        impl Foo {
            #[external_body]
            pub fn bar(&self) -> u32;
        }
    "#;
    let ast = crate::parser::parse_str_for_tests(text).unwrap();
    let state = build_state(&ast, &IP::from("test")).unwrap();
    let item = state
        .type_registry()
        .get(&IP::from("test::Foo"), &ItemLocation::internal())
        .unwrap();
    let bar_count = item
        .resolved()
        .and_then(|r| r.inner.as_type())
        .map(|td| {
            td.associated_functions
                .iter()
                .filter(|f| f.name == "bar")
                .count()
        })
        .unwrap_or(0);
    assert_eq!(
        bar_count, 2,
        "both cfg-disjoint `bar` declarations should surface"
    );
}

#[test]
fn cfg_identical_same_name_methods_rejected() {
    let text = r#"
        type Foo { x: u32 }
        #[cfg(backend = "cpp")]
        impl Foo {
            #[external_body]
            pub fn bar(&self) -> u32;
        }
        #[cfg(backend = "cpp")]
        impl Foo {
            #[external_body]
            pub fn bar(&self) -> u32;
        }
    "#;
    let ast = crate::parser::parse_str_for_tests(text).unwrap();
    let err = build_state(&ast, &IP::from("test")).unwrap_err();
    assert!(
        matches!(err, SemanticError::DuplicateDefinition { .. }),
        "expected DuplicateDefinition, got {err:?}"
    );
}

#[test]
fn ungated_overlaps_gated_same_name_methods_rejected() {
    // An ungated method is "always active", so it overlaps any cfg-gated
    // method of the same name → duplicate.
    let text = r#"
        type Foo { x: u32 }
        impl Foo {
            #[external_body]
            pub fn bar(&self) -> u32;
        }
        #[cfg(backend = "rust")]
        impl Foo {
            #[external_body]
            pub fn bar(&self) -> u32;
        }
    "#;
    let ast = crate::parser::parse_str_for_tests(text).unwrap();
    let err = build_state(&ast, &IP::from("test")).unwrap_err();
    assert!(
        matches!(err, SemanticError::DuplicateDefinition { .. }),
        "expected DuplicateDefinition, got {err:?}"
    );
}
