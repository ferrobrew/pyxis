//! Tests for `backend NAME { ... }` block validation.

use crate::{
    grammar::test_aliases::*,
    semantic::{error::SemanticError, builder::SemanticBuilder},
};

use super::util::pointer_size;

#[cfg(feature = "cpp")]
#[test]
fn cpp_backend_accepts_definition_modifier() {
    let module = M::new().with_backends([B::new("cpp")
        .with_epilogue("bool foo();")
        .with_epilogue_definition("bool foo() { return true; }")]);

    let mut s = SemanticBuilder::new(4);
    s.add_module(&module, &IP::from("test")).unwrap();
    // Build should succeed; the cpp backend is allowed to declare a
    // separate `definition` slot.
    s.build().expect("cpp + definition should validate");
}

#[test]
fn rust_backend_rejects_epilogue_definition() {
    let module = M::new().with_backends([B::new("rust").with_epilogue_definition("// nope")]);

    let mut s = SemanticBuilder::new(4);
    s.add_module(&module, &IP::from("test")).unwrap();
    let err = s.build().expect_err("rust + definition should be rejected");
    match err {
        SemanticError::BackendDefinitionNotSupported { backend, .. } => {
            assert_eq!(backend, crate::Backend::Rust);
        }
        other => panic!("expected BackendDefinitionNotSupported, got {other:?}"),
    }
}

#[cfg(feature = "json")]
#[test]
fn json_backend_rejects_prologue_definition() {
    let module = M::new().with_backends([B::new("json").with_prologue_definition("// nope")]);

    let mut s = SemanticBuilder::new(4);
    s.add_module(&module, &IP::from("test")).unwrap();
    let err = s.build().expect_err("json + definition should be rejected");
    match err {
        SemanticError::BackendDefinitionNotSupported { backend, .. } => {
            assert_eq!(backend, crate::Backend::Json);
        }
        other => panic!("expected BackendDefinitionNotSupported, got {other:?}"),
    }
}

#[test]
fn backend_for_type_resolves_in_same_module() {
    let module = M::new()
        .with_definitions([ID::new(
            (V::Public, "Widget"),
            TD::new([TS::field((V::Public, "id"), T::ident("u32"))]),
        )])
        .with_backends([B::new("rust").with_epilogue_for("impl Widget {}", "Widget")]);

    let mut s = SemanticBuilder::new(pointer_size());
    s.add_module(&module, &IP::from("test")).unwrap();
    let state = s.build().expect("for Widget should resolve");

    // The grammar `for Widget` should be resolved to the absolute path.
    let module = state.modules().get(&IP::from("test")).unwrap();
    let rust = &module.backends[&crate::Backend::Rust][0];
    assert_eq!(
        rust.epilogue.for_type.as_ref(),
        Some(&IP::from("test::Widget"))
    );
}

#[test]
fn backend_for_type_not_found_is_rejected() {
    let module = M::new()
        .with_definitions([ID::new(
            (V::Public, "Widget"),
            TD::new([TS::field((V::Public, "id"), T::ident("u32"))]),
        )])
        .with_backends([B::new("rust").with_epilogue_for("// nope", "Ghost")]);

    let mut s = SemanticBuilder::new(pointer_size());
    s.add_module(&module, &IP::from("test")).unwrap();
    let err = s.build().expect_err("for Ghost should be rejected");
    match err {
        SemanticError::BackendForTargetNotFound { target, module, .. } => {
            assert_eq!(target, IP::from("Ghost"));
            assert_eq!(module, IP::from("test"));
        }
        other => panic!("expected BackendForTargetNotFound, got {other:?}"),
    }
}

#[test]
fn backend_for_type_cross_module_is_rejected() {
    let module_a =
        M::new().with_backends([B::new("rust").with_epilogue_for("// nope", "b::OtherType")]);
    let module_b = M::new().with_definitions([ID::new(
        (V::Public, "OtherType"),
        TD::new([TS::field((V::Public, "v"), T::ident("u32"))]),
    )]);

    let mut s = SemanticBuilder::new(pointer_size());
    s.add_module(&module_a, &IP::from("a")).unwrap();
    s.add_module(&module_b, &IP::from("b")).unwrap();
    let err = s.build().expect_err("cross-module for should be rejected");
    match err {
        SemanticError::BackendForTargetCrossModule {
            target,
            module,
            defined_in,
            ..
        } => {
            assert_eq!(target, IP::from("b::OtherType"));
            assert_eq!(module, IP::from("a"));
            assert_eq!(defined_in, IP::from("b"));
        }
        other => panic!("expected BackendForTargetCrossModule, got {other:?}"),
    }
}
