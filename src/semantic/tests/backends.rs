//! Tests for standalone `prologue`/`epilogue` splice validation.

use crate::{
    grammar::test_aliases::*,
    semantic::{builder::SemanticBuilder, error::SemanticError},
};

use super::util::pointer_size;

#[test]
fn cpp_gated_splice_accepts_definition_modifier() {
    let module = M::new().with_splices([
        SP::epilogue("bool foo();").cfg_backend("cpp"),
        SP::epilogue("bool foo() { return true; }")
            .definition()
            .cfg_backend("cpp"),
    ]);

    let mut s = SemanticBuilder::new(4);
    s.add_module(&module, &IP::from("test")).unwrap();
    // Build should succeed; a cpp-gated splice is allowed to use the
    // `definition` slot.
    s.build().expect("cpp + definition should validate");
}

#[test]
fn rust_gated_definition_is_rejected() {
    let module = M::new().with_splices([SP::epilogue("// nope").definition().cfg_backend("rust")]);

    let mut s = SemanticBuilder::new(4);
    s.add_module(&module, &IP::from("test")).unwrap();
    let err = s.build().expect_err("rust + definition should be rejected");
    assert!(
        matches!(err, SemanticError::SpliceDefinitionNotCppOnly { .. }),
        "expected SpliceDefinitionNotCppOnly, got {err:?}"
    );
}

#[test]
fn json_gated_definition_is_rejected() {
    let module = M::new().with_splices([SP::prologue("// nope").definition().cfg_backend("json")]);

    let mut s = SemanticBuilder::new(4);
    s.add_module(&module, &IP::from("test")).unwrap();
    let err = s.build().expect_err("json + definition should be rejected");
    assert!(
        matches!(err, SemanticError::SpliceDefinitionNotCppOnly { .. }),
        "expected SpliceDefinitionNotCppOnly, got {err:?}"
    );
}

#[test]
fn ungated_definition_is_rejected() {
    // An ungated `definition` names no backend, so it can't be cpp-only.
    let module = M::new().with_splices([SP::prologue("// nope").definition()]);

    let mut s = SemanticBuilder::new(4);
    s.add_module(&module, &IP::from("test")).unwrap();
    let err = s
        .build()
        .expect_err("ungated definition should be rejected");
    assert!(
        matches!(err, SemanticError::SpliceDefinitionNotCppOnly { .. }),
        "expected SpliceDefinitionNotCppOnly, got {err:?}"
    );
}

#[test]
fn splice_for_type_resolves_in_same_module() {
    let module = M::new()
        .with_definitions([ID::new(
            (V::Public, "Widget"),
            TD::new([TS::field((V::Public, "id"), T::ident("u32"))]),
        )])
        .with_splices([SP::epilogue("impl Widget {}")
            .for_type("Widget")
            .cfg_backend("rust")]);

    let mut s = SemanticBuilder::new(pointer_size());
    s.add_module(&module, &IP::from("test")).unwrap();
    let state = s.build().expect("for Widget should resolve");

    // The grammar `for Widget` should be resolved to the absolute path.
    let module = state.modules().get(&IP::from("test")).unwrap();
    assert_eq!(
        module.splices[0].for_type.as_ref(),
        Some(&IP::from("test::Widget"))
    );
}

#[test]
fn splice_for_type_not_found_is_rejected() {
    let module = M::new()
        .with_definitions([ID::new(
            (V::Public, "Widget"),
            TD::new([TS::field((V::Public, "id"), T::ident("u32"))]),
        )])
        .with_splices([SP::epilogue("// nope")
            .for_type("Ghost")
            .cfg_backend("rust")]);

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
fn splice_for_type_cross_module_is_rejected() {
    let module_a = M::new().with_splices([SP::epilogue("// nope")
        .for_type("b::OtherType")
        .cfg_backend("rust")]);
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
