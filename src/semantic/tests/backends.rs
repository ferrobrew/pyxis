//! Tests for `backend NAME { ... }` block validation.

use crate::{
    grammar::test_aliases::*,
    semantic::{error::SemanticError, semantic_state::SemanticState},
};

#[test]
fn cpp_backend_accepts_definition_modifier() {
    let module = M::new().with_backends([B::new("cpp")
        .with_epilogue("bool foo();")
        .with_epilogue_definition("bool foo() { return true; }")]);

    let mut s = SemanticState::new(4);
    s.add_module(&module, &IP::from("test")).unwrap();
    // Build should succeed; the cpp backend is allowed to declare a
    // separate `definition` slot.
    s.build().expect("cpp + definition should validate");
}

#[test]
fn rust_backend_rejects_epilogue_definition() {
    let module = M::new().with_backends([B::new("rust").with_epilogue_definition("// nope")]);

    let mut s = SemanticState::new(4);
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

    let mut s = SemanticState::new(4);
    s.add_module(&module, &IP::from("test")).unwrap();
    let err = s.build().expect_err("json + definition should be rejected");
    match err {
        SemanticError::BackendDefinitionNotSupported { backend, .. } => {
            assert_eq!(backend, crate::Backend::Json);
        }
        other => panic!("expected BackendDefinitionNotSupported, got {other:?}"),
    }
}
