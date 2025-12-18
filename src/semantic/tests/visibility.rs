//! Tests for visibility/privacy rules across modules.
//!
//! These tests verify that Rust-like visibility semantics are enforced:
//! - Private items are only visible within the same module and child modules
//! - Public items are visible everywhere

use crate::{
    grammar::test_aliases::*,
    semantic::{error::SemanticError, semantic_state::SemanticState},
    span::{ItemLocation, StripLocations},
};

use super::util::pointer_size;

/// Test that public types can be imported and used from another module.
#[test]
fn can_use_public_type_from_another_module() {
    let module_a = M::new()
        .with_uses([IP::from("module_b::PublicType")])
        .with_definitions([ID::new(
            (V::Public, "TypeA"),
            TD::new([TS::field((V::Private, "field"), T::ident("PublicType"))]),
        )]);

    let module_b = M::new().with_definitions([ID::new(
        (V::Public, "PublicType"),
        TD::new([TS::field((V::Public, "value"), T::ident("u32"))]),
    )]);

    let mut semantic_state = SemanticState::new(pointer_size());
    semantic_state
        .add_module(&module_a, &IP::from("module_a"))
        .unwrap();
    semantic_state
        .add_module(&module_b, &IP::from("module_b"))
        .unwrap();

    // This should succeed - PublicType is public
    let result = semantic_state.build();
    assert!(
        result.is_ok(),
        "Should be able to use public type from another module, got error: {:?}",
        result.err()
    );
}

/// Test that private types cannot be imported from another module.
#[test]
fn cannot_use_private_type_from_another_module() {
    let module_a = M::new()
        .with_uses([IP::from("module_b::PrivateType")])
        .with_definitions([ID::new(
            (V::Public, "TypeA"),
            TD::new([TS::field((V::Private, "field"), T::ident("PrivateType"))]),
        )]);

    let module_b = M::new().with_definitions([ID::new(
        (V::Private, "PrivateType"),
        TD::new([TS::field((V::Public, "value"), T::ident("u32"))]),
    )]);

    let mut semantic_state = SemanticState::new(pointer_size());
    semantic_state
        .add_module(&module_a, &IP::from("module_a"))
        .unwrap();
    semantic_state
        .add_module(&module_b, &IP::from("module_b"))
        .unwrap();

    // This should fail - PrivateType is private
    let err = semantic_state.build().unwrap_err();
    assert_eq!(
        err.strip_locations(),
        SemanticError::PrivateItemAccess {
            item_path: IP::from("module_b::PrivateType"),
            from_module: IP::from("module_a"),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

/// Test that private types can be used within the same module.
#[test]
fn can_use_private_type_within_same_module() {
    let module = M::new().with_definitions([
        ID::new(
            (V::Private, "PrivateType"),
            TD::new([TS::field((V::Public, "value"), T::ident("u32"))]),
        ),
        ID::new(
            (V::Public, "PublicType"),
            TD::new([TS::field((V::Private, "inner"), T::ident("PrivateType"))]),
        ),
    ]);

    let mut semantic_state = SemanticState::new(pointer_size());
    semantic_state
        .add_module(&module, &IP::from("test_module"))
        .unwrap();

    // This should succeed - private types can be used within the same module
    let result = semantic_state.build();
    assert!(
        result.is_ok(),
        "Should be able to use private type within same module, got error: {:?}",
        result.err()
    );
}

/// Test that braced imports with a private type fail.
#[test]
fn cannot_use_braced_import_with_private_type() {
    let module_a = M::new()
        .with_use_trees([UT::group(
            "module_b",
            [UT::path("PublicType"), UT::path("PrivateType")],
        )])
        .with_definitions([ID::new(
            (V::Public, "TypeA"),
            TD::new([TS::field((V::Public, "value"), T::ident("u32"))]),
        )]);

    let module_b = M::new().with_definitions([
        ID::new(
            (V::Public, "PublicType"),
            TD::new([TS::field((V::Public, "value"), T::ident("u32"))]),
        ),
        ID::new(
            (V::Private, "PrivateType"),
            TD::new([TS::field((V::Public, "value"), T::ident("u32"))]),
        ),
    ]);

    let mut semantic_state = SemanticState::new(pointer_size());
    semantic_state
        .add_module(&module_a, &IP::from("module_a"))
        .unwrap();
    semantic_state
        .add_module(&module_b, &IP::from("module_b"))
        .unwrap();

    // This should fail - PrivateType is private
    let err = semantic_state.build().unwrap_err();
    assert_eq!(
        err.strip_locations(),
        SemanticError::PrivateItemAccess {
            item_path: IP::from("module_b::PrivateType"),
            from_module: IP::from("module_a"),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

/// Test that child modules can access parent's private types.
#[test]
fn child_module_can_access_parent_private_type() {
    // Create parent module with a private type
    let parent_module = M::new().with_definitions([ID::new(
        (V::Private, "ParentPrivate"),
        TD::new([TS::field((V::Public, "value"), T::ident("u32"))]),
    )]);

    // Create child module that uses parent's private type
    let child_module = M::new()
        .with_uses([IP::from("parent::ParentPrivate")])
        .with_definitions([ID::new(
            (V::Public, "ChildType"),
            TD::new([TS::field((V::Private, "inner"), T::ident("ParentPrivate"))]),
        )]);

    let mut semantic_state = SemanticState::new(pointer_size());
    semantic_state
        .add_module(&parent_module, &IP::from("parent"))
        .unwrap();
    semantic_state
        .add_module(&child_module, &IP::from("parent::child"))
        .unwrap();

    // This should succeed - child modules can access parent's private items
    let result = semantic_state.build();
    assert!(
        result.is_ok(),
        "Child module should be able to access parent's private type, got error: {:?}",
        result.err()
    );
}

/// Test that sibling modules cannot access each other's private types.
#[test]
fn sibling_module_cannot_access_private_type() {
    // Create two sibling modules under the same parent
    let sibling_a = M::new().with_definitions([ID::new(
        (V::Private, "PrivateA"),
        TD::new([TS::field((V::Public, "value"), T::ident("u32"))]),
    )]);

    let sibling_b = M::new()
        .with_uses([IP::from("parent::sibling_a::PrivateA")])
        .with_definitions([ID::new(
            (V::Public, "TypeB"),
            TD::new([TS::field((V::Private, "inner"), T::ident("PrivateA"))]),
        )]);

    let mut semantic_state = SemanticState::new(pointer_size());
    semantic_state
        .add_module(&sibling_a, &IP::from("parent::sibling_a"))
        .unwrap();
    semantic_state
        .add_module(&sibling_b, &IP::from("parent::sibling_b"))
        .unwrap();

    // This should fail - sibling_b cannot access sibling_a's private type
    let err = semantic_state.build().unwrap_err();
    assert_eq!(
        err.strip_locations(),
        SemanticError::PrivateItemAccess {
            item_path: IP::from("parent::sibling_a::PrivateA"),
            from_module: IP::from("parent::sibling_b"),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

/// Test that deeply nested child can access ancestor's private type.
#[test]
fn deeply_nested_child_can_access_ancestor_private_type() {
    let ancestor = M::new().with_definitions([ID::new(
        (V::Private, "AncestorPrivate"),
        TD::new([TS::field((V::Public, "value"), T::ident("u32"))]),
    )]);

    // Empty middle module
    let middle = M::new();

    let deep_child = M::new()
        .with_uses([IP::from("ancestor::AncestorPrivate")])
        .with_definitions([ID::new(
            (V::Public, "DeepType"),
            TD::new([TS::field(
                (V::Private, "inner"),
                T::ident("AncestorPrivate"),
            )]),
        )]);

    let mut semantic_state = SemanticState::new(pointer_size());
    semantic_state
        .add_module(&ancestor, &IP::from("ancestor"))
        .unwrap();
    semantic_state
        .add_module(&middle, &IP::from("ancestor::middle"))
        .unwrap();
    semantic_state
        .add_module(&deep_child, &IP::from("ancestor::middle::deep"))
        .unwrap();

    // This should succeed - deeply nested children can access ancestor's private items
    let result = semantic_state.build();
    assert!(
        result.is_ok(),
        "Deeply nested child should be able to access ancestor's private type, got error: {:?}",
        result.err()
    );
}

/// Test that multiple private types from the same module are all inaccessible.
#[test]
fn cannot_use_multiple_private_types_from_another_module() {
    let module_a = M::new()
        .with_uses([IP::from("module_b::Private1")])
        .with_definitions([ID::new(
            (V::Public, "TypeA"),
            TD::new([TS::field((V::Public, "value"), T::ident("u32"))]),
        )]);

    let module_b = M::new().with_definitions([
        ID::new(
            (V::Private, "Private1"),
            TD::new([TS::field((V::Public, "value"), T::ident("u32"))]),
        ),
        ID::new(
            (V::Private, "Private2"),
            TD::new([TS::field((V::Public, "value"), T::ident("u32"))]),
        ),
    ]);

    let mut semantic_state = SemanticState::new(pointer_size());
    semantic_state
        .add_module(&module_a, &IP::from("module_a"))
        .unwrap();
    semantic_state
        .add_module(&module_b, &IP::from("module_b"))
        .unwrap();

    // Should fail trying to use Private1
    let err = semantic_state.build().unwrap_err();
    assert_eq!(
        err.strip_locations(),
        SemanticError::PrivateItemAccess {
            item_path: IP::from("module_b::Private1"),
            from_module: IP::from("module_a"),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

/// Test that a mix of public and private types - only public should be accessible.
#[test]
fn can_use_only_public_types_from_mixed_module() {
    let module_a = M::new()
        .with_uses([IP::from("module_b::PublicType")])
        .with_definitions([ID::new(
            (V::Public, "TypeA"),
            TD::new([TS::field((V::Private, "field"), T::ident("PublicType"))]),
        )]);

    let module_b = M::new().with_definitions([
        ID::new(
            (V::Private, "PrivateType"),
            TD::new([TS::field((V::Public, "value"), T::ident("u32"))]),
        ),
        ID::new(
            (V::Public, "PublicType"),
            TD::new([TS::field((V::Public, "value"), T::ident("u32"))]),
        ),
    ]);

    let mut semantic_state = SemanticState::new(pointer_size());
    semantic_state
        .add_module(&module_a, &IP::from("module_a"))
        .unwrap();
    semantic_state
        .add_module(&module_b, &IP::from("module_b"))
        .unwrap();

    // Should succeed - only using public type
    let result = semantic_state.build();
    assert!(
        result.is_ok(),
        "Should be able to use public type when module also has private types, got error: {:?}",
        result.err()
    );
}
