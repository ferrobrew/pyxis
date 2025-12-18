//! Tests for error message quality and content.

use crate::{
    grammar::test_aliases::*,
    semantic::error::{SemanticError, UnresolvedTypeReference},
    span::ItemLocation,
};

use super::util::*;

#[test]
fn unresolved_type_includes_type_name_in_error() {
    // Test that when a type can't be resolved, the error includes the specific missing type name
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "MyType"),
            TD::new([TS::field(
                (V::Private, "my_field"),
                T::ident("NonexistentType"),
            )]),
        )]),
        SemanticError::TypeResolutionStalled {
            unresolved_types: vec!["test::MyType".to_string()],
            resolved_types: vec![],
            unresolved_references: vec![UnresolvedTypeReference {
                type_name: "NonexistentType".to_string(),
                location: ItemLocation::test(),
                context: "field `my_field` of type `test::MyType`".to_string(),
            }],
        },
    );
}

#[test]
fn unresolved_type_in_enum_includes_context() {
    // Test that enum base type errors include proper context
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "MyEnum"),
            ED::new(
                T::ident("NonexistentBase"),
                [ES::field("A"), ES::field("B")],
                [],
            ),
        )]),
        SemanticError::TypeResolutionStalled {
            unresolved_types: vec!["test::MyEnum".to_string()],
            resolved_types: vec![],
            unresolved_references: vec![UnresolvedTypeReference {
                type_name: "NonexistentBase".to_string(),
                location: ItemLocation::test(),
                context: "base type of enum `test::MyEnum`".to_string(),
            }],
        },
    );
}

#[test]
fn multiple_unresolved_types_are_all_reported() {
    // Test that multiple missing types are all reported
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Type1"),
                TD::new([TS::field((V::Private, "field_a"), T::ident("Missing1"))]),
            ),
            ID::new(
                (V::Public, "Type2"),
                TD::new([TS::field((V::Private, "field_b"), T::ident("Missing2"))]),
            ),
        ]),
        SemanticError::TypeResolutionStalled {
            unresolved_types: vec!["test::Type1".to_string(), "test::Type2".to_string()],
            resolved_types: vec![],
            unresolved_references: vec![
                UnresolvedTypeReference {
                    type_name: "Missing1".to_string(),
                    location: ItemLocation::test(),
                    context: "field `field_a` of type `test::Type1`".to_string(),
                },
                UnresolvedTypeReference {
                    type_name: "Missing2".to_string(),
                    location: ItemLocation::test(),
                    context: "field `field_b` of type `test::Type2`".to_string(),
                },
            ],
        },
    );
}
