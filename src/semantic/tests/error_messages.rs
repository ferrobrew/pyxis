//! Tests for error message quality and content.

use crate::{
    grammar::test_aliases::*,
    semantic::error::{SemanticError, UnresolvedTypeContext, UnresolvedTypeReference},
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
                context: UnresolvedTypeContext::StructField {
                    field_name: "my_field".to_string(),
                    type_path: IP::from("test::MyType"),
                },
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
                context: UnresolvedTypeContext::EnumBaseType {
                    enum_path: IP::from("test::MyEnum"),
                },
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
                    context: UnresolvedTypeContext::StructField {
                        field_name: "field_a".to_string(),
                        type_path: IP::from("test::Type1"),
                    },
                },
                UnresolvedTypeReference {
                    type_name: "Missing2".to_string(),
                    location: ItemLocation::test(),
                    context: UnresolvedTypeContext::StructField {
                        field_name: "field_b".to_string(),
                        type_path: IP::from("test::Type2"),
                    },
                },
            ],
        },
    );
}
