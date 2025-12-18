//! Tests for module imports and use statements.

use crate::{
    grammar::test_aliases::*,
    semantic::{error::SemanticError, semantic_state::SemanticState, types::test_aliases::*},
    span::ItemLocation,
};

use pretty_assertions::assert_eq;

#[test]
fn can_use_type_from_another_module() {
    let module1 = M::new()
        .with_uses([IP::from("module2::TestType2")])
        .with_definitions([ID::new(
            (V::Public, "TestType1"),
            TD::new([TS::field((V::Private, "field"), T::ident("TestType2"))]),
        )]);
    let module2 = M::new().with_definitions([ID::new(
        (V::Public, "TestType2"),
        TD::new([TS::field((V::Private, "field"), T::ident("u32"))]),
    )]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();
    semantic_state
        .add_module(&module2, &IP::from("module2"))
        .unwrap();
    let semantic_state = semantic_state.build().unwrap();

    let path = IP::from("module1::TestType1");
    let resolved_type = semantic_state
        .type_registry()
        .get(&path, &ItemLocation::test())
        .cloned()
        .expect("failed to get type");
    assert_eq!(
        resolved_type,
        SID::defined_resolved(
            (SV::Public, path.clone(),),
            SISR::new(
                (4, 4,),
                STD::new().with_regions([SR::field(
                    (SV::Private, "field"),
                    ST::raw("module2::TestType2")
                )])
            )
        )
    );
}

#[test]
fn can_use_braced_imports_from_another_module() {
    // Test that braced imports like `use math::{Matrix4, Vector3}` work correctly.
    // The UseTree::Group is flattened by the semantic layer's scope() function.
    let module1 = M::new()
        .with_use_trees([UT::group(
            "math",
            [UT::path("Matrix4"), UT::path("Vector3")],
        )])
        .with_definitions([ID::new(
            (V::Public, "Transform"),
            TD::new([
                TS::field((V::Private, "matrix"), T::ident("Matrix4")),
                TS::field((V::Private, "position"), T::ident("Vector3")),
            ]),
        )]);
    let module_math = M::new().with_definitions([
        ID::new(
            (V::Public, "Matrix4"),
            TD::new([TS::field((V::Public, "data"), T::ident("f32").array(16))]),
        ),
        ID::new(
            (V::Public, "Vector3"),
            TD::new([
                TS::field((V::Public, "x"), T::ident("f32")),
                TS::field((V::Public, "y"), T::ident("f32")),
                TS::field((V::Public, "z"), T::ident("f32")),
            ]),
        ),
    ]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();
    semantic_state
        .add_module(&module_math, &IP::from("math"))
        .unwrap();
    let semantic_state = semantic_state.build().unwrap();

    // Verify the Transform type was resolved correctly with both imported types
    let path = IP::from("module1::Transform");
    let resolved_type = semantic_state
        .type_registry()
        .get(&path, &ItemLocation::test())
        .cloned()
        .expect("failed to get Transform type");
    assert_eq!(
        resolved_type,
        SID::defined_resolved(
            (SV::Public, path.clone(),),
            SISR::new(
                (76, 4,), // 64 bytes for Matrix4 + 12 bytes for Vector3
                STD::new().with_regions([
                    SR::field((SV::Private, "matrix"), ST::raw("math::Matrix4")),
                    SR::field((SV::Private, "position"), ST::raw("math::Vector3")),
                ])
            )
        )
    );
}

#[test]
fn will_fail_on_use_of_nonexistent_type() {
    use crate::span::StripLocations;

    // Test that using a type that doesn't exist produces an error
    let module1 = M::new()
        .with_uses([IP::from("math::NonExistent")])
        .with_definitions([ID::new(
            (V::Public, "Test"),
            TD::new([TS::field((V::Private, "value"), T::ident("u32"))]),
        )]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();

    let err = semantic_state.build().unwrap_err();
    assert_eq!(
        err.strip_locations(),
        SemanticError::UseItemNotFound {
            path: IP::from("math::NonExistent"),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn will_fail_on_braced_import_with_nonexistent_type() {
    use crate::span::StripLocations;

    // Test that braced imports with a nonexistent type produce an error
    let module1 = M::new()
        .with_use_trees([UT::group(
            "math",
            [UT::path("Matrix4"), UT::path("NonExistent")],
        )])
        .with_definitions([ID::new(
            (V::Public, "Test"),
            TD::new([TS::field((V::Private, "value"), T::ident("u32"))]),
        )]);
    let module_math = M::new().with_definitions([ID::new(
        (V::Public, "Matrix4"),
        TD::new([TS::field((V::Public, "data"), T::ident("f32").array(16))]),
    )]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();
    semantic_state
        .add_module(&module_math, &IP::from("math"))
        .unwrap();

    let err = semantic_state.build().unwrap_err();
    assert_eq!(
        err.strip_locations(),
        SemanticError::UseItemNotFound {
            path: IP::from("math::NonExistent"),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn will_fail_on_use_of_nonexistent_module() {
    use crate::span::StripLocations;

    // Test that using a module that doesn't exist produces an error
    let module1 = M::new()
        .with_uses([IP::from("nonexistent::SomeType")])
        .with_definitions([ID::new(
            (V::Public, "Test"),
            TD::new([TS::field((V::Private, "value"), T::ident("u32"))]),
        )]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();

    let err = semantic_state.build().unwrap_err();
    assert_eq!(
        err.strip_locations(),
        SemanticError::UseItemNotFound {
            path: IP::from("nonexistent::SomeType"),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn can_use_module_in_use_statement() {
    // Test that using a module path (not just a type) is allowed
    // This is useful for `use math` to bring the module into scope
    let module1 = M::new()
        .with_uses([IP::from("math")])
        .with_definitions([ID::new(
            (V::Public, "Test"),
            TD::new([TS::field((V::Private, "value"), T::ident("u32"))]),
        )]);
    let module_math = M::new().with_definitions([ID::new(
        (V::Public, "Vector3"),
        TD::new([TS::field((V::Public, "x"), T::ident("f32"))]),
    )]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();
    semantic_state
        .add_module(&module_math, &IP::from("math"))
        .unwrap();

    // Should succeed because "math" is a valid module
    semantic_state.build().unwrap();
}
