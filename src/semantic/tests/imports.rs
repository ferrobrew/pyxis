//! Tests for module imports and use statements.

use crate::{
    grammar::test_aliases::*,
    semantic::{builder::SemanticBuilder, error::SemanticError, types::test_aliases::*},
    span::{ItemLocation, StripLocations},
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

    let mut builder = SemanticBuilder::new(4);
    builder.add_module(&module1, &IP::from("module1")).unwrap();
    builder.add_module(&module2, &IP::from("module2")).unwrap();
    let resolved = builder.build().unwrap();

    let path = IP::from("module1::TestType1");
    let resolved_type = resolved
        .type_registry()
        .get(&path, &ItemLocation::test())
        .cloned()
        .expect("failed to get type");
    assert_eq!(
        resolved_type.strip_locations(),
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

    let mut builder = SemanticBuilder::new(4);
    builder.add_module(&module1, &IP::from("module1")).unwrap();
    builder.add_module(&module_math, &IP::from("math")).unwrap();
    let resolved = builder.build().unwrap();

    // Verify the Transform type was resolved correctly with both imported types
    let path = IP::from("module1::Transform");
    let resolved_type = resolved
        .type_registry()
        .get(&path, &ItemLocation::test())
        .cloned()
        .expect("failed to get Transform type");
    assert_eq!(
        resolved_type.strip_locations(),
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
    // Test that using a type that doesn't exist produces an error
    let module1 = M::new()
        .with_uses([IP::from("math::NonExistent")])
        .with_definitions([ID::new(
            (V::Public, "Test"),
            TD::new([TS::field((V::Private, "value"), T::ident("u32"))]),
        )]);

    let mut builder = SemanticBuilder::new(4);
    builder.add_module(&module1, &IP::from("module1")).unwrap();

    let err = builder.build().unwrap_err();
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

    let mut builder = SemanticBuilder::new(4);
    builder.add_module(&module1, &IP::from("module1")).unwrap();
    builder.add_module(&module_math, &IP::from("math")).unwrap();

    let err = builder.build().unwrap_err();
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
    // Test that using a module that doesn't exist produces an error
    let module1 = M::new()
        .with_uses([IP::from("nonexistent::SomeType")])
        .with_definitions([ID::new(
            (V::Public, "Test"),
            TD::new([TS::field((V::Private, "value"), T::ident("u32"))]),
        )]);

    let mut builder = SemanticBuilder::new(4);
    builder.add_module(&module1, &IP::from("module1")).unwrap();

    let err = builder.build().unwrap_err();
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
fn pub_use_reexport_is_consumable_cross_module() {
    // `foo` defines Bar; `mid` re-exports it with `pub use foo::Bar`; `top`
    // imports the re-export as `mid::Bar` and uses it. The field must resolve to
    // the *canonical* `foo::Bar`, not the alias `mid::Bar`.
    let foo = M::new().with_definitions([ID::new(
        (V::Public, "Bar"),
        TD::new([TS::field((V::Private, "x"), T::ident("u32"))]),
    )]);
    let mid = M::new().with_pub_uses([IP::from("foo::Bar")]);
    let top = M::new()
        .with_uses([IP::from("mid::Bar")])
        .with_definitions([ID::new(
            (V::Public, "Top"),
            TD::new([TS::field((V::Private, "b"), T::ident("Bar"))]),
        )]);

    let mut builder = SemanticBuilder::new(4);
    builder.add_module(&foo, &IP::from("foo")).unwrap();
    builder.add_module(&mid, &IP::from("mid")).unwrap();
    builder.add_module(&top, &IP::from("top")).unwrap();
    let resolved = builder.build().unwrap();

    let top_ty = resolved
        .type_registry()
        .get(&IP::from("top::Top"), &ItemLocation::test())
        .expect("Top should resolve")
        .resolved()
        .expect("Top should be resolved")
        .inner
        .as_type()
        .expect("Top is a type")
        .clone();
    assert_eq!(top_ty.regions.len(), 1);
    assert_eq!(top_ty.regions[0].type_ref, ST::raw("foo::Bar"));
}

#[test]
fn pub_use_reexport_resolves_by_absolute_path() {
    // A consumer can reference a re-exported item by its aliased absolute path
    // (`mid::Bar`) without importing it, e.g. behind a pointer. It resolves to
    // the canonical `foo::Bar`.
    let foo = M::new().with_definitions([ID::new(
        (V::Public, "Bar"),
        TD::new([TS::field((V::Private, "x"), T::ident("u32"))]),
    )]);
    let mid = M::new().with_pub_uses([IP::from("foo::Bar")]);
    let top = M::new().with_definitions([ID::new(
        (V::Public, "Top"),
        TD::new([TS::field(
            (V::Private, "b"),
            T::ident("mid::Bar").mut_pointer(),
        )]),
    )]);

    let mut builder = SemanticBuilder::new(4);
    builder.add_module(&foo, &IP::from("foo")).unwrap();
    builder.add_module(&mid, &IP::from("mid")).unwrap();
    builder.add_module(&top, &IP::from("top")).unwrap();
    let resolved = builder.build().unwrap();

    let top_ty = resolved
        .type_registry()
        .get(&IP::from("top::Top"), &ItemLocation::test())
        .expect("Top should resolve")
        .resolved()
        .expect("Top should be resolved")
        .inner
        .as_type()
        .expect("Top is a type")
        .clone();
    assert_eq!(top_ty.regions.len(), 1);
    assert_eq!(
        top_ty.regions[0].type_ref,
        ST::raw("foo::Bar").mut_pointer()
    );
}

#[test]
fn plain_use_is_not_implicitly_reexported() {
    // Regression guard: a *plain* `use foo::Bar` in `mid` must NOT make
    // `mid::Bar` importable elsewhere. `top`'s `use mid::Bar` has to fail —
    // implicit re-export is not a thing.
    let foo = M::new().with_definitions([ID::new(
        (V::Public, "Bar"),
        TD::new([TS::field((V::Private, "x"), T::ident("u32"))]),
    )]);
    // `mid` imports Bar privately and uses it in a definition of its own.
    let mid = M::new()
        .with_uses([IP::from("foo::Bar")])
        .with_definitions([ID::new(
            (V::Public, "MidThing"),
            TD::new([TS::field((V::Private, "b"), T::ident("Bar"))]),
        )]);
    let top = M::new()
        .with_uses([IP::from("mid::Bar")])
        .with_definitions([ID::new(
            (V::Public, "Top"),
            TD::new([TS::field((V::Private, "b"), T::ident("Bar"))]),
        )]);

    let mut builder = SemanticBuilder::new(4);
    builder.add_module(&foo, &IP::from("foo")).unwrap();
    builder.add_module(&mid, &IP::from("mid")).unwrap();
    builder.add_module(&top, &IP::from("top")).unwrap();

    let err = builder.build().unwrap_err();
    assert_eq!(
        err.strip_locations(),
        SemanticError::UseItemNotFound {
            path: IP::from("mid::Bar"),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn plain_use_reexport_not_referenceable_by_path() {
    // The path-reference counterpart: `top` referencing `mid::Bar` (privately
    // imported into `mid`) by absolute path must not resolve.
    let foo = M::new().with_definitions([ID::new(
        (V::Public, "Bar"),
        TD::new([TS::field((V::Private, "x"), T::ident("u32"))]),
    )]);
    let mid = M::new()
        .with_uses([IP::from("foo::Bar")])
        .with_definitions([ID::new(
            (V::Public, "MidThing"),
            TD::new([TS::field((V::Private, "b"), T::ident("Bar"))]),
        )]);
    let top = M::new().with_definitions([ID::new(
        (V::Public, "Top"),
        TD::new([TS::field(
            (V::Private, "b"),
            T::ident("mid::Bar").mut_pointer(),
        )]),
    )]);

    let mut builder = SemanticBuilder::new(4);
    builder.add_module(&foo, &IP::from("foo")).unwrap();
    builder.add_module(&mid, &IP::from("mid")).unwrap();
    builder.add_module(&top, &IP::from("top")).unwrap();

    // `mid::Bar` is not a real item and not re-exported, so `Top` stalls.
    assert!(builder.build().is_err());
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

    let mut builder = SemanticBuilder::new(4);
    builder.add_module(&module1, &IP::from("module1")).unwrap();
    builder.add_module(&module_math, &IP::from("math")).unwrap();

    // Should succeed because "math" is a valid module
    builder.build().unwrap();
}
