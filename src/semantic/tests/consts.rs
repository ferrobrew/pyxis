//! Tests for `const` declarations, focusing on resolution behaviour that the
//! codegen corpus doesn't exercise (module-qualified enum-value references).

use crate::{
    grammar::test_aliases::*,
    semantic::{
        builder::SemanticBuilder,
        types::{ConstValue, ItemDefinitionInner, Type},
    },
    span::ItemLocation,
};

use pretty_assertions::assert_eq;

/// A module-level `const` whose value is an enum variant reached through a
/// module-qualified path (`graphics::Color::Red`) must resolve: the final
/// segment is the variant and everything before it is the (possibly
/// module-qualified) enum path.
#[test]
fn can_resolve_module_qualified_enum_value_const() {
    let graphics = M::new().with_definitions([ID::new(
        (V::Public, "Color"),
        ED::new(
            T::ident("u8"),
            [ES::field("Red"), ES::field("Green"), ES::field("Blue")],
            [],
        ),
    )]);
    // Import the enum type, but reference the variant through a
    // module-qualified value path to exercise multi-segment resolution.
    let app = M::new()
        .with_uses([IP::from("graphics::Color")])
        .with_definitions([ID::new(
            (V::Public, "DEFAULT_COLOR"),
            CD::new(
                T::ident("Color"),
                path_expr(IP::from("graphics::Color::Red")),
            ),
        )]);

    let mut builder = SemanticBuilder::new(4);
    builder
        .add_module(&graphics, &IP::from("graphics"))
        .unwrap();
    builder.add_module(&app, &IP::from("app")).unwrap();
    let resolved = builder.build().unwrap();

    let item = resolved
        .type_registry()
        .get(&IP::from("app::DEFAULT_COLOR"), &ItemLocation::test())
        .cloned()
        .expect("failed to get DEFAULT_COLOR const");

    let inner = &item.resolved().expect("const should be resolved").inner;
    let ItemDefinitionInner::Constant(cd) = inner else {
        panic!("expected a constant, got {inner:?}");
    };
    assert_eq!(cd.type_, Type::raw("graphics::Color"));
    assert_eq!(
        cd.value,
        ConstValue::EnumValue(IP::from("graphics::Color::Red"))
    );
}

/// Helper: build a single-module program and return the resolved const value
/// for `test::NAME`.
fn resolve_const(name: &str, module: &M) -> ConstValue {
    let mut builder = SemanticBuilder::new(4);
    builder.add_module(module, &IP::from("test")).unwrap();
    let resolved = builder.build().unwrap();
    let path_str = format!("test::{name}");
    let item = resolved
        .type_registry()
        .get(&IP::from(path_str.as_str()), &ItemLocation::test())
        .cloned()
        .expect("failed to get const");
    let inner = &item.resolved().expect("const should be resolved").inner;
    let ItemDefinitionInner::Constant(cd) = inner else {
        panic!("expected a constant, got {inner:?}");
    };
    cd.value.clone()
}

/// Helper: build a single-module program and expect a build error.
fn expect_build_error(module: &M) -> String {
    let mut builder = SemanticBuilder::new(4);
    builder.add_module(module, &IP::from("test")).unwrap();
    builder
        .build()
        .err()
        .map(|e| format!("{e}"))
        .unwrap_or_else(|| panic!("expected build error but build succeeded"))
}

// === C-string tests ===

#[test]
fn c_string_const_with_cstr_type() {
    let m = M::new().with_definitions([ID::new(
        (V::Public, "DLL_NAME"),
        CD::new(T::ident("cstr"), c_string_literal("kernel32.dll")),
    )]);
    assert_eq!(
        resolve_const("DLL_NAME", &m),
        ConstValue::CString("kernel32.dll".to_string())
    );
}

#[test]
fn c_string_const_with_wrong_type_errors() {
    let m = M::new().with_definitions([ID::new(
        (V::Public, "BAD"),
        CD::new(T::ident("i32"), c_string_literal("hello")),
    )]);
    let err = expect_build_error(&m);
    assert!(err.contains("`cstr`"), "error should mention cstr: {err}");
}

// === Struct initializer tests ===

#[test]
fn struct_initializer_succeeds_on_copyable_type() {
    let m = M::new().with_definitions([
        ID::new(
            (V::Public, "Vec3"),
            TD::new([TS::field((V::Public, "x"), T::ident("f32"))])
                .with_attributes([A::copyable()]),
        ),
        ID::new(
            (V::Public, "ORIGIN"),
            CD::new(
                T::ident("Vec3"),
                struct_literal_expr(
                    IP::from("Vec3"),
                    vec![(Ident::from("x"), float_literal("0.0"))],
                ),
            ),
        ),
    ]);
    let value = resolve_const("ORIGIN", &m);
    match value {
        ConstValue::Struct { fields, .. } => {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].0, "x");
        }
        other => panic!("expected Struct, got {other:?}"),
    }
}

#[test]
fn struct_initializer_on_non_copyable_type_errors() {
    let m = M::new().with_definitions([
        ID::new(
            (V::Public, "Vec3"),
            TD::new([TS::field((V::Public, "x"), T::ident("f32"))]),
            // no #[copyable]
        ),
        ID::new(
            (V::Public, "ORIGIN"),
            CD::new(
                T::ident("Vec3"),
                struct_literal_expr(
                    IP::from("Vec3"),
                    vec![(Ident::from("x"), float_literal("0.0"))],
                ),
            ),
        ),
    ]);
    let err = expect_build_error(&m);
    assert!(
        err.contains("copyable"),
        "error should mention copyable: {err}"
    );
}

#[test]
fn struct_initializer_missing_field_errors() {
    let m = M::new().with_definitions([
        ID::new(
            (V::Public, "Vec3"),
            TD::new([
                TS::field((V::Public, "x"), T::ident("f32")),
                TS::field((V::Public, "y"), T::ident("f32")),
            ])
            .with_attributes([A::copyable()]),
        ),
        ID::new(
            (V::Public, "ORIGIN"),
            CD::new(
                T::ident("Vec3"),
                struct_literal_expr(
                    IP::from("Vec3"),
                    vec![(Ident::from("x"), float_literal("0.0"))],
                ),
            ),
        ),
    ]);
    let err = expect_build_error(&m);
    assert!(
        err.contains("missing field"),
        "error should mention missing field: {err}"
    );
}

// === Array initializer tests ===

#[test]
fn array_initializer_with_correct_count_succeeds() {
    let m = M::new().with_definitions([ID::new(
        (V::Public, "ARR"),
        CD::new(
            T::array(T::ident("i32"), 3),
            array_literal_expr(vec![int_literal(1), int_literal(2), int_literal(3)]),
        ),
    )]);
    let value = resolve_const("ARR", &m);
    match value {
        ConstValue::Array(elements) => {
            assert_eq!(elements.len(), 3);
        }
        other => panic!("expected Array, got {other:?}"),
    }
}

#[test]
fn array_initializer_with_wrong_count_errors() {
    let m = M::new().with_definitions([ID::new(
        (V::Public, "ARR"),
        CD::new(
            T::array(T::ident("i32"), 3),
            array_literal_expr(vec![int_literal(1), int_literal(2)]),
        ),
    )]);
    let err = expect_build_error(&m);
    assert!(
        err.contains("array of 3"),
        "error should mention array count: {err}"
    );
}

// === Constant alias tests ===

#[test]
fn constant_alias_with_matching_type_succeeds() {
    let m = M::new().with_definitions([
        ID::new(
            (V::Public, "MAX_HEALTH"),
            CD::new(T::ident("i32"), int_literal(100)),
        ),
        ID::new(
            (V::Public, "DEFAULT_HEALTH"),
            CD::new(T::ident("i32"), ident_expr("MAX_HEALTH")),
        ),
    ]);
    let value = resolve_const("DEFAULT_HEALTH", &m);
    assert_eq!(value, ConstValue::ConstRef(IP::from("test::MAX_HEALTH")));
}

#[test]
fn constant_alias_with_mismatched_type_errors() {
    let m = M::new().with_definitions([
        ID::new(
            (V::Public, "MAX_HEALTH"),
            CD::new(T::ident("i32"), int_literal(100)),
        ),
        ID::new(
            (V::Public, "BAD_ALIAS"),
            CD::new(T::ident("f32"), ident_expr("MAX_HEALTH")),
        ),
    ]);
    let err = expect_build_error(&m);
    assert!(
        err.contains("constant of type"),
        "error should mention type mismatch: {err}"
    );
}

#[test]
fn struct_initializer_with_duplicate_field_errors() {
    let m = M::new().with_definitions([
        ID::new(
            (V::Public, "Vec3"),
            TD::new([
                TS::field((V::Public, "x"), T::ident("f32")),
                TS::field((V::Public, "y"), T::ident("f32")),
            ])
            .with_attributes([A::copyable()]),
        ),
        ID::new(
            (V::Public, "DUP"),
            CD::new(
                T::ident("Vec3"),
                struct_literal_expr(
                    IP::from("Vec3"),
                    vec![
                        (Ident::from("x"), float_literal("0.0")),
                        (Ident::from("x"), float_literal("1.0")),
                        (Ident::from("y"), float_literal("0.0")),
                    ],
                ),
            ),
        ),
    ]);
    let err = expect_build_error(&m);
    assert!(
        err.contains("duplicate field"),
        "error should mention duplicate field: {err}"
    );
}

#[test]
fn struct_initializer_with_unknown_field_errors() {
    let m = M::new().with_definitions([
        ID::new(
            (V::Public, "Vec3"),
            TD::new([TS::field((V::Public, "x"), T::ident("f32"))])
                .with_attributes([A::copyable()]),
        ),
        ID::new(
            (V::Public, "BAD"),
            CD::new(
                T::ident("Vec3"),
                struct_literal_expr(
                    IP::from("Vec3"),
                    vec![
                        (Ident::from("x"), float_literal("0.0")),
                        (Ident::from("z"), float_literal("0.0")),
                    ],
                ),
            ),
        ),
    ]);
    let err = expect_build_error(&m);
    assert!(
        err.contains("unknown field"),
        "error should mention unknown field: {err}"
    );
}

#[test]
fn constant_alias_referencing_nonexistent_const_errors() {
    let m = M::new().with_definitions([ID::new(
        (V::Public, "BAD"),
        CD::new(T::ident("i32"), ident_expr("NONEXISTENT")),
    )]);
    let err = expect_build_error(&m);
    assert!(
        err.contains("constant") || err.contains("not found"),
        "error should mention missing constant: {err}"
    );
}
