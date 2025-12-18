//! Tests for enum type resolution.

use crate::{
    grammar::test_aliases::{int_literal, *},
    semantic::types::test_aliases::*,
};

use super::util::*;
use pretty_assertions::assert_eq;

#[test]
fn can_resolve_enum() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            ED::new(
                T::ident("u32"),
                [
                    ES::field_with_expr("Item0", int_literal(-2)),
                    ES::field("Item1"),
                    ES::field("Item2"),
                    ES::field_with_expr("Item3", int_literal(10)),
                    ES::field("Item4"),
                ],
                [A::singleton(0x1234)],
            ),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SED::new(ST::raw("u32"))
                    .with_variants([
                        ("Item0", -2),
                        ("Item1", -1),
                        ("Item2", 0),
                        ("Item3", 10),
                        ("Item4", 11),
                    ])
                    .with_singleton(0x1234),
            ),
        )],
    );
}

#[test]
fn can_resolve_enum_with_associated_functions() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_definitions([ID::new(
                (V::Public, "TestEnum"),
                ED::new(
                    T::ident("u32"),
                    [ES::field("None"), ES::field("Some"), ES::field("Value")],
                    [],
                ),
            )])
            .with_impls([FB::new(
                "TestEnum",
                [
                    F::new((V::Public, "test"), []).with_attributes([A::address(0x123)]),
                    F::new((V::Public, "another_test"), [Ar::const_self()])
                        .with_attributes([A::address(0x456)])
                        .with_return_type(T::ident("i32")),
                ],
            )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestEnum"),
            SISR::new(
                (4, 4),
                SED::new(ST::raw("u32"))
                    .with_variants([("None", 0), ("Some", 1), ("Value", 2)])
                    .with_associated_functions([
                        SF::new((SV::Public, "test"), SFB::address(0x123), SCC::System),
                        SF::new(
                            (SV::Public, "another_test"),
                            SFB::address(0x456),
                            SCC::for_member_function(pointer_size()),
                        )
                        .with_arguments([SAr::const_self()])
                        .with_return_type(ST::raw("i32")),
                    ]),
            ),
        )],
    );
}

#[test]
fn can_carry_backend_across() {
    let prologue = r#"
        use std::ffi::CString;
        use std::os::raw::c_char;
    "#
    .trim();

    let epilogue = r#"
        fn main() {
            println!("Hello, world!");
        }
    "#
    .trim();

    // Intentionally double-include the epilogue to test if it's correctly carried across
    let ast = M::new().with_backends([
        B::new("rust")
            .with_prologue(prologue)
            .with_epilogue(epilogue),
        B::new("rust").with_epilogue(epilogue),
    ]);
    let test_path = IP::from("test");

    let state = build_state(&ast, &test_path).unwrap();
    let module = state.modules().get(&test_path).unwrap();

    assert_eq!(
        module.backends.get("rust").unwrap(),
        &[
            SB::new(prologue.to_string(), epilogue.to_string()),
            SB::new(None, epilogue.to_string()),
        ]
    );
}
