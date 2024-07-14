use crate::{
    grammar::test_aliases::*,
    semantic_analysis::{
        semantic_state::{ResolvedSemanticState, SemanticState},
        types::test_aliases::*,
    },
};

use pretty_assertions::assert_eq;

use anyhow::Context;

fn build_state(module: &M, module_path: &IP) -> anyhow::Result<ResolvedSemanticState> {
    let mut semantic_state = SemanticState::new(4);
    semantic_state.add_module(module, module_path)?;
    semantic_state.build()
}

fn assert_ast_produces_type_definitions(
    module: M,
    type_definitions: impl IntoIterator<Item = SID>,
) {
    let module_path = IP::from("test");

    let state = build_state(&module, &module_path).unwrap();
    let created_module = state.modules().get(&module_path).unwrap();
    let type_registry = state.type_registry();

    let mut expected_type_definitions: Vec<_> = type_definitions.into_iter().collect();
    let mut created_type_definitions: Vec<_> =
        created_module.definitions(type_registry).cloned().collect();

    expected_type_definitions.sort_by_key(|t| t.path.clone());
    created_type_definitions.sort_by_key(|t| t.path.clone());

    assert_eq!(created_type_definitions, expected_type_definitions);
}

fn assert_ast_produces_failure(module: M, failure: &str) {
    assert_eq!(
        build_state(&module, &IP::from("test"))
            .unwrap_err()
            .to_string(),
        failure
    );
}

fn unknown(size: usize) -> ST {
    ST::raw("u8").array(size)
}

#[test]
fn can_resolve_basic_struct() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            "TestType",
            TD::new([
                TS::field("field_1", T::ident("i32")),
                TS::field("_", T::unknown(4)),
                TS::field("field_2", T::ident("u64")),
            ]),
        )]),
        [SID::defined_resolved(
            "test::TestType",
            SISR {
                size: 16,
                inner: STD::new()
                    .with_regions([
                        SR::field("field_1", ST::raw("i32")),
                        SR::field("_field_4", unknown(4)),
                        SR::field("field_2", ST::raw("u64")),
                    ])
                    .into(),
            },
        )],
    );
}

#[test]
fn can_resolve_pointer_to_another_struct() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                "TestType1",
                TD::new([TS::field("field_1", T::ident("u64"))]),
            ),
            ID::new(
                "TestType2",
                TD::new([
                    TS::field("field_1", T::ident("i32")),
                    TS::field("field_2", T::ident("TestType1")),
                    TS::field("field_3", T::ident("TestType1").const_pointer()),
                    TS::field("field_4", T::ident("TestType1").mut_pointer()),
                ]),
            ),
        ]),
        [
            SID::defined_resolved(
                "test::TestType1",
                SISR {
                    size: 8,
                    inner: STD::new()
                        .with_regions([SR::field("field_1", ST::raw("u64"))])
                        .into(),
                },
            ),
            SID::defined_resolved(
                "test::TestType2",
                SISR {
                    size: 20,
                    inner: STD::new()
                        .with_regions([
                            SR::field("field_1", ST::raw("i32")),
                            SR::field("field_2", ST::raw("test::TestType1")),
                            SR::field("field_3", ST::raw("test::TestType1").const_pointer()),
                            SR::field("field_4", ST::raw("test::TestType1").mut_pointer()),
                        ])
                        .into(),
                },
            ),
        ],
    );
}

#[test]
fn can_resolve_complex_type() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_definitions([
                ID::new(
                    "TestType",
                    TD::new([
                        TS::field("field_1", T::ident("i32")),
                        TS::field("_", T::unknown(4)),
                    ]),
                ),
                ID::new(
                    "Singleton",
                    TD::new([
                        TS::field("max_num_1", T::ident("u16")).with_attributes([A::address(0x78)]),
                        TS::field("max_num_2", T::ident("u16")),
                        TS::field("test_type", T::ident("TestType"))
                            .with_attributes([A::address(0xA00)]),
                        TS::field("settings", T::unknown(804)),
                    ])
                    .with_attributes([A::size(0x1750), A::singleton(0x1_200_000)]),
                ),
            ])
            .with_impls([FB::new(
                "Singleton",
                [F::new(
                    "test_function",
                    [
                        Ar::MutSelf,
                        Ar::field("arg1", T::ident("TestType").mut_pointer()),
                        Ar::field("arg2", T::ident("i32")),
                        Ar::field("arg3", T::ident("u32").const_pointer()),
                    ],
                )
                .with_attributes([A::address(0x800_000)])
                .with_return_type(T::ident("TestType").mut_pointer())],
            )]),
        [
            SID::defined_resolved(
                "test::TestType",
                SISR {
                    size: 8,
                    inner: STD::new()
                        .with_regions([
                            SR::field("field_1", ST::raw("i32")),
                            SR::field("_field_4", unknown(4)),
                        ])
                        .into(),
                },
            ),
            SID::defined_resolved(
                "test::Singleton",
                SISR {
                    size: 0x1750,
                    inner: STD::new()
                        .with_regions([
                            SR::field("_field_0", unknown(0x78)),
                            SR::field("max_num_1", ST::raw("u16")),
                            SR::field("max_num_2", ST::raw("u16")),
                            SR::field("_field_7c", unknown(0x984)),
                            SR::field("test_type", ST::raw("test::TestType")),
                            SR::field("settings", unknown(804)),
                            SR::field("_field_d2c", unknown(0xA24)),
                        ])
                        .with_free_functions([SF::new("test_function")
                            .with_address(0x800_000)
                            .with_arguments([
                                SAr::MutSelf,
                                SAr::field(
                                    "arg1".to_string(),
                                    ST::raw("test::TestType").mut_pointer(),
                                ),
                                SAr::field("arg2", ST::raw("i32")),
                                SAr::field("arg3", ST::raw("u32").const_pointer()),
                            ])
                            .with_return_type(ST::raw("test::TestType").mut_pointer())])
                        .with_singleton(0x1_200_000)
                        .into(),
                },
            ),
        ],
    );
}

#[test]
fn will_eventually_terminate_with_an_unknown_type() {
    assert_ast_produces_failure(
        M::new().with_definitions([ID::new(
            "TestType2",
            TD::new([TS::field("field_2", T::ident("TestType1"))]),
        )]),
        r#"type resolution will not terminate, failed on types: ["test::TestType2"] (resolved types: [])"#,
    );
}

#[test]
fn can_use_type_from_another_module() {
    let module1 = M::new()
        .with_uses([IP::from("module2::TestType2")])
        .with_definitions([ID::new(
            "TestType1",
            TD::new([TS::field("field", T::ident("TestType2"))]),
        )]);
    let module2 = M::new().with_definitions([ID::new(
        "TestType2",
        TD::new([TS::field("field", T::ident("u32"))]),
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
        .get(&path)
        .cloned()
        .context("failed to get type")
        .unwrap();
    assert_eq!(
        resolved_type,
        SID::defined_resolved(
            path.clone(),
            SISR {
                size: 4,
                inner: STD::new()
                    .with_regions([SR::field("field", ST::raw("module2::TestType2"))])
                    .into(),
            },
        )
    );
}

#[test]
fn will_fail_on_an_extern_without_size() {
    assert_ast_produces_failure(
        M::new().with_extern_types([("TestType".into(), vec![])]),
        "failed to find size attribute for extern type",
    );
}

#[test]
fn can_resolve_embed_of_an_extern() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_extern_types([("TestType1".into(), vec![A::size(16)])])
            .with_definitions([ID::new(
                "TestType2",
                TD::new([
                    TS::field("field_1", T::ident("i32")),
                    TS::field("field_2", T::ident("TestType1")),
                    TS::field("field_3", T::ident("TestType1").const_pointer()),
                    TS::field("field_4", T::ident("TestType1").mut_pointer()),
                ]),
            )]),
        [
            SID::defined_resolved(
                "test::TestType2",
                SISR {
                    size: 28,
                    inner: STD::new()
                        .with_regions([
                            SR::field("field_1", ST::raw("i32")),
                            SR::field("field_2", ST::raw("test::TestType1")),
                            SR::field("field_3", ST::raw("test::TestType1").const_pointer()),
                            SR::field("field_4", ST::raw("test::TestType1").mut_pointer()),
                        ])
                        .into(),
                },
            ),
            SID {
                path: "test::TestType1".into(),
                state: SISR {
                    size: 16,
                    inner: STD::new().with_regions([]).into(),
                }
                .into(),
                category: SIC::Extern,
            },
        ],
    );
}

#[test]
fn will_fail_on_type_with_vfuncs_but_no_vftable() {
    assert_ast_produces_failure(
        M::new()
            .with_definitions([ID::new("TestType", TD::new([]))])
            .with_vftable([FB::new(
                "TestType",
                [F::new(
                    "test_function0",
                    [
                        Ar::MutSelf,
                        Ar::Field(TF::new("arg0", T::ident("u32"))),
                        Ar::Field(TF::new("arg1", T::ident("f32"))),
                    ],
                )
                .with_return_type("i32")],
            )]),
        "type test::TestType has vftable functions but no vftable field",
    );
}

#[test]
fn can_generate_vftable() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_definitions([ID::new("TestType", TD::new([TF::vftable().into()]))])
            .with_vftable([FB::new(
                "TestType",
                [
                    F::new(
                        "test_function0",
                        [
                            Ar::MutSelf,
                            Ar::Field(TF::new("arg0", T::ident("u32"))),
                            Ar::Field(TF::new("arg1", T::ident("f32"))),
                        ],
                    )
                    .with_return_type("i32"),
                    F::new(
                        "test_function1",
                        [
                            Ar::MutSelf,
                            Ar::Field(TF::new("arg0", T::ident("u32"))),
                            Ar::Field(TF::new("arg1", T::ident("f32"))),
                        ],
                    ),
                ],
            )
            .with_attributes([A::size(4)])]),
        [
            // TestType
            SID::defined_resolved(
                "test::TestType",
                SISR {
                    size: 4,
                    inner: STD::new()
                        .with_regions([SR::field(
                            "vftable",
                            ST::raw("test::TestTypeVftable").const_pointer(),
                        )])
                        .with_vftable_functions([
                            SF::new("test_function0")
                                .with_arguments([
                                    SAr::MutSelf,
                                    SAr::field("arg0", ST::raw("u32")),
                                    SAr::field("arg1", ST::raw("f32")),
                                ])
                                .with_return_type(ST::raw("i32")),
                            SF::new("test_function1").with_arguments([
                                SAr::MutSelf,
                                SAr::field("arg0", ST::raw("u32")),
                                SAr::field("arg1", ST::raw("f32")),
                            ]),
                            make_vfunc(2),
                            make_vfunc(3),
                        ])
                        .into(),
                },
            ),
            // TestTypeVftable
            SID::defined_resolved(
                "test::TestTypeVftable",
                SISR {
                    size: 16,
                    inner: STD::new()
                        .with_regions([
                            SR::field(
                                "test_function0",
                                ST::function(
                                    [
                                        ("this", ST::raw("test::TestType").mut_pointer()),
                                        ("arg0", ST::raw("u32")),
                                        ("arg1", ST::raw("f32")),
                                    ],
                                    ST::raw("i32"),
                                ),
                            ),
                            SR::field(
                                "test_function1",
                                ST::function(
                                    [
                                        ("this", ST::raw("test::TestType").mut_pointer()),
                                        ("arg0", ST::raw("u32")),
                                        ("arg1", ST::raw("f32")),
                                    ],
                                    None,
                                ),
                            ),
                            make_vfunc_region(2),
                            make_vfunc_region(3),
                        ])
                        .into(),
                },
            ),
        ],
    );
}

#[test]
fn can_generate_vftable_with_indices() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_definitions([ID::new("TestType", TD::new([TF::vftable().into()]))])
            .with_vftable([FB::new(
                "TestType",
                [
                    F::new(
                        "test_function0",
                        [
                            Ar::MutSelf,
                            Ar::Field(TF::new("arg0", T::ident("u32"))),
                            Ar::Field(TF::new("arg1", T::ident("f32"))),
                        ],
                    )
                    .with_attributes([A::index(2)])
                    .with_return_type("i32"),
                    F::new(
                        "test_function1",
                        [
                            Ar::MutSelf,
                            Ar::Field(TF::new("arg0", T::ident("u32"))),
                            Ar::Field(TF::new("arg1", T::ident("f32"))),
                        ],
                    )
                    .with_attributes([A::index(5)]),
                ],
            )
            .with_attributes([A::size(8)])]),
        [
            // TestType
            SID::defined_resolved(
                "test::TestType",
                SISR {
                    size: 4,
                    inner: STD::new()
                        .with_regions([SR::field(
                            "vftable",
                            ST::raw("test::TestTypeVftable").const_pointer(),
                        )])
                        .with_vftable_functions([
                            make_vfunc(0),
                            make_vfunc(1),
                            SF::new("test_function0")
                                .with_arguments([
                                    SAr::MutSelf,
                                    SAr::field("arg0", ST::raw("u32")),
                                    SAr::field("arg1", ST::raw("f32")),
                                ])
                                .with_return_type(ST::raw("i32")),
                            make_vfunc(3),
                            make_vfunc(4),
                            SF::new("test_function1").with_arguments([
                                SAr::MutSelf,
                                SAr::field("arg0", ST::raw("u32")),
                                SAr::field("arg1", ST::raw("f32")),
                            ]),
                            make_vfunc(6),
                            make_vfunc(7),
                        ])
                        .into(),
                },
            ),
            // TestTypeVftable
            SID::defined_resolved(
                "test::TestTypeVftable",
                SISR {
                    size: 32,
                    inner: STD::new()
                        .with_regions([
                            make_vfunc_region(0),
                            make_vfunc_region(1),
                            SR::field(
                                "test_function0",
                                ST::function(
                                    [
                                        ("this", ST::raw("test::TestType").mut_pointer()),
                                        ("arg0", ST::raw("u32")),
                                        ("arg1", ST::raw("f32")),
                                    ],
                                    ST::raw("i32"),
                                ),
                            ),
                            make_vfunc_region(3),
                            make_vfunc_region(4),
                            SR::field(
                                "test_function1",
                                ST::function(
                                    [
                                        ("this", ST::raw("test::TestType").mut_pointer()),
                                        ("arg0", ST::raw("u32")),
                                        ("arg1", ST::raw("f32")),
                                    ],
                                    None,
                                ),
                            ),
                            make_vfunc_region(6),
                            make_vfunc_region(7),
                        ])
                        .into(),
                },
            ),
        ],
    );
}

fn make_vfunc(index: usize) -> SF {
    SF::new(format!("_vfunc_{}", index)).with_arguments([SAr::MutSelf])
}

fn make_vfunc_region(index: usize) -> SR {
    SR::field(
        format!("_vfunc_{}", index),
        ST::function([("this", ST::raw("test::TestType").mut_pointer())], None),
    )
}

#[test]
fn can_define_extern_value() {
    let module1 = M::new().with_extern_values([(
        "test".into(),
        T::ident("u32").mut_pointer(),
        vec![A::address(0x1337)],
    )]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();
    let semantic_state = semantic_state.build().unwrap();

    let extern_value = semantic_state
        .modules()
        .get(&IP::from("module1"))
        .unwrap()
        .extern_values
        .first()
        .unwrap();

    assert_eq!(
        extern_value,
        &("test".into(), ST::raw("u32").mut_pointer(), 0x1337)
    );
}

#[test]
fn can_resolve_enum() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            "TestType",
            ED::new(
                T::ident("u32"),
                [
                    ES::field("Item1"),
                    ES::field("Item2"),
                    ES::field_with_expr("Item3", E::IntLiteral(10)),
                    ES::field("Item4"),
                ],
                [A::singleton(0x1234)],
            ),
        )]),
        [SID::defined_resolved(
            "test::TestType",
            SISR {
                size: 4,
                inner: SED::new(ST::raw("u32"))
                    .with_fields([("Item1", 0), ("Item2", 1), ("Item3", 10), ("Item4", 11)])
                    .with_singleton(0x1234)
                    .into(),
            },
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

    let ast = M::new().with_backends([B::new("rust")
        .with_prologue(prologue)
        .with_epilogue(epilogue)]);
    let test_path = IP::from("test");

    let state = build_state(&ast, &test_path).unwrap();
    let module = state.modules().get(&test_path).unwrap();

    assert_eq!(
        module.backends.get("rust").unwrap(),
        &SB {
            prologue: Some(prologue.to_string()),
            epilogue: Some(epilogue.to_string()),
        }
    );
}

#[test]
fn can_extract_copyable_and_cloneable_correctly() {
    // Check cloneable
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            "TestType",
            TD::new([TS::field("field_1", T::ident("i32"))]).with_attributes([A::cloneable()]),
        )]),
        [SID::defined_resolved(
            "test::TestType",
            SISR {
                size: 4,
                inner: STD::new()
                    .with_regions([SR::field("field_1", ST::raw("i32"))])
                    .with_cloneable(true)
                    .into(),
            },
        )],
    );

    // Check copyable -> copyable + cloneable
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            "TestType",
            TD::new([TS::field("field_1", T::ident("i32"))]).with_attributes([A::copyable()]),
        )]),
        [SID::defined_resolved(
            "test::TestType",
            SISR {
                size: 4,
                inner: STD::new()
                    .with_regions([SR::field("field_1", ST::raw("i32"))])
                    .with_copyable(true)
                    .with_cloneable(true)
                    .into(),
            },
        )],
    );
}

#[test]
fn can_handle_defaultable_on_primitive_types() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            "TestType",
            TD::new([
                TS::field("field_1", T::ident("i32")),
                TS::field("field_2", T::ident("f32").array(16)),
            ])
            .with_attributes([A::defaultable()]),
        )]),
        [SID::defined_resolved(
            "test::TestType",
            SISR {
                size: 68,
                inner: STD::new()
                    .with_regions([
                        SR::field("field_1", ST::raw("i32")),
                        SR::field("field_2", ST::raw("f32").array(16)),
                    ])
                    .with_defaultable(true)
                    .into(),
            },
        )],
    );
}

#[test]
fn will_reject_defaultable_on_pointer() {
    assert_ast_produces_failure(
        M::new().with_definitions([ID::new(
            "TestType",
            TD::new([TS::field("field_1", T::ident("i32").mut_pointer())])
                .with_attributes([A::defaultable()]),
        )]),
        "field field_1 of type test::TestType is not a defaultable type (pointer or function?)",
    );
}

#[test]
fn will_reject_defaultable_on_enum_field() {
    assert_ast_produces_failure(
        M::new().with_definitions([
            ID::new(
                "TestType",
                TD::new([TS::field("field_1", T::ident("TestEnum"))])
                    .with_attributes([A::defaultable()]),
            ),
            ID::new(
                "TestEnum",
                ED::new(T::ident("u32"), [ES::field("Item1")], []),
            ),
        ]),
        "field field_1 of type test::TestType is not a defaultable type (non-type?)",
    );
}

#[test]
fn will_reject_defaultable_on_non_defaultable_type() {
    assert_ast_produces_failure(
        M::new().with_definitions([
            ID::new(
                "TestType",
                TD::new([TS::field("field_1", T::ident("TestNonDefaultable"))])
                    .with_attributes([A::defaultable()]),
            ),
            ID::new("TestNonDefaultable", TD::new([])),
        ]),
        "field field_1 of type test::TestType is not marked as defaultable",
    );
}

pub mod inheritance {
    //! Tests for inheritance of types with optional vftables.
    //!
    //! One base class
    //! --------------
    //! We set up two types: BaseA and Derived.
    //! Derived derives from BaseA.
    //!
    //! We need to test, where 'x' marks the presence of a vftable:
    //!
    //! BaseA | Drved
    //! --------------
    //!       |
    //!       |   x
    //!   x   |
    //!   x   |   x
    //!
    //! Two base classes
    //! ----------------
    //! We set up three types: BaseA, BaseB and Derived.
    //! Derived derives from both BaseA and BaseB.
    //!
    //! However, note that [/layout/msvc2022/output.txt] demonstrates that
    //! a compiler will rearrange structs to put an inherited-from type
    //! with a vftable at the start of the type, which means we don't need
    //! to test the BaseA no-vftable BaseB vftable cases, as these are isomorphic
    //! to BaseA vftable BaseB no-vftable.
    //!
    //! We need to test, where 'x' marks the presence of a vftable:
    //!
    //! BaseA | BaseB | Drved
    //! ----------------------
    //!       |       |
    //!       |       |   x
    //!   x   |       |
    //!   x   |       |   x
    //!   x   |   x   |
    //!   x   |   x   |   x
    //!
    //! Multiple levels of inheritance
    //! ------------------------------
    //! We set up three types: BaseA, Derived and DerivedDerived.
    //! Derived derives from BaseA, and DerivedDerived derives from Derived.
    //!
    //! We need to test, where 'x' marks the presence of a vftable:
    //!
    //! BaseA | Drved | Drv2d
    //! ----------------------
    //!       |       |
    //!       |       |   x
    //!       |   x   |
    //!       |   x   |   x
    //!   x   |       |
    //!   x   |       |   x
    //!   x   |   x   |
    //!   x   |   x   |   x

    use super::*;

    mod dsl {
        use super::*;

        #[derive(Clone, Debug)]
        pub enum InheritancePrimitiveType {
            I32,
            U32,
            U64,
            F32,
            Unknown(usize),
            Named(String, usize),
        }
        pub type IPT = InheritancePrimitiveType;
        impl IPT {
            fn size(&self) -> usize {
                match self {
                    IPT::I32 => 4,
                    IPT::U32 => 4,
                    IPT::U64 => 8,
                    IPT::F32 => 4,
                    IPT::Unknown(size) => *size,
                    IPT::Named(_, size) => *size,
                }
            }

            fn to_grammar(&self) -> T {
                match self {
                    IPT::I32 => T::ident("i32"),
                    IPT::U32 => T::ident("u32"),
                    IPT::U64 => T::ident("u64"),
                    IPT::F32 => T::ident("f32"),
                    IPT::Unknown(size) => T::unknown(*size),
                    IPT::Named(name, _) => T::ident(name.as_str()),
                }
            }

            fn to_semantic(&self) -> ST {
                match self {
                    IPT::I32 => ST::raw("i32"),
                    IPT::U32 => ST::raw("u32"),
                    IPT::U64 => ST::raw("u64"),
                    IPT::F32 => ST::raw("f32"),
                    IPT::Unknown(size) => unknown(*size),
                    IPT::Named(name, _) => ST::raw(format!("test::{name}").as_str()),
                }
            }
        }

        pub struct InheritanceType {
            name: String,
            fields: Vec<(String, IPT, Vec<A>)>,
            vftable: Option<IV>,
        }
        pub type IT = InheritanceType;
        impl IT {
            pub fn new(
                name: impl Into<String>,
                fields: impl IntoIterator<Item = (String, IPT, Vec<A>)>,
            ) -> Self {
                Self {
                    name: name.into(),
                    fields: fields.into_iter().collect(),
                    vftable: None,
                }
            }

            pub fn with_vftable(mut self, vftable: IV) -> Self {
                self.vftable = Some(vftable);
                self
            }

            pub fn to_grammar(&self) -> ID {
                ID::new(
                    &self.name,
                    TD::new(
                        Iterator::chain(
                            self.vftable.as_ref().iter().map(|_| TS::vftable()),
                            self.fields.iter().map(|(name, ty, attrs)| {
                                TS::field(
                                    &if name.starts_with('_') {
                                        "_".to_string()
                                    } else {
                                        name.to_string()
                                    },
                                    ty.to_grammar(),
                                )
                                .with_attributes(attrs.clone())
                            }),
                        )
                        .collect::<Vec<_>>(),
                    ),
                )
            }

            pub fn to_semantic(&self) -> SID {
                let f = &self.fields;
                let size: usize = f.iter().map(|(_, ty, _)| ty.size()).sum::<usize>()
                    + self.vftable.as_ref().map(|_| 4).unwrap_or(0);

                let mut regions = f
                    .iter()
                    .map(|(name, ty, _)| SR::field(name.clone(), ty.to_semantic()))
                    .collect::<Vec<_>>();

                if let Some(vftable) = &self.vftable {
                    regions.insert(
                        0,
                        SR::field(
                            "vftable",
                            ST::raw(format!("test::{}", vftable.name_vftable).as_str())
                                .const_pointer(),
                        ),
                    );
                }

                let type_definition = STD::new().with_regions(regions);
                let type_definition = if let Some(vftable) = &self.vftable {
                    type_definition.with_vftable_functions(
                        vftable
                            .functions
                            .iter()
                            .map(|func| func.to_semantic())
                            .collect::<Vec<_>>(),
                    )
                } else {
                    type_definition
                };

                SID::defined_resolved(
                    format!("test::{}", self.name).as_str(),
                    SISR {
                        size,
                        inner: type_definition.into(),
                    },
                )
            }
        }

        #[derive(Clone, Debug)]
        pub struct InheritanceVftable {
            name: String,
            name_vftable: String,
            functions: Vec<IF>,
        }
        pub type IV = InheritanceVftable;
        impl IV {
            pub fn new(
                name: impl Into<String>,
                name_vftable: impl Into<String>,
                functions: impl IntoIterator<Item = IF>,
            ) -> Self {
                Self {
                    name: name.into(),
                    name_vftable: name_vftable.into(),
                    functions: functions.into_iter().collect(),
                }
            }

            pub fn to_grammar(&self) -> FB {
                FB::new(
                    self.name.as_str(),
                    self.functions
                        .iter()
                        .map(|func| func.to_grammar())
                        .collect::<Vec<_>>(),
                )
            }

            pub fn to_semantic(&self) -> SID {
                SID::defined_resolved(
                    format!("test::{}", self.name_vftable).as_str(),
                    SISR {
                        size: self.functions.len() * 4,
                        inner: STD::new()
                            .with_regions(
                                self.functions
                                    .iter()
                                    .map(|func| func.to_semantic_region(self.name.as_str()))
                                    .collect::<Vec<_>>(),
                            )
                            .into(),
                    },
                )
            }
        }

        #[derive(Clone, Debug)]
        pub struct InheritanceFunction {
            name: String,
            arguments: Vec<IFA>,
            return_type: IPT,
        }
        pub type IF = InheritanceFunction;
        #[derive(Clone, Debug)]
        pub enum InheritanceFunctionArgument {
            MutSelf,
            Field(String, IPT),
        }
        pub type IFA = InheritanceFunctionArgument;
        impl IFA {
            pub fn field(name: impl Into<String>, ty: IPT) -> Self {
                Self::Field(name.into(), ty)
            }
        }
        impl IF {
            pub fn new(
                name: impl Into<String>,
                arguments: impl IntoIterator<Item = IFA>,
                return_type: IPT,
            ) -> Self {
                Self {
                    name: name.into(),
                    arguments: arguments.into_iter().collect(),
                    return_type,
                }
            }

            fn to_grammar(&self) -> F {
                F::new(
                    &self.name,
                    self.arguments
                        .iter()
                        .map(|arg| match arg {
                            IFA::MutSelf => Ar::MutSelf,
                            IFA::Field(name, ty) => {
                                Ar::Field(TF::new(name.as_str(), ty.to_grammar()))
                            }
                        })
                        .collect::<Vec<_>>(),
                )
                .with_return_type(self.return_type.to_grammar())
            }

            fn to_semantic(&self) -> SF {
                SF::new(&self.name)
                    .with_arguments(
                        self.arguments
                            .iter()
                            .map(|arg| match arg {
                                IFA::MutSelf => SAr::MutSelf,
                                IFA::Field(name, ty) => SAr::field(name.clone(), ty.to_semantic()),
                            })
                            .collect::<Vec<_>>(),
                    )
                    .with_return_type(self.return_type.to_semantic())
            }

            fn to_semantic_region(&self, type_name: &str) -> SR {
                SR::field(
                    &self.name,
                    ST::function(
                        self.arguments
                            .iter()
                            .map(|arg| match arg {
                                IFA::MutSelf => (
                                    "this",
                                    ST::raw(format!("test::{type_name}").as_str()).mut_pointer(),
                                ),
                                IFA::Field(name, ty) => (name.as_str(), ty.to_semantic()),
                            })
                            .collect::<Vec<_>>(),
                        self.return_type.to_semantic(),
                    ),
                )
            }
        }
    }
    use dsl::*;

    fn derived_vfunc() -> IF {
        IF::new(
            "derived_vfunc",
            [
                IFA::MutSelf,
                IFA::field("arg0", IPT::U32),
                IFA::field("arg1", IPT::F32),
            ],
            IPT::I32,
        )
    }

    #[test]
    fn base_a_vf0_base_b_vf0_derived_vf0() {
        let base_a = IT::new(
            "BaseA",
            [
                ("field_1".to_string(), IPT::I32, vec![]),
                ("_field_4".to_string(), IPT::Unknown(4), vec![]),
                ("field_2".to_string(), IPT::U64, vec![]),
            ],
        );

        let base_b = IT::new(
            "BaseB",
            [
                ("field_1".to_string(), IPT::U64, vec![]),
                ("_field_8".to_string(), IPT::Unknown(4), vec![]),
                ("field_2".to_string(), IPT::I32, vec![]),
            ],
        );

        let derived = IT::new(
            "Derived",
            [
                (
                    "base_a".to_string(),
                    IPT::Named("BaseA".to_string(), 16),
                    vec![A::base()],
                ),
                (
                    "base_b".to_string(),
                    IPT::Named("BaseB".to_string(), 16),
                    vec![A::base()],
                ),
            ],
        );

        assert_ast_produces_type_definitions(
            M::new().with_definitions([
                base_a.to_grammar(),
                base_b.to_grammar(),
                derived.to_grammar(),
            ]),
            [
                base_a.to_semantic(),
                base_b.to_semantic(),
                derived.to_semantic(),
            ],
        );
    }

    #[test]
    fn base_a_vf0_base_b_vf0_derived_vf1() {
        let base_a = IT::new(
            "BaseA",
            [
                ("field_1".to_string(), IPT::I32, vec![]),
                ("_field_4".to_string(), IPT::Unknown(4), vec![]),
                ("field_2".to_string(), IPT::U64, vec![]),
            ],
        );

        let base_b = IT::new(
            "BaseB",
            [
                ("field_1".to_string(), IPT::U64, vec![]),
                ("_field_8".to_string(), IPT::Unknown(4), vec![]),
                ("field_2".to_string(), IPT::I32, vec![]),
            ],
        );

        let derived_vftable = IV::new("Derived", "DerivedVftable", [derived_vfunc()]);
        let derived = IT::new(
            "Derived",
            [
                (
                    "base_a".to_string(),
                    IPT::Named("BaseA".to_string(), 16),
                    vec![A::base()],
                ),
                (
                    "base_b".to_string(),
                    IPT::Named("BaseB".to_string(), 16),
                    vec![A::base()],
                ),
            ],
        )
        .with_vftable(derived_vftable.clone());

        assert_ast_produces_type_definitions(
            M::new()
                .with_definitions([
                    base_a.to_grammar(),
                    base_b.to_grammar(),
                    derived.to_grammar(),
                ])
                .with_vftable([derived_vftable.to_grammar()]),
            [
                base_a.to_semantic(),
                base_b.to_semantic(),
                derived.to_semantic(),
                derived_vftable.to_semantic(),
            ],
        );
    }
}
