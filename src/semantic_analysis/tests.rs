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
            V::Public,
            "TestType",
            TD::new([
                TS::field(V::Public, "field_1", T::ident("i32")),
                TS::field(V::Private, "_", T::unknown(4)),
                TS::field(V::Public, "field_2", T::ident("u64")),
            ])
            .with_attributes([A::align(8)]),
        )]),
        [SID::defined_resolved(
            SV::Public,
            "test::TestType",
            SISR {
                size: 16,
                alignment: 8,
                inner: STD::new()
                    .with_regions([
                        SR::field(SV::Public, "field_1", ST::raw("i32")),
                        SR::field(SV::Private, "_field_4", unknown(4)),
                        SR::field(SV::Public, "field_2", ST::raw("u64")),
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
                V::Public,
                "TestType1",
                TD::new([TS::field(V::Public, "field_1", T::ident("u64"))]),
            ),
            ID::new(
                V::Public,
                "TestType2",
                TD::new([
                    TS::field(V::Public, "field_1", T::ident("i32")),
                    TS::field(V::Public, "field_2", T::ident("TestType1"))
                        .with_attributes([A::address(8)]),
                    TS::field(V::Public, "field_3", T::ident("TestType1").const_pointer()),
                    TS::field(V::Public, "field_4", T::ident("TestType1").mut_pointer()),
                ])
                .with_attributes([A::align(8)]),
            ),
        ]),
        [
            SID::defined_resolved(
                SV::Public,
                "test::TestType1",
                SISR {
                    size: 8,
                    alignment: 8,
                    inner: STD::new()
                        .with_regions([SR::field(SV::Public, "field_1", ST::raw("u64"))])
                        .into(),
                },
            ),
            SID::defined_resolved(
                SV::Public,
                "test::TestType2",
                SISR {
                    size: 24,
                    alignment: 8,
                    inner: STD::new()
                        .with_regions([
                            SR::field(SV::Public, "field_1", ST::raw("i32")),
                            SR::field(SV::Private, "_field_4", unknown(4)),
                            SR::field(SV::Public, "field_2", ST::raw("test::TestType1")),
                            SR::field(
                                SV::Public,
                                "field_3",
                                ST::raw("test::TestType1").const_pointer(),
                            ),
                            SR::field(
                                SV::Public,
                                "field_4",
                                ST::raw("test::TestType1").mut_pointer(),
                            ),
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
                    V::Public,
                    "TestType",
                    TD::new([
                        TS::field(V::Public, "field_1", T::ident("i32")),
                        TS::field(V::Private, "_", T::unknown(4)),
                    ]),
                ),
                ID::new(
                    V::Public,
                    "Singleton",
                    TD::new([
                        TS::field(V::Public, "max_num_1", T::ident("u16"))
                            .with_attributes([A::address(0x78)]),
                        TS::field(V::Public, "max_num_2", T::ident("u16")),
                        TS::field(V::Public, "test_type", T::ident("TestType"))
                            .with_attributes([A::address(0xA00)]),
                        TS::field(V::Public, "settings", T::unknown(804)),
                    ])
                    .with_attributes([A::size(0x1750), A::singleton(0x1_200_000)]),
                ),
            ])
            .with_impls([FB::new(
                "Singleton",
                [F::new(
                    V::Public,
                    "test_function",
                    [
                        Ar::MutSelf,
                        Ar::named("arg1", T::ident("TestType").mut_pointer()),
                        Ar::named("arg2", T::ident("i32")),
                        Ar::named("arg3", T::ident("u32").const_pointer()),
                    ],
                )
                .with_attributes([A::address(0x800_000)])
                .with_return_type(T::ident("TestType").mut_pointer())],
            )]),
        [
            SID::defined_resolved(
                SV::Public,
                "test::TestType",
                SISR {
                    size: 8,
                    alignment: 4,
                    inner: STD::new()
                        .with_regions([
                            SR::field(SV::Public, "field_1", ST::raw("i32")),
                            SR::field(SV::Private, "_field_4", unknown(4)),
                        ])
                        .into(),
                },
            ),
            SID::defined_resolved(
                SV::Public,
                "test::Singleton",
                SISR {
                    size: 0x1750,
                    alignment: 4,
                    inner: STD::new()
                        .with_regions([
                            SR::field(SV::Private, "_field_0", unknown(0x78)),
                            SR::field(SV::Public, "max_num_1", ST::raw("u16")),
                            SR::field(SV::Public, "max_num_2", ST::raw("u16")),
                            SR::field(SV::Private, "_field_7c", unknown(0x984)),
                            SR::field(SV::Public, "test_type", ST::raw("test::TestType")),
                            SR::field(SV::Public, "settings", unknown(804)),
                            SR::field(SV::Private, "_field_d2c", unknown(0xA24)),
                        ])
                        .with_free_functions([SF::new(SV::Public, "test_function")
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
            V::Public,
            "TestType2",
            TD::new([TS::field(V::Private, "field_2", T::ident("TestType1"))]),
        )]),
        r#"type resolution will not terminate, failed on types: ["test::TestType2"] (resolved types: [])"#,
    );
}

#[test]
fn can_use_type_from_another_module() {
    let module1 = M::new()
        .with_uses([IP::from("module2::TestType2")])
        .with_definitions([ID::new(
            V::Public,
            "TestType1",
            TD::new([TS::field(V::Private, "field", T::ident("TestType2"))]),
        )]);
    let module2 = M::new().with_definitions([ID::new(
        V::Public,
        "TestType2",
        TD::new([TS::field(V::Private, "field", T::ident("u32"))]),
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
            SV::Public,
            path.clone(),
            SISR {
                size: 4,
                alignment: 4,
                inner: STD::new()
                    .with_regions([SR::field(
                        SV::Private,
                        "field",
                        ST::raw("module2::TestType2")
                    )])
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
            .with_extern_types([("TestType1".into(), vec![A::size(16), A::align(4)])])
            .with_definitions([ID::new(
                V::Public,
                "TestType2",
                TD::new([
                    TS::field(V::Public, "field_1", T::ident("i32")),
                    TS::field(V::Public, "field_2", T::ident("TestType1")),
                    TS::field(V::Public, "field_3", T::ident("TestType1").const_pointer()),
                    TS::field(V::Public, "field_4", T::ident("TestType1").mut_pointer()),
                ]),
            )]),
        [
            SID::defined_resolved(
                SV::Public,
                "test::TestType2",
                SISR {
                    size: 28,
                    alignment: 4,
                    inner: STD::new()
                        .with_regions([
                            SR::field(SV::Public, "field_1", ST::raw("i32")),
                            SR::field(SV::Public, "field_2", ST::raw("test::TestType1")),
                            SR::field(
                                SV::Public,
                                "field_3",
                                ST::raw("test::TestType1").const_pointer(),
                            ),
                            SR::field(
                                SV::Public,
                                "field_4",
                                ST::raw("test::TestType1").mut_pointer(),
                            ),
                        ])
                        .into(),
                },
            ),
            SID {
                visibility: SV::Public,
                path: "test::TestType1".into(),
                state: SISR {
                    size: 16,
                    alignment: 4,
                    inner: STD::new().with_regions([]).into(),
                }
                .into(),
                category: SIC::Extern,
            },
        ],
    );
}

#[test]
fn can_generate_vftable() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            TD::new([TS::vftable(
                [
                    F::new(
                        V::Public,
                        "test_function0",
                        [
                            Ar::MutSelf,
                            Ar::named("arg0", T::ident("u32")),
                            Ar::named("arg1", T::ident("f32")),
                        ],
                    )
                    .with_return_type("i32"),
                    F::new(
                        V::Public,
                        "test_function1",
                        [
                            Ar::MutSelf,
                            Ar::named("arg0", T::ident("u32")),
                            Ar::named("arg1", T::ident("f32")),
                        ],
                    ),
                ],
                [A::size(4)],
            )]),
        )]),
        [
            // TestType
            SID::defined_resolved(
                SV::Public,
                "test::TestType",
                SISR {
                    size: 4,
                    alignment: 4,
                    inner: STD::new()
                        .with_regions([SR::field(
                            SV::Public,
                            "vftable",
                            ST::raw("test::TestTypeVftable").const_pointer(),
                        )])
                        .with_vftable_functions([
                            SF::new(SV::Public, "test_function0")
                                .with_arguments([
                                    SAr::MutSelf,
                                    SAr::field("arg0", ST::raw("u32")),
                                    SAr::field("arg1", ST::raw("f32")),
                                ])
                                .with_return_type(ST::raw("i32")),
                            SF::new(SV::Public, "test_function1").with_arguments([
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
                SV::Public,
                "test::TestTypeVftable",
                SISR {
                    size: 16,
                    alignment: 4,
                    inner: STD::new()
                        .with_regions([
                            SR::field(
                                SV::Public,
                                "test_function0",
                                ST::function(
                                    SCC::Thiscall,
                                    [
                                        ("this", ST::raw("test::TestType").mut_pointer()),
                                        ("arg0", ST::raw("u32")),
                                        ("arg1", ST::raw("f32")),
                                    ],
                                    ST::raw("i32"),
                                ),
                            ),
                            SR::field(
                                SV::Public,
                                "test_function1",
                                ST::function(
                                    SCC::Thiscall,
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
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            TD::new([TS::vftable(
                [
                    F::new(
                        V::Public,
                        "test_function0",
                        [
                            Ar::MutSelf,
                            Ar::named("arg0", T::ident("u32")),
                            Ar::named("arg1", T::ident("f32")),
                        ],
                    )
                    .with_attributes([A::index(2)])
                    .with_return_type("i32"),
                    F::new(
                        V::Public,
                        "test_function1",
                        [
                            Ar::MutSelf,
                            Ar::named("arg0", T::ident("u32")),
                            Ar::named("arg1", T::ident("f32")),
                        ],
                    )
                    .with_attributes([A::index(5)]),
                ],
                [A::size(8)],
            )]),
        )]),
        [
            // TestType
            SID::defined_resolved(
                SV::Public,
                "test::TestType",
                SISR {
                    size: 4,
                    alignment: 4,
                    inner: STD::new()
                        .with_regions([SR::field(
                            SV::Public,
                            "vftable",
                            ST::raw("test::TestTypeVftable").const_pointer(),
                        )])
                        .with_vftable_functions([
                            make_vfunc(0),
                            make_vfunc(1),
                            SF::new(SV::Public, "test_function0")
                                .with_arguments([
                                    SAr::MutSelf,
                                    SAr::field("arg0", ST::raw("u32")),
                                    SAr::field("arg1", ST::raw("f32")),
                                ])
                                .with_return_type(ST::raw("i32")),
                            make_vfunc(3),
                            make_vfunc(4),
                            SF::new(SV::Public, "test_function1").with_arguments([
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
                SV::Public,
                "test::TestTypeVftable",
                SISR {
                    size: 32,
                    alignment: 4,
                    inner: STD::new()
                        .with_regions([
                            make_vfunc_region(0),
                            make_vfunc_region(1),
                            SR::field(
                                SV::Public,
                                "test_function0",
                                ST::function(
                                    SCC::Thiscall,
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
                                SV::Public,
                                "test_function1",
                                ST::function(
                                    SCC::Thiscall,
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

#[test]
fn will_propagate_calling_convention_for_impl_and_vftable() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_definitions([ID::new(
                V::Public,
                "TestType",
                TD::new([TS::vftable(
                    [F::new(
                        V::Public,
                        "test_function0",
                        [
                            Ar::MutSelf,
                            Ar::named("arg0", T::ident("u32")),
                            Ar::named("arg1", T::ident("f32")),
                        ],
                    )
                    .with_return_type("i32")
                    .with_attributes([A::calling_convention("cdecl")])],
                    [],
                )]),
            )])
            .with_impls([FB::new(
                "TestType",
                [F::new(
                    V::Public,
                    "test_function",
                    [Ar::named("arg1", T::ident("i32"))],
                )
                .with_attributes([A::address(0x800_000), A::calling_convention("cdecl")])
                .with_return_type(T::ident("i32"))],
            )]),
        [
            // TestType
            SID::defined_resolved(
                SV::Public,
                "test::TestType",
                SISR {
                    size: 4,
                    alignment: 4,
                    inner: STD::new()
                        .with_regions([SR::field(
                            SV::Public,
                            "vftable",
                            ST::raw("test::TestTypeVftable").const_pointer(),
                        )])
                        .with_vftable_functions([SF::new(SV::Public, "test_function0")
                            .with_arguments([
                                SAr::MutSelf,
                                SAr::field("arg0", ST::raw("u32")),
                                SAr::field("arg1", ST::raw("f32")),
                            ])
                            .with_calling_convention(SCC::Cdecl)
                            .with_return_type(ST::raw("i32"))])
                        .with_free_functions([SF::new(SV::Public, "test_function")
                            .with_address(0x800_000)
                            .with_calling_convention(SCC::Cdecl)
                            .with_arguments([SAr::field("arg1", ST::raw("i32"))])
                            .with_return_type(ST::raw("i32"))])
                        .into(),
                },
            ),
            // TestTypeVftable
            SID::defined_resolved(
                SV::Public,
                "test::TestTypeVftable",
                SISR {
                    size: 4,
                    alignment: 4,
                    inner: STD::new()
                        .with_regions([SR::field(
                            SV::Public,
                            "test_function0",
                            ST::function(
                                SCC::Cdecl,
                                [
                                    ("this", ST::raw("test::TestType").mut_pointer()),
                                    ("arg0", ST::raw("u32")),
                                    ("arg1", ST::raw("f32")),
                                ],
                                ST::raw("i32"),
                            ),
                        )])
                        .into(),
                },
            ),
        ],
    );
}

// HACK_SKIP_VFTABLE: <https://github.com/philpax/pyxis/issues/13>
#[test]
fn can_generate_vftable_without_vftable() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            TD::new([
                TS::vftable(
                    [F::new(
                        V::Public,
                        "test_function0",
                        [
                            Ar::MutSelf,
                            Ar::named("arg0", T::ident("u32")),
                            Ar::named("arg1", T::ident("f32")),
                        ],
                    )
                    .with_return_type("i32")],
                    [],
                ),
                TS::field(V::Private, "test", T::ident("u32")).with_attributes([A::address(8)]),
            ])
            .with_attributes([A::hack_skip_vftable(), A::size(16)]),
        )]),
        [
            // TestType
            SID::defined_resolved(
                SV::Public,
                "test::TestType",
                SISR {
                    size: 16,
                    alignment: 4,
                    inner: STD::new()
                        .with_regions([
                            SR::field(
                                SV::Public,
                                "vftable",
                                ST::raw("test::TestTypeVftable").const_pointer(),
                            ),
                            SR::field(SV::Private, "_field_4", unknown(4)),
                            SR::field(SV::Private, "test", ST::raw("u32")),
                            SR::field(SV::Private, "_field_c", unknown(4)),
                        ])
                        .with_vftable_functions([SF::new(SV::Public, "test_function0")
                            .with_arguments([
                                SAr::MutSelf,
                                SAr::field("arg0", ST::raw("u32")),
                                SAr::field("arg1", ST::raw("f32")),
                            ])
                            .with_return_type(ST::raw("i32"))])
                        .into(),
                },
            ),
            // TestTypeWithoutVftable
            SID::defined_resolved(
                SV::Public,
                "test::TestTypeWithoutVftable",
                SISR {
                    size: 12,
                    alignment: 4,
                    inner: STD::new()
                        .with_regions([
                            SR::field(SV::Private, "_field_0", unknown(4)),
                            SR::field(SV::Private, "test", ST::raw("u32")),
                            SR::field(SV::Private, "_field_8", unknown(4)),
                        ])
                        .into(),
                },
            ),
            // TestTypeVftable
            SID::defined_resolved(
                SV::Public,
                "test::TestTypeVftable",
                SISR {
                    size: 4,
                    alignment: 4,
                    inner: STD::new()
                        .with_regions([SR::field(
                            SV::Public,
                            "test_function0",
                            ST::function(
                                SCC::Thiscall,
                                [
                                    ("this", ST::raw("test::TestType").mut_pointer()),
                                    ("arg0", ST::raw("u32")),
                                    ("arg1", ST::raw("f32")),
                                ],
                                ST::raw("i32"),
                            ),
                        )])
                        .into(),
                },
            ),
        ],
    );
}

fn make_vfunc(index: usize) -> SF {
    SF::new(SV::Private, format!("_vfunc_{}", index)).with_arguments([SAr::MutSelf])
}

fn make_vfunc_region(index: usize) -> SR {
    SR::field(
        SV::Private,
        format!("_vfunc_{}", index),
        ST::function(
            SCC::Thiscall,
            [("this", ST::raw("test::TestType").mut_pointer())],
            None,
        ),
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
            V::Public,
            "TestType",
            ED::new(
                T::ident("u32"),
                [
                    ES::field_with_expr("Item0", E::IntLiteral(-2)),
                    ES::field("Item1"),
                    ES::field("Item2"),
                    ES::field_with_expr("Item3", E::IntLiteral(10)),
                    ES::field("Item4"),
                ],
                [A::singleton(0x1234)],
            ),
        )]),
        [SID::defined_resolved(
            SV::Public,
            "test::TestType",
            SISR {
                size: 4,
                alignment: 4,
                inner: SED::new(ST::raw("u32"))
                    .with_fields([
                        ("Item0", -2),
                        ("Item1", -1),
                        ("Item2", 0),
                        ("Item3", 10),
                        ("Item4", 11),
                    ])
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
        &vec![
            SB {
                prologue: Some(prologue.to_string()),
                epilogue: Some(epilogue.to_string()),
            },
            SB {
                prologue: None,
                epilogue: Some(epilogue.to_string()),
            }
        ]
    );
}

#[test]
fn can_extract_copyable_and_cloneable_correctly() {
    // Check cloneable
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            TD::new([TS::field(V::Private, "field_1", T::ident("i32"))])
                .with_attributes([A::cloneable()]),
        )]),
        [SID::defined_resolved(
            SV::Public,
            "test::TestType",
            SISR {
                size: 4,
                alignment: 4,
                inner: STD::new()
                    .with_regions([SR::field(SV::Private, "field_1", ST::raw("i32"))])
                    .with_cloneable(true)
                    .into(),
            },
        )],
    );

    // Check copyable -> copyable + cloneable
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            TD::new([TS::field(V::Private, "field_1", T::ident("i32"))])
                .with_attributes([A::copyable()]),
        )]),
        [SID::defined_resolved(
            SV::Public,
            "test::TestType",
            SISR {
                size: 4,
                alignment: 4,
                inner: STD::new()
                    .with_regions([SR::field(SV::Private, "field_1", ST::raw("i32"))])
                    .with_copyable(true)
                    .with_cloneable(true)
                    .into(),
            },
        )],
    );
}

#[test]
fn can_extract_copyable_and_cloneable_for_enum_correctly() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            ED::new(
                T::ident("u32"),
                [ES::field("Item1"), ES::field("Item2")],
                [],
            )
            .with_attributes([A::cloneable()]),
        )]),
        [SID::defined_resolved(
            SV::Public,
            "test::TestType",
            SISR {
                size: 4,
                alignment: 4,
                inner: SED::new(ST::raw("u32"))
                    .with_fields([("Item1", 0), ("Item2", 1)])
                    .with_cloneable(true)
                    .into(),
            },
        )],
    );

    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            ED::new(
                T::ident("u32"),
                [ES::field("Item1"), ES::field("Item2")],
                [],
            )
            .with_attributes([A::copyable()]),
        )]),
        [SID::defined_resolved(
            SV::Public,
            "test::TestType",
            SISR {
                size: 4,
                alignment: 4,
                inner: SED::new(ST::raw("u32"))
                    .with_fields([("Item1", 0), ("Item2", 1)])
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
            V::Public,
            "TestType",
            TD::new([
                TS::field(V::Private, "field_1", T::ident("i32")),
                TS::field(V::Private, "field_2", T::ident("f32").array(16)),
            ])
            .with_attributes([A::defaultable()]),
        )]),
        [SID::defined_resolved(
            SV::Public,
            "test::TestType",
            SISR {
                size: 68,
                alignment: 4,
                inner: STD::new()
                    .with_regions([
                        SR::field(SV::Private, "field_1", ST::raw("i32")),
                        SR::field(SV::Private, "field_2", ST::raw("f32").array(16)),
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
            V::Public,
            "TestType",
            TD::new([TS::field(
                V::Private,
                "field_1",
                T::ident("i32").mut_pointer(),
            )])
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
                V::Public,
                "TestType",
                TD::new([TS::field(V::Private, "field_1", T::ident("TestEnum"))])
                    .with_attributes([A::defaultable()]),
            ),
            ID::new(
                V::Public,
                "TestEnum",
                ED::new(T::ident("u32"), [ES::field("Item1")], []),
            ),
        ]),
        "field field_1 of type test::TestType is not a defaultable type",
    );
}

#[test]
fn can_handle_defaultable_on_enum_with_default_field() {
    assert_ast_produces_failure(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            ED::new(
                T::ident("u32"),
                [ES::field("Item1"), ES::field("Item2")],
                [],
            )
            .with_attributes([A::defaultable()]),
        )]),
        "enum test::TestType is marked as defaultable but has no default variant set",
    );

    assert_ast_produces_failure(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            ED::new(
                T::ident("u32"),
                [
                    ES::field("Item1"),
                    ES::field("Item2").with_attributes([A::default()]),
                ],
                [],
            )
            .with_attributes([]),
        )]),
        "enum test::TestType has a default variant set but is not marked as defaultable",
    );

    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            ED::new(
                T::ident("u32"),
                [
                    ES::field("Item1"),
                    ES::field("Item2").with_attributes([A::default()]),
                ],
                [],
            )
            .with_attributes([A::defaultable()]),
        )]),
        [SID::defined_resolved(
            SV::Public,
            "test::TestType",
            SISR {
                size: 4,
                alignment: 4,
                inner: SED::new(ST::raw("u32"))
                    .with_fields([("Item1", 0), ("Item2", 1)])
                    .with_defaultable(true)
                    .with_default_index(1)
                    .into(),
            },
        )],
    );
}

#[test]
fn will_reject_defaultable_on_non_defaultable_type() {
    assert_ast_produces_failure(
        M::new().with_definitions([
            ID::new(
                V::Public,
                "TestType",
                TD::new([TS::field(
                    V::Private,
                    "field_1",
                    T::ident("TestNonDefaultable"),
                )])
                .with_attributes([A::defaultable()]),
            ),
            ID::new(V::Public, "TestNonDefaultable", TD::new([])),
        ]),
        "field field_1 of type test::TestType is not a defaultable type",
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

    #[allow(clippy::upper_case_acronyms)]
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
            alignment: usize,
            fields: Vec<(String, IPT, Vec<A>)>,
            vftable: Option<IV>,
        }
        pub type IT = InheritanceType;
        impl IT {
            pub fn new(
                name: impl Into<String>,
                alignment: usize,
                fields: impl IntoIterator<Item = (String, IPT, Vec<A>)>,
            ) -> Self {
                Self {
                    name: name.into(),
                    alignment,
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
                    V::Public,
                    &self.name,
                    TD::new(
                        Iterator::chain(
                            self.vftable.as_ref().iter().map(|v| {
                                TS::vftable(
                                    v.functions
                                        .iter()
                                        .map(|func| func.to_grammar())
                                        .collect::<Vec<_>>(),
                                    [],
                                )
                            }),
                            self.fields.iter().map(|(name, ty, attrs)| {
                                let underscore_start = name.starts_with('_');
                                TS::field(
                                    if underscore_start {
                                        V::Private
                                    } else {
                                        V::Public
                                    },
                                    &if underscore_start {
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
                    )
                    .with_attributes([A::align(self.alignment)]),
                )
            }

            pub fn to_semantic(&self) -> SID {
                let f = &self.fields;
                let size: usize = f.iter().map(|(_, ty, _)| ty.size()).sum::<usize>()
                    + self.vftable.as_ref().map(|_| 4).unwrap_or(0);

                let mut regions = f
                    .iter()
                    .map(|(name, ty, _)| {
                        let visibility = if name.starts_with('_') {
                            SV::Private
                        } else {
                            SV::Public
                        };
                        SR::field(visibility, name.clone(), ty.to_semantic())
                    })
                    .collect::<Vec<_>>();

                if let Some(vftable) = &self.vftable {
                    regions.insert(
                        0,
                        SR::field(
                            SV::Public,
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
                    SV::Public,
                    format!("test::{}", self.name).as_str(),
                    SISR {
                        size,
                        alignment: 4,
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

            pub fn to_semantic(&self) -> SID {
                SID::defined_resolved(
                    SV::Public,
                    format!("test::{}", self.name_vftable).as_str(),
                    SISR {
                        size: self.functions.len() * 4,
                        alignment: 4,
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
                    V::Public,
                    &self.name,
                    self.arguments
                        .iter()
                        .map(|arg| match arg {
                            IFA::MutSelf => Ar::MutSelf,
                            IFA::Field(name, ty) => Ar::named(name.as_str(), ty.to_grammar()),
                        })
                        .collect::<Vec<_>>(),
                )
                .with_return_type(self.return_type.to_grammar())
            }

            fn to_semantic(&self) -> SF {
                SF::new(SV::Public, &self.name)
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
                    SV::Public,
                    &self.name,
                    ST::function(
                        SCC::Thiscall,
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

    mod one_base_class {
        use super::*;
        use dsl::*;

        fn vfunc(name: impl Into<String>) -> IF {
            IF::new(
                name,
                [
                    IFA::MutSelf,
                    IFA::field("arg0", IPT::U32),
                    IFA::field("arg1", IPT::F32),
                ],
                IPT::I32,
            )
        }

        #[test]
        fn b0_d0() {
            let base = IT::new(
                "Base",
                8,
                [
                    ("field_1".to_string(), IPT::I32, vec![]),
                    ("_field_4".to_string(), IPT::Unknown(4), vec![]),
                    ("field_2".to_string(), IPT::U64, vec![]),
                ],
            );

            let derived = IT::new(
                "Derived",
                8,
                [(
                    "base".to_string(),
                    IPT::Named("Base".to_string(), 16),
                    vec![A::base()],
                )],
            );

            assert_ast_produces_type_definitions(
                M::new().with_definitions([base.to_grammar(), derived.to_grammar()]),
                [base.to_semantic(), derived.to_semantic()],
            );
        }

        #[test]
        fn b0_d1() {
            let base = IT::new(
                "Base",
                8,
                [
                    ("field_1".to_string(), IPT::I32, vec![]),
                    ("_field_4".to_string(), IPT::Unknown(4), vec![]),
                    ("field_2".to_string(), IPT::U64, vec![]),
                ],
            );

            let derived_vftable = IV::new("Derived", "DerivedVftable", [vfunc("derived_vfunc")]);
            let derived = IT::new(
                "Derived",
                8,
                [(
                    "base".to_string(),
                    IPT::Named("Base".to_string(), 16),
                    vec![A::base()],
                )],
            )
            .with_vftable(derived_vftable.clone());

            assert_ast_produces_type_definitions(
                M::new().with_definitions([base.to_grammar(), derived.to_grammar()]),
                [
                    base.to_semantic(),
                    derived.to_semantic(),
                    derived_vftable.to_semantic(),
                ],
            );
        }

        #[test]
        #[ignore]
        fn b1_d0() {
            todo!();
        }

        #[test]
        #[ignore]
        fn b1_d1() {
            todo!();
        }
    }

    mod two_base_classes {
        use super::*;
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
        fn a0_b0_d0() {
            let base_a = IT::new(
                "BaseA",
                8,
                [
                    ("field_1".to_string(), IPT::I32, vec![]),
                    ("_field_4".to_string(), IPT::Unknown(4), vec![]),
                    ("field_2".to_string(), IPT::U64, vec![]),
                ],
            );

            let base_b = IT::new(
                "BaseB",
                8,
                [
                    ("field_1".to_string(), IPT::U64, vec![]),
                    ("_field_8".to_string(), IPT::Unknown(4), vec![]),
                    ("field_2".to_string(), IPT::I32, vec![]),
                ],
            );

            let derived = IT::new(
                "Derived",
                8,
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
        fn a0_b0_d1() {
            let base_a = IT::new(
                "BaseA",
                8,
                [
                    ("field_1".to_string(), IPT::I32, vec![]),
                    ("_field_4".to_string(), IPT::Unknown(4), vec![]),
                    ("field_2".to_string(), IPT::U64, vec![]),
                ],
            );

            let base_b = IT::new(
                "BaseB",
                8,
                [
                    ("field_1".to_string(), IPT::U64, vec![]),
                    ("_field_8".to_string(), IPT::Unknown(4), vec![]),
                    ("field_2".to_string(), IPT::I32, vec![]),
                ],
            );

            let derived_vftable = IV::new("Derived", "DerivedVftable", [derived_vfunc()]);
            let derived = IT::new(
                "Derived",
                8,
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
                M::new().with_definitions([
                    base_a.to_grammar(),
                    base_b.to_grammar(),
                    derived.to_grammar(),
                ]),
                [
                    base_a.to_semantic(),
                    base_b.to_semantic(),
                    derived.to_semantic(),
                    derived_vftable.to_semantic(),
                ],
            );
        }
    }
}
