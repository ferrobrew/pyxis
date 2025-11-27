use crate::{
    grammar::test_aliases::{int_literal, *},
    semantic::{error::SemanticError, semantic_state::SemanticState, types::test_aliases::*},
    span::ItemLocation,
};

use pretty_assertions::assert_eq;

mod alignment;
mod inheritance;
mod min_size;
mod util;
use util::*;

#[test]
fn can_resolve_basic_struct() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([
                TS::field((V::Public, "field_1"), T::ident("i32")),
                TS::field((V::Private, "_"), T::unknown(4)),
                TS::field((V::Public, "field_2"), T::ident("u64")),
            ])
            .with_attributes([A::align(8)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (16, 8),
                STD::new().with_regions([
                    SR::field((SV::Public, "field_1"), ST::raw("i32")),
                    SR::field((SV::Private, "_field_4"), unknown(4)),
                    SR::field((SV::Public, "field_2"), ST::raw("u64")),
                ]),
            ),
        )],
    );
}

#[test]
fn can_resolve_pointer_to_another_struct() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "TestType1"),
                TD::new([TS::field((V::Public, "field_1"), T::ident("u64"))]),
            ),
            ID::new(
                (V::Public, "TestType2"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("i32")),
                    TS::field((V::Public, "field_2"), T::ident("TestType1"))
                        .with_attributes([A::address(8)]),
                    TS::field(
                        (V::Public, "field_3"),
                        T::ident("TestType1").const_pointer(),
                    ),
                    TS::field((V::Public, "field_4"), T::ident("TestType1").mut_pointer()),
                ])
                .with_attributes([A::align(8)]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::TestType1"),
                SISR::new(
                    (8, 8),
                    STD::new().with_regions([SR::field((SV::Public, "field_1"), ST::raw("u64"))]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::TestType2"),
                SISR::new(
                    (16 + 2 * pointer_size(), 8),
                    STD::new().with_regions([
                        SR::field((SV::Public, "field_1"), ST::raw("i32")),
                        SR::field((SV::Private, "_field_4"), unknown(4)),
                        SR::field((SV::Public, "field_2"), ST::raw("test::TestType1")),
                        SR::field(
                            (SV::Public, "field_3"),
                            ST::raw("test::TestType1").const_pointer(),
                        ),
                        SR::field(
                            (SV::Public, "field_4"),
                            ST::raw("test::TestType1").mut_pointer(),
                        ),
                    ]),
                ),
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
                    (V::Public, "TestType"),
                    TD::new([
                        TS::field((V::Public, "field_1"), T::ident("i32")),
                        TS::field((V::Private, "_"), T::unknown(4)),
                    ]),
                ),
                ID::new(
                    (V::Public, "Singleton"),
                    TD::new([
                        TS::field((V::Public, "max_num_1"), T::ident("u16"))
                            .with_attributes([A::address(0x78)]),
                        TS::field((V::Public, "max_num_2"), T::ident("u16")),
                        TS::field((V::Public, "test_type"), T::ident("TestType"))
                            .with_attributes([A::address(0xA00)]),
                        TS::field((V::Public, "settings"), T::unknown(804)),
                    ])
                    .with_attributes([A::size(0x1750), A::singleton(0x1_200_000)]),
                ),
            ])
            .with_impls([FB::new(
                "Singleton",
                [F::new(
                    (V::Public, "test_function"),
                    [
                        Ar::mut_self(),
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
                (SV::Public, "test::TestType"),
                SISR::new(
                    (8, pointer_size()),
                    STD::new().with_regions([
                        SR::field((SV::Public, "field_1"), ST::raw("i32")),
                        SR::field((SV::Private, "_field_4"), unknown(4)),
                    ]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Singleton"),
                SISR::new(
                    (0x1750, pointer_size()),
                    STD::new()
                        .with_regions([
                            SR::field((SV::Private, "_field_0"), unknown(0x78)),
                            SR::field((SV::Public, "max_num_1"), ST::raw("u16")),
                            SR::field((SV::Public, "max_num_2"), ST::raw("u16")),
                            SR::field((SV::Private, "_field_7c"), unknown(0x984)),
                            SR::field((SV::Public, "test_type"), ST::raw("test::TestType")),
                            SR::field((SV::Public, "settings"), unknown(804)),
                            SR::field((SV::Private, "_field_d2c"), unknown(0xA24)),
                        ])
                        .with_associated_functions([SF::new(
                            (SV::Public, "test_function"),
                            SFB::address(0x800_000),
                            SCC::for_member_function(pointer_size()),
                        )
                        .with_arguments([
                            SAr::mut_self(),
                            SAr::field("arg1".to_string(), ST::raw("test::TestType").mut_pointer()),
                            SAr::field("arg2", ST::raw("i32")),
                            SAr::field("arg3", ST::raw("u32").const_pointer()),
                        ])
                        .with_return_type(ST::raw("test::TestType").mut_pointer())])
                        .with_singleton(0x1_200_000),
                ),
            ),
        ],
    );
}

#[test]
fn will_eventually_terminate_with_an_unknown_type() {
    assert_ast_produces_error(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType2"),
            TD::new([TS::field((V::Private, "field_2"), T::ident("TestType1"))]),
        )]),
        |err| {
            matches!(
                err,
                SemanticError::TypeResolutionStalled { unresolved_types, resolved_types }
                if unresolved_types.contains(&"test::TestType2".to_string())
                    && resolved_types.is_empty()
            )
        },
    );
}

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
        .add_module(&module1, &IP::from("module1"), None)
        .unwrap();
    semantic_state
        .add_module(&module2, &IP::from("module2"), None)
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
fn will_fail_on_an_extern_without_size() {
    let err = build_state(
        &M::new().with_extern_types([("TestType".into(), As::default())]),
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(
        err.to_string().contains(
            "failed to find `size` attribute for extern type `TestType` in module `test`"
        )
    );
}

#[test]
fn can_resolve_embed_of_an_extern() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_extern_types([(
                "TestType1".into(),
                As::from_iter([A::size(16), A::align(4)]),
            )])
            .with_definitions([ID::new(
                (V::Public, "TestType2"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("u64")),
                    TS::field((V::Public, "field_2"), T::ident("TestType1")),
                    TS::field(
                        (V::Public, "field_3"),
                        T::ident("TestType1").const_pointer(),
                    ),
                    TS::field((V::Public, "field_4"), T::ident("TestType1").mut_pointer()),
                ])
                .with_attributes([A::align(8)]),
            )]),
        [
            SID::defined_resolved(
                (SV::Public, "test::TestType2"),
                SISR::new(
                    (24 + 2 * pointer_size(), 8),
                    STD::new().with_regions([
                        SR::field((SV::Public, "field_1"), ST::raw("u64")),
                        SR::field((SV::Public, "field_2"), ST::raw("test::TestType1")),
                        SR::field(
                            (SV::Public, "field_3"),
                            ST::raw("test::TestType1").const_pointer(),
                        ),
                        SR::field(
                            (SV::Public, "field_4"),
                            ST::raw("test::TestType1").mut_pointer(),
                        ),
                    ]),
                ),
            ),
            SID::category_resolved(
                (SV::Public, "test::TestType1"),
                SISR::new((16, 4), STD::new().with_regions([])),
                SIC::Extern,
            ),
        ],
    );
}

#[test]
fn can_generate_vftable() {
    let vftable_type = ST::raw("test::TestTypeVftable").const_pointer();
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::vftable([
                F::new(
                    (V::Public, "test_function0"),
                    [
                        Ar::mut_self(),
                        Ar::named("arg0", T::ident("u32")),
                        Ar::named("arg1", T::ident("f32")),
                    ],
                )
                .with_return_type("i32"),
                F::new(
                    (V::Public, "test_function1"),
                    [
                        Ar::mut_self(),
                        Ar::named("arg0", T::ident("u32")),
                        Ar::named("arg1", T::ident("f32")),
                    ],
                ),
            ])
            .with_attributes([A::size(4)])]),
        )]),
        [
            // TestType
            SID::defined_resolved(
                (SV::Public, "test::TestType"),
                SISR::new((pointer_size(), pointer_size()), {
                    STD::new()
                        .with_regions([SR::field((SV::Private, "vftable"), vftable_type.clone())])
                        .with_vftable(STV::new(
                            [
                                SF::new(
                                    (SV::Public, "test_function0"),
                                    SFB::vftable("test_function0"),
                                    SCC::for_member_function(pointer_size()),
                                )
                                .with_arguments([
                                    SAr::mut_self(),
                                    SAr::field("arg0", ST::raw("u32")),
                                    SAr::field("arg1", ST::raw("f32")),
                                ])
                                .with_return_type(ST::raw("i32")),
                                SF::new(
                                    (SV::Public, "test_function1"),
                                    SFB::vftable("test_function1"),
                                    SCC::for_member_function(pointer_size()),
                                )
                                .with_arguments([
                                    SAr::mut_self(),
                                    SAr::field("arg0", ST::raw("u32")),
                                    SAr::field("arg1", ST::raw("f32")),
                                ]),
                                make_vfunc(2),
                                make_vfunc(3),
                            ],
                            None,
                            vftable_type,
                        ))
                }),
            ),
            // TestTypeVftable
            SID::defined_resolved(
                (SV::Public, "test::TestTypeVftable"),
                SISR::new(
                    (4 * pointer_size(), pointer_size()),
                    STD::new().with_regions([
                        SR::field(
                            (SV::Public, "test_function0"),
                            ST::function(
                                SCC::for_member_function(pointer_size()),
                                [
                                    ("this", ST::raw("test::TestType").mut_pointer()),
                                    ("arg0", ST::raw("u32")),
                                    ("arg1", ST::raw("f32")),
                                ],
                                ST::raw("i32"),
                            ),
                        ),
                        SR::field(
                            (SV::Public, "test_function1"),
                            ST::function(
                                SCC::for_member_function(pointer_size()),
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
                    ]),
                ),
            ),
        ],
    );
}

#[test]
fn can_generate_vftable_with_indices() {
    let vftable_type = ST::raw("test::TestTypeVftable").const_pointer();
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::vftable([
                F::new(
                    (V::Public, "test_function0"),
                    [
                        Ar::mut_self(),
                        Ar::named("arg0", T::ident("u32")),
                        Ar::named("arg1", T::ident("f32")),
                    ],
                )
                .with_attributes([A::index(2)])
                .with_return_type("i32"),
                F::new(
                    (V::Public, "test_function1"),
                    [
                        Ar::mut_self(),
                        Ar::named("arg0", T::ident("u32")),
                        Ar::named("arg1", T::ident("f32")),
                    ],
                )
                .with_attributes([A::index(5)]),
            ])
            .with_attributes([A::size(8)])]),
        )]),
        [
            // TestType
            SID::defined_resolved(
                (SV::Public, "test::TestType"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new()
                        .with_regions([SR::field((SV::Private, "vftable"), vftable_type.clone())])
                        .with_vftable(STV::new(
                            [
                                make_vfunc(0),
                                make_vfunc(1),
                                SF::new(
                                    (SV::Public, "test_function0"),
                                    SFB::vftable("test_function0"),
                                    SCC::for_member_function(pointer_size()),
                                )
                                .with_arguments([
                                    SAr::mut_self(),
                                    SAr::field("arg0", ST::raw("u32")),
                                    SAr::field("arg1", ST::raw("f32")),
                                ])
                                .with_return_type(ST::raw("i32")),
                                make_vfunc(3),
                                make_vfunc(4),
                                SF::new(
                                    (SV::Public, "test_function1"),
                                    SFB::vftable("test_function1"),
                                    SCC::for_member_function(pointer_size()),
                                )
                                .with_arguments([
                                    SAr::mut_self(),
                                    SAr::field("arg0", ST::raw("u32")),
                                    SAr::field("arg1", ST::raw("f32")),
                                ]),
                                make_vfunc(6),
                                make_vfunc(7),
                            ],
                            None,
                            vftable_type,
                        )),
                ),
            ),
            // TestTypeVftable
            SID::defined_resolved(
                (SV::Public, "test::TestTypeVftable"),
                SISR::new(
                    (8 * pointer_size(), pointer_size()),
                    STD::new().with_regions([
                        make_vfunc_region(0),
                        make_vfunc_region(1),
                        SR::field(
                            (SV::Public, "test_function0"),
                            ST::function(
                                SCC::for_member_function(pointer_size()),
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
                            (SV::Public, "test_function1"),
                            ST::function(
                                SCC::for_member_function(pointer_size()),
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
                    ]),
                ),
            ),
        ],
    );
}

#[test]
fn will_propagate_calling_convention_for_impl_and_vftable() {
    let vftable_type = ST::raw("test::TestTypeVftable").const_pointer();
    assert_ast_produces_type_definitions(
        M::new()
            .with_definitions([ID::new(
                (V::Public, "TestType"),
                TD::new([TS::vftable([F::new(
                    (V::Public, "test_function0"),
                    [
                        Ar::mut_self(),
                        Ar::named("arg0", T::ident("u32")),
                        Ar::named("arg1", T::ident("f32")),
                    ],
                )
                .with_return_type("i32")
                .with_attributes([A::calling_convention("cdecl")])])]),
            )])
            .with_impls([FB::new(
                "TestType",
                [F::new(
                    (V::Public, "test_function"),
                    [Ar::named("arg1", T::ident("i32"))],
                )
                .with_attributes([A::address(0x800_000), A::calling_convention("cdecl")])
                .with_return_type(T::ident("i32"))],
            )]),
        [
            // TestType
            SID::defined_resolved(
                (SV::Public, "test::TestType"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    {
                        STD::new()
                            .with_regions([SR::field(
                                (SV::Private, "vftable"),
                                vftable_type.clone(),
                            )])
                            .with_vftable(STV::new(
                                [SF::new(
                                    (SV::Public, "test_function0"),
                                    SFB::vftable("test_function0"),
                                    SCC::for_member_function(pointer_size()),
                                )
                                .with_arguments([
                                    SAr::mut_self(),
                                    SAr::field("arg0", ST::raw("u32")),
                                    SAr::field("arg1", ST::raw("f32")),
                                ])
                                .with_calling_convention(SCC::Cdecl)
                                .with_return_type(ST::raw("i32"))],
                                None,
                                vftable_type,
                            ))
                    }
                    .with_associated_functions([SF::new(
                        (SV::Public, "test_function"),
                        SFB::address(0x800_000),
                        SCC::for_member_function(pointer_size()),
                    )
                    .with_calling_convention(SCC::Cdecl)
                    .with_arguments([SAr::field("arg1", ST::raw("i32"))])
                    .with_return_type(ST::raw("i32"))]),
                ),
            ),
            // TestTypeVftable
            SID::defined_resolved(
                (SV::Public, "test::TestTypeVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "test_function0"),
                        ST::function(
                            SCC::Cdecl,
                            [
                                ("this", ST::raw("test::TestType").mut_pointer()),
                                ("arg0", ST::raw("u32")),
                                ("arg1", ST::raw("f32")),
                            ],
                            ST::raw("i32"),
                        ),
                    )]),
                ),
            ),
        ],
    );
}

fn make_vfunc(index: usize) -> SF {
    let name = format!("_vfunc_{index}");
    SF::new(
        (SV::Private, name.clone()),
        SFB::vftable(name),
        SCC::for_member_function(pointer_size()),
    )
    .with_arguments([SAr::mut_self()])
}

fn make_vfunc_region(index: usize) -> SR {
    SR::field(
        (SV::Private, format!("_vfunc_{index}")),
        ST::function(
            SCC::for_member_function(pointer_size()),
            [("this", ST::raw("test::TestType").mut_pointer())],
            None,
        ),
    )
}

#[test]
fn can_define_extern_value() {
    let module1 = M::new().with_extern_values([EV::new(
        V::Public,
        "test",
        T::ident("u32").mut_pointer(),
        [A::address(0x1337)],
    )]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"), None)
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
        &SEV {
            visibility: SV::Public,
            name: "test".into(),
            type_: ST::raw("u32").mut_pointer(),
            address: 0x1337,
            location: ItemLocation::test(),
        }
    );
}

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
                    .with_fields([
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
                    .with_fields([("None", 0), ("Some", 1), ("Value", 2)])
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

#[test]
fn can_extract_copyable_and_cloneable_correctly() {
    // Check cloneable
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Private, "field_1"), T::ident("i32"))])
                .with_attributes([A::cloneable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                STD::new()
                    .with_regions([SR::field((SV::Private, "field_1"), ST::raw("i32"))])
                    .with_cloneable(true),
            ),
        )],
    );

    // Check copyable -> copyable + cloneable
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Private, "field_1"), T::ident("i32"))])
                .with_attributes([A::copyable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                STD::new()
                    .with_regions([SR::field((SV::Private, "field_1"), ST::raw("i32"))])
                    .with_copyable(true)
                    .with_cloneable(true),
            ),
        )],
    );
}

#[test]
fn can_extract_copyable_and_cloneable_for_enum_correctly() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            ED::new(
                T::ident("u32"),
                [ES::field("Item1"), ES::field("Item2")],
                [],
            )
            .with_attributes([A::cloneable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SED::new(ST::raw("u32"))
                    .with_fields([("Item1", 0), ("Item2", 1)])
                    .with_cloneable(true),
            ),
        )],
    );

    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            ED::new(
                T::ident("u32"),
                [ES::field("Item1"), ES::field("Item2")],
                [],
            )
            .with_attributes([A::copyable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SED::new(ST::raw("u32"))
                    .with_fields([("Item1", 0), ("Item2", 1)])
                    .with_copyable(true)
                    .with_cloneable(true),
            ),
        )],
    );
}

#[test]
fn can_handle_defaultable_on_primitive_types() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([
                TS::field((V::Private, "field_1"), T::ident("u64")),
                TS::field((V::Private, "field_2"), T::ident("f32").array(16)),
            ])
            .with_attributes([A::defaultable(), A::align(8)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (72, 8),
                STD::new()
                    .with_regions([
                        SR::field((SV::Private, "field_1"), ST::raw("u64")),
                        SR::field((SV::Private, "field_2"), ST::raw("f32").array(16)),
                    ])
                    .with_defaultable(true),
            ),
        )],
    );
}

#[test]
fn will_reject_defaultable_on_pointer() {
    let err = build_state(
        &M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field(
                (V::Private, "field_1"),
                T::ident("i32").mut_pointer(),
            )])
            .with_attributes([A::defaultable()]),
        )]),
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(err.to_string().contains(
        "field `field_1` of type `test::TestType` is not a defaultable type (pointer or function?)"
    ));
}

#[test]
fn will_reject_defaultable_on_enum_field() {
    let err = build_state(
        &M::new().with_definitions([
            ID::new(
                (V::Public, "TestType"),
                TD::new([TS::field((V::Private, "field_1"), T::ident("TestEnum"))])
                    .with_attributes([A::defaultable()]),
            ),
            ID::new(
                (V::Public, "TestEnum"),
                ED::new(T::ident("u32"), [ES::field("Item1")], []),
            ),
        ]),
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(
        err.to_string()
            .contains("field `field_1` of type `test::TestType` is not a defaultable type")
    );
}

#[test]
fn can_handle_defaultable_on_enum_with_default_field() {
    let err1 = build_state(
        &M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            ED::new(
                T::ident("u32"),
                [ES::field("Item1"), ES::field("Item2")],
                [],
            )
            .with_attributes([A::defaultable()]),
        )]),
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(
        err1.to_string().contains(
            "enum `test::TestType` is marked as defaultable but has no default variant set"
        )
    );

    let err2 = build_state(
        &M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
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
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(err2.to_string().contains(
        "enum `test::TestType` has a default variant set but is not marked as defaultable"
    ));

    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
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
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SED::new(ST::raw("u32"))
                    .with_fields([("Item1", 0), ("Item2", 1)])
                    .with_default(1),
            ),
        )],
    );
}

#[test]
fn will_reject_defaultable_on_non_defaultable_type() {
    let err = build_state(
        &M::new().with_definitions([
            ID::new(
                (V::Public, "TestType"),
                TD::new([TS::field(
                    (V::Private, "field_1"),
                    T::ident("TestNonDefaultable"),
                )])
                .with_attributes([A::defaultable()]),
            ),
            ID::new((V::Public, "TestNonDefaultable"), TD::new([])),
        ]),
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(
        err.to_string()
            .contains("field `field_1` of type `test::TestType` is not a defaultable type")
    );
}

#[test]
fn will_reject_types_that_are_larger_than_their_specified_size() {
    let err = build_state(
        &M::new().with_definitions([
            ID::new(
                (V::Public, "Matrix4"),
                TD::new([TS::field((V::Public, "data"), T::ident("f32").array(16))]),
            ),
            ID::new(
                (V::Public, "TestType"),
                TD::new([TS::field(
                    (V::Public, "matrices"),
                    T::ident("Matrix4").array(8),
                )])
                .with_attributes([A::size(0x100)]),
            ),
        ]),
        &IP::from("test"),
    )
    .unwrap_err();
    let err_str = err.to_string();
    assert!(err_str.contains("while processing `test::TestType`"));
    assert!(
        err_str.contains(
            "calculated size 512 for type `test::TestType` does not match target size 256"
        )
    );
    assert!(err_str.contains("is your target size correct?"));
}

#[test]
fn can_propagate_doc_comments() {
    let created_module = assert_ast_produces_type_definitions(
        M::new()
            .with_definitions([ID::new(
                (V::Private, "TestType"),
                TD::new([
                    TS::vftable([F::new((V::Private, "test_vfunc"), [Ar::const_self()])
                        .with_doc_comments(vec![" My test vfunc!".to_string()])]),
                    TS::field((V::Private, "field_1"), T::ident("u64"))
                        .with_doc_comments(vec![" This is a field doc comment".to_string()])
                        .with_attributes([A::address(8)]),
                ])
                .with_attributes([A::align(8)]),
            )
            .with_doc_comments(vec![" This is a doc comment".to_string()])])
            .with_impls([FB::new(
                "TestType",
                [F::new((V::Private, "test_func"), [Ar::const_self()])
                    .with_doc_comments(vec![" My test func!".to_string()])
                    .with_attributes([A::address(0x123)])],
            )])
            .with_doc_comments(vec![
                " This is a module doc comment".to_string(),
                " The best of its kind".to_string(),
            ]),
        [
            SID::defined_resolved(
                (SV::Private, "test::TestType"),
                SISR::new(
                    (16, 8),
                    STD::new()
                        .with_regions(filter_out_empty_regions([
                            SR::field(
                                (SV::Private, "vftable"),
                                ST::raw("test::TestTypeVftable").const_pointer(),
                            ),
                            pad_up_to_8_region(),
                            SR::field((SV::Private, "field_1"), ST::raw("u64"))
                                .with_doc([" This is a field doc comment"]),
                        ]))
                        .with_doc([" This is a doc comment"])
                        .with_vftable(STV::new(
                            [SF::new(
                                (SV::Private, "test_vfunc"),
                                SFB::vftable("test_vfunc"),
                                SCC::for_member_function(pointer_size()),
                            )
                            .with_arguments([SAr::const_self()])
                            .with_doc([" My test vfunc!"])],
                            None,
                            ST::raw("test::TestTypeVftable").const_pointer(),
                        ))
                        .with_associated_functions([SF::new(
                            (SV::Private, "test_func"),
                            SFB::address(0x123),
                            SCC::for_member_function(pointer_size()),
                        )
                        .with_arguments([SAr::const_self()])
                        .with_doc([" My test func!"])]),
                ),
            ),
            SID::defined_resolved(
                (SV::Private, "test::TestTypeVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Private, "test_vfunc"),
                        ST::function(
                            SCC::for_member_function(pointer_size()),
                            [("this", ST::raw("test::TestType").const_pointer())],
                            None,
                        ),
                    )
                    .with_doc([" My test vfunc!"])]),
                ),
            ),
        ],
    );

    assert_eq!(
        created_module.doc,
        vec![
            " This is a module doc comment".to_string(),
            " The best of its kind".to_string()
        ]
    );
}

#[test]
fn can_resolve_bitflags() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)),
                    BFS::field("Item3", int_literal(0b0100)),
                    BFS::field("Item4", int_literal(0b1000)),
                ],
                [A::singleton(0x1234)],
            ),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SBFD::new(ST::raw("u32"))
                    .with_fields([
                        ("Item1", 0b0001),
                        ("Item2", 0b0010),
                        ("Item3", 0b0100),
                        ("Item4", 0b1000),
                    ])
                    .with_singleton(0x1234),
            ),
        )],
    );
}

#[test]
fn bitflags_handle_defaultable_correctly() {
    let err1 = build_state(
        &M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)),
                ],
                [],
            )
            .with_attributes([A::defaultable()]),
        )]),
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(err1.to_string().contains(
        "bitflags `test::TestType` is marked as defaultable but has no default value set"
    ));

    let err2 = build_state(
        &M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)).with_attributes([A::default()]),
                ],
                [],
            )
            .with_attributes([]),
        )]),
        &IP::from("test"),
    )
    .unwrap_err();
    assert!(err2.to_string().contains(
        "bitflags `test::TestType` has a default value set but is not marked as defaultable"
    ));

    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)).with_attributes([A::default()]),
                ],
                [],
            )
            .with_attributes([A::defaultable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SBFD::new(ST::raw("u32"))
                    .with_fields([("Item1", 0b0001), ("Item2", 0b0010)])
                    .with_default(1),
            ),
        )],
    );
}

#[test]
fn bitflags_with_invalid_underlying_type_are_rejected() {
    for invalid_type in ["i8", "i16", "i32", "i64", "i128", "Lol"] {
        let err = build_state(
            &M::new().with_definitions([
                ID::new((V::Public, "Lol"), TD::new([])),
                ID::new(
                    (V::Public, "TestType"),
                    BFD::new(
                        T::ident(invalid_type),
                        [
                            BFS::field("Item1", int_literal(0b0001)),
                            BFS::field("Item2", int_literal(0b0010)),
                        ],
                        [],
                    ),
                ),
            ]),
            &IP::from("test"),
        )
        .unwrap_err();
        let expected = if invalid_type == "Lol" {
            "bitflags definition `test::TestType` has a type that is not a predefined type: test::Lol".to_string()
        } else {
            format!(
                "bitflags definition `test::TestType` has a type that is not an unsigned integer: {invalid_type}"
            )
        };
        assert!(err.to_string().contains(&expected));
    }

    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)),
                ],
                [],
            ),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SBFD::new(ST::raw("u32")).with_fields([("Item1", 0b0001), ("Item2", 0b0010)]),
            ),
        )],
    );
}

#[test]
fn can_resolve_freestanding_functions() {
    let ast = M::new()
        .with_definitions([ID::new(
            (V::Public, "TestEnum"),
            ED::new(T::ident("u32"), [ES::field("None")], []),
        )])
        .with_impls([FB::new(
            "TestEnum",
            [F::new((V::Public, "test"), []).with_attributes([A::address(0x123)])],
        )])
        .with_functions([
            F::new((V::Public, "freestanding"), []).with_attributes([A::address(0x456)]),
            F::new(
                (V::Private, "another_freestanding"),
                [Ar::named("arg1", T::ident("i32"))],
            )
            .with_attributes([A::address(0x789)])
            .with_return_type(T::ident("i32")),
        ]);

    let test_path = IP::from("test");
    let state = build_state(&ast, &test_path).unwrap();
    let module = state.modules().get(&test_path).unwrap();

    assert_eq!(module.functions().len(), 2);

    // Check first freestanding function
    let func1 = &module.functions()[0];
    assert_eq!(func1.name, "freestanding");
    assert_eq!(func1.visibility, SV::Public);
    assert_eq!(func1.arguments.len(), 0);
    assert_eq!(func1.return_type, None);
    assert_eq!(func1.calling_convention, SCC::System);
    assert!(matches!(func1.body, SFB::Address { address: 0x456 }));

    // Check second freestanding function
    let func2 = &module.functions()[1];
    assert_eq!(func2.name, "another_freestanding");
    assert_eq!(func2.visibility, SV::Private);
    assert_eq!(func2.arguments.len(), 1);
    assert!(matches!(&*func2.arguments[0], SAr::Field(name, _) if name == "arg1"));
    assert_eq!(func2.return_type, Some(ST::raw("i32")));
    assert_eq!(func2.calling_convention, SCC::System);
    assert!(matches!(func2.body, SFB::Address { address: 0x789 }));
}
