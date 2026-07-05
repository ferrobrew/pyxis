//! Tests for virtual function table (vftable) generation.

use crate::{
    grammar::test_aliases::*,
    semantic::{error::SemanticError, types::test_aliases::*},
    span::ItemLocation,
};

use super::util::*;

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

#[test]
fn will_reject_descending_vftable_indices() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "Widget"),
            TD::new([TS::vftable([
                F::new((V::Public, "GetSize"), [Ar::mut_self()])
                    .with_attributes([A::index(2)])
                    .with_return_type("u64"),
                F::new(
                    (V::Public, "SetFlags"),
                    [Ar::mut_self(), Ar::named("flags", T::ident("u32"))],
                )
                .with_attributes([A::index(4)]),
                F::new((V::Public, "GetFlags"), [Ar::mut_self()])
                    .with_attributes([A::index(3)])
                    .with_return_type("u32"),
            ])]),
        )]),
        SemanticError::VftableNonAscendingIndex {
            item_path: IP::from("test::Widget"),
            function_name: "GetFlags".to_string(),
            declared_index: 3,
            min_index: 5,
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn will_reject_duplicate_vftable_indices() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "Widget"),
            TD::new([TS::vftable([
                F::new((V::Public, "A"), [Ar::mut_self()]).with_attributes([A::index(2)]),
                F::new((V::Public, "B"), [Ar::mut_self()]).with_attributes([A::index(2)]),
            ])]),
        )]),
        SemanticError::VftableNonAscendingIndex {
            item_path: IP::from("test::Widget"),
            function_name: "B".to_string(),
            declared_index: 2,
            min_index: 3,
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn will_accept_explicit_index_matching_next_slot() {
    // index == output.len() is valid (no padding needed, goes to the declared slot)
    let vftable_type = ST::raw("test::TestTypeVftable").const_pointer();
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::vftable([
                F::new((V::Public, "f0"), [Ar::mut_self()]).with_attributes([A::index(0)]),
                F::new((V::Public, "f1"), [Ar::mut_self()]).with_attributes([A::index(1)]),
            ])]),
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
                                SF::new(
                                    (SV::Public, "f0"),
                                    SFB::vftable("f0"),
                                    SCC::for_member_function(pointer_size()),
                                )
                                .with_arguments([SAr::mut_self()]),
                                SF::new(
                                    (SV::Public, "f1"),
                                    SFB::vftable("f1"),
                                    SCC::for_member_function(pointer_size()),
                                )
                                .with_arguments([SAr::mut_self()]),
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
                    (2 * pointer_size(), pointer_size()),
                    STD::new().with_regions([
                        SR::field(
                            (SV::Public, "f0"),
                            ST::function(
                                SCC::for_member_function(pointer_size()),
                                [("this", ST::raw("test::TestType").mut_pointer())],
                                None,
                            ),
                        ),
                        SR::field(
                            (SV::Public, "f1"),
                            ST::function(
                                SCC::for_member_function(pointer_size()),
                                [("this", ST::raw("test::TestType").mut_pointer())],
                                None,
                            ),
                        ),
                    ]),
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
