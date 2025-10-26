//! Tests for diamond inheritance (because C++ programmers can't be trusted to
//! avoid footguns)

use super::*;

#[test]
fn multiple_inheritance_with_vftables() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_definitions([
                ID::new(
                    (V::Public, "Base"),
                    TD::new([TS::vftable([vfunc_grammar("destructor")])])
                        .with_attributes([A::align(pointer_size())]),
                ),
                ID::new(
                    (V::Public, "BaseA"),
                    TD::new([
                        TS::vftable([vfunc_grammar("destructor")]),
                        TS::field((V::Public, "base"), T::ident("Base"))
                            .with_attributes([A::base()]),
                    ])
                    .with_attributes([A::align(pointer_size())]),
                ),
                ID::new(
                    (V::Public, "BaseB"),
                    TD::new([
                        TS::vftable([vfunc_grammar("destructor")]),
                        TS::field((V::Public, "base"), T::ident("Base"))
                            .with_attributes([A::base()]),
                    ])
                    .with_attributes([A::align(pointer_size())]),
                ),
                ID::new(
                    (V::Public, "Derived"),
                    TD::new([
                        TS::vftable([vfunc_grammar("destructor")]),
                        TS::field((V::Public, "base_a"), T::ident("BaseA"))
                            .with_attributes([A::base()]),
                        TS::field((V::Public, "base_b"), T::ident("BaseB"))
                            .with_attributes([A::base()]),
                    ])
                    .with_attributes([A::align(pointer_size())]),
                ),
            ])
            .with_impls([
                FB::new(
                    "BaseA",
                    [F::new((V::Public, "associated"), [Ar::MutSelf])
                        .with_attributes([A::address(0x123)])],
                ),
                FB::new(
                    "BaseB",
                    [F::new((V::Public, "associated"), [Ar::MutSelf])
                        .with_attributes([A::address(0x123)])],
                ),
            ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Base"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new()
                        .with_regions([SR::field(
                            (SV::Private, "vftable"),
                            ST::raw("test::BaseVftable").const_pointer(),
                        )])
                        .with_vftable(STV::new(
                            [vfunc_semantic("destructor")],
                            None,
                            ST::raw("test::BaseVftable").const_pointer(),
                        )),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([vfunc_region("destructor", "test::Base")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseA"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new()
                        .with_regions([
                            SR::field((SV::Public, "base"), ST::raw("test::Base")).marked_as_base()
                        ])
                        .with_vftable(STV::new(
                            [vfunc_semantic("destructor")],
                            "base".to_string(),
                            ST::raw("test::BaseAVftable").const_pointer(),
                        ))
                        .with_associated_functions([SF::new(
                            (SV::Public, "associated"),
                            SFB::address(0x123),
                            SCC::for_member_function(pointer_size()),
                        )
                        .with_arguments([SAr::MutSelf])]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseAVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([vfunc_region("destructor", "test::BaseA")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseB"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new()
                        .with_regions([
                            SR::field((SV::Public, "base"), ST::raw("test::Base")).marked_as_base()
                        ])
                        .with_vftable(STV::new(
                            [vfunc_semantic("destructor")],
                            "base".to_string(),
                            ST::raw("test::BaseBVftable").const_pointer(),
                        ))
                        .with_associated_functions([SF::new(
                            (SV::Public, "associated"),
                            SFB::address(0x123),
                            SCC::for_member_function(pointer_size()),
                        )
                        .with_arguments([SAr::MutSelf])]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseBVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([vfunc_region("destructor", "test::BaseB")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Derived"),
                SISR::new(
                    (2 * pointer_size(), pointer_size()),
                    STD::new()
                        .with_regions([
                            SR::field((SV::Public, "base_a"), ST::raw("test::BaseA"))
                                .marked_as_base(),
                            SR::field((SV::Public, "base_b"), ST::raw("test::BaseB"))
                                .marked_as_base(),
                        ])
                        .with_vftable(STV::new(
                            [vfunc_semantic("destructor")],
                            "base_a".to_string(),
                            ST::raw("test::DerivedVftable").const_pointer(),
                        ))
                        .with_associated_functions([
                            // associated
                            SF::new(
                                (SV::Public, "associated"),
                                SFB::field("base_a", "associated"),
                                SCC::for_member_function(pointer_size()),
                            )
                            .with_arguments([SAr::MutSelf]),
                            // base_b_associated
                            SF::new(
                                (SV::Public, "base_b_associated"),
                                SFB::field("base_b", "associated"),
                                SCC::for_member_function(pointer_size()),
                            )
                            .with_arguments([SAr::MutSelf]),
                            // base_b_destructor
                            SF::new(
                                (SV::Public, "base_b_destructor"),
                                SFB::field("base_b", "destructor"),
                                SCC::for_member_function(pointer_size()),
                            )
                            .with_arguments([
                                SAr::MutSelf,
                                SAr::field("arg0", ST::raw("u32")),
                                SAr::field("arg1", ST::raw("f32")),
                            ])
                            .with_return_type(ST::raw("i32"))
                            .with_calling_convention(SCC::for_member_function(pointer_size())),
                        ]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::DerivedVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([vfunc_region("destructor", "test::Derived")]),
                ),
            ),
        ],
    );
}
