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

use super::*;

#[test]
fn a0_b0_d0() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "BaseA"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("i32")),
                    TS::field((V::Public, "field_2"), T::ident("u64"))
                        .with_attributes([A::address(8)]),
                ])
                .with_attributes([A::align(8), A::size(16)]),
            ),
            ID::new(
                (V::Public, "BaseB"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("u64")),
                    TS::field((V::Public, "field_2"), T::ident("i32")),
                ])
                .with_attributes([A::align(8), A::size(16)]),
            ),
            ID::new(
                (V::Public, "Derived"),
                TD::new([
                    TS::field((V::Public, "base_a"), T::ident("BaseA"))
                        .with_attributes([A::base()]),
                    TS::field((V::Public, "base_b"), T::ident("BaseB"))
                        .with_attributes([A::base()]),
                ])
                .with_attributes([A::align(8)]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::BaseA"),
                SISR::new(
                    (16, 8),
                    STD::new().with_regions([
                        SR::field((SV::Public, "field_1"), ST::raw("i32")),
                        SR::field((SV::Private, "_field_4"), ST::array(ST::raw("u8"), 4)),
                        SR::field((SV::Public, "field_2"), ST::raw("u64")),
                    ]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseB"),
                SISR::new(
                    (16, 8),
                    STD::new().with_regions([
                        SR::field((SV::Public, "field_1"), ST::raw("u64")),
                        SR::field((SV::Public, "field_2"), ST::raw("i32")),
                        SR::field((SV::Private, "_field_c"), ST::array(ST::raw("u8"), 4)),
                    ]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Derived"),
                SISR::new(
                    (32, 8),
                    STD::new().with_regions([
                        SR::field((SV::Public, "base_a"), ST::raw("test::BaseA")).marked_as_base(),
                        SR::field((SV::Public, "base_b"), ST::raw("test::BaseB")).marked_as_base(),
                    ]),
                ),
            ),
        ],
    );
}

#[test]
fn a0_b0_d1() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "BaseA"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("i32")),
                    TS::field((V::Public, "field_2"), T::ident("u64"))
                        .with_attributes([A::address(8)]),
                ])
                .with_attributes([A::align(8), A::size(16)]),
            ),
            ID::new(
                (V::Public, "BaseB"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("u64")),
                    TS::field((V::Public, "field_2"), T::ident("i32")),
                ])
                .with_attributes([A::align(8), A::size(16)]),
            ),
            ID::new(
                (V::Public, "Derived"),
                TD::new([
                    TS::vftable([vfunc_grammar("derived_vfunc")]),
                    TS::field((V::Public, "base_a"), T::ident("BaseA"))
                        .with_attributes([A::base(), A::address(8)]),
                    TS::field((V::Public, "base_b"), T::ident("BaseB"))
                        .with_attributes([A::base()]),
                ])
                .with_attributes([A::align(8)]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::BaseA"),
                SISR::new(
                    (16, 8),
                    STD::new().with_regions([
                        SR::field((SV::Public, "field_1"), ST::raw("i32")),
                        SR::field((SV::Private, "_field_4"), ST::array(ST::raw("u8"), 4)),
                        SR::field((SV::Public, "field_2"), ST::raw("u64")),
                    ]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseB"),
                SISR::new(
                    (16, 8),
                    STD::new().with_regions([
                        SR::field((SV::Public, "field_1"), ST::raw("u64")),
                        SR::field((SV::Public, "field_2"), ST::raw("i32")),
                        SR::field((SV::Private, "_field_c"), ST::array(ST::raw("u8"), 4)),
                    ]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Derived"),
                SISR::new(
                    (40, 8),
                    STD::new()
                        .with_regions(filter_out_empty_regions([
                            SR::field(
                                (SV::Private, "vftable"),
                                ST::raw("test::DerivedVftable").const_pointer(),
                            ),
                            pad_up_to_8_region(),
                            SR::field((SV::Public, "base_a"), ST::raw("test::BaseA"))
                                .marked_as_base(),
                            SR::field((SV::Public, "base_b"), ST::raw("test::BaseB"))
                                .marked_as_base(),
                        ]))
                        .with_vftable(STV::new(
                            [vfunc_semantic("derived_vfunc")],
                            None,
                            ST::raw("test::DerivedVftable").const_pointer(),
                        )),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::DerivedVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([vfunc_region("derived_vfunc", "test::Derived")]),
                ),
            ),
        ],
    );
}

#[test]
fn a1_b0_d0() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "BaseA"),
                TD::new([
                    TS::vftable([vfunc_grammar("base_a_vfunc")]),
                    TS::field((V::Public, "field_1"), T::ident("i32"))
                        .with_attributes([A::address(8)]),
                    TS::field((V::Public, "field_2"), T::ident("u64"))
                        .with_attributes([A::address(16)]),
                ])
                .with_attributes([A::align(8), A::size(24)]),
            ),
            ID::new(
                (V::Public, "BaseB"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("u64")),
                    TS::field((V::Public, "field_2"), T::ident("i32")),
                ])
                .with_attributes([A::align(8), A::size(16)]),
            ),
            ID::new(
                (V::Public, "Derived"),
                TD::new([
                    TS::field((V::Public, "base_a"), T::ident("BaseA"))
                        .with_attributes([A::base()]),
                    TS::field((V::Public, "base_b"), T::ident("BaseB"))
                        .with_attributes([A::base()]),
                ])
                .with_attributes([A::align(8)]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::BaseA"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions(filter_out_empty_regions([
                            SR::field(
                                (SV::Private, "vftable"),
                                ST::raw("test::BaseAVftable").const_pointer(),
                            ),
                            pad_up_to_8_region(),
                            SR::field((SV::Public, "field_1"), ST::raw("i32")),
                            SR::field((SV::Private, "_field_c"), ST::array(ST::raw("u8"), 4)),
                            SR::field((SV::Public, "field_2"), ST::raw("u64")),
                        ]))
                        .with_vftable(STV::new(
                            [vfunc_semantic("base_a_vfunc")],
                            None,
                            ST::raw("test::BaseAVftable").const_pointer(),
                        )),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseAVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([vfunc_region("base_a_vfunc", "test::BaseA")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseB"),
                SISR::new(
                    (16, 8),
                    STD::new().with_regions([
                        SR::field((SV::Public, "field_1"), ST::raw("u64")),
                        SR::field((SV::Public, "field_2"), ST::raw("i32")),
                        SR::field((SV::Private, "_field_c"), ST::array(ST::raw("u8"), 4)),
                    ]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Derived"),
                SISR::new(
                    (40, 8),
                    STD::new()
                        .with_regions([
                            SR::field((SV::Public, "base_a"), ST::raw("test::BaseA"))
                                .marked_as_base(),
                            SR::field((SV::Public, "base_b"), ST::raw("test::BaseB"))
                                .marked_as_base(),
                        ])
                        .with_vftable(STV::new(
                            [vfunc_semantic("base_a_vfunc")],
                            "base_a".to_string(),
                            ST::raw("test::BaseAVftable").const_pointer(),
                        )),
                ),
            ),
        ],
    );
}

#[test]
fn a1_b0_d1() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "BaseA"),
                TD::new([
                    TS::vftable([vfunc_grammar("base_a_vfunc")]),
                    TS::field((V::Public, "field_1"), T::ident("i32"))
                        .with_attributes([A::address(8)]),
                    TS::field((V::Public, "field_2"), T::ident("u64"))
                        .with_attributes([A::address(16)]),
                ])
                .with_attributes([A::align(8), A::size(24)]),
            ),
            ID::new(
                (V::Public, "BaseB"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("u64")),
                    TS::field((V::Public, "field_2"), T::ident("i32")),
                ])
                .with_attributes([A::align(8), A::size(16)]),
            ),
            ID::new(
                (V::Public, "Derived"),
                TD::new([
                    TS::vftable([
                        vfunc_grammar("base_a_vfunc"),
                        vfunc_grammar("derived_vfunc"),
                    ]),
                    TS::field((V::Public, "base_a"), T::ident("BaseA"))
                        .with_attributes([A::base()]),
                    TS::field((V::Public, "base_b"), T::ident("BaseB"))
                        .with_attributes([A::base()]),
                ])
                .with_attributes([A::align(8)]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::BaseA"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions(filter_out_empty_regions([
                            SR::field(
                                (SV::Private, "vftable"),
                                ST::raw("test::BaseAVftable").const_pointer(),
                            ),
                            pad_up_to_8_region(),
                            SR::field((SV::Public, "field_1"), ST::raw("i32")),
                            SR::field((SV::Private, "_field_c"), ST::array(ST::raw("u8"), 4)),
                            SR::field((SV::Public, "field_2"), ST::raw("u64")),
                        ]))
                        .with_vftable(STV::new(
                            [vfunc_semantic("base_a_vfunc")],
                            None,
                            ST::raw("test::BaseAVftable").const_pointer(),
                        )),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseAVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([vfunc_region("base_a_vfunc", "test::BaseA")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseB"),
                SISR::new(
                    (16, 8),
                    STD::new().with_regions([
                        SR::field((SV::Public, "field_1"), ST::raw("u64")),
                        SR::field((SV::Public, "field_2"), ST::raw("i32")),
                        SR::field((SV::Private, "_field_c"), ST::array(ST::raw("u8"), 4)),
                    ]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Derived"),
                SISR::new(
                    (40, 8),
                    STD::new()
                        .with_regions([
                            SR::field((SV::Public, "base_a"), ST::raw("test::BaseA"))
                                .marked_as_base(),
                            SR::field((SV::Public, "base_b"), ST::raw("test::BaseB"))
                                .marked_as_base(),
                        ])
                        .with_vftable(STV::new(
                            [
                                vfunc_semantic("base_a_vfunc"),
                                vfunc_semantic("derived_vfunc"),
                            ],
                            "base_a".to_string(),
                            ST::raw("test::DerivedVftable").const_pointer(),
                        )),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::DerivedVftable"),
                SISR::new(
                    (2 * pointer_size(), pointer_size()),
                    STD::new().with_regions([
                        vfunc_region("base_a_vfunc", "test::Derived"),
                        vfunc_region("derived_vfunc", "test::Derived"),
                    ]),
                ),
            ),
        ],
    );
}

#[test]
fn a1_b1_d0() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "BaseA"),
                TD::new([
                    TS::vftable([vfunc_grammar("base_a_vfunc")]),
                    TS::field((V::Public, "field_1"), T::ident("i32"))
                        .with_attributes([A::address(8)]),
                    TS::field((V::Public, "field_2"), T::ident("u64"))
                        .with_attributes([A::address(16)]),
                ])
                .with_attributes([A::align(8), A::size(24)]),
            ),
            ID::new(
                (V::Public, "BaseB"),
                TD::new([
                    TS::vftable([vfunc_grammar("base_b_vfunc")]),
                    TS::field((V::Public, "field_1"), T::ident("u64"))
                        .with_attributes([A::address(8)]),
                    TS::field((V::Public, "field_2"), T::ident("i32")),
                ])
                .with_attributes([A::align(8), A::size(24)]),
            ),
            ID::new(
                (V::Public, "Derived"),
                TD::new([
                    TS::field((V::Public, "base_a"), T::ident("BaseA"))
                        .with_attributes([A::base()]),
                    TS::field((V::Public, "base_b"), T::ident("BaseB"))
                        .with_attributes([A::base()]),
                ])
                .with_attributes([A::align(8)]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::BaseA"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions(filter_out_empty_regions([
                            SR::field(
                                (SV::Private, "vftable"),
                                ST::raw("test::BaseAVftable").const_pointer(),
                            ),
                            pad_up_to_8_region(),
                            SR::field((SV::Public, "field_1"), ST::raw("i32")),
                            SR::field((SV::Private, "_field_c"), ST::array(ST::raw("u8"), 4)),
                            SR::field((SV::Public, "field_2"), ST::raw("u64")),
                        ]))
                        .with_vftable(STV::new(
                            [vfunc_semantic("base_a_vfunc")],
                            None,
                            ST::raw("test::BaseAVftable").const_pointer(),
                        )),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseAVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([vfunc_region("base_a_vfunc", "test::BaseA")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseB"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions(filter_out_empty_regions([
                            SR::field(
                                (SV::Private, "vftable"),
                                ST::raw("test::BaseBVftable").const_pointer(),
                            ),
                            pad_up_to_8_region(),
                            SR::field((SV::Public, "field_1"), ST::raw("u64")),
                            SR::field((SV::Public, "field_2"), ST::raw("i32")),
                            SR::field((SV::Private, "_field_14"), ST::array(ST::raw("u8"), 4)),
                        ]))
                        .with_vftable(STV::new(
                            [vfunc_semantic("base_b_vfunc")],
                            None,
                            ST::raw("test::BaseBVftable").const_pointer(),
                        )),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseBVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([vfunc_region("base_b_vfunc", "test::BaseB")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Derived"),
                SISR::new(
                    (48, 8),
                    STD::new()
                        .with_regions([
                            SR::field((SV::Public, "base_a"), ST::raw("test::BaseA"))
                                .marked_as_base(),
                            SR::field((SV::Public, "base_b"), ST::raw("test::BaseB"))
                                .marked_as_base(),
                        ])
                        .with_vftable(STV::new(
                            [vfunc_semantic("base_a_vfunc")],
                            "base_a".to_string(),
                            ST::raw("test::BaseAVftable").const_pointer(),
                        ))
                        .with_associated_functions([
                            vfunc_semantic("base_b_vfunc").with_body(SFB::field("base_b"))
                        ]),
                ),
            ),
        ],
    );
}

#[test]
fn a1_b1_d1_with_associated_functions() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_definitions([
                ID::new(
                    (V::Public, "BaseA"),
                    TD::new([
                        TS::vftable([vfunc_grammar("base_a_vfunc")]),
                        TS::field((V::Public, "field_1"), T::ident("i32"))
                            .with_attributes([A::address(8)]),
                        TS::field((V::Public, "field_2"), T::ident("u64"))
                            .with_attributes([A::address(16)]),
                    ])
                    .with_attributes([A::align(8), A::size(24)]),
                ),
                ID::new(
                    (V::Public, "BaseB"),
                    TD::new([
                        TS::vftable([vfunc_grammar("base_b_vfunc")]),
                        TS::field((V::Public, "field_1"), T::ident("u64"))
                            .with_attributes([A::address(8)]),
                        TS::field((V::Public, "field_2"), T::ident("i32")),
                    ])
                    .with_attributes([A::align(8), A::size(24)]),
                ),
                ID::new(
                    (V::Public, "Derived"),
                    TD::new([
                        TS::vftable([
                            vfunc_grammar("base_a_vfunc"),
                            vfunc_grammar("derived_vfunc"),
                        ]),
                        TS::field((V::Public, "base_a"), T::ident("BaseA"))
                            .with_attributes([A::base()]),
                        TS::field((V::Public, "base_b"), T::ident("BaseB"))
                            .with_attributes([A::base()]),
                    ])
                    .with_attributes([A::align(8)]),
                ),
            ])
            .with_impls([
                FB::new(
                    "BaseA",
                    [F::new((V::Public, "base_a_associated"), [Ar::MutSelf])
                        .with_attributes([A::address(0x123)])],
                ),
                FB::new(
                    "BaseB",
                    [F::new((V::Public, "base_b_associated"), [Ar::MutSelf])
                        .with_attributes([A::address(0x456)])],
                ),
                FB::new(
                    "Derived",
                    [F::new((V::Public, "derived_associated"), [Ar::MutSelf])
                        .with_attributes([A::address(0x789)])],
                ),
            ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::BaseA"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions(filter_out_empty_regions([
                            SR::field(
                                (SV::Private, "vftable"),
                                ST::raw("test::BaseAVftable").const_pointer(),
                            ),
                            pad_up_to_8_region(),
                            SR::field((SV::Public, "field_1"), ST::raw("i32")),
                            SR::field((SV::Private, "_field_c"), ST::array(ST::raw("u8"), 4)),
                            SR::field((SV::Public, "field_2"), ST::raw("u64")),
                        ]))
                        .with_vftable(STV::new(
                            [vfunc_semantic("base_a_vfunc")],
                            None,
                            ST::raw("test::BaseAVftable").const_pointer(),
                        ))
                        .with_associated_functions([SF::new(
                            (SV::Public, "base_a_associated"),
                            SFB::address(0x123),
                        )
                        .with_arguments([SAr::MutSelf])]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseAVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([vfunc_region("base_a_vfunc", "test::BaseA")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseB"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions(filter_out_empty_regions([
                            SR::field(
                                (SV::Private, "vftable"),
                                ST::raw("test::BaseBVftable").const_pointer(),
                            ),
                            pad_up_to_8_region(),
                            SR::field((SV::Public, "field_1"), ST::raw("u64")),
                            SR::field((SV::Public, "field_2"), ST::raw("i32")),
                            SR::field((SV::Private, "_field_14"), ST::array(ST::raw("u8"), 4)),
                        ]))
                        .with_vftable(STV::new(
                            [vfunc_semantic("base_b_vfunc")],
                            None,
                            ST::raw("test::BaseBVftable").const_pointer(),
                        ))
                        .with_associated_functions([SF::new(
                            (SV::Public, "base_b_associated"),
                            SFB::address(0x456),
                        )
                        .with_arguments([SAr::MutSelf])]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseBVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([vfunc_region("base_b_vfunc", "test::BaseB")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Derived"),
                SISR::new(
                    (48, 8),
                    STD::new()
                        .with_regions([
                            SR::field((SV::Public, "base_a"), ST::raw("test::BaseA"))
                                .marked_as_base(),
                            SR::field((SV::Public, "base_b"), ST::raw("test::BaseB"))
                                .marked_as_base(),
                        ])
                        .with_vftable(STV::new(
                            [
                                vfunc_semantic("base_a_vfunc"),
                                vfunc_semantic("derived_vfunc"),
                            ],
                            "base_a".to_string(),
                            ST::raw("test::DerivedVftable").const_pointer(),
                        ))
                        .with_associated_functions([
                            SF::new((SV::Public, "base_a_associated"), SFB::field("base_a"))
                                .with_arguments([SAr::MutSelf]),
                            SF::new((SV::Public, "base_b_associated"), SFB::field("base_b"))
                                .with_arguments([SAr::MutSelf]),
                            vfunc_semantic("base_b_vfunc").with_body(SFB::field("base_b")),
                            SF::new((SV::Public, "derived_associated"), SFB::address(0x789))
                                .with_arguments([SAr::MutSelf]),
                        ]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::DerivedVftable"),
                SISR::new(
                    (2 * pointer_size(), pointer_size()),
                    STD::new().with_regions([
                        vfunc_region("base_a_vfunc", "test::Derived"),
                        vfunc_region("derived_vfunc", "test::Derived"),
                    ]),
                ),
            ),
        ],
    );
}

// TODO: vfuncs + associated funcs for a1_b1_d1
