//! Multiple levels of inheritance
//! ------------------------------
//! We set up three types: Base, Derived and DerivedDerived.
//! Derived derives from Base, and DerivedDerived derives from Derived.
//!
//! We need to test, where 'x' marks the presence of a vftable:
//!
//!  Base | Drved | Drv2d
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

#[test]
fn b0_d0_dd0() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Base"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("i32")),
                    TS::field((V::Public, "field_2"), T::ident("u64"))
                        .with_attributes([A::address(8)]),
                ])
                .with_attributes([A::align(8)]),
            ),
            ID::new(
                (V::Public, "Derived"),
                TD::new([
                    TS::field((V::Public, "base"), T::ident("Base")).with_attributes([A::base()])
                ])
                .with_attributes([A::align(8)]),
            ),
            ID::new(
                (V::Public, "DerivedDerived"),
                TD::new([TS::field((V::Public, "derived"), T::ident("Derived"))
                    .with_attributes([A::base()])])
                .with_attributes([A::align(8)]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Base"),
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
                (SV::Public, "test::Derived"),
                SISR::new(
                    (16, 8),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "base"),
                        ST::raw("test::Base"),
                    )
                    .marked_as_base()]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::DerivedDerived"),
                SISR::new(
                    (16, 8),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "derived"),
                        ST::raw("test::Derived"),
                    )
                    .marked_as_base()]),
                ),
            ),
        ],
    );
}

#[test]
fn b0_d0_dd1() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Base"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("i32")),
                    TS::field((V::Public, "field_2"), T::ident("u64"))
                        .with_attributes([A::address(8)]),
                ])
                .with_attributes([A::align(8)]),
            ),
            ID::new(
                (V::Public, "Derived"),
                TD::new([
                    TS::field((V::Public, "base"), T::ident("Base")).with_attributes([A::base()])
                ])
                .with_attributes([A::align(8)]),
            ),
            ID::new(
                (V::Public, "DerivedDerived"),
                TD::new([
                    TS::vftable([vfunc_grammar("derived_derived_vfunc")], []),
                    TS::field((V::Public, "derived"), T::ident("Derived"))
                        .with_attributes([A::base(), A::address(8)]),
                ])
                .with_attributes([A::align(8)]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Base"),
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
                (SV::Public, "test::Derived"),
                SISR::new(
                    (16, 8),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "base"),
                        ST::raw("test::Base"),
                    )
                    .marked_as_base()]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::DerivedDerived"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions(filter_out_empty_regions([
                            SR::field(
                                (SV::Private, "vftable"),
                                ST::raw("test::DerivedDerivedVftable").const_pointer(),
                            ),
                            pad_up_to_8_region(),
                            SR::field((SV::Public, "derived"), ST::raw("test::Derived"))
                                .marked_as_base(),
                        ]))
                        .with_vftable(STV::new(
                            [vfunc_semantic("derived_derived_vfunc")],
                            None,
                            ST::raw("test::DerivedDerivedVftable").const_pointer(),
                        )),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::DerivedDerivedVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([vfunc_region(
                        "derived_derived_vfunc",
                        "test::DerivedDerived",
                    )]),
                ),
            ),
        ],
    );
}

#[test]
fn b0_d1_dd0() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Base"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("i32")),
                    TS::field((V::Public, "field_2"), T::ident("u64"))
                        .with_attributes([A::address(8)]),
                ])
                .with_attributes([A::align(8)]),
            ),
            ID::new(
                (V::Public, "Derived"),
                TD::new([
                    TS::vftable([vfunc_grammar("derived_vfunc")], []),
                    TS::field((V::Public, "base"), T::ident("Base"))
                        .with_attributes([A::base(), A::address(8)]),
                ])
                .with_attributes([A::align(8)]),
            ),
            ID::new(
                (V::Public, "DerivedDerived"),
                TD::new([TS::field((V::Public, "derived"), T::ident("Derived"))
                    .with_attributes([A::base()])])
                .with_attributes([A::align(8)]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Base"),
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
                (SV::Public, "test::Derived"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions(filter_out_empty_regions([
                            SR::field(
                                (SV::Private, "vftable"),
                                ST::raw("test::DerivedVftable").const_pointer(),
                            ),
                            pad_up_to_8_region(),
                            SR::field((SV::Public, "base"), ST::raw("test::Base")).marked_as_base(),
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
            SID::defined_resolved(
                (SV::Public, "test::DerivedDerived"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions([SR::field(
                            (SV::Public, "derived"),
                            ST::raw("test::Derived"),
                        )
                        .marked_as_base()])
                        .with_vftable(STV::new(
                            [vfunc_semantic("derived_vfunc")],
                            "derived".to_string(),
                            ST::raw("test::DerivedVftable").const_pointer(),
                        )),
                ),
            ),
        ],
    );
}

#[test]
fn b0_d1_dd1() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Base"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("i32")),
                    TS::field((V::Public, "field_2"), T::ident("u64"))
                        .with_attributes([A::address(8)]),
                ])
                .with_attributes([A::align(8)]),
            ),
            ID::new(
                (V::Public, "Derived"),
                TD::new([
                    TS::vftable([vfunc_grammar("derived_vfunc")], []),
                    TS::field((V::Public, "base"), T::ident("Base"))
                        .with_attributes([A::base(), A::address(8)]),
                ])
                .with_attributes([A::align(8)]),
            ),
            ID::new(
                (V::Public, "DerivedDerived"),
                TD::new([
                    TS::vftable(
                        [
                            vfunc_grammar("derived_vfunc"),
                            vfunc_grammar("derived_derived_vfunc"),
                        ],
                        [],
                    ),
                    TS::field((V::Public, "derived"), T::ident("Derived"))
                        .with_attributes([A::base()]),
                ])
                .with_attributes([A::align(8)]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Base"),
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
                (SV::Public, "test::Derived"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions(filter_out_empty_regions([
                            SR::field(
                                (SV::Private, "vftable"),
                                ST::raw("test::DerivedVftable").const_pointer(),
                            ),
                            pad_up_to_8_region(),
                            SR::field((SV::Public, "base"), ST::raw("test::Base")).marked_as_base(),
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
            SID::defined_resolved(
                (SV::Public, "test::DerivedDerived"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions([SR::field(
                            (SV::Public, "derived"),
                            ST::raw("test::Derived"),
                        )
                        .marked_as_base()])
                        .with_vftable(STV::new(
                            [
                                vfunc_semantic("derived_vfunc"),
                                vfunc_semantic("derived_derived_vfunc"),
                            ],
                            "derived".to_string(),
                            ST::raw("test::DerivedDerivedVftable").const_pointer(),
                        )),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::DerivedDerivedVftable"),
                SISR::new(
                    (2 * pointer_size(), pointer_size()),
                    STD::new().with_regions([
                        vfunc_region("derived_vfunc", "test::DerivedDerived"),
                        vfunc_region("derived_derived_vfunc", "test::DerivedDerived"),
                    ]),
                ),
            ),
        ],
    );
}
