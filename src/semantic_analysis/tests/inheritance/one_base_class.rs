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

use super::*;

#[test]
fn b0_d0() {
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
        ],
    );
}

#[test]
fn b0_d1() {
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
                            ["vftable"],
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
fn b1_d0() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Base"),
                TD::new([
                    TS::vftable([vfunc_grammar("base_vfunc")], []),
                    TS::field((V::Public, "field_1"), T::ident("i32"))
                        .with_attributes([A::address(8)]),
                    TS::field((V::Public, "field_2"), T::ident("u64"))
                        .with_attributes([A::address(16)]),
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
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Base"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions(filter_out_empty_regions([
                            SR::field(
                                (SV::Private, "vftable"),
                                ST::raw("test::BaseVftable").const_pointer(),
                            ),
                            pad_up_to_8_region(),
                            SR::field((SV::Public, "field_1"), ST::raw("i32")),
                            SR::field((SV::Private, "_field_c"), ST::array(ST::raw("u8"), 4)),
                            SR::field((SV::Public, "field_2"), ST::raw("u64")),
                        ]))
                        .with_vftable(STV::new(
                            [vfunc_semantic("base_vfunc")],
                            ["vftable"],
                            ST::raw("test::BaseVftable").const_pointer(),
                        )),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([vfunc_region("base_vfunc", "test::Base")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Derived"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions([
                            SR::field((SV::Public, "base"), ST::raw("test::Base")).marked_as_base()
                        ])
                        .with_vftable(STV::new(
                            [vfunc_semantic("base_vfunc")],
                            ["base", "vftable"],
                            ST::raw("test::BaseVftable").const_pointer(),
                        )),
                ),
            ),
        ],
    );
}

#[test]
fn b1_d1() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Base"),
                TD::new([
                    TS::vftable([vfunc_grammar("base_vfunc")], []),
                    TS::field((V::Public, "field_1"), T::ident("i32"))
                        .with_attributes([A::address(8)]),
                    TS::field((V::Public, "field_2"), T::ident("u64"))
                        .with_attributes([A::address(16)]),
                ])
                .with_attributes([A::align(8)]),
            ),
            ID::new(
                (V::Public, "Derived"),
                TD::new([
                    TS::vftable(
                        [vfunc_grammar("base_vfunc"), vfunc_grammar("derived_vfunc")],
                        [],
                    ),
                    TS::field((V::Public, "base"), T::ident("Base")).with_attributes([A::base()]),
                ])
                .with_attributes([A::align(8)]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Base"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions(filter_out_empty_regions([
                            SR::field(
                                (SV::Private, "vftable"),
                                ST::raw("test::BaseVftable").const_pointer(),
                            ),
                            pad_up_to_8_region(),
                            SR::field((SV::Public, "field_1"), ST::raw("i32")),
                            SR::field((SV::Private, "_field_c"), ST::array(ST::raw("u8"), 4)),
                            SR::field((SV::Public, "field_2"), ST::raw("u64")),
                        ]))
                        .with_vftable(STV::new(
                            [vfunc_semantic("base_vfunc")],
                            ["vftable"],
                            ST::raw("test::BaseVftable").const_pointer(),
                        )),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::BaseVftable"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([vfunc_region("base_vfunc", "test::Base")]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Derived"),
                SISR::new(
                    (24, 8),
                    STD::new()
                        .with_regions([
                            SR::field((SV::Public, "base"), ST::raw("test::Base")).marked_as_base()
                        ])
                        .with_vftable(STV::new(
                            [
                                vfunc_semantic("base_vfunc"),
                                vfunc_semantic("derived_vfunc"),
                            ],
                            ["base", "vftable"],
                            ST::raw("test::DerivedVftable").const_pointer(),
                        )),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::DerivedVftable"),
                SISR::new(
                    (2 * pointer_size(), pointer_size()),
                    STD::new().with_regions([
                        vfunc_region("base_vfunc", "test::Derived"),
                        vfunc_region("derived_vfunc", "test::Derived"),
                    ]),
                ),
            ),
        ],
    );
}

#[test]
fn b1_d1_without_overlapping_vfuncs_will_fail() {
    assert_ast_produces_failure(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Base"),
                TD::new([
                    TS::vftable(
                        [vfunc_grammar("base_vfunc1"), vfunc_grammar("base_vfunc2")],
                        [],
                    ),
                    TS::field((V::Public, "field_1"), T::ident("i32"))
                        .with_attributes([A::address(8)]),
                    TS::field((V::Public, "field_2"), T::ident("u64"))
                        .with_attributes([A::address(16)]),
                ])
                .with_attributes([A::align(8)]),
            ),
            ID::new(
                (V::Public, "Derived"),
                TD::new([
                    TS::vftable(
                        [
                            vfunc_grammar("base_vfunc1"),
                            vfunc_grammar("not_base_vfunc2"),
                            vfunc_grammar("derived_vfunc"),
                        ],
                        [],
                    ),
                    TS::field((V::Public, "base"), T::ident("Base")).with_attributes([A::base()]),
                ])
                .with_attributes([A::align(8)]),
            ),
        ]),
        concat!(
            "while processing `test::Derived`\n",
            "vftable for `test::Derived` has function ",
            r#"`pub extern "thiscall" fn not_base_vfunc2(&mut self, arg0: u32, arg1: f32) -> i32` "#,
            "at index 1 but base class `base` has function ",
            r#"`pub extern "thiscall" fn base_vfunc2(&mut self, arg0: u32, arg1: f32) -> i32`"#
        ),
    );
}
