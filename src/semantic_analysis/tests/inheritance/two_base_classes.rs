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
                    TS::vftable([vfunc_grammar("derived_vfunc")], []),
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
                    TS::vftable([vfunc_grammar("base_a_vfunc")], []),
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
