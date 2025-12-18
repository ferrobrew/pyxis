//! Tests for documentation comment propagation.

use crate::{grammar::test_aliases::*, semantic::types::test_aliases::*};

use super::util::*;
use pretty_assertions::assert_eq;

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
