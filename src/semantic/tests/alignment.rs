use super::*;
use crate::semantic::error::SemanticError;

#[test]
fn size_not_multiple_of_alignment_should_be_rejected() {
    assert_ast_produces_error(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([
                TS::field((V::Public, "field_1"), T::ident("i32")),
                TS::field((V::Private, "_"), T::unknown(3)),
            ])
            .with_attributes([A::align(4)]),
        )]),
        |err| {
            matches!(
                err,
                SemanticError::SizeNotAlignmentMultiple { size: 7, alignment: 4, item_path, .. }
                if item_path.to_string() == "test::TestType"
            )
        },
    );
}

#[test]
fn unaligned_field_should_be_rejected() {
    assert_ast_produces_error(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([
                TS::field((V::Public, "field_1"), T::ident("i32")).with_attributes([A::address(1)])
            ]),
        )]),
        |err| {
            matches!(
                err,
                SemanticError::FieldNotAligned {
                    field_name,
                    item_path,
                    address: 1,
                    required_alignment: 4,
                    ..
                }
                if field_name == "field_1" && item_path.to_string() == "test::TestType"
            )
        },
    );
}

#[test]
fn align_incongruent_with_fields_should_be_rejected() {
    assert_ast_produces_error(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([
                TS::field((V::Public, "field_1"), T::ident("i32")),
                TS::field((V::Public, "field_2"), T::ident("i32")),
                TS::field((V::Public, "field_3"), T::ident("u64")),
            ])
            .with_attributes([A::align(4)]),
        )]),
        |err| {
            matches!(
                err,
                SemanticError::AlignmentBelowMinimum {
                    alignment: 4,
                    required_alignment: 8,
                    item_path,
                    ..
                }
                if item_path.to_string() == "test::TestType"
            )
        },
    );
}

#[test]
fn both_align_and_packed_should_be_rejected() {
    assert_ast_produces_error(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Public, "field_1"), T::ident("i32"))])
                .with_attributes([A::align(4), A::packed()]),
        )]),
        |err| {
            matches!(
                err,
                SemanticError::ConflictingAttributes {
                    attr1,
                    attr2,
                    item_path,
                    ..
                }
                if (attr1 == "packed" && attr2 == "align" || attr1 == "align" && attr2 == "packed")
                    && item_path.to_string() == "test::TestType"
            )
        },
    );
}

#[test]
fn type_with_bool_at_end_should_be_rejected_due_to_alignment() {
    assert_ast_produces_error(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Public, "field_1"), T::ident("bool"))
                .with_attributes([A::address(0xEC4)])]),
        )]),
        |err| {
            matches!(
                err,
                SemanticError::SizeNotAlignmentMultiple {
                    size: 3781,
                    alignment,
                    item_path,
                    ..
                }
                if *alignment == pointer_size() && item_path.to_string() == "test::TestType"
            )
        },
    );

    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Public, "field_1"), T::ident("bool"))
                .with_attributes([A::address(0xEC4)])])
            .with_attributes([A::size(0xEC8)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (0xEC8, pointer_size()),
                STD::new().with_regions([
                    SR::field((SV::Private, "_field_0"), unknown(0xEC4)),
                    SR::field((SV::Public, "field_1"), ST::raw("bool")),
                    SR::field((SV::Private, "_field_ec5"), unknown(3)),
                ]),
            ),
        )],
    );
}
