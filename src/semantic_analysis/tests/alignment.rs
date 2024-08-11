use super::*;

#[test]
fn size_not_multiple_of_alignment_should_be_rejected() {
    assert_ast_produces_failure(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            TD::new([
                TS::field(V::Public, "field_1", T::ident("i32")),
                TS::field(V::Private, "_", T::unknown(3)),
            ])
            .with_attributes([A::align(4)]),
        )]),
        "the type `test::TestType` has a size of 7, which is not a multiple of its alignment 4",
    );
}

#[test]
fn unaligned_field_should_be_rejected() {
    assert_ast_produces_failure(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            TD::new([TS::field(V::Public, "field_1", T::ident("i32"))
                .with_attributes([A::address(1)])]),
        )]),
        "field `field_1` of type `test::TestType` is located at 0x1, which is not divisible by 4 (the alignment of the type of the field)",
    );
}

#[test]
fn align_incongruent_with_fields_should_be_rejected() {
    assert_ast_produces_failure(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            TD::new([
                TS::field(V::Public, "field_1", T::ident("i32")),
                TS::field(V::Public, "field_2", T::ident("i32")),
                TS::field(V::Public, "field_3", T::ident("u64")),
            ])
            .with_attributes([A::align(4)]),
        )]),
        "alignment 4 is less than minimum required alignment 8 for type `test::TestType`",
    );
}

#[test]
fn both_align_and_packed_should_be_rejected() {
    assert_ast_produces_failure(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            TD::new([TS::field(V::Public, "field_1", T::ident("i32"))])
                .with_attributes([A::align(4), A::packed()]),
        )]),
        "cannot specify both `packed` and `align` attributes for type `test::TestType`",
    );
}

#[test]
fn type_with_bool_at_end_should_be_rejected_due_to_alignment() {
    assert_ast_produces_failure(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            TD::new([TS::field(V::Public, "field_1", T::ident("bool"))
                .with_attributes([A::address(0xEC4)])]),
        )]),
        &format!(
            concat!(
                "the type `test::TestType` has a size of 3781, ",
                "which is not a multiple of its alignment {}",
            ),
            pointer_size()
        ),
    );

    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            V::Public,
            "TestType",
            TD::new([TS::field(V::Public, "field_1", T::ident("bool"))
                .with_attributes([A::address(0xEC4)])])
            .with_attributes([A::size(0xEC8)]),
        )]),
        [SID::defined_resolved(
            SV::Public,
            "test::TestType",
            SISR::new(
                0xEC8,
                pointer_size(),
                STD::new().with_regions([
                    SR::field(SV::Private, "_field_0", unknown(0xEC4)),
                    SR::field(SV::Public, "field_1", ST::raw("bool")),
                    SR::field(SV::Private, "_field_ec5", unknown(3)),
                ]),
            ),
        )],
    );
}
