use super::*;
use crate::semantic::error::SemanticError;

#[test]
fn min_size_pads_to_minimum() {
    // A type with natural size 4, min_size 16, should be padded to 16
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Public, "field_1"), T::ident("i32"))])
                .with_attributes([A::min_size(16)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (16, pointer_size()),
                STD::new().with_regions([
                    SR::field((SV::Public, "field_1"), ST::raw("i32")),
                    SR::field((SV::Private, "_field_4"), unknown(12)),
                ]),
            ),
        )],
    );
}

#[test]
fn min_size_rounds_up_to_alignment() {
    // A type with alignment 16 and min_size 24 should be rounded up to 32
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Public, "field_1"), T::ident("i32"))])
                .with_attributes([A::min_size(24), A::align(16)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (32, 16),
                STD::new().with_regions([
                    SR::field((SV::Public, "field_1"), ST::raw("i32")),
                    SR::field((SV::Private, "_field_4"), unknown(28)),
                ]),
            ),
        )],
    );
}

#[test]
fn min_size_allows_natural_size_larger_than_minimum() {
    // A type with natural size 16 and min_size 8 should remain size 16
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([
                TS::field((V::Public, "field_1"), T::ident("u64")),
                TS::field((V::Public, "field_2"), T::ident("u64")),
            ])
            .with_attributes([A::min_size(8), A::align(8)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (16, 8),
                STD::new().with_regions([
                    SR::field((SV::Public, "field_1"), ST::raw("u64")),
                    SR::field((SV::Public, "field_2"), ST::raw("u64")),
                ]),
            ),
        )],
    );
}

#[test]
fn min_size_with_explicit_address() {
    // min_size should work with explicit field addresses
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([
                TS::field((V::Public, "field_1"), T::ident("i32")),
                TS::field((V::Public, "field_2"), T::ident("i32")).with_attributes([A::address(8)]),
            ])
            .with_attributes([A::min_size(32)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (32, pointer_size()),
                STD::new().with_regions([
                    SR::field((SV::Public, "field_1"), ST::raw("i32")),
                    SR::field((SV::Private, "_field_4"), unknown(4)),
                    SR::field((SV::Public, "field_2"), ST::raw("i32")),
                    SR::field((SV::Private, "_field_c"), unknown(20)),
                ]),
            ),
        )],
    );
}

#[test]
fn both_size_and_min_size_should_be_rejected() {
    assert_ast_produces_error(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Public, "field_1"), T::ident("i32"))])
                .with_attributes([A::size(16), A::min_size(16)]),
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
                if (attr1 == "size" && attr2 == "min_size" || attr1 == "min_size" && attr2 == "size")
                    && item_path.to_string() == "test::TestType"
            )
        },
    );
}

#[test]
fn min_size_equal_to_natural_size() {
    // min_size equal to natural size should work fine
    // Note: alignment is 4 (from i32) since there's only one field
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Public, "field_1"), T::ident("i32"))])
                .with_attributes([A::min_size(4)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                STD::new().with_regions([SR::field((SV::Public, "field_1"), ST::raw("i32"))]),
            ),
        )],
    );
}

#[test]
fn min_size_with_packed() {
    // min_size with packed should use alignment 1
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([
                TS::field((V::Public, "field_1"), T::ident("i32")),
                TS::field((V::Public, "field_2"), T::ident("bool")),
            ])
            .with_attributes([A::packed(), A::min_size(10)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (10, 1),
                STD::new()
                    .with_regions([
                        SR::field((SV::Public, "field_1"), ST::raw("i32")),
                        SR::field((SV::Public, "field_2"), ST::raw("bool")),
                        SR::field((SV::Private, "_field_5"), unknown(5)),
                    ])
                    .with_packed(true),
            ),
        )],
    );
}
