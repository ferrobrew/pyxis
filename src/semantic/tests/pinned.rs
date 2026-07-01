//! Tests for the `#[pinned]` attribute — non-relocatable types.

use crate::{grammar::test_aliases::*, semantic::types::test_aliases::*};

use super::util::*;

#[test]
fn can_extract_pinned_correctly() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Private, "field_1"), T::ident("i32"))])
                .with_attributes([A::pinned()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                STD::new()
                    .with_regions([SR::field((SV::Private, "field_1"), ST::raw("i32"))])
                    .with_pinned(true),
            ),
        )],
    );
}

#[test]
fn pinned_defaults_to_false() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Private, "field_1"), T::ident("i32"))]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                STD::new()
                    .with_regions([SR::field((SV::Private, "field_1"), ST::raw("i32"))])
                    .with_pinned(false),
            ),
        )],
    );
}

#[test]
fn can_extract_pinned_for_enum() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            ED::new(
                T::ident("u32"),
                [
                    ES::field("Item1"),
                    ES::field("Item2").with_attributes([A::default()]),
                ],
                [A::pinned(), A::defaultable()],
            ),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SED::new(ST::raw("u32"))
                    .with_variants([("Item1", 0), ("Item2", 1)])
                    .with_default(1)
                    .with_pinned(true),
            ),
        )],
    );
}

#[test]
fn can_extract_pinned_for_bitflags() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)).with_attributes([A::default()]),
                ],
                [A::pinned(), A::defaultable()],
            ),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SBFD::new(ST::raw("u32"))
                    .with_flags([("Item1", 0b0001), ("Item2", 0b0010)])
                    .with_default(1)
                    .with_pinned(true),
            ),
        )],
    );
}

#[test]
fn pinned_can_combine_with_defaultable() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Private, "field_1"), T::ident("i32"))])
                .with_attributes([A::pinned(), A::defaultable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                STD::new()
                    .with_regions([SR::field((SV::Private, "field_1"), ST::raw("i32"))])
                    .with_defaultable(true)
                    .with_pinned(true),
            ),
        )],
    );
}
