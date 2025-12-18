//! Tests for bitflags type resolution.

use crate::{
    grammar::test_aliases::{int_literal, *},
    semantic::{error::SemanticError, types::test_aliases::*},
    span::ItemLocation,
};

use super::util::*;

#[test]
fn can_resolve_bitflags() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)),
                    BFS::field("Item3", int_literal(0b0100)),
                    BFS::field("Item4", int_literal(0b1000)),
                ],
                [A::singleton(0x1234)],
            ),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SBFD::new(ST::raw("u32"))
                    .with_flags([
                        ("Item1", 0b0001),
                        ("Item2", 0b0010),
                        ("Item3", 0b0100),
                        ("Item4", 0b1000),
                    ])
                    .with_singleton(0x1234),
            ),
        )],
    );
}

#[test]
fn bitflags_handle_defaultable_correctly() {
    // Bitflags marked as defaultable but has no default value
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)),
                ],
                [],
            )
            .with_attributes([A::defaultable()]),
        )]),
        SemanticError::BitflagsDefaultableMissingDefault {
            item_path: IP::from("test::TestType"),
            location: ItemLocation::test(),
        },
    );

    // Bitflags has a default value but is not marked as defaultable
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)).with_attributes([A::default()]),
                ],
                [],
            )
            .with_attributes([]),
        )]),
        SemanticError::BitflagsDefaultWithoutDefaultable {
            item_path: IP::from("test::TestType"),
            location: ItemLocation::test(),
        },
    );

    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)).with_attributes([A::default()]),
                ],
                [],
            )
            .with_attributes([A::defaultable()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SBFD::new(ST::raw("u32"))
                    .with_flags([("Item1", 0b0001), ("Item2", 0b0010)])
                    .with_default(1),
            ),
        )],
    );
}

#[test]
fn bitflags_with_invalid_underlying_type_are_rejected() {
    use crate::semantic::error::BitflagsExpectedType;

    // Test signed integer types (should fail with UnsignedInteger expected)
    for invalid_type in ["i8", "i16", "i32", "i64", "i128"] {
        assert_ast_produces_exact_error(
            M::new().with_definitions([
                ID::new((V::Public, "Lol"), TD::new([])),
                ID::new(
                    (V::Public, "TestType"),
                    BFD::new(
                        T::ident(invalid_type),
                        [
                            BFS::field("Item1", int_literal(0b0001)),
                            BFS::field("Item2", int_literal(0b0010)),
                        ],
                        [],
                    ),
                ),
            ]),
            SemanticError::BitflagsInvalidType {
                expected: BitflagsExpectedType::UnsignedInteger,
                found: ST::raw(invalid_type),
                item_path: IP::from("test::TestType"),
                location: ItemLocation::test(),
            },
        );
    }

    // Test non-predefined type (should fail with PredefinedType expected)
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            ID::new((V::Public, "Lol"), TD::new([])),
            ID::new(
                (V::Public, "TestType"),
                BFD::new(
                    T::ident("Lol"),
                    [
                        BFS::field("Item1", int_literal(0b0001)),
                        BFS::field("Item2", int_literal(0b0010)),
                    ],
                    [],
                ),
            ),
        ]),
        SemanticError::BitflagsInvalidType {
            expected: BitflagsExpectedType::PredefinedType,
            found: ST::raw("test::Lol"),
            item_path: IP::from("test::TestType"),
            location: ItemLocation::test(),
        },
    );

    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            BFD::new(
                T::ident("u32"),
                [
                    BFS::field("Item1", int_literal(0b0001)),
                    BFS::field("Item2", int_literal(0b0010)),
                ],
                [],
            ),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::TestType"),
            SISR::new(
                (4, 4),
                SBFD::new(ST::raw("u32")).with_flags([("Item1", 0b0001), ("Item2", 0b0010)]),
            ),
        )],
    );
}
