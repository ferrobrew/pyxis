//! Tests for rejecting zero-size types embedded as concrete fields (issue #105).
//!
//! A fieldless type (or a generic instantiation resolving to size 0) has no
//! representable layout in backends that lack zero-size objects. Embedding it
//! as a concrete field would silently disagree with the semantic layout
//! assumption and shift every subsequent field. The diagnostic suggests
//! `#[min_size(1)]` or an explicit `#[size(...)]` on the field's type.

use crate::{
    grammar::test_aliases::*,
    semantic::{error::SemanticError, types::test_aliases::*},
    span::ItemLocation,
};

use super::util::*;

/// A fieldless type (no fields → size 0) embedded as a field in another type
/// must produce `ZeroSizeFieldEmbedding`.
#[test]
fn embedding_zero_size_type_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            ID::new((V::Public, "Marker"), TD::new([])),
            ID::new(
                (V::Public, "Holder"),
                TD::new([TS::field((V::Public, "marker"), T::ident("Marker"))])
                    .with_attributes([A::size(pointer_size())]),
            ),
        ]),
        SemanticError::ZeroSizeFieldEmbedding {
            field_name: "marker".to_string(),
            item_path: IP::from("test::Holder"),
            field_type: ST::raw("test::Marker"),
            location: ItemLocation::test(),
        },
    );
}

/// The same fieldless type given a nonzero footprint via `#[min_size(8)]` can
/// be embedded as a field. The embedding succeeds with the correct resolved
/// size and regions.
#[test]
fn min_size_allows_zero_size_type_embedding() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Marker"),
                TD::new([]).with_attributes([A::min_size(8)]),
            ),
            ID::new(
                (V::Public, "Holder"),
                TD::new([
                    TS::field((V::Public, "marker"), T::ident("Marker")),
                    TS::field((V::Public, "value"), T::ident("u32"))
                        .with_attributes([A::address(8)]),
                ])
                .with_attributes([A::size(16), A::align(4)]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Holder"),
                SISR::new(
                    (16, 4),
                    STD::new().with_regions([
                        SR::field((SV::Public, "marker"), ST::raw("test::Marker")),
                        SR::field((SV::Public, "value"), ST::raw("u32")),
                        SR::field((SV::Private, "_field_c"), unknown(4)),
                    ]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::Marker"),
                SISR::new(
                    (8, 1),
                    STD::new().with_regions([SR::field((SV::Private, "_field_0"), unknown(8))]),
                ),
            ),
        ],
    );
}

/// A `*const void` field (pointer to a zero-size type) is allowed and does
/// not trigger the error. Pointers have nonzero size.
#[test]
fn pointer_to_zero_size_type_is_allowed() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Holder"),
            TD::new([TS::field(
                (V::Public, "ptr"),
                T::ident("void").const_pointer(),
            )])
            .with_attributes([A::size(pointer_size())]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::Holder"),
            SISR::new(
                (pointer_size(), pointer_size()),
                STD::new().with_regions([SR::field(
                    (SV::Public, "ptr"),
                    ST::raw("void").const_pointer(),
                )]),
            ),
        )],
    );
}

/// A `[void; 0]` field (zero-size array of a zero-size type) is allowed.
/// Zero-size arrays are handled by `Regions::push`'s existing special case
/// and are silently ignored. This documents that the check doesn't over-reach.
#[test]
fn array_of_zero_size_type_is_allowed() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Holder"),
            TD::new([
                TS::field((V::Public, "arr"), T::ident("void").array(0)),
                TS::field((V::Public, "value"), T::ident("u32")),
            ])
            .with_attributes([A::size(4)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::Holder"),
            SISR::new(
                (4, 4),
                STD::new().with_regions([SR::field((SV::Public, "value"), ST::raw("u32"))]),
            ),
        )],
    );
}

/// A generic instantiation that resolves to size 0 (e.g. `Wrapper<Empty>`
/// where `Wrapper` has no fields) as a field is rejected with
/// `ZeroSizeFieldEmbedding`. The diagnostic renders the full generic type.
#[test]
fn generic_instantiation_resolving_to_zero_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            ID::generic((V::Public, "Wrapper"), [TP::new("T")], TD::new([])),
            ID::new((V::Public, "Empty"), TD::new([])),
            ID::new(
                (V::Public, "Holder"),
                TD::new([TS::field(
                    (V::Public, "wrapper"),
                    T::generic("Wrapper", [T::ident("Empty")]),
                )])
                .with_attributes([A::size(pointer_size())]),
            ),
        ]),
        SemanticError::ZeroSizeFieldEmbedding {
            field_name: "wrapper".to_string(),
            item_path: IP::from("test::Holder"),
            field_type: ST::generic("test::Wrapper", [ST::raw("test::Empty")]),
            location: ItemLocation::test(),
        },
    );
}
