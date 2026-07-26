//! Tests for union layout and the constructs a union body rejects.

use crate::{
    grammar::test_aliases::*,
    semantic::{
        error::{AttributeName, SemanticError},
        types::test_aliases::*,
    },
    span::ItemLocation,
};

use super::util::*;

/// A union is as large as its largest member, and every member starts at 0.
#[test]
fn union_takes_the_size_of_its_largest_member() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Payload"),
            UD::new([
                TS::field((V::Public, "as_int"), T::ident("i32")),
                TS::field((V::Public, "as_long"), T::ident("u64")),
                TS::field((V::Public, "as_byte"), T::ident("u8")),
            ]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::Payload"),
            SISR::new(
                (8, 8),
                SUD::new().with_regions([
                    SR::field((SV::Public, "as_int"), ST::raw("i32")),
                    SR::field((SV::Public, "as_long"), ST::raw("u64")),
                    SR::field((SV::Public, "as_byte"), ST::raw("u8")),
                ]),
            ),
        )],
    );
}

/// Unlike a type, a union with no explicit alignment does *not* fall back to
/// the pointer size — widening it would inflate its size.
#[test]
fn union_alignment_is_the_strictest_member_not_the_pointer_size() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Small"),
            UD::new([
                TS::field((V::Public, "a"), T::ident("u8")),
                TS::field((V::Public, "b"), T::ident("u8")),
            ]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::Small"),
            SISR::new(
                (1, 1),
                SUD::new().with_regions([
                    SR::field((SV::Public, "a"), ST::raw("u8")),
                    SR::field((SV::Public, "b"), ST::raw("u8")),
                ]),
            ),
        )],
    );
}

/// The natural size is rounded up to the alignment.
#[test]
fn union_size_rounds_up_to_alignment() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Mixed"),
            UD::new([
                TS::field((V::Public, "wide"), T::ident("u32")),
                TS::field((V::Public, "narrow"), T::ident("u8").array(5)),
            ]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::Mixed"),
            SISR::new(
                (8, 4),
                SUD::new().with_regions([
                    SR::field((SV::Public, "wide"), ST::raw("u32")),
                    SR::field((SV::Public, "narrow"), ST::raw("u8").array(5)),
                ]),
            ),
        )],
    );
}

/// `#[size]` pads the union out past its largest member. The padding is another
/// whole-width member, not a tail — a union has no tail — so the backends have
/// something to emit that actually makes `sizeof` match.
#[test]
fn union_size_attribute_pads_out() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Padded"),
            UD::new([
                TS::field((V::Public, "small"), T::ident("u32")),
                TS::field((V::Public, "medium"), T::ident("u64")),
            ])
            .with_attributes([A::size(16)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::Padded"),
            SISR::new(
                (16, 8),
                SUD::new().with_regions([
                    SR::field((SV::Public, "small"), ST::raw("u32")),
                    SR::field((SV::Public, "medium"), ST::raw("u64")),
                    SR::field((SV::Private, "_padding"), ST::raw("u8").array(16)),
                ]),
            ),
        )],
    );
}

/// `#[min_size]` raises the floor, and pads the same way `#[size]` does.
#[test]
fn union_min_size_attribute_pads_out() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Floored"),
            UD::new([TS::field((V::Public, "small"), T::ident("u32"))])
                .with_attributes([A::min_size(12)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::Floored"),
            SISR::new(
                (12, 4),
                SUD::new().with_regions([
                    SR::field((SV::Public, "small"), ST::raw("u32")),
                    SR::field((SV::Private, "_padding"), ST::raw("u8").array(12)),
                ]),
            ),
        )],
    );
}

/// A union that already fills its declared size gains no padding member.
#[test]
fn union_exactly_at_declared_size_is_not_padded() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Exact"),
            UD::new([TS::field((V::Public, "value"), T::ident("u32"))])
                .with_attributes([A::size(4)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::Exact"),
            SISR::new(
                (4, 4),
                SUD::new().with_regions([SR::field((SV::Public, "value"), ST::raw("u32"))]),
            ),
        )],
    );
}

/// `#[align]` over-aligns, and the size rounds up to match.
#[test]
fn union_align_attribute_over_aligns() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "OverAligned"),
            UD::new([TS::field((V::Public, "value"), T::ident("u32"))])
                .with_attributes([A::align(16)]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::OverAligned"),
            SISR::new(
                (16, 16),
                SUD::new().with_regions([SR::field((SV::Public, "value"), ST::raw("u32"))]),
            ),
        )],
    );
}

/// `#[packed]` drops the alignment to 1.
#[test]
fn union_packed_drops_alignment_to_one() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Packed"),
            UD::new([
                TS::field((V::Public, "word"), T::ident("u16")),
                TS::field((V::Public, "bytes"), T::ident("u8").array(2)),
            ])
            .with_attributes([A::packed()]),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::Packed"),
            SISR::new(
                (2, 1),
                SUD::new().with_packed(true).with_regions([
                    SR::field((SV::Public, "word"), ST::raw("u16")),
                    SR::field((SV::Public, "bytes"), ST::raw("u8").array(2)),
                ]),
            ),
        )],
    );
}

/// An inline `pub name: union { … }` field becomes a generated module-scope
/// sibling item plus an ordinary field pointing at it.
#[test]
fn inline_union_field_generates_a_sibling_item() {
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "Scratch"),
            TD::new([
                TS::field((V::Public, "tag"), T::ident("u64")),
                TS::union_field(
                    (V::Public, "data"),
                    UD::new([
                        TS::field((V::Public, "as_u64"), T::ident("u64")),
                        TS::field((V::Public, "as_bytes"), T::ident("u8").array(8)),
                    ]),
                ),
            ])
            .with_attributes([A::align(8)]),
        )]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Scratch"),
                SISR::new(
                    (16, 8),
                    STD::new().with_regions([
                        SR::field((SV::Public, "tag"), ST::raw("u64")),
                        SR::field((SV::Public, "data"), ST::raw("test::ScratchDataUnion")),
                    ]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::ScratchDataUnion"),
                SISR::new(
                    (8, 8),
                    SUD::new().with_regions([
                        SR::field((SV::Public, "as_u64"), ST::raw("u64")),
                        SR::field((SV::Public, "as_bytes"), ST::raw("u8").array(8)),
                    ]),
                ),
            ),
        ],
    );
}

#[test]
fn base_in_a_union_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "Bad"),
            UD::new([
                TS::field((V::Public, "a"), T::ident("u32")).with_attributes([A::base()]),
                TS::field((V::Public, "b"), T::ident("u32")),
            ]),
        )]),
        SemanticError::UnionBaseNotAllowed {
            item_path: IP::from("test::Bad"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn vftable_in_a_union_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "Bad"),
            UD::new([
                TS::vftable([F::new((V::Public, "f"), [Ar::mut_self()])]),
                TS::field((V::Public, "b"), T::ident("u32")),
            ]),
        )]),
        SemanticError::UnionVftableNotAllowed {
            item_path: IP::from("test::Bad"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn address_on_a_union_member_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "Bad"),
            UD::new([
                TS::field((V::Public, "a"), T::ident("u32")),
                TS::field((V::Public, "b"), T::ident("u32")).with_attributes([A::address(4)]),
            ]),
        )]),
        SemanticError::UnionMemberAddress {
            member_name: "b".to_string(),
            item_path: IP::from("test::Bad"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn empty_union_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new((V::Public, "Bad"), UD::new([]))]),
        SemanticError::EmptyUnion {
            item_path: IP::from("test::Bad"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn member_larger_than_declared_size_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "Bad"),
            UD::new([TS::field((V::Public, "wide"), T::ident("u32"))])
                .with_attributes([A::size(2)]),
        )]),
        SemanticError::UnionMemberExceedsSize {
            member_name: "wide".to_string(),
            member_size: 4,
            declared_size: 2,
            item_path: IP::from("test::Bad"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn align_below_member_requirement_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "Bad"),
            UD::new([TS::field((V::Public, "wide"), T::ident("u64"))])
                .with_attributes([A::align(2)]),
        )]),
        SemanticError::AlignmentBelowMinimum {
            alignment: 2,
            required_alignment: 8,
            item_path: IP::from("test::Bad"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn anonymous_union_member_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "Bad"),
            UD::new([
                TS::field((V::Public, "_"), T::ident("u32")),
                TS::field((V::Public, "b"), T::ident("u32")),
            ]),
        )]),
        SemanticError::UnionAnonymousMember {
            item_path: IP::from("test::Bad"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn anonymous_inline_union_field_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "Bad"),
            TD::new([TS::union_field(
                (V::Public, "_"),
                UD::new([TS::field((V::Public, "a"), T::ident("u32"))]),
            )]),
        )]),
        SemanticError::UnionAnonymousMember {
            item_path: IP::from("test::Bad"),
            location: ItemLocation::test(),
        },
    );
}

/// An inline union's item is synthesised at a module-scope path, so anything
/// declared inside it would be registered nowhere. Reject rather than drop.
#[test]
fn nested_item_in_an_inline_union_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "Outer"),
            TD::new([TS::union_field(
                (V::Public, "payload"),
                UD::new([
                    TS::field((V::Public, "a"), T::ident("u32")),
                    TS::item(ID::new(
                        (V::Public, "Nested"),
                        TD::new([TS::field((V::Public, "z"), T::ident("u32"))]),
                    )),
                ]),
            )]),
        )]),
        SemanticError::InlineUnionNestedItem {
            item_name: "Nested".to_string(),
            item_path: IP::from("test::OuterPayloadUnion"),
            location: ItemLocation::test(),
        },
    );
}

/// The generated name must be free: overwriting a user's item would leave the
/// enclosing type asserting a size the replacement no longer has.
#[test]
fn inline_union_colliding_with_a_declared_item_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            ID::new(
                (V::Public, "ScratchDataUnion"),
                UD::new([TS::field((V::Public, "a"), T::ident("u64"))]),
            ),
            ID::new(
                (V::Public, "Scratch"),
                TD::new([TS::union_field(
                    (V::Public, "data"),
                    UD::new([TS::field((V::Public, "x"), T::ident("u16"))]),
                )]),
            ),
        ]),
        SemanticError::InlineUnionNameCollision {
            generated_path: IP::from("test::ScratchDataUnion"),
            item_path: IP::from("test::Scratch"),
            location: ItemLocation::test(),
        },
    );
}

/// Underscores are separators, not characters, when the field name is
/// PascalCased — so `a_b` and `a__b` generate the same name.
#[test]
fn two_inline_unions_generating_the_same_name_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "T"),
            TD::new([
                TS::union_field(
                    (V::Public, "a_b"),
                    UD::new([TS::field((V::Public, "x"), T::ident("u8"))]),
                ),
                TS::union_field(
                    (V::Public, "a__b"),
                    UD::new([TS::field((V::Public, "y"), T::ident("u64"))]),
                ),
            ]),
        )]),
        SemanticError::InlineUnionNameCollision {
            generated_path: IP::from("test::TABUnion"),
            item_path: IP::from("test::T"),
            location: ItemLocation::test(),
        },
    );
}

/// Two *different* types can also land on one name; they never meet during
/// resolution, so this is caught when their generated items are merged.
#[test]
fn two_types_generating_the_same_union_name_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([
            ID::new(
                (V::Public, "A"),
                TD::new([TS::union_field(
                    (V::Public, "b_c"),
                    UD::new([TS::field((V::Public, "x"), T::ident("u8"))]),
                )]),
            ),
            ID::new(
                (V::Public, "AB"),
                TD::new([TS::union_field(
                    (V::Public, "c"),
                    UD::new([TS::field((V::Public, "y"), T::ident("u64"))]),
                )]),
            ),
        ]),
        SemanticError::InlineUnionNameCollision {
            generated_path: IP::from("test::ABCUnion"),
            item_path: IP::from("test::A"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn both_size_and_min_size_on_a_union_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "Bad"),
            UD::new([TS::field((V::Public, "a"), T::ident("u32"))])
                .with_attributes([A::size(8), A::min_size(8)]),
        )]),
        SemanticError::ConflictingAttributes {
            attr1: AttributeName::Size,
            attr2: AttributeName::MinSize,
            item_path: IP::from("test::Bad"),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn packed_and_align_on_a_union_is_rejected() {
    assert_ast_produces_exact_error(
        M::new().with_definitions([ID::new(
            (V::Public, "Bad"),
            UD::new([TS::field((V::Public, "a"), T::ident("u32"))])
                .with_attributes([A::packed(), A::align(8)]),
        )]),
        SemanticError::ConflictingAttributes {
            attr1: AttributeName::Packed,
            attr2: AttributeName::Align,
            item_path: IP::from("test::Bad"),
            location: ItemLocation::test(),
        },
    );
}
