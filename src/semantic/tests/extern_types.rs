//! Tests for extern type resolution.

use crate::{
    grammar::test_aliases::*,
    semantic::{error::SemanticError, types::test_aliases::*},
    span::ItemLocation,
};

use super::util::*;

#[test]
fn will_fail_on_an_extern_without_size() {
    assert_ast_produces_exact_error(
        M::new().with_extern_types([("TestType".into(), As::default())]),
        SemanticError::MissingExternAttribute {
            attribute_name: "size".to_string(),
            extern_kind: "extern type".to_string(),
            type_name: "TestType".to_string(),
            module_name: "test".to_string(),
            location: ItemLocation::test(),
        },
    );
}

#[test]
fn can_resolve_embed_of_an_extern() {
    assert_ast_produces_type_definitions(
        M::new()
            .with_extern_types([(
                "TestType1".into(),
                As::from_iter([A::size(16), A::align(4)]),
            )])
            .with_definitions([ID::new(
                (V::Public, "TestType2"),
                TD::new([
                    TS::field((V::Public, "field_1"), T::ident("u64")),
                    TS::field((V::Public, "field_2"), T::ident("TestType1")),
                    TS::field(
                        (V::Public, "field_3"),
                        T::ident("TestType1").const_pointer(),
                    ),
                    TS::field((V::Public, "field_4"), T::ident("TestType1").mut_pointer()),
                ])
                .with_attributes([A::align(8)]),
            )]),
        [
            SID::defined_resolved(
                (SV::Public, "test::TestType2"),
                SISR::new(
                    (24 + 2 * pointer_size(), 8),
                    STD::new().with_regions([
                        SR::field((SV::Public, "field_1"), ST::raw("u64")),
                        SR::field((SV::Public, "field_2"), ST::raw("test::TestType1")),
                        SR::field(
                            (SV::Public, "field_3"),
                            ST::raw("test::TestType1").const_pointer(),
                        ),
                        SR::field(
                            (SV::Public, "field_4"),
                            ST::raw("test::TestType1").mut_pointer(),
                        ),
                    ]),
                ),
            ),
            SID::category_resolved(
                (SV::Public, "test::TestType1"),
                SISR::new((16, 4), STD::new().with_regions([])),
                SIC::Extern,
            ),
        ],
    );
}
