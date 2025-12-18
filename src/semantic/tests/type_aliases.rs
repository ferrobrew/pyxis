//! Tests for type alias resolution.

use crate::{
    grammar::test_aliases::*,
    semantic::{semantic_state::SemanticState, types::test_aliases::*},
    span::ItemLocation,
};

use super::util::*;
use pretty_assertions::assert_eq;

#[test]
fn can_resolve_type_alias() {
    // Test basic type alias resolution: type IntPtr = *const i32;
    assert_ast_produces_type_definitions(
        M::new().with_definitions([ID::new(
            (V::Public, "IntPtr"),
            TAD::new(T::ident("i32").const_pointer()),
        )]),
        [SID::defined_resolved(
            (SV::Public, "test::IntPtr"),
            SISR::new((0, 1), STAD::new(ST::raw("i32").const_pointer(), vec![])),
        )],
    );
}

#[test]
fn can_resolve_type_alias_to_user_type() {
    // Test type alias to a user-defined type
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "Texture"),
                TD::new([TS::field((V::Public, "data"), T::ident("u32"))]),
            ),
            ID::new(
                (V::Public, "TexturePtr"),
                TAD::new(T::ident("Texture").const_pointer()),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Texture"),
                SISR::new(
                    (4, 4),
                    STD::new().with_regions([SR::field((SV::Public, "data"), ST::raw("u32"))]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::TexturePtr"),
                SISR::new(
                    (0, 1),
                    STAD::new(ST::raw("test::Texture").const_pointer(), vec![]),
                ),
            ),
        ],
    );
}

#[test]
fn can_use_type_alias_in_struct_field() {
    // Test using a type alias as a field type
    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            ID::new(
                (V::Public, "IntPtr"),
                TAD::new(T::ident("i32").const_pointer()),
            ),
            ID::new(
                (V::Public, "Container"),
                TD::new([TS::field((V::Public, "ptr"), T::ident("IntPtr"))]),
            ),
        ]),
        [
            SID::defined_resolved(
                (SV::Public, "test::Container"),
                SISR::new(
                    (pointer_size(), pointer_size()),
                    STD::new().with_regions([SR::field(
                        (SV::Public, "ptr"),
                        ST::raw("i32").const_pointer(),
                    )]),
                ),
            ),
            SID::defined_resolved(
                (SV::Public, "test::IntPtr"),
                SISR::new((0, 1), STAD::new(ST::raw("i32").const_pointer(), vec![])),
            ),
        ],
    );
}

#[test]
fn can_use_type_alias_as_reexport() {
    // Test cross-module type alias re-export semantics:
    // - module_a: defines Texture
    // - module_b: imports Texture from A, defines TexturePtr = *const Texture
    // - module_c: imports TexturePtr from B (NOT Texture from A), uses it in a struct
    // This tests that visibility is checked at definition time, not usage time.
    let module_a = M::new().with_definitions([ID::new(
        (V::Public, "Texture"),
        TD::new([TS::field((V::Public, "data"), T::ident("u32"))]),
    )]);

    let module_b = M::new()
        .with_uses([IP::from("module_a::Texture")])
        .with_definitions([ID::new(
            (V::Public, "TexturePtr"),
            TAD::new(T::ident("Texture").const_pointer()),
        )]);

    // module_c imports only TexturePtr, not Texture - demonstrating re-export semantics
    let module_c = M::new()
        .with_uses([IP::from("module_b::TexturePtr")])
        .with_definitions([ID::new(
            (V::Public, "Container"),
            TD::new([TS::field((V::Public, "ptr"), T::ident("TexturePtr"))]),
        )]);

    let mut semantic_state = SemanticState::new(pointer_size());
    semantic_state
        .add_module(&module_a, &IP::from("module_a"))
        .unwrap();
    semantic_state
        .add_module(&module_b, &IP::from("module_b"))
        .unwrap();
    semantic_state
        .add_module(&module_c, &IP::from("module_c"))
        .unwrap();
    let semantic_state = semantic_state.build().unwrap();

    // Verify Container was created correctly with the aliased type
    let container_path = IP::from("module_c::Container");
    let container = semantic_state
        .type_registry()
        .get(&container_path, &ItemLocation::test())
        .expect("Container should exist");

    let resolved = container.resolved().expect("Container should be resolved");
    let type_def = resolved
        .inner
        .as_type()
        .expect("Container should be a type");

    // The field should be *const module_a::Texture (the fully resolved type)
    assert_eq!(type_def.regions.len(), 1);
    assert_eq!(
        type_def.regions[0].type_ref,
        ST::raw("module_a::Texture").const_pointer()
    );

    // Also verify the type alias itself was resolved correctly
    let alias_path = IP::from("module_b::TexturePtr");
    let alias = semantic_state
        .type_registry()
        .get(&alias_path, &ItemLocation::test())
        .expect("TexturePtr alias should exist");

    let alias_resolved = alias.resolved().expect("TexturePtr should be resolved");
    let alias_def = alias_resolved
        .inner
        .as_type_alias()
        .expect("TexturePtr should be a type alias");

    assert_eq!(
        alias_def.target,
        ST::raw("module_a::Texture").const_pointer()
    );
}
