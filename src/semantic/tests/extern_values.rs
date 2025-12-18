//! Tests for extern value definitions.

use crate::{
    grammar::test_aliases::*,
    semantic::{semantic_state::SemanticState, types::test_aliases::*},
    span::ItemLocation,
};

use pretty_assertions::assert_eq;

#[test]
fn can_define_extern_value() {
    let module1 = M::new().with_extern_values([EV::new(
        V::Public,
        "test",
        T::ident("u32").mut_pointer(),
        [A::address(0x1337)],
    )]);

    let mut semantic_state = SemanticState::new(4);
    semantic_state
        .add_module(&module1, &IP::from("module1"))
        .unwrap();
    let semantic_state = semantic_state.build().unwrap();

    let extern_value = semantic_state
        .modules()
        .get(&IP::from("module1"))
        .unwrap()
        .extern_values
        .first()
        .unwrap();

    assert_eq!(
        extern_value,
        &SEV {
            visibility: SV::Public,
            name: "test".into(),
            type_: ST::raw("u32").mut_pointer(),
            address: 0x1337,
            location: ItemLocation::test(),
        }
    );
}
