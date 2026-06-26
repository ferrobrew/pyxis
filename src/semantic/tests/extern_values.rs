//! Tests for extern value definitions.

use crate::{
    grammar::test_aliases::*,
    semantic::{builder::SemanticBuilder, types::test_aliases::*},
    span::{ItemLocation, StripLocations},
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

    let mut builder = SemanticBuilder::new(4);
    builder
        .add_module(&module1, &IP::from("module1"))
        .unwrap();
    let resolved = builder.build().unwrap();

    let extern_value = resolved
        .modules()
        .get(&IP::from("module1"))
        .unwrap()
        .extern_values
        .first()
        .unwrap();

    assert_eq!(
        extern_value.clone().strip_locations(),
        SEV {
            visibility: SV::Public,
            name: "test".into(),
            type_: ST::raw("u32").mut_pointer(),
            address: 0x1337,
            doc: vec![],
            location: ItemLocation::test(),
        }
    );
}
