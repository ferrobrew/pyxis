//! Tests for extern value definitions.

use crate::{
    grammar::test_aliases::*,
    semantic::{
        builder::SemanticBuilder,
        types::{ItemDefinitionInner, Type},
    },
    span::ItemLocation,
};

use pretty_assertions::assert_eq;

/// An `extern` value is an ordinary registry item (like a `const`): it resolves
/// to an `ItemDefinitionInner::ExternValue` keyed by its full path, carrying the
/// resolved type and the `#[address(...)]`.
#[test]
fn can_define_extern_value() {
    let module1 = M::new().with_definitions([ID::new(
        (V::Public, "test"),
        EVD::new(T::ident("u32").mut_pointer()).with_attributes([A::address(0x1337)]),
    )]);

    let mut builder = SemanticBuilder::new(4);
    builder.add_module(&module1, &IP::from("module1")).unwrap();
    let resolved = builder.build().unwrap();

    let item = resolved
        .type_registry()
        .get(&IP::from("module1::test"), &ItemLocation::test())
        .cloned()
        .expect("failed to get extern value item");

    assert_eq!(item.visibility, crate::semantic::types::Visibility::Public);
    let inner = &item
        .resolved()
        .expect("extern value should be resolved")
        .inner;
    let ItemDefinitionInner::ExternValue(ev) = inner else {
        panic!("expected an extern value, got {inner:?}");
    };
    assert_eq!(ev.type_, Type::raw("u32").mut_pointer());
    assert_eq!(ev.address, 0x1337);
}
