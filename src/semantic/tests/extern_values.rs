//! Tests for extern value definitions.

use crate::{
    grammar::test_aliases::*,
    semantic::{
        SemanticError,
        builder::SemanticBuilder,
        types::{ItemDefinitionInner, Type},
    },
    span::{ItemLocation, StripLocations},
};

use pretty_assertions::assert_eq;

/// An `extern` value resolves to an `ItemDefinitionInner::ExternValue` registry
/// item (like a `const`) keyed by its full path, carrying the resolved type and
/// the `#[address(...)]`.
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

/// A value item (here an extern value) nested inside a *generic* type is
/// rejected: the backends can't emit a correctly-qualified `impl<T> Parent<T>`
/// accessor for it. Nesting in a non-generic type is fine (see the corpus).
#[test]
fn extern_value_nested_in_generic_type_is_rejected() {
    let module = M::new().with_definitions([ID::generic(
        (V::Public, "Wrapper"),
        [TP::new("T")],
        TD::new([
            TS::item(ID::new(
                (V::Public, "g_inst"),
                EVD::new(T::ident("u32").mut_pointer()).with_attributes([A::address(0x9000)]),
            )),
            TS::field((V::Public, "value"), T::ident("T").mut_pointer()),
        ]),
    )]);

    let mut builder = SemanticBuilder::new(4);
    builder.add_module(&module, &IP::from("test")).unwrap();

    let err = builder.build().unwrap_err();
    assert_eq!(
        err.strip_locations(),
        SemanticError::ValueItemInGenericParent {
            item_path: IP::from("test::Wrapper::g_inst"),
            parent_path: IP::from("test::Wrapper"),
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}
