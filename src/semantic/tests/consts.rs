//! Tests for `const` declarations, focusing on resolution behaviour that the
//! codegen corpus doesn't exercise (module-qualified enum-value references).

use crate::{
    grammar::test_aliases::*,
    semantic::{
        builder::SemanticBuilder,
        types::{ConstValue, ItemDefinitionInner, Type},
    },
    span::ItemLocation,
};

use pretty_assertions::assert_eq;

/// A module-level `const` whose value is an enum variant reached through a
/// module-qualified path (`graphics::Color::Red`) must resolve: the final
/// segment is the variant and everything before it is the (possibly
/// module-qualified) enum path.
#[test]
fn can_resolve_module_qualified_enum_value_const() {
    let graphics = M::new().with_definitions([ID::new(
        (V::Public, "Color"),
        ED::new(
            T::ident("u8"),
            [ES::field("Red"), ES::field("Green"), ES::field("Blue")],
            [],
        ),
    )]);
    // Import the enum type, but reference the variant through a
    // module-qualified value path to exercise multi-segment resolution.
    let app = M::new()
        .with_uses([IP::from("graphics::Color")])
        .with_definitions([ID::new(
            (V::Public, "DEFAULT_COLOR"),
            CD::new(
                T::ident("Color"),
                path_expr(IP::from("graphics::Color::Red")),
            ),
        )]);

    let mut builder = SemanticBuilder::new(4);
    builder
        .add_module(&graphics, &IP::from("graphics"))
        .unwrap();
    builder.add_module(&app, &IP::from("app")).unwrap();
    let resolved = builder.build().unwrap();

    let item = resolved
        .type_registry()
        .get(&IP::from("app::DEFAULT_COLOR"), &ItemLocation::test())
        .cloned()
        .expect("failed to get DEFAULT_COLOR const");

    let inner = &item.resolved().expect("const should be resolved").inner;
    let ItemDefinitionInner::Constant(cd) = inner else {
        panic!("expected a constant, got {inner:?}");
    };
    assert_eq!(cd.type_, Type::raw("graphics::Color"));
    assert_eq!(
        cd.value,
        ConstValue::EnumValue(IP::from("graphics::Color::Red"))
    );
}
