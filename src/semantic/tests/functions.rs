//! Tests for freestanding function resolution.

use crate::{grammar::test_aliases::*, semantic::types::test_aliases::*, span::StripLocations};

use super::util::*;
use pretty_assertions::assert_eq;

#[test]
fn can_resolve_freestanding_functions() {
    let ast = M::new()
        .with_definitions([ID::new(
            (V::Public, "TestEnum"),
            ED::new(T::ident("u32"), [ES::field("None")], []),
        )])
        .with_impls([FB::new(
            "TestEnum",
            [F::new((V::Public, "test"), []).with_attributes([A::address(0x123)])],
        )])
        .with_functions([
            F::new((V::Public, "freestanding"), []).with_attributes([A::address(0x456)]),
            F::new(
                (V::Private, "another_freestanding"),
                [Ar::named("arg1", T::ident("i32"))],
            )
            .with_attributes([A::address(0x789)])
            .with_return_type(T::ident("i32")),
        ]);

    let test_path = IP::from("test");
    let state = build_state(&ast, &test_path).unwrap();
    let module = state.modules().get(&test_path).unwrap();

    assert_eq!(module.functions().len(), 2);

    // Check first freestanding function
    let func1 = &module.functions()[0];
    assert_eq!(func1.name, "freestanding");
    assert_eq!(func1.visibility, SV::Public);
    assert_eq!(func1.arguments.len(), 0);
    assert_eq!(func1.return_type, None);
    assert_eq!(func1.calling_convention, SCC::System);
    assert_eq!(func1.body, SFB::Address { address: 0x456 });

    // Check second freestanding function
    let func2 = &module.functions()[1];
    assert_eq!(func2.name, "another_freestanding");
    assert_eq!(func2.visibility, SV::Private);
    assert_eq!(func2.arguments.len(), 1);
    assert_eq!(
        func2.arguments[0].strip_locations(),
        SAr::field("arg1", ST::raw("i32")).strip_locations()
    );
    assert_eq!(func2.return_type, Some(ST::raw("i32")));
    assert_eq!(func2.calling_convention, SCC::System);
    assert_eq!(func2.body, SFB::Address { address: 0x789 });
}
