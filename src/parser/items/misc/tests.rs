use crate::{
    grammar::test_aliases::*,
    parser::{error::ParseError, parse_str_for_tests},
    span::{ItemLocation, StripLocations},
    tokenizer::TokenKind,
};
use pretty_assertions::assert_eq;

#[test]
fn can_parse_type_alias() {
    let text = r#"
    pub type IntPtr = *const i32;
    "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Public, "IntPtr"),
        TAD::new(T::const_pointer(T::ident("i32"))),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_type_alias_with_complex_type() {
    let text = r#"
    pub type ArrayPtr = *mut [u32; 16];
    "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Public, "ArrayPtr"),
        TAD::new(T::mut_pointer(T::array(T::ident("u32"), 16))),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_type_alias_with_path() {
    let text = r#"
    pub type TexturePtr = *const module::Texture;
    "#;

    let ast = M::new().with_definitions([ID::new(
        (V::Public, "TexturePtr"),
        TAD::new(T::const_pointer(T::ident("module::Texture"))),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_generic_type_alias_single_param() {
    // Generic type alias with single type parameter
    let text = r#"
    pub type SharedPtr<T> = *mut T;
    "#;

    let ast = M::new().with_definitions([ID::generic(
        (V::Public, "SharedPtr"),
        [TP::new("T")],
        TAD::new(T::ident("T").mut_pointer()),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_generic_type_alias_multiple_params() {
    // Generic type alias with multiple type parameters
    let text = r#"
    pub type MapEntry<K, V> = Pair<K, V>;
    "#;

    let ast = M::new().with_definitions([ID::generic(
        (V::Public, "MapEntry"),
        [TP::new("K"), TP::new("V")],
        TAD::new(T::generic("Pair", [T::ident("K"), T::ident("V")])),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_generic_type_alias_with_generic_target() {
    // Type alias that wraps a generic type
    let text = r#"
    pub type EntityPtr<T> = Shared<T>;
    "#;

    let ast = M::new().with_definitions([ID::generic(
        (V::Public, "EntityPtr"),
        [TP::new("T")],
        TAD::new(T::generic("Shared", [T::ident("T")])),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn can_parse_generic_type_alias_with_pointer_to_generic() {
    // Generic type alias to pointer of generic type
    let text = r#"
    pub type WeakRef<T> = *const Weak<T>;
    "#;

    let ast = M::new().with_definitions([ID::generic(
        (V::Public, "WeakRef"),
        [TP::new("T")],
        TAD::new(T::generic("Weak", [T::ident("T")]).const_pointer()),
    )]);

    assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
}

#[test]
fn type_alias_missing_target() {
    let text = r#"
    type IntPtr =;
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedType {
            found: TokenKind::Semi,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn type_alias_missing_semicolon() {
    let text = r#"
    type IntPtr = i32
    type Another {}
    "#;
    let err = parse_str_for_tests(text).unwrap_err();
    assert_eq!(
        err.strip_locations(),
        ParseError::ExpectedToken {
            expected: vec![TokenKind::Semi],
            found: TokenKind::Type,
            location: ItemLocation::test(),
        }
        .strip_locations()
    );
}

#[test]
fn can_parse_module_level_const() {
    let text = r#"
    pub const MAX_HEALTH: i32 = 100;
    "#;
    let ast = parse_str_for_tests(text).unwrap().strip_locations();
    let module = &ast.items;
    assert_eq!(module.len(), 1);
    match &module[0] {
        crate::grammar::ModuleItem::Definition { definition } => {
            assert_eq!(definition.name.as_str(), "MAX_HEALTH");
            assert!(matches!(
                &definition.inner,
                crate::grammar::ItemDefinitionInner::Constant(_)
            ));
        }
        _ => panic!("Expected Definition, got {:?}", module[0]),
    }
}

#[test]
fn can_parse_const_float_value() {
    let text = r#"
    pub const PI: f32 = 3.14159;
    "#;
    let ast = parse_str_for_tests(text).unwrap().strip_locations();
    match &ast.items[0] {
        crate::grammar::ModuleItem::Definition { definition } => {
            let crate::grammar::ItemDefinitionInner::Constant(cd) = &definition.inner else {
                panic!("Expected Constant");
            };
            assert!(cd.expr.float_literal().is_some());
            assert_eq!(cd.expr.float_literal().unwrap(), "3.14159");
        }
        _ => panic!("Expected Definition"),
    }
}

#[test]
fn can_parse_const_string_value() {
    let text = r#"
    pub const NAME: str = "Pyxis";
    "#;
    let ast = parse_str_for_tests(text).unwrap().strip_locations();
    match &ast.items[0] {
        crate::grammar::ModuleItem::Definition { definition } => {
            let crate::grammar::ItemDefinitionInner::Constant(cd) = &definition.inner else {
                panic!("Expected Constant");
            };
            assert_eq!(cd.expr.string_literal(), Some("Pyxis"));
        }
        _ => panic!("Expected Definition"),
    }
}

#[test]
fn can_parse_const_enum_value() {
    let text = r#"
    pub const DEFAULT: Color = Color::Red;
    "#;
    let ast = parse_str_for_tests(text).unwrap().strip_locations();
    match &ast.items[0] {
        crate::grammar::ModuleItem::Definition { definition } => {
            let crate::grammar::ItemDefinitionInner::Constant(cd) = &definition.inner else {
                panic!("Expected Constant");
            };
            assert!(cd.expr.path().is_some());
            assert_eq!(cd.expr.path().unwrap().to_string(), "Color::Red");
        }
        _ => panic!("Expected Definition"),
    }
}

#[test]
fn can_parse_nested_const_in_type() {
    let text = r#"
    pub type Player {
        pub const STARTING_GOLD: u32 = 500,
        pub health: i32,
    }
    "#;
    let ast = parse_str_for_tests(text).unwrap().strip_locations();
    match &ast.items[0] {
        crate::grammar::ModuleItem::Definition { definition } => {
            let crate::grammar::ItemDefinitionInner::Type(td) = &definition.inner else {
                panic!("Expected Type");
            };
            let items: Vec<_> = td.items.iter().collect();
            assert_eq!(items.len(), 2); // const + field
        }
        _ => panic!("Expected Definition"),
    }
}

#[test]
fn can_parse_nested_const_in_enum() {
    let text = r#"
    pub enum Color: u8 {
        Red,
        Green,
        pub const DEFAULT: Color = Color::Red,
    }
    "#;
    let ast = parse_str_for_tests(text).unwrap().strip_locations();
    match &ast.items[0] {
        crate::grammar::ModuleItem::Definition { definition } => {
            let crate::grammar::ItemDefinitionInner::Enum(ed) = &definition.inner else {
                panic!("Expected Enum");
            };
            let items: Vec<_> = ed.items.iter().collect();
            assert_eq!(items.len(), 3); // Red + Green + DEFAULT const
        }
        _ => panic!("Expected Definition"),
    }
}

#[test]
fn can_parse_nested_const_in_bitflags() {
    let text = r#"
    pub bitflags Flags: u32 {
        READ = 1,
        WRITE = 2,
        pub const DEFAULT_MASK: u32 = 3,
    }
    "#;
    let ast = parse_str_for_tests(text).unwrap().strip_locations();
    match &ast.items[0] {
        crate::grammar::ModuleItem::Definition { definition } => {
            let crate::grammar::ItemDefinitionInner::Bitflags(bd) = &definition.inner else {
                panic!("Expected Bitflags");
            };
            let items: Vec<_> = bd.items.iter().collect();
            assert_eq!(items.len(), 3); // READ + WRITE + DEFAULT_MASK const
        }
        _ => panic!("Expected Definition"),
    }
}

#[test]
fn module_level_const_requires_semicolon() {
    // Missing terminator is rejected.
    assert!(parse_str_for_tests("pub const MAX: i32 = 100").is_err());
    // A trailing comma is not a valid module-level terminator.
    assert!(parse_str_for_tests("pub const MAX: i32 = 100,").is_err());
}

#[test]
fn module_level_extern_value_requires_semicolon() {
    assert!(parse_str_for_tests("pub extern GLOBAL: i32").is_err());
    assert!(parse_str_for_tests("pub extern GLOBAL: i32,").is_err());
    assert!(parse_str_for_tests("pub extern GLOBAL: i32;").is_ok());
}

#[test]
fn nested_const_trailing_comma_is_optional() {
    // No trailing comma before the closing brace parses fine.
    let text = r#"
    pub type Player {
        pub const STARTING_GOLD: u32 = 500
    }
    "#;
    let ast = parse_str_for_tests(text).unwrap().strip_locations();
    let crate::grammar::ModuleItem::Definition { definition } = &ast.items[0] else {
        panic!("Expected Definition");
    };
    let crate::grammar::ItemDefinitionInner::Type(td) = &definition.inner else {
        panic!("Expected Type");
    };
    assert_eq!(td.items.len(), 1);
}

#[test]
fn nested_const_rejects_semicolon_terminator() {
    // Bodies separate items with `,`; a `;` is not a valid separator.
    let text = r#"
    pub type Player {
        pub const STARTING_GOLD: u32 = 500;
    }
    "#;
    assert!(parse_str_for_tests(text).is_err());
}

#[test]
fn module_level_type_alias_requires_semicolon() {
    assert!(parse_str_for_tests("pub type Alias = u32").is_err());
    assert!(parse_str_for_tests("pub type Alias = u32,").is_err());
    assert!(parse_str_for_tests("pub type Alias = u32;").is_ok());
}

#[test]
fn nested_type_alias_uses_comma_not_semicolon() {
    // A nested alias follows the body rule: optional `,`, and `;` is rejected.
    let comma = r#"
    pub type Outer {
        pub type InnerAlias = u32,
        pub field: u32,
    }
    "#;
    assert!(parse_str_for_tests(comma).is_ok());

    // Trailing comma is optional before the closing brace.
    let no_comma = r#"
    pub type Outer {
        pub type InnerAlias = u32
    }
    "#;
    assert!(parse_str_for_tests(no_comma).is_ok());

    // A `;` terminator is not accepted inside a body.
    let semi = r#"
    pub type Outer {
        pub type InnerAlias = u32;
        pub field: u32,
    }
    "#;
    assert!(parse_str_for_tests(semi).is_err());
}
