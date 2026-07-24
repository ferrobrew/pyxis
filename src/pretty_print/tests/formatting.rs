use pretty_assertions::assert_eq;

use crate::{parser::parse_str_for_tests, pretty_print::*};

#[test]
fn test_pretty_print_basic() {
    let text = r#"
    pub type Test {
        field: i32,
    }
    "#;

    let expected = r#"
pub type Test {
    field: i32,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);
}

#[test]
fn opaque_and_braced_empty_types_are_distinct() {
    // An opaque type keeps its `;`; an empty braced body keeps its braces.
    // Both must round-trip to themselves rather than collapsing together.
    for text in ["pub type Marker;", "pub type Marker {\n}"] {
        let module = parse_str_for_tests(text).unwrap();
        assert_eq!(pretty_print(&module), text);
    }
}

#[test]
fn pub_use_reexport_round_trips() {
    // `pub use` (an explicit re-export) must survive a round-trip and stay
    // distinct from a plain `use`.
    let text = "pub use math::Vector3;\nuse math::Matrix4;";
    let module = parse_str_for_tests(text).unwrap();
    assert_eq!(pretty_print(&module), text);
}

#[test]
fn test_pretty_print_module_inner_attributes() {
    // Module-level inner attributes (`#![...]`) must survive a round-trip
    // so `pyxis fmt` doesn't strip them.
    let text = r#"
    // a module-level inner attribute
    #![rust(example_flag)]

    pub type Foo {
        field: i32,
    }
    "#;

    let expected = r#"
// a module-level inner attribute
#![rust(example_flag)]

pub type Foo {
    field: i32,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);
}

#[test]
fn test_pretty_print_with_comments() {
    let text = r#"
// This is a regular comment
/// This is a doc comment
pub type Test {
    // Field comment
    field1: i32,
    /// Doc comment for field2
    field2: bool,
}

#[singleton(0x1_18F_B64), size(0x40), align(16)] // 0x3C
pub type InputDeviceManager {
    #[address(0x18)]
    pub enabled: bool,
}
    "#;

    let output = r#"
// This is a regular comment
/// This is a doc comment
pub type Test {
    // Field comment
    field1: i32,
    /// Doc comment for field2
    field2: bool,
}

#[singleton(0x1_18F_B64), size(0x40), align(16)] // 0x3C
pub type InputDeviceManager {
    #[address(0x18)]
    pub enabled: bool,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    dbg!(&module);
    dbg!(&printed);

    assert_eq!(printed, output);
}

#[test]
fn test_pretty_print_comments_in_attributes() {
    let text = r#"
#[singleton(0x1_18F_C20), size(0x620 /* actually 0x61C */), align(16)]
pub type AnarkGui {
    vftable {},

    #[address(0x1A0)]
    pub next_state: AnarkState,
    pub active_state: AnarkState,
}
    "#;

    let output = r#"
#[singleton(0x1_18F_C20), size(0x620 /* actually 0x61C */), align(16)]
pub type AnarkGui {
    vftable {},

    #[address(0x1A0)]
    pub next_state: AnarkState,
    pub active_state: AnarkState,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, output);
}

#[test]
fn test_pretty_print_multiple_trailing_comments() {
    let text = r#"
#[size(0x10)] // size comment
// another comment
pub type MultiCommentTest {
    field: i32,
}

#[align(8)] /* block comment */
pub type BlockCommentTest {
    value: u64,
}
    "#;

    let expected = r#"
#[size(0x10)] // size comment
// another comment
pub type MultiCommentTest {
    field: i32,
}

#[align(8)] /* block comment */
pub type BlockCommentTest {
    value: u64,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);

    // Parse again to verify round-trip
    let module2 = parse_str_for_tests(&printed).unwrap();
    let printed2 = pretty_print(&module2);

    assert_eq!(printed, printed2);
}

#[test]
fn test_pretty_print_multiple_non_inline_trailing_comments() {
    let text = r#"
#[size(0x10)]
// size comment
// another comment
pub type MultiCommentTest {
    field: i32,
}

#[align(8)]
/* block comment */
pub type BlockCommentTest {
    value: u64,
}
    "#;

    let expected = r#"
#[size(0x10)]
// size comment
// another comment
pub type MultiCommentTest {
    field: i32,
}

#[align(8)]
/* block comment */
pub type BlockCommentTest {
    value: u64,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);

    // Parse again to verify round-trip
    let module2 = parse_str_for_tests(&printed).unwrap();
    let printed2 = pretty_print(&module2);

    assert_eq!(printed, printed2);
}

#[test]
fn test_pretty_print_enum_with_trailing_comments() {
    let text = r#"
#[repr(u32)] // enum representation
pub enum State: u32 {
    Idle = 0,
    // State comment
    Active = 1,
    Done = 2,
}
    "#;

    let expected = r#"
#[repr(u32)] // enum representation
pub enum State: u32 {
    Idle = 0,
    // State comment
    Active = 1,
    Done = 2,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);

    // Parse again to verify round-trip
    let module2 = parse_str_for_tests(&printed).unwrap();
    let printed2 = pretty_print(&module2);

    assert_eq!(printed, printed2);
}

#[test]
fn test_pretty_print_bitflags_with_trailing_comments() {
    let text = r#"
#[repr(u32)] // flags representation
pub bitflags Flags: u32 {
    // Flag comment
    READ = 0x1,
    WRITE = 0x2,
    EXECUTE = 0x4,
}
    "#;

    let expected = r#"
#[repr(u32)] // flags representation
pub bitflags Flags: u32 {
    // Flag comment
    READ = 0x1,
    WRITE = 0x2,
    EXECUTE = 0x4,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);

    // Parse again to verify round-trip
    let module2 = parse_str_for_tests(&printed).unwrap();
    let printed2 = pretty_print(&module2);

    assert_eq!(printed, printed2);
}

#[test]
fn test_pretty_print_mixed_comments() {
    let text = r#"
// Module level comment

/// Documentation for Foo
#[size(0x20)] // Foo size
pub type Foo {
    // Field comment
    /// Field documentation
    field1: i32,
    field2: bool, // inline field comment
}

// Separator comment

/// Documentation for Bar
#[align(16)] /* alignment */
pub type Bar {
    value: u64,
}
    "#;

    let expected = r#"
// Module level comment

/// Documentation for Foo
#[size(0x20)] // Foo size
pub type Foo {
    // Field comment
    /// Field documentation
    field1: i32,
    field2: bool, // inline field comment
}

// Separator comment

/// Documentation for Bar
#[align(16)] /* alignment */
pub type Bar {
    value: u64,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);

    // Parse again to verify round-trip
    let module2 = parse_str_for_tests(&printed).unwrap();
    let printed2 = pretty_print(&module2);

    assert_eq!(printed, printed2);
}

#[test]
fn test_pretty_print_no_attributes_with_comments() {
    let text = r#"
// Comment before definition
pub type SimpleType {
    field: i32,
}
    "#;

    let expected = r#"
// Comment before definition
pub type SimpleType {
    field: i32,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);

    // Parse again to verify round-trip
    let module2 = parse_str_for_tests(&printed).unwrap();
    let printed2 = pretty_print(&module2);

    assert_eq!(printed, printed2);
}

#[test]
fn test_pretty_print_comment_before_definition_with_doc_and_attributes() {
    // A plain comment immediately preceding a definition should stay
    // attached to it; the formatter must not insert a blank line between
    // them, even when the definition has both doc comments and attributes.
    let text = r#"
// Plain comment
/// Doc comment
#[size(0x4)]
pub type Foo {
    field: i32,
}
    "#;

    let expected = r#"
// Plain comment
/// Doc comment
#[size(0x4)]
pub type Foo {
    field: i32,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);

    // Parse again to verify round-trip
    let module2 = parse_str_for_tests(&printed).unwrap();
    let printed2 = pretty_print(&module2);

    assert_eq!(printed, printed2);
}

#[test]
fn test_pretty_print_doc_comment_on_freestanding_function() {
    let text = r#"
/// Doc comment for a freestanding function
#[address(0x123)]
pub fn test();
    "#;

    let expected = r#"
/// Doc comment for a freestanding function
#[address(0x123)]
pub fn test();
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);

    // Parse again to verify round-trip
    let module2 = parse_str_for_tests(&printed).unwrap();
    let printed2 = pretty_print(&module2);

    assert_eq!(printed, printed2);
}

#[test]
fn test_pretty_print_comment_on_separate_line_after_attributes() {
    let text = r#"
#[size(0x10)]
// First comment on separate line
// Second comment on separate line
pub type Foo {
    field: i32,
}
    "#;

    let expected = r#"
#[size(0x10)]
// First comment on separate line
// Second comment on separate line
pub type Foo {
    field: i32,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);

    // Parse again to verify round-trip
    let module2 = parse_str_for_tests(&printed).unwrap();
    let printed2 = pretty_print(&module2);

    assert_eq!(printed, printed2);
}

#[test]
fn test_pretty_print_multiple_inline_comments() {
    let text = r#"
#[size(0x10)] // comment1 // comment2
pub type Foo {
    field: i32,
}
    "#;

    let expected = r#"
#[size(0x10)] // comment1 // comment2
pub type Foo {
    field: i32,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);

    // Parse again to verify round-trip
    let module2 = parse_str_for_tests(&printed).unwrap();
    let printed2 = pretty_print(&module2);

    assert_eq!(printed, printed2);
}

#[test]
fn test_pretty_print_complex_comment_layout() {
    let text = r#"
#[size(0x20)] // inline comment
// separate line comment 1
// separate line comment 2
pub type ComplexLayout {
    field1: i32,
    field2: bool, // field comment
}

#[align(8)] /* block */ /* another block */
pub type MultipleBlocks {
    value: u64,
}
    "#;

    let expected = r#"
#[size(0x20)] // inline comment
// separate line comment 1
// separate line comment 2
pub type ComplexLayout {
    field1: i32,
    field2: bool, // field comment
}

#[align(8)] /* block */ /* another block */
pub type MultipleBlocks {
    value: u64,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);

    // Parse again to verify round-trip
    let module2 = parse_str_for_tests(&printed).unwrap();
    let printed2 = pretty_print(&module2);

    assert_eq!(printed, printed2);
}
