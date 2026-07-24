use pretty_assertions::assert_eq;

use crate::{parser::parse_str_for_tests, pretty_print::*};

#[test]
fn test_multiline_regular_splice_becomes_raw() {
    // A multi-line splice body is a code block: it renders as a raw string
    // across real lines rather than a single-line `"\n...\n"` escape soup,
    // regardless of whether the source used a regular or raw literal.
    let text = r#"
#[cfg(backend = "rust")]
prologue "\n    use crate::shared_ptr::*;\n    use std::mem::ManuallyDrop;\n";
    "#;

    let expected = r##"#[cfg(backend = "rust")]
prologue r#"
    use crate::shared_ptr::*;
    use std::mem::ManuallyDrop;
"#;"##;

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);
}

#[test]
fn test_single_line_regular_splice_stays_regular() {
    // A single-line body has no newline to lay out, so it keeps its
    // regular-string form.
    let text = r#"
#[cfg(backend = "rust")]
epilogue "pub const K: u32 = 1;";
    "#;

    let expected = r#"#[cfg(backend = "rust")]
epilogue "pub const K: u32 = 1;";"#;

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);
}

#[test]
fn test_preserve_raw_string_format_in_splice() {
    let text = r##"
#[cfg(backend = "rust")]
prologue r#"
    use crate::shared_ptr::*;
    use std::mem::ManuallyDrop;
"#;
    "##;

    let expected = r##"
#[cfg(backend = "rust")]
prologue r#"
    use crate::shared_ptr::*;
    use std::mem::ManuallyDrop;
"#;
    "##
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);
}

#[test]
fn test_preserve_module_doc_comments() {
    let text = r#"
//! This is a render block.

#[size(8), align(4)]
pub type RenderBlock {
    field: u32,
}
    "#;

    let expected = r#"
//! This is a render block.

#[size(8), align(4)]
pub type RenderBlock {
    field: u32,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);
}

#[test]
fn test_preserve_extern_type_doc_comments() {
    let text = r#"
/// `ManuallyDrop<SharedPtr<u32>>` is used instead of `SharedPtr<u32>` to avoid
/// the `Drop` implementation of `SharedPtr<u32>` being called when the `RenderBlock`
/// is dropped. The destructor, which we call in `drop`, will decrement the refcount
/// for us.
#[size(8), align(4)]
extern type ManuallyDrop<SharedPtr<u32>>;
    "#;

    let expected = r#"
/// `ManuallyDrop<SharedPtr<u32>>` is used instead of `SharedPtr<u32>` to avoid
/// the `Drop` implementation of `SharedPtr<u32>` being called when the `RenderBlock`
/// is dropped. The destructor, which we call in `drop`, will decrement the refcount
/// for us.
#[size(8), align(4)]
extern type ManuallyDrop<SharedPtr<u32>>;
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);
}

#[test]
fn test_binary_literal_formatting_u8() {
    let text = r#"
#[copyable]
pub bitflags CameraState: u8 {
    m_UseOffCenter = 0b0000_0001,
    m_ScreenshotSeriesRunning = 0b0000_0010,
    m_Ortho = 0b0000_0100,
    m_ComputeView = 0b0000_1000,
    m_DirtyProjection = 0b0001_0000,
    m_IsRenderCamera = 0b0010_0000,
}
    "#;

    let expected = r#"
#[copyable]
pub bitflags CameraState: u8 {
    m_UseOffCenter = 0b0000_0001,
    m_ScreenshotSeriesRunning = 0b0000_0010,
    m_Ortho = 0b0000_0100,
    m_ComputeView = 0b0000_1000,
    m_DirtyProjection = 0b0001_0000,
    m_IsRenderCamera = 0b0010_0000,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);
}

#[test]
fn test_binary_literal_formatting_u32() {
    let text = r#"
pub bitflags TestFlags: u32 {
    FLAG_1 = 0b0000_0000_0000_0000_0000_0000_0000_0001,
    FLAG_2 = 0b0000_0000_0000_0000_0000_0000_0000_0010,
    FLAG_BIG = 0b1000_0000_0000_0000_0000_0000_0000_0000,
}
    "#;

    let expected = r#"
pub bitflags TestFlags: u32 {
    FLAG_1 = 0b0000_0000_0000_0000_0000_0000_0000_0001,
    FLAG_2 = 0b0000_0000_0000_0000_0000_0000_0000_0010,
    FLAG_BIG = 0b1000_0000_0000_0000_0000_0000_0000_0000,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);
}

#[test]
fn test_pinned_attribute_round_trips() {
    let text = r#"
#[pinned]
pub type PinnedType {
    pub value: u32,
}
    "#;

    let expected = r#"
#[pinned]
pub type PinnedType {
    pub value: u32,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);
}

#[test]
fn test_nested_enum_only_no_blank() {
    let text = r#"
    pub type Outer {
        pub enum InnerEnum: u8 {
            A,
            B,
        }
    }
    "#;

    let expected = r#"
pub type Outer {
    pub enum InnerEnum: u8 {
        A,
        B,
    },
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);
}

#[test]
fn test_field_only_no_blank() {
    let text = r#"
    pub type Outer {
        pub field: u32,
    }
    "#;

    let expected = r#"
pub type Outer {
    pub field: u32,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);
}

#[test]
fn test_nested_and_field_one_blank() {
    let text = r#"
    pub type Outer {
        pub field: u32,
        pub enum InnerEnum: u8 {
            A,
            B,
        }
    }
    "#;

    // Nested items should be reordered first, with a blank line between
    // the nested item group and the field group.
    let expected = r#"
pub type Outer {
    pub enum InnerEnum: u8 {
        A,
        B,
    },

    pub field: u32,
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);

    assert_eq!(printed, expected);
}

#[test]
fn test_nested_round_trip() {
    let text = r#"
pub type Outer {
    pub field: u32,
    pub enum InnerEnum: u8 {
        A,
        B,
    }
    pub type InnerType {
        pub inner_field: u16,
    }
}
    "#
    .trim();

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);
    // Round-trip: parse the printed output and print again
    let module2 = parse_str_for_tests(&printed).unwrap();
    let printed2 = pretty_print(&module2);

    assert_eq!(printed, printed2);
}

#[test]
fn new_const_forms_round_trip() {
    // C-string literals, array literals, and const aliases must all
    // survive a round-trip through pretty-print.
    let text = "pub const DLL_NAME: cstr = c\"kernel32.dll\";\n\
pub const ARR: [i32; 3] = [1, 2, 3];\n\
pub const ALIAS: i32 = ARR;";

    let module = parse_str_for_tests(text).unwrap();
    let printed = pretty_print(&module);
    let module2 = parse_str_for_tests(&printed).unwrap();
    let printed2 = pretty_print(&module2);

    assert_eq!(printed, printed2);
}
