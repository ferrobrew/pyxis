//! C++ identifier escaping (keywords, C-runtime globals) and the
//! predefined-type → C++ primitive mapping.

use std::borrow::Cow;

use crate::semantic::types::PredefinedItem;

/// C-runtime identifiers that live in the *global* namespace of every
/// generated translation unit, because the runtime header's mandatory
/// includes pull them in (notably `<atomic>` transitively includes
/// `<time.h>`, which declares `::clock`). A pyxis module emitted as a
/// global `namespace` with one of these names is a "redefinition as a
/// different kind of symbol", so they're escaped in *namespace position*
/// only — they're harmless as field/member/type names (those are scoped).
const CPP_RESERVED_GLOBALS: &[&str] = &[
    "clock",
    "time",
    "difftime",
    "mktime",
    "asctime",
    "ctime",
    "gmtime",
    "localtime",
    "strftime",
    "clock_t",
    "time_t",
    "tm",
    "timespec",
];

/// Escape pyxis identifiers that collide with C++ reserved words by
/// suffixing an underscore. Idempotent for non-conflicting names.
pub fn cpp_ident(name: &str) -> Cow<'_, str> {
    if CPP_KEYWORDS.contains(&name) {
        Cow::Owned(format!("{name}_"))
    } else {
        Cow::Borrowed(name)
    }
}

/// Like [`cpp_ident`], but for module/namespace segments: also escapes
/// C-runtime globals (see [`CPP_RESERVED_GLOBALS`]) that collide with a
/// global `namespace` of the same name.
pub fn cpp_namespace_ident(name: &str) -> Cow<'_, str> {
    if CPP_KEYWORDS.contains(&name) || CPP_RESERVED_GLOBALS.contains(&name) {
        Cow::Owned(format!("{name}_"))
    } else {
        Cow::Borrowed(name)
    }
}

const CPP_KEYWORDS: &[&str] = &[
    "alignas",
    "alignof",
    "and",
    "and_eq",
    "asm",
    "auto",
    "bitand",
    "bitor",
    "bool",
    "break",
    "case",
    "catch",
    "char",
    "char8_t",
    "char16_t",
    "char32_t",
    "class",
    "compl",
    "concept",
    "const",
    "consteval",
    "constexpr",
    "constinit",
    "const_cast",
    "continue",
    "co_await",
    "co_return",
    "co_yield",
    "decltype",
    "default",
    "delete",
    "do",
    "double",
    "dynamic_cast",
    "else",
    "enum",
    "explicit",
    "export",
    "extern",
    "false",
    "float",
    "for",
    "friend",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "mutable",
    "namespace",
    "new",
    "noexcept",
    "not",
    "not_eq",
    "nullptr",
    "operator",
    "or",
    "or_eq",
    "private",
    "protected",
    "public",
    "register",
    "reinterpret_cast",
    "requires",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "static_assert",
    "static_cast",
    "struct",
    "switch",
    "template",
    "this",
    "thread_local",
    "throw",
    "true",
    "try",
    "typedef",
    "typeid",
    "typename",
    "union",
    "unsigned",
    "using",
    "virtual",
    "void",
    "volatile",
    "wchar_t",
    "while",
    "xor",
    "xor_eq",
];

pub(super) fn predefined_to_cpp(p: PredefinedItem) -> &'static str {
    match p {
        PredefinedItem::Void => "void",
        PredefinedItem::Bool => "bool",
        PredefinedItem::U8 => "::std::uint8_t",
        PredefinedItem::U16 => "::std::uint16_t",
        PredefinedItem::U32 => "::std::uint32_t",
        PredefinedItem::U64 => "::std::uint64_t",
        PredefinedItem::U128 => "::std::uint64_t /* u128 */",
        PredefinedItem::I8 => "::std::int8_t",
        PredefinedItem::I16 => "::std::int16_t",
        PredefinedItem::I32 => "::std::int32_t",
        PredefinedItem::I64 => "::std::int64_t",
        PredefinedItem::I128 => "::std::int64_t /* i128 */",
        PredefinedItem::F32 => "float",
        PredefinedItem::F64 => "double",
        PredefinedItem::CChar => "char",
        // Atomics get real bindings in Phase 3 via #[cpp_header]/<atomic>;
        // for now use opaque size-correct placeholders.
        PredefinedItem::AtomicBool => "::pyxis::AtomicBool",
        PredefinedItem::AtomicU8 => "::pyxis::AtomicU8",
        PredefinedItem::AtomicU16 => "::pyxis::AtomicU16",
        PredefinedItem::AtomicU32 => "::pyxis::AtomicU32",
        PredefinedItem::AtomicU64 => "::pyxis::AtomicU64",
        PredefinedItem::AtomicI8 => "::pyxis::AtomicI8",
        PredefinedItem::AtomicI16 => "::pyxis::AtomicI16",
        PredefinedItem::AtomicI32 => "::pyxis::AtomicI32",
        PredefinedItem::AtomicI64 => "::pyxis::AtomicI64",
        PredefinedItem::Str => "const char* const",
        PredefinedItem::CStr => "const char* const",
    }
}

#[cfg(test)]
mod ident_tests {
    use super::{cpp_ident, cpp_namespace_ident};

    #[test]
    fn keywords_are_escaped_everywhere() {
        assert_eq!(cpp_ident("class"), "class_");
        assert_eq!(cpp_namespace_ident("class"), "class_");
    }

    #[test]
    fn plain_identifiers_are_untouched() {
        assert_eq!(cpp_ident("Clock"), "Clock");
        assert_eq!(cpp_namespace_ident("world"), "world");
    }

    #[test]
    fn runtime_globals_are_escaped_only_in_namespace_position() {
        // `clock` collides with the C-runtime `::clock` only as a global
        // namespace; it's fine as a field/type name.
        assert_eq!(cpp_namespace_ident("clock"), "clock_");
        assert_eq!(cpp_ident("clock"), "clock");
    }
}
