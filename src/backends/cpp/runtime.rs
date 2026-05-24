//! Single source of truth for the cpp backend's calling-convention
//! shim macros. Both `pyxis_runtime.hpp` (emitted by
//! [`super::write_runtime_header`]) and `render.rs`'s call-site lookup
//! derive from the table below, so renaming or adding a macro is one
//! edit in one place.

use std::fmt::Write as _;

use crate::semantic::types::CallingConvention;

/// One entry per calling-convention shim macro. `name` is the
/// `PYXIS_*` identifier that lands in emitted code; the keyword /
/// attribute fields drive the `#define` for each platform branch.
#[derive(Copy, Clone)]
pub struct CcMacro {
    /// PYXIS_* macro identifier (e.g. `"PYXIS_CDECL"`).
    pub name: &'static str,
    /// MSVC keyword the macro expands to under `_MSC_VER`.
    pub msvc_keyword: &'static str,
    /// Clang/GCC i386 attribute name (passed as
    /// `__attribute__((NAME))`). `None` means "no equivalent on this
    /// branch, expand to nothing" - currently just `vectorcall`.
    pub gnu_i386_attr: Option<&'static str>,
}

pub const PYXIS_CC_MACROS: &[CcMacro] = &[
    CcMacro {
        name: "PYXIS_CDECL",
        msvc_keyword: "__cdecl",
        gnu_i386_attr: Some("cdecl"),
    },
    CcMacro {
        name: "PYXIS_STDCALL",
        msvc_keyword: "__stdcall",
        gnu_i386_attr: Some("stdcall"),
    },
    CcMacro {
        name: "PYXIS_FASTCALL",
        msvc_keyword: "__fastcall",
        gnu_i386_attr: Some("fastcall"),
    },
    CcMacro {
        name: "PYXIS_THISCALL",
        msvc_keyword: "__thiscall",
        gnu_i386_attr: Some("thiscall"),
    },
    CcMacro {
        name: "PYXIS_VECTORCALL",
        msvc_keyword: "__vectorcall",
        // Clang/GCC don't have a real vectorcall on i386; expand to
        // nothing on the non-MSVC branch so the dev-loop compile-check
        // still works. Binaries that actually rely on vectorcall
        // semantics must build against the MSVC arm.
        gnu_i386_attr: None,
    },
];

/// Return the `PYXIS_*` macro that should precede a function pointer
/// of the given calling convention, or `None` for `System` (no shim
/// needed). The returned string has a trailing space so callers can
/// concatenate it directly into a signature.
pub fn macro_emit(cc: CallingConvention) -> &'static str {
    match cc {
        // C and Cdecl both map to the cdecl macro - both target the
        // MSVC `__cdecl` ABI for our binary-binding purposes.
        CallingConvention::C | CallingConvention::Cdecl => "PYXIS_CDECL ",
        CallingConvention::Stdcall => "PYXIS_STDCALL ",
        CallingConvention::Fastcall => "PYXIS_FASTCALL ",
        CallingConvention::Thiscall => "PYXIS_THISCALL ",
        CallingConvention::Vectorcall => "PYXIS_VECTORCALL ",
        CallingConvention::System => "",
    }
}

/// Generate the calling-convention `#define` block for the runtime
/// header. Three platform branches:
/// 1. `_MSC_VER` — use MSVC keywords.
/// 2. Clang/GCC on i386 — expand to `__attribute__((...))` if available.
/// 3. Everything else — no-op so generated headers still compile-check
///    on hosts where the calling convention is meaningless.
pub fn runtime_header_defines() -> String {
    let mut out = String::new();
    writeln!(out, "#if defined(_MSC_VER)").unwrap();
    for m in PYXIS_CC_MACROS {
        writeln!(out, "#  define {} {}", m.name, m.msvc_keyword).unwrap();
    }
    writeln!(
        out,
        "#elif (defined(__i386__) || defined(_M_IX86)) && (defined(__GNUC__) || defined(__clang__))"
    )
    .unwrap();
    for m in PYXIS_CC_MACROS {
        match m.gnu_i386_attr {
            Some(attr) => writeln!(out, "#  define {} __attribute__(({}))", m.name, attr).unwrap(),
            // No equivalent in clang/GCC on i386; the in-header comment
            // explains why a downstream reader sees an empty macro here.
            None => {
                writeln!(
                    out,
                    "// `{}` expands to nothing on this branch — clang/GCC \
                     don't have a real equivalent on i386; binaries that need \
                     the convention's semantics must build against the MSVC arm.",
                    m.name
                )
                .unwrap();
                writeln!(out, "#  define {}", m.name).unwrap();
            }
        }
    }
    writeln!(out, "#else").unwrap();
    for m in PYXIS_CC_MACROS {
        writeln!(out, "#  define {}", m.name).unwrap();
    }
    write!(out, "#endif").unwrap();
    out
}
