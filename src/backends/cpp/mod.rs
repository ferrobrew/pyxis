//! C++ backend.
//!
//! Emits one `.hpp` per pyxis module (with forward-decls + includes + full
//! definitions in a single header), plus a top-level `CMakeLists.txt`,
//! `pyxis_runtime.hpp`, and `cmake-toolchains/xwin-x86.cmake` so the project
//! compiles into a static library targeting MSVC ABI.
//!
//! Phase 1 status: structs / enums / bitflags / type aliases. Vftables,
//! functions, generics, externs, per-module prologue/epilogue, and CMake
//! generation are filled in by later phases.

use std::fmt::Write as _;
use std::path::{Path, PathBuf};

use crate::{
    backends::{BackendError, Result},
    config::Project,
    grammar::ItemPath,
    semantic::{Module, ResolvedSemanticState, types::ItemDefinitionInner},
};

mod cmake;
mod deps;
mod extern_bindings;
mod render;

use extern_bindings::{CppExternBinding, build_cpp_extern_bindings};

/// Top-level C++-backend entry point. Builds the cross-module C++ binding
/// map once, then emits a `.hpp` (and matching `.cpp` if needed) per module,
/// the shared `pyxis_runtime.hpp`, and the project-level CMake glue.
pub fn build(
    out_dir: &Path,
    semantic_state: &ResolvedSemanticState,
    project: &Project,
) -> Result<()> {
    let bindings = build_cpp_extern_bindings(semantic_state);
    for (key, module) in semantic_state.modules() {
        write_module(out_dir, key, semantic_state, module, &bindings)?;
    }
    write_runtime_header(out_dir)?;
    write_cmake(out_dir, project)?;
    Ok(())
}

/// Emit `<out_dir>/include/<module>/...hpp` (and a matching `.cpp` if there
/// are address-bound free functions or extern values to define) for a single
/// module.
fn write_module(
    out_dir: &Path,
    key: &ItemPath,
    semantic_state: &ResolvedSemanticState,
    module: &Module,
    bindings: &std::collections::BTreeMap<ItemPath, CppExternBinding>,
) -> Result<()> {
    if key.is_empty() {
        return Ok(());
    }

    let registry = semantic_state.type_registry();
    let ctx = render::RenderCtx::new(key, registry, bindings);
    let module_deps = deps::collect_module_deps(key, module, registry, bindings);
    let cpp_backends = module.backends.get("cpp");
    let prologue: String = cpp_backends
        .map(|bs| {
            bs.iter()
                .filter_map(|b| b.prologue.as_deref())
                .collect::<Vec<_>>()
                .join("\n")
        })
        .unwrap_or_default();
    let epilogue: String = cpp_backends
        .map(|bs| {
            bs.iter()
                .filter_map(|b| b.epilogue.as_deref())
                .collect::<Vec<_>>()
                .join("\n")
        })
        .unwrap_or_default();

    let mut body = String::new();
    let mut post = String::new();
    let mut wrote_anything = false;
    // Topologically sort items by intra-module FullDef edges so that any
    // by-value reference (`Aabb { Vector3 min; }`) lands after its target
    // is fully defined. Templates and independent items break ties by
    // template-first-then-alphabetical.
    let raw_items: Vec<_> = module.definitions(registry).collect();
    let sorted_items = deps::topo_sort_module_items(key, raw_items, registry, bindings);
    for item in sorted_items {
        let Some(rendered) = render::render_item(item, ctx)? else {
            continue;
        };
        if !rendered.decl.is_empty() {
            if wrote_anything {
                writeln!(body)?;
            }
            body.push_str(&rendered.decl);
            wrote_anything = true;
        }
        if !rendered.post.is_empty() {
            post.push_str(&rendered.post);
        }
    }

    // Module-level free functions (`#[address] fn foo()`).
    let mut public_functions: Vec<_> = module
        .functions()
        .iter()
        .filter(|f| f.visibility == crate::semantic::types::Visibility::Public)
        .collect();
    public_functions.sort_by(|a, b| a.name.cmp(&b.name));
    for func in &public_functions {
        if let Some(text) = render::render_free_function_decl(func, ctx)? {
            if wrote_anything {
                writeln!(body)?;
            }
            body.push_str(&text);
            wrote_anything = true;
        }
    }

    // Module-level extern values (`extern foo: Bar`).
    let mut sorted_externs: Vec<_> = module.extern_values.iter().collect();
    sorted_externs.sort_by(|a, b| a.name.cmp(&b.name));
    for ev in &sorted_externs {
        let text = render::render_extern_value_decl(ev, ctx)?;
        if wrote_anything {
            writeln!(body)?;
        }
        body.push_str(&text);
        wrote_anything = true;
    }

    if !wrote_anything
        && module_deps.include_modules.is_empty()
        && module_deps.include_headers.is_empty()
        && prologue.is_empty()
        && epilogue.is_empty()
    {
        return Ok(());
    }

    let header_path = module_to_header_path(out_dir, key);
    if let Some(parent) = header_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| BackendError::Io {
            error: e,
            context: format!("Failed to create directory {}", parent.display()),
        })?;
    }

    let mut out = String::new();
    writeln!(out, "// @generated by pyxis — do not edit")?;
    writeln!(out, "#pragma once")?;
    writeln!(out)?;
    writeln!(out, "#include <cstdint>")?;
    writeln!(out, "#include <cstddef>")?;
    writeln!(out, "#include \"pyxis_runtime.hpp\"")?;

    // External `#include`s pulled in via #[cpp_header] on extern types.
    if !module_deps.include_headers.is_empty() {
        writeln!(out)?;
        for header in &module_deps.include_headers {
            writeln!(out, "#include {header}")?;
        }
    }

    // Includes for FullDef cross-module deps.
    if !module_deps.include_modules.is_empty() {
        writeln!(out)?;
        for dep_module in &module_deps.include_modules {
            let path = module_to_relative_include(dep_module);
            writeln!(out, "#include \"{path}\"")?;
        }
    }

    // Forward declarations for FwdOnly cross-module deps.
    if !module_deps.forward_decls.is_empty() {
        writeln!(out)?;
        for (dep_module, items) in &module_deps.forward_decls {
            open_namespace(&mut out, dep_module)?;
            for item_path in items {
                let leaf = item_path.last().map(|s| s.as_str()).unwrap_or("");
                let kind = forward_decl_kind(item_path, semantic_state);
                writeln!(out, "    {kind} {leaf};")?;
            }
            close_namespace(&mut out, dep_module)?;
        }
    }

    // Module-level prologue (e.g. JC2's hand-written shared_ptr / atomic
    // template specializations) is spliced in *before* the namespace block,
    // so it can also pull in additional `#include`s if needed.
    if !prologue.is_empty() {
        writeln!(out)?;
        for line in prologue.lines() {
            writeln!(out, "{line}")?;
        }
    }

    if wrote_anything || !epilogue.is_empty() {
        writeln!(out)?;
        open_namespace(&mut out, key)?;
        // Intra-module forward declarations: every non-alias, non-extern
        // resolved item gets a forward decl up top so pointer-typed fields
        // (and generic instantiations of pointer-only templates) can
        // reference peers defined later in the file. Templates need a full
        // template-parameter signature on their forward decl.
        let mut intra_fwd: Vec<String> = Vec::new();
        for item in module.definitions(registry) {
            if item.is_predefined()
                || matches!(item.category, crate::semantic::types::ItemCategory::Extern)
            {
                continue;
            }
            let Some(resolved) = item.resolved() else {
                continue;
            };
            let kind = match &resolved.inner {
                ItemDefinitionInner::Type(_) => "struct",
                ItemDefinitionInner::Enum(_) | ItemDefinitionInner::Bitflags(_) => continue,
                ItemDefinitionInner::TypeAlias(_) => continue,
            };
            let Some(leaf) = item.path.last() else {
                continue;
            };
            if item.is_generic() {
                let params = item
                    .type_parameters
                    .iter()
                    .map(|p| format!("class {p}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                intra_fwd.push(format!("template <{params}> {kind} {leaf};"));
            } else {
                intra_fwd.push(format!("{kind} {leaf};"));
            }
        }
        intra_fwd.sort();
        intra_fwd.dedup();
        if !intra_fwd.is_empty() {
            for line in &intra_fwd {
                writeln!(out, "    {line}")?;
            }
            writeln!(out)?;
        }
        for line in body.lines() {
            if line.is_empty() {
                writeln!(out)?;
            } else {
                writeln!(out, "    {line}")?;
            }
        }
        if !post.is_empty() {
            writeln!(out)?;
            for line in post.lines() {
                if line.is_empty() {
                    writeln!(out)?;
                } else {
                    writeln!(out, "    {line}")?;
                }
            }
        }
        if !epilogue.is_empty() {
            writeln!(out)?;
            for line in epilogue.lines() {
                if line.is_empty() {
                    writeln!(out)?;
                } else {
                    writeln!(out, "    {line}")?;
                }
            }
        }
        close_namespace(&mut out, key)?;
    }

    std::fs::write(&header_path, &out).map_err(|e| BackendError::Io {
        error: e,
        context: format!("Failed to write header to {}", header_path.display()),
    })?;

    // Emit a matching .cpp if there are out-of-line definitions to produce
    // (free functions with #[address], extern values).
    let needs_cpp = !public_functions.is_empty() || !sorted_externs.is_empty();
    if needs_cpp {
        let cpp_path = module_to_source_path(out_dir, key);
        if let Some(parent) = cpp_path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| BackendError::Io {
                error: e,
                context: format!("Failed to create directory {}", parent.display()),
            })?;
        }
        let mut cpp = String::new();
        writeln!(cpp, "// @generated by pyxis — do not edit")?;
        writeln!(cpp)?;
        let header_include = module_to_relative_include(key);
        writeln!(cpp, "#include \"{header_include}\"")?;
        writeln!(cpp)?;
        open_namespace(&mut cpp, key)?;
        let mut wrote_def = false;
        for func in &public_functions {
            if let Some(text) = render::render_free_function_definition(func, ctx)? {
                for line in text.lines() {
                    if line.is_empty() {
                        writeln!(cpp)?;
                    } else {
                        writeln!(cpp, "    {line}")?;
                    }
                }
                writeln!(cpp)?;
                wrote_def = true;
            }
        }
        for ev in &sorted_externs {
            let text = render::render_extern_value_definition(ev, ctx)?;
            for line in text.lines() {
                if line.is_empty() {
                    writeln!(cpp)?;
                } else {
                    writeln!(cpp, "    {line}")?;
                }
            }
            wrote_def = true;
        }
        let _ = wrote_def;
        close_namespace(&mut cpp, key)?;
        std::fs::write(&cpp_path, &cpp).map_err(|e| BackendError::Io {
            error: e,
            context: format!("Failed to write source to {}", cpp_path.display()),
        })?;
    }

    Ok(())
}

fn module_to_source_path(out_dir: &Path, module_path: &ItemPath) -> PathBuf {
    let mut p = out_dir.join("src");
    let segs: Vec<_> = module_path.iter().collect();
    for (i, seg) in segs.iter().enumerate() {
        if i + 1 == segs.len() {
            p.push(format!("{}.cpp", seg.as_str()));
        } else {
            p.push(seg.as_str());
        }
    }
    p
}

/// Emit `<out_dir>/include/pyxis_runtime.hpp` — shared typedefs / utility
/// templates used by every generated module.
pub fn write_runtime_header(out_dir: &Path) -> Result<()> {
    let path = out_dir.join("include").join("pyxis_runtime.hpp");
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| BackendError::Io {
            error: e,
            context: format!("Failed to create directory {}", parent.display()),
        })?;
    }

    let out = r#"// @generated by pyxis — do not edit
#pragma once

#include <atomic>
#include <cstdint>

// Calling-convention shim. We always target MSVC ABI (x86 32-bit Windows via
// clang-cl + xwin); on non-MSVC dev hosts the macros expand to nothing so
// generated headers still compile-check during iteration.
#if defined(_MSC_VER)
#  define PYXIS_CDECL __cdecl
#  define PYXIS_STDCALL __stdcall
#  define PYXIS_FASTCALL __fastcall
#  define PYXIS_THISCALL __thiscall
#  define PYXIS_VECTORCALL __vectorcall
#elif (defined(__i386__) || defined(_M_IX86)) && (defined(__GNUC__) || defined(__clang__))
#  define PYXIS_CDECL __attribute__((cdecl))
#  define PYXIS_STDCALL __attribute__((stdcall))
#  define PYXIS_FASTCALL __attribute__((fastcall))
#  define PYXIS_THISCALL __attribute__((thiscall))
#  define PYXIS_VECTORCALL __attribute__((sysv_abi))
#else
#  define PYXIS_CDECL
#  define PYXIS_STDCALL
#  define PYXIS_FASTCALL
#  define PYXIS_THISCALL
#  define PYXIS_VECTORCALL
#endif

namespace pyxis {
// Atomic primitives map to `std::atomic<T>`. On every reasonable platform
// these specialise to the same size and alignment as the underlying integer
// (so the layout `static_assert`s pyxis emits hold), and they give real
// load/store/CAS semantics for projects that need them (e.g. JC2's
// SharedPtr refcount logic).
using AtomicBool = ::std::atomic<bool>;
using AtomicU8   = ::std::atomic<::std::uint8_t>;
using AtomicU16  = ::std::atomic<::std::uint16_t>;
using AtomicU32  = ::std::atomic<::std::uint32_t>;
using AtomicU64  = ::std::atomic<::std::uint64_t>;
using AtomicI8   = ::std::atomic<::std::int8_t>;
using AtomicI16  = ::std::atomic<::std::int16_t>;
using AtomicI32  = ::std::atomic<::std::int32_t>;
using AtomicI64  = ::std::atomic<::std::int64_t>;

// Wrapper that suppresses `T`'s destructor (mirrors Rust's
// `core::mem::ManuallyDrop<T>`). Same layout as T; the inner value lives in
// a union, so when a `ManuallyDrop<T>` goes out of scope, *nothing* runs on
// `value` — to destroy it you must call `md.value.~T()` explicitly. The
// default constructor leaves `value` uninitialised, again matching Rust's
// ManuallyDrop semantics for fields within zero-initialised parents.
template <class T>
union ManuallyDrop {
    T value;
    ManuallyDrop() {}
    ~ManuallyDrop() {}
};
} // namespace pyxis
"#;

    std::fs::write(&path, out).map_err(|e| BackendError::Io {
        error: e,
        context: format!("Failed to write {}", path.display()),
    })?;
    Ok(())
}

/// Emit `<out_dir>/CMakeLists.txt` and `<out_dir>/cmake-toolchains/xwin-x86.cmake`.
pub fn write_cmake(out_dir: &Path, project: &Project) -> Result<()> {
    cmake::write_cmake(out_dir, project)
}

fn module_to_header_path(out_dir: &Path, module_path: &ItemPath) -> PathBuf {
    let mut p = out_dir.join("include");
    let segs: Vec<_> = module_path.iter().collect();
    for (i, seg) in segs.iter().enumerate() {
        if i + 1 == segs.len() {
            p.push(format!("{}.hpp", seg.as_str()));
        } else {
            p.push(seg.as_str());
        }
    }
    p
}

fn module_to_relative_include(module_path: &ItemPath) -> String {
    let mut s = String::new();
    let segs: Vec<_> = module_path.iter().collect();
    for (i, seg) in segs.iter().enumerate() {
        if i > 0 {
            s.push('/');
        }
        s.push_str(seg.as_str());
    }
    s.push_str(".hpp");
    s
}

fn open_namespace(out: &mut String, module_path: &ItemPath) -> Result<()> {
    write!(out, "namespace ")?;
    for (i, seg) in module_path.iter().enumerate() {
        if i > 0 {
            write!(out, "::")?;
        }
        write!(out, "{}", seg.as_str())?;
    }
    writeln!(out, " {{")?;
    Ok(())
}

fn close_namespace(out: &mut String, module_path: &ItemPath) -> Result<()> {
    write!(out, "}} // namespace ")?;
    for (i, seg) in module_path.iter().enumerate() {
        if i > 0 {
            write!(out, "::")?;
        }
        write!(out, "{}", seg.as_str())?;
    }
    writeln!(out)?;
    Ok(())
}

fn forward_decl_kind(item_path: &ItemPath, semantic_state: &ResolvedSemanticState) -> &'static str {
    let registry = semantic_state.type_registry();
    if let Ok(item) = registry.get(item_path, &crate::span::ItemLocation::internal())
        && let Some(resolved) = item.resolved()
    {
        return match &resolved.inner {
            ItemDefinitionInner::Type(_) => "struct",
            ItemDefinitionInner::Enum(_) | ItemDefinitionInner::Bitflags(_) => {
                // Forward-declaring an enum requires its underlying type;
                // skip the safer-but-broken option and fall back to `struct`
                // — Phase 3 will refine this once we emit the underlying
                // type alongside.
                "enum class"
            }
            ItemDefinitionInner::TypeAlias(_) => "struct",
        };
    }
    "struct"
}
