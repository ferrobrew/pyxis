//! Module → file-tree orchestration: writing headers/sources per module,
//! the shared runtime header, CMake glue, and the namespace/path/include
//! helpers shared between the assembler and the orchestrator.

use std::{
    fmt::Write as _,
    path::{Path, PathBuf},
};

use crate::{
    backends::{
        BackendError, Result,
        cpp::{deps, render},
    },
    grammar::ItemPath,
    semantic::{Module, SemanticOutput, types::ItemDefinitionInner},
};

use super::{
    assemble::{assemble_header, assemble_source, extract_cpp_splices, render_module_body},
    cmake,
    extern_bindings::CppExternBinding,
    runtime,
};

/// Emit `<out_dir>/include/<module>/...hpp` (and a matching `.cpp` if there
/// are address-bound free functions or extern values to define) for a single
/// module. Orchestrator: pulls together splices, renders items, and writes
/// the header and (optionally) the source file.
pub(super) fn write_module(
    out_dir: &Path,
    key: &ItemPath,
    semantic_state: &SemanticOutput,
    module: &Module,
    bindings: &std::collections::BTreeMap<ItemPath, CppExternBinding>,
) -> Result<()> {
    if key.is_empty() {
        return Ok(());
    }

    let registry = semantic_state.type_registry();
    let cfg_ctx = crate::parser::cfg::CfgContext {
        backend: crate::Backend::Cpp,
    };
    let ctx = render::RenderCtx::new(
        key,
        registry,
        bindings,
        cfg_ctx,
        semantic_state.module_doc_links(key),
    );
    let module_deps = deps::collect_module_deps(key, module, registry, bindings);
    let splices = extract_cpp_splices(module);
    let body = render_module_body(key, module, registry, bindings, ctx)?;

    // Skip writing anything if the module contributes no declarations,
    // no cross-module deps, and no user-supplied splices.
    if !body.wrote_anything
        && module.reexports().is_empty()
        && module_deps.include_modules.is_empty()
        && module_deps.include_headers.is_empty()
        && splices.prologue.is_empty()
        && splices.epilogue.is_empty()
        && splices.prologue_def.is_empty()
        && splices.epilogue_def.is_empty()
    {
        return Ok(());
    }

    // Header.
    let header_path = module_to_header_path(out_dir, key);
    if let Some(parent) = header_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| BackendError::Io {
            error: e,
            context: format!("Failed to create directory {}", parent.display()),
        })?;
    }
    let header_text = assemble_header(
        key,
        semantic_state,
        module,
        registry,
        ctx,
        &module_deps,
        &body.body,
        &splices,
    )?;
    std::fs::write(&header_path, &header_text).map_err(|e| BackendError::Io {
        error: e,
        context: format!("Failed to write header to {}", header_path.display()),
    })?;

    // Source file, if there are out-of-line definitions to produce:
    // free functions with #[address], extern values, non-template
    // member definitions hoisted out of the header, or a user-supplied
    // `prologue definition` / `epilogue definition` block.
    // Extern values' `.cpp` getter definitions land in `body.post_cpp`, so the
    // post_cpp check below covers them.
    let needs_cpp = !body.public_functions.is_empty()
        || !body.post_cpp.is_empty()
        || !splices.prologue_def.is_empty()
        || !splices.epilogue_def.is_empty();
    if needs_cpp {
        let cpp_path = module_to_emitted_path(out_dir, key, "src", "cpp");
        if let Some(parent) = cpp_path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| BackendError::Io {
                error: e,
                context: format!("Failed to create directory {}", parent.display()),
            })?;
        }
        let cpp_text = assemble_source(key, ctx, &body, &splices, &module_deps)?;
        std::fs::write(&cpp_path, &cpp_text).map_err(|e| BackendError::Io {
            error: e,
            context: format!("Failed to write source to {}", cpp_path.display()),
        })?;
    }

    Ok(())
}

/// Map a module path to its emitted file location. `base_dir_name` picks
/// the subtree (`"include"` for headers, `"src"` for sources) and `ext`
/// is the extension (`"hpp"` or `"cpp"`); intermediate segments become
/// directories, and the trailing segment becomes the filename.
fn module_to_emitted_path(
    out_dir: &Path,
    module_path: &ItemPath,
    base_dir_name: &str,
    ext: &str,
) -> PathBuf {
    let mut p = out_dir.join(base_dir_name);
    let segs: Vec<_> = module_path.iter().collect();
    for (i, seg) in segs.iter().enumerate() {
        if i + 1 == segs.len() {
            p.push(format!("{}.{ext}", seg.as_str()));
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

    let cc_defines = runtime::runtime_header_defines();
    let out = format!(
        r#"// @generated by pyxis — do not edit
#pragma once

#include <atomic>
#include <cstdint>

// Calling-convention shim. We always target MSVC ABI (x86 32-bit Windows via
// clang-cl + xwin); on non-MSVC dev hosts the macros expand to nothing so
// generated headers still compile-check during iteration. The macro list
// is generated from `cpp::runtime::PYXIS_CC_MACROS`.
{cc_defines}

namespace pyxis {{
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
union ManuallyDrop {{
    T value;
    ManuallyDrop() {{}}
    ~ManuallyDrop() {{}}
}};
}} // namespace pyxis
"#
    );

    std::fs::write(&path, out).map_err(|e| BackendError::Io {
        error: e,
        context: format!("Failed to write {}", path.display()),
    })?;
    Ok(())
}

/// Emit `<out_dir>/CMakeLists.txt` and `<out_dir>/cmake-toolchains/xwin-x86.cmake`.
pub fn write_cmake(out_dir: &Path, project: &crate::config::Project) -> Result<()> {
    cmake::write_cmake(out_dir, project)
}

fn module_to_header_path(out_dir: &Path, module_path: &ItemPath) -> PathBuf {
    module_to_emitted_path(out_dir, module_path, "include", "hpp")
}

pub(super) fn module_to_relative_include(module_path: &ItemPath) -> String {
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

pub(super) fn open_namespace(out: &mut String, module_path: &ItemPath) -> Result<()> {
    write!(out, "namespace ")?;
    for (i, seg) in module_path.iter().enumerate() {
        if i > 0 {
            write!(out, "::")?;
        }
        write!(out, "{}", render::cpp_namespace_ident(seg.as_str()))?;
    }
    writeln!(out, " {{")?;
    Ok(())
}

pub(super) fn close_namespace(out: &mut String, module_path: &ItemPath) -> Result<()> {
    write!(out, "}} // namespace ")?;
    for (i, seg) in module_path.iter().enumerate() {
        if i > 0 {
            write!(out, "::")?;
        }
        write!(out, "{}", render::cpp_namespace_ident(seg.as_str()))?;
    }
    writeln!(out)?;
    Ok(())
}

/// Parse the argument of an `#include` directive. Returns `Some("<...>"
/// or `Some("\"...\"")` for a bracketed or quoted include, or `None`
/// for a non-include line. Whitespace before `#include` and between
/// `#include` and the argument is tolerated.
pub(super) fn parse_include_arg(line: &str) -> Option<&str> {
    let trimmed = line.trim_start();
    let rest = trimmed.strip_prefix("#include")?;
    let rest = rest.trim_start();
    if let Some(stripped) = rest.strip_prefix('"') {
        let end = stripped.find('"')?;
        Some(&rest[..end + 2])
    } else if let Some(stripped) = rest.strip_prefix('<') {
        let end = stripped.find('>')?;
        Some(&rest[..end + 2])
    } else {
        None
    }
}

/// Emit a forward-decl line (without trailing newline) for `item_path`.
/// Enums/bitflags need their underlying type spelled out, otherwise the
/// re-declaration at the point of definition fails with "redeclared with
/// different underlying type" since `enum class Foo;` defaults to `int`.
pub(super) fn forward_decl_line(
    item_path: &ItemPath,
    semantic_state: &SemanticOutput,
    ctx: render::RenderCtx,
) -> String {
    let leaf = render::cpp_ident(item_path.last().map(|s| s.as_str()).unwrap_or(""));
    let registry = semantic_state.type_registry();
    if let Ok(item) = registry.get(item_path, &crate::span::ItemLocation::internal())
        && let Some(resolved) = item.resolved()
    {
        match &resolved.inner {
            ItemDefinitionInner::Type(_) => return format!("struct {leaf};"),
            ItemDefinitionInner::Union(_) => return format!("union {leaf};"),
            ItemDefinitionInner::Enum(ed) => {
                let underlying = render::render_type(&ed.type_, ctx)
                    .unwrap_or_else(|_| "::std::int32_t".to_string());
                return format!("enum class {leaf} : {underlying};");
            }
            ItemDefinitionInner::Bitflags(bd) => {
                let underlying = render::render_type(&bd.type_, ctx)
                    .unwrap_or_else(|_| "::std::int32_t".to_string());
                return format!("enum class {leaf} : {underlying};");
            }
            ItemDefinitionInner::TypeAlias(_) => return format!("struct {leaf};"),
            ItemDefinitionInner::Constant(_) => return format!("struct {leaf};"),
            ItemDefinitionInner::ExternValue(_) => return format!("struct {leaf};"),
        }
    }
    format!("struct {leaf};")
}
