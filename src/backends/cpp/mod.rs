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
mod runtime;

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

    // Pre-flight: detect cross-module FullDef cycles by aggregating each
    // module's FullDef cross-module deps and running SCC over the
    // module-level graph. A cycle here means two (or more) modules each
    // need each other's full type definitions — irresolvable by forward
    // declarations.
    let registry = semantic_state.type_registry();
    let mut module_full_deps: std::collections::BTreeMap<
        ItemPath,
        std::collections::BTreeSet<ItemPath>,
    > = std::collections::BTreeMap::new();
    for (key, module) in semantic_state.modules() {
        if key.is_empty() {
            continue;
        }
        let module_deps = deps::collect_module_deps(key, module, registry, &bindings);
        module_full_deps.insert(key.clone(), module_deps.include_modules);
    }
    if let Some(cycle) = deps::first_scc_cycle(&module_full_deps) {
        return Err(crate::backends::BackendError::Cpp(
            crate::backends::error::CppBackendError::LayoutCycle {
                scope: crate::backends::error::CppLayoutCycleScope::CrossModule,
                cycle,
                location: crate::span::ItemLocation::internal(),
            },
        ));
    }

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
    let cfg_ctx = crate::parser::cfg::CfgContext {
        backend: crate::Backend::Cpp,
    };
    let ctx = render::RenderCtx::new(key, registry, bindings, cfg_ctx);
    let module_deps = deps::collect_module_deps(key, module, registry, bindings);
    let cpp_backends = module.backends.get(&crate::Backend::Cpp);
    // Raw-string prologue/epilogue text usually starts and ends with a
    // newline because users write `r#"<newline>...<newline>"#`. Trim
    // those edges so we don't emit double blank lines around the splice.
    // Also dedent each block by its common leading-whitespace prefix so
    // text users wrote indented inside the raw string lands flush at
    // the splice site (the orchestrator reapplies its own indent).
    fn join_slot<'a>(
        bs: &'a [crate::semantic::types::Backend],
        pick: impl Fn(&'a crate::semantic::types::Backend) -> Option<&'a String>,
    ) -> String {
        // Two newlines = one blank line between adjacent splice blocks
        // so multiple `epilogue definition` directives in a file render
        // with the same visual rhythm as definitions within a single block.
        // Each block is dedented, then trimmed of leading/trailing
        // whitespace lines so the explicit `\n\n` separator does the
        // entire job of inter-block spacing - no stacking from internal
        // trailing whitespace.
        bs.iter()
            .filter_map(pick)
            .map(|s| {
                let dedented = dedent(s.trim_matches('\n'));
                dedented.trim().to_string()
            })
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>()
            .join("\n\n")
    }

    /// Strip the common leading-whitespace prefix from every non-empty
    /// line. Empty lines pass through unchanged.
    fn dedent(s: &str) -> String {
        let min_indent = s
            .lines()
            .filter(|l| !l.trim().is_empty())
            .map(|l| l.len() - l.trim_start().len())
            .min()
            .unwrap_or(0);
        if min_indent == 0 {
            return s.to_string();
        }
        s.lines()
            .map(|l| {
                if l.trim().is_empty() {
                    ""
                } else {
                    &l[min_indent..]
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }
    let prologue: String = cpp_backends
        .map(|bs| join_slot(bs, |b| b.prologue.header.as_ref()))
        .unwrap_or_default();
    let prologue_def: String = cpp_backends
        .map(|bs| join_slot(bs, |b| b.prologue.definition.as_ref()))
        .unwrap_or_default();
    let epilogue: String = cpp_backends
        .map(|bs| join_slot(bs, |b| b.epilogue.header.as_ref()))
        .unwrap_or_default();
    let epilogue_def: String = cpp_backends
        .map(|bs| join_slot(bs, |b| b.epilogue.definition.as_ref()))
        .unwrap_or_default();

    let mut body = String::new();
    let mut post_header = String::new();
    let mut post_cpp = String::new();
    let mut wrote_anything = false;
    // Topologically sort items by intra-module FullDef edges so that any
    // by-value reference (`Aabb { Vector3 min; }`) lands after its target
    // is fully defined. Templates and independent items break ties by
    // template-first-then-alphabetical.
    let raw_items: Vec<_> = module
        .definitions(registry)
        .filter(|item| ctx.cfg_passes(&item.cfg))
        .collect();
    let sorted_items = deps::topo_sort_module_items(key, raw_items, registry, bindings)?;
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
        if !rendered.post_header.is_empty() {
            post_header.push_str(&rendered.post_header);
        }
        if !rendered.post_cpp.is_empty() {
            post_cpp.push_str(&rendered.post_cpp);
        }
    }

    // Module-level free functions (`#[address] fn foo()`).
    let mut public_functions: Vec<_> = module
        .functions()
        .iter()
        .filter(|f| f.visibility == crate::semantic::types::Visibility::Public)
        .filter(|f| ctx.cfg_passes(&f.cfg))
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

    // Module-level extern values (`extern foo: Bar`). Pack the
    // declarations tight as a block (no blank line between each), with
    // a single blank line separating the block from the surrounding
    // items.
    let mut sorted_externs: Vec<_> = module.extern_values.iter().collect();
    sorted_externs.sort_by(|a, b| a.name.cmp(&b.name));
    if !sorted_externs.is_empty() {
        if wrote_anything {
            writeln!(body)?;
        }
        for ev in &sorted_externs {
            let text = render::render_extern_value_decl(ev, ctx)?;
            body.push_str(&text);
        }
        wrote_anything = true;
    }

    if !wrote_anything
        && module_deps.include_modules.is_empty()
        && module_deps.include_headers.is_empty()
        && prologue.is_empty()
        && epilogue.is_empty()
        && prologue_def.is_empty()
        && epilogue_def.is_empty()
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
    // Track every `#include` we emit so the user's prologue doesn't
    // double up when it spells out a header we've already pulled in
    // automatically.
    let mut emitted_includes: std::collections::BTreeSet<String> =
        std::collections::BTreeSet::new();
    let emit_include = |out: &mut String,
                        include_arg: &str,
                        emitted: &mut std::collections::BTreeSet<String>|
     -> std::fmt::Result {
        if emitted.insert(include_arg.to_string()) {
            writeln!(out, "#include {include_arg}")?;
        }
        Ok(())
    };
    writeln!(out, "// @generated by pyxis — do not edit")?;
    writeln!(out, "#pragma once")?;
    writeln!(out)?;
    emit_include(&mut out, "<cstdint>", &mut emitted_includes)?;
    emit_include(&mut out, "<cstddef>", &mut emitted_includes)?;
    emit_include(&mut out, "\"pyxis_runtime.hpp\"", &mut emitted_includes)?;

    // External `#include`s pulled in via #[cpp_header] on extern types.
    if !module_deps.include_headers.is_empty() {
        let mut wrote_section = false;
        for header in &module_deps.include_headers {
            if !emitted_includes.contains(header) {
                if !wrote_section {
                    writeln!(out)?;
                    wrote_section = true;
                }
                emit_include(&mut out, header, &mut emitted_includes)?;
            }
        }
    }

    // Includes for FullDef cross-module deps.
    if !module_deps.include_modules.is_empty() {
        let mut wrote_section = false;
        for dep_module in &module_deps.include_modules {
            let path = format!("\"{}\"", module_to_relative_include(dep_module));
            if !emitted_includes.contains(&path) {
                if !wrote_section {
                    writeln!(out)?;
                    wrote_section = true;
                }
                emit_include(&mut out, &path, &mut emitted_includes)?;
            }
        }
    }

    // Forward declarations for FwdOnly cross-module deps.
    if !module_deps.forward_decls.is_empty() {
        writeln!(out)?;
        for (dep_module, items) in &module_deps.forward_decls {
            open_namespace(&mut out, dep_module)?;
            for item_path in items {
                let line = forward_decl_line(item_path, semantic_state, ctx);
                writeln!(out, "    {line}")?;
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
            // Skip `#include` lines that match an already-emitted
            // include - lets users redundantly spell out a header in
            // their prologue without producing a duplicate `#include`
            // in the output.
            if let Some(arg) = parse_include_arg(line)
                && emitted_includes.contains(arg)
            {
                continue;
            }
            if let Some(arg) = parse_include_arg(line) {
                emitted_includes.insert(arg.to_string());
            }
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
            if !ctx.cfg_passes(&item.cfg) {
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
        let post_header_trimmed = post_header.trim_end_matches('\n');
        if !post_header_trimmed.is_empty() {
            writeln!(out)?;
            for line in post_header_trimmed.lines() {
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

    // Emit a matching .cpp if there are out-of-line definitions to produce:
    // free functions with #[address], extern values, non-template member
    // definitions hoisted out of the header, or a user-supplied
    // `prologue definition` / `epilogue definition` block.
    let needs_cpp = !public_functions.is_empty()
        || !sorted_externs.is_empty()
        || !post_cpp.is_empty()
        || !prologue_def.is_empty()
        || !epilogue_def.is_empty();
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

        // `prologue definition` text lands here, *before* the namespace -
        // analogous to how `prologue` lands before the namespace in the
        // header. Lets the user pull in source-private `#include`s
        // (e.g. `<windows.h>`, `<d3d10.h>`) without leaking them into
        // every .hpp consumer.
        if !prologue_def.is_empty() {
            writeln!(cpp)?;
            for line in prologue_def.lines() {
                if line.is_empty() {
                    writeln!(cpp)?;
                } else {
                    writeln!(cpp, "{line}")?;
                }
            }
        }

        writeln!(cpp)?;
        open_namespace(&mut cpp, key)?;
        let mut wrote_def = false;

        // Collect free-function and extern-value definitions into a
        // buffer so we can trim any trailing blank line before the
        // explicit section separator.
        let mut free_def_buf = String::new();
        for func in &public_functions {
            if let Some(text) = render::render_free_function_definition(func, ctx)? {
                free_def_buf.push_str(&text);
                free_def_buf.push('\n');
            }
        }
        for ev in &sorted_externs {
            let text = render::render_extern_value_definition(ev, ctx)?;
            free_def_buf.push_str(&text);
        }
        let free_def_trimmed = free_def_buf.trim_end_matches('\n');
        if !free_def_trimmed.is_empty() {
            for line in free_def_trimmed.lines() {
                if line.is_empty() {
                    writeln!(cpp)?;
                } else {
                    writeln!(cpp, "    {line}")?;
                }
            }
            wrote_def = true;
        }
        // Each emitted block ends with a trailing blank line (per-method
        // separator). Trim those off before joining sections so we don't
        // stack blanks into double-blanks at the section boundary.
        let post_cpp_trimmed = post_cpp.trim_end_matches('\n');
        if !post_cpp_trimmed.is_empty() {
            if wrote_def {
                writeln!(cpp)?;
            }
            for line in post_cpp_trimmed.lines() {
                if line.is_empty() {
                    writeln!(cpp)?;
                } else {
                    writeln!(cpp, "    {line}")?;
                }
            }
            wrote_def = true;
        }
        let epilogue_def_trimmed = epilogue_def.trim_end_matches('\n');
        if !epilogue_def_trimmed.is_empty() {
            if wrote_def {
                writeln!(cpp)?;
            }
            for line in epilogue_def_trimmed.lines() {
                if line.is_empty() {
                    writeln!(cpp)?;
                } else {
                    writeln!(cpp, "    {line}")?;
                }
            }
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

/// Parse the argument of an `#include` directive. Returns `Some("<...>"
/// or `Some("\"...\"")` for a bracketed or quoted include, or `None`
/// for a non-include line. Whitespace before `#include` and between
/// `#include` and the argument is tolerated.
fn parse_include_arg(line: &str) -> Option<&str> {
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
fn forward_decl_line(
    item_path: &ItemPath,
    semantic_state: &ResolvedSemanticState,
    ctx: render::RenderCtx,
) -> String {
    let leaf = item_path.last().map(|s| s.as_str()).unwrap_or("");
    let registry = semantic_state.type_registry();
    if let Ok(item) = registry.get(item_path, &crate::span::ItemLocation::internal())
        && let Some(resolved) = item.resolved()
    {
        match &resolved.inner {
            ItemDefinitionInner::Type(_) => return format!("struct {leaf};"),
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
        }
    }
    format!("struct {leaf};")
}
