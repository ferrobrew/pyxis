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
    semantic::{
        Module, SemanticOutput, TypeRegistry,
        types::{ExternValue, Function, ItemDefinitionInner},
    },
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
pub fn build(out_dir: &Path, semantic_state: &SemanticOutput, project: &Project) -> Result<()> {
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

/// All four splice payloads pulled from a module's `backend cpp { ... }`
/// blocks, dedented and joined so each field is splice-ready text. Each
/// field is empty if no backend block populated that slot.
#[derive(Default)]
struct CppSplices {
    /// `prologue ...` — lands above the namespace in the `.hpp`.
    prologue: String,
    /// `epilogue ...` — lands at the bottom of the namespace in the `.hpp`.
    epilogue: String,
    /// `prologue definition ...` — lands above the namespace in the `.cpp`.
    prologue_def: String,
    /// `epilogue definition ...` — lands at the bottom of the namespace in the `.cpp`.
    epilogue_def: String,
}

/// Output of rendering every item in a module. `body` / `post_header` /
/// `post_cpp` are unindented raw text; the namespace + indentation
/// passes happen in [`assemble_header`] / [`assemble_source`].
struct ModuleBody<'a> {
    body: String,
    post_header: String,
    post_cpp: String,
    /// True if any item produced any output. Used by the caller to
    /// decide whether the module needs a header at all.
    wrote_anything: bool,
    /// Module-level free functions in deterministic order.
    public_functions: Vec<&'a Function>,
    /// Module-level extern values in deterministic order.
    sorted_externs: Vec<&'a ExternValue>,
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

/// Raw-string prologue/epilogue text usually starts and ends with a
/// newline because users write `r#"<newline>...<newline>"#`. Trim
/// those edges so we don't emit double blank lines around the splice;
/// also dedent each block by its common leading-whitespace prefix so
/// indented raw-string content lands flush at the splice site (the
/// orchestrator reapplies its own indent).
///
/// Multiple splice blocks targeting the same slot are joined with one
/// blank line between each so they render with the same visual rhythm
/// as definitions within a single block.
fn join_slot<'a>(
    bs: &'a [crate::semantic::types::Backend],
    pick: impl Fn(&'a crate::semantic::types::Backend) -> Option<&'a String>,
) -> String {
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

/// Pull all four splice slots out of a module's cpp backend blocks.
fn extract_cpp_splices(cpp_backends: Option<&Vec<crate::semantic::types::Backend>>) -> CppSplices {
    let Some(bs) = cpp_backends else {
        return CppSplices::default();
    };
    CppSplices {
        prologue: join_slot(bs, |b| b.prologue.header.as_ref()),
        epilogue: join_slot(bs, |b| b.epilogue.header.as_ref()),
        prologue_def: join_slot(bs, |b| b.prologue.definition.as_ref()),
        epilogue_def: join_slot(bs, |b| b.epilogue.definition.as_ref()),
    }
}

/// Render every item, free function, and extern value in the module
/// into intermediate buffers. The intra-module FullDef edges are
/// topologically sorted here so by-value references appear after
/// their target's full definition.
fn render_module_body<'a>(
    key: &ItemPath,
    module: &'a Module,
    registry: &TypeRegistry,
    bindings: &std::collections::BTreeMap<ItemPath, CppExternBinding>,
    ctx: render::RenderCtx,
) -> Result<ModuleBody<'a>> {
    let mut body = String::new();
    let mut post_header = String::new();
    let mut post_cpp = String::new();
    let mut wrote_anything = false;

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

    Ok(ModuleBody {
        body,
        post_header,
        post_cpp,
        wrote_anything,
        public_functions,
        sorted_externs,
    })
}

/// Compute the intra-module forward-declaration lines: every non-alias,
/// non-extern resolved item gets a forward decl at the top of the
/// namespace so pointer-typed fields and generic instantiations of
/// pointer-only templates can reference peers defined later in the file.
/// Templates carry a full template-parameter clause on their forward
/// decl. The returned vec is sorted + deduplicated.
fn intra_module_forward_decls(
    module: &Module,
    registry: &TypeRegistry,
    ctx: render::RenderCtx,
) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
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
        let Some(leaf) = item.path.last() else {
            continue;
        };
        let leaf = render::cpp_ident(leaf.as_str());
        // Enums/bitflags are forward-declared with their underlying type
        // (a scoped enum so declared is a complete type, usable by value),
        // so a struct method signature can name an enum defined later in
        // the file. Type aliases can't be forward-declared.
        let line = match &resolved.inner {
            ItemDefinitionInner::Type(_) if item.is_generic() => {
                let params = item
                    .type_parameters
                    .iter()
                    .map(|p| format!("class {p}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("template <{params}> struct {leaf};")
            }
            ItemDefinitionInner::Type(_) => format!("struct {leaf};"),
            ItemDefinitionInner::Enum(ed) => {
                let underlying = render::render_type(&ed.type_, ctx)
                    .unwrap_or_else(|_| "::std::int32_t".to_string());
                format!("enum class {leaf} : {underlying};")
            }
            ItemDefinitionInner::Bitflags(bd) => {
                let underlying = render::render_type(&bd.type_, ctx)
                    .unwrap_or_else(|_| "::std::int32_t".to_string());
                format!("enum class {leaf} : {underlying};")
            }
            ItemDefinitionInner::TypeAlias(_) => continue,
        };
        out.push(line);
    }
    out.sort();
    out.dedup();
    out
}

/// Append `#include <header>` (or `"header"`) to `out` if it hasn't
/// already been emitted; track seen includes in `emitted`. The arg is
/// stored verbatim so callers control angle-bracket vs quote form.
fn emit_include(
    out: &mut String,
    include_arg: &str,
    emitted: &mut std::collections::BTreeSet<String>,
) -> std::fmt::Result {
    if emitted.insert(include_arg.to_string()) {
        writeln!(out, "#include {include_arg}")?;
    }
    Ok(())
}

/// Assemble the full `.hpp` text: pragma + automatic includes +
/// cross-module includes/forward decls + module prologue + namespace
/// block containing intra-module forward decls, every item body, any
/// out-of-class definitions that must stay header-visible, and the
/// user's epilogue.
#[allow(clippy::too_many_arguments)]
fn assemble_header(
    key: &ItemPath,
    semantic_state: &SemanticOutput,
    module: &Module,
    registry: &TypeRegistry,
    ctx: render::RenderCtx,
    module_deps: &deps::ModuleDeps,
    body: &str,
    post_header: &str,
    splices: &CppSplices,
) -> Result<String> {
    let mut out = String::new();
    // Track every `#include` we emit so the user's prologue doesn't
    // double up when it spells out a header we've already pulled in
    // automatically.
    let mut emitted_includes: std::collections::BTreeSet<String> =
        std::collections::BTreeSet::new();
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
    if !splices.prologue.is_empty() {
        writeln!(out)?;
        for line in splices.prologue.lines() {
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

    let has_namespace_body =
        !body.is_empty() || !post_header.is_empty() || !splices.epilogue.is_empty();
    if has_namespace_body {
        writeln!(out)?;
        open_namespace(&mut out, key)?;
        let intra_fwd = intra_module_forward_decls(module, registry, ctx);
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
        if !splices.epilogue.is_empty() {
            writeln!(out)?;
            for line in splices.epilogue.lines() {
                if line.is_empty() {
                    writeln!(out)?;
                } else {
                    writeln!(out, "    {line}")?;
                }
            }
        }
        close_namespace(&mut out, key)?;
    }
    Ok(out)
}

/// Assemble the full `.cpp` text: header include + optional source-
/// private prologue + namespace block containing free-function
/// definitions, hoisted out-of-class member definitions, and the
/// user's source-side epilogue.
fn assemble_source(
    key: &ItemPath,
    ctx: render::RenderCtx,
    body: &ModuleBody<'_>,
    splices: &CppSplices,
    module_deps: &deps::ModuleDeps,
) -> Result<String> {
    let mut cpp = String::new();
    writeln!(cpp, "// @generated by pyxis — do not edit")?;
    writeln!(cpp)?;
    let header_include = module_to_relative_include(key);
    writeln!(cpp, "#include \"{header_include}\"")?;

    // The header only forward-declares its FwdOnly deps (pointer / function
    // signature uses). A function *defined* here may use such a type by
    // value (e.g. a by-value parameter or return), which needs the complete
    // type — so pull in those headers in the .cpp. A .cpp is a leaf, so the
    // extra includes can't create a cycle the way they could in the header.
    let mut extra_includes: Vec<String> = module_deps
        .forward_decls
        .keys()
        .map(module_to_relative_include)
        .filter(|inc| inc != &header_include)
        .collect();
    extra_includes.sort();
    extra_includes.dedup();
    for inc in extra_includes {
        writeln!(cpp, "#include \"{inc}\"")?;
    }

    // `prologue definition` text lands here, *before* the namespace -
    // analogous to how `prologue` lands before the namespace in the
    // header. Lets the user pull in source-private `#include`s
    // (e.g. `<windows.h>`, `<d3d10.h>`) without leaking them into
    // every .hpp consumer.
    if !splices.prologue_def.is_empty() {
        writeln!(cpp)?;
        for line in splices.prologue_def.lines() {
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
    for func in &body.public_functions {
        if let Some(text) = render::render_free_function_definition(func, ctx)? {
            free_def_buf.push_str(&text);
            free_def_buf.push('\n');
        }
    }
    for ev in &body.sorted_externs {
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
    let post_cpp_trimmed = body.post_cpp.trim_end_matches('\n');
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
    let epilogue_def_trimmed = splices.epilogue_def.trim_end_matches('\n');
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
    Ok(cpp)
}

/// Emit `<out_dir>/include/<module>/...hpp` (and a matching `.cpp` if there
/// are address-bound free functions or extern values to define) for a single
/// module. Orchestrator: pulls together splices, renders items, and writes
/// the header and (optionally) the source file.
fn write_module(
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
    let ctx = render::RenderCtx::new(key, registry, bindings, cfg_ctx);
    let module_deps = deps::collect_module_deps(key, module, registry, bindings);
    let splices = extract_cpp_splices(module.backends.get(&crate::Backend::Cpp));
    let body = render_module_body(key, module, registry, bindings, ctx)?;

    // Skip writing anything if the module contributes no declarations,
    // no cross-module deps, and no user-supplied splices.
    if !body.wrote_anything
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
        &body.post_header,
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
    let needs_cpp = !body.public_functions.is_empty()
        || !body.sorted_externs.is_empty()
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
pub fn write_cmake(out_dir: &Path, project: &Project) -> Result<()> {
    cmake::write_cmake(out_dir, project)
}

fn module_to_header_path(out_dir: &Path, module_path: &ItemPath) -> PathBuf {
    module_to_emitted_path(out_dir, module_path, "include", "hpp")
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
        write!(out, "{}", render::cpp_namespace_ident(seg.as_str()))?;
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
        write!(out, "{}", render::cpp_namespace_ident(seg.as_str()))?;
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
