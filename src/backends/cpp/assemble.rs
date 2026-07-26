//! Assembly of a module's rendered item bodies into full `.hpp`/`.cpp` text:
//! splice extraction, item-body concatenation, forward decls, re-export
//! aliases, and the include/namespace scaffolding around them.

use std::fmt::Write as _;

use crate::{
    backends::{
        Result,
        cpp::{deps, render},
    },
    grammar::ItemPath,
    semantic::{
        Module, SemanticOutput, TypeRegistry,
        types::{Function, ItemDefinitionInner},
    },
};

use super::extern_bindings::CppExternBinding;

/// All four splice payloads pulled from a module's `backend cpp { ... }`
/// blocks, dedented and joined so each field is splice-ready text. Each
/// field is empty if no backend block populated that slot.
#[derive(Default)]
pub(super) struct CppSplices {
    /// `prologue ...` — lands above the namespace in the `.hpp`.
    pub(super) prologue: String,
    /// `epilogue ...` — lands at the bottom of the namespace in the `.hpp`.
    pub(super) epilogue: String,
    /// `prologue definition ...` — lands above the namespace in the `.cpp`.
    pub(super) prologue_def: String,
    /// `epilogue definition ...` — lands at the bottom of the namespace in the `.cpp`.
    pub(super) epilogue_def: String,
}

/// Output of rendering every item in a module. `body` / `post_cpp`
/// are unindented raw text; the namespace + indentation passes happen
/// in [`assemble_header`] / [`assemble_source`].
pub(super) struct ModuleBody<'a> {
    pub(super) body: String,
    pub(super) post_cpp: String,
    /// True if any item produced any output. Used by the caller to
    /// decide whether the module needs a header at all.
    pub(super) wrote_anything: bool,
    /// Module-level free functions in deterministic order.
    pub(super) public_functions: Vec<&'a Function>,
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
fn join_slot(splices: &[&crate::semantic::types::Splice]) -> String {
    splices
        .iter()
        .map(|s| {
            let dedented = dedent(s.text.trim_matches('\n'));
            dedented.trim().to_string()
        })
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join("\n\n")
}

/// Pull all four splice slots (header/source × prologue/epilogue) out of a
/// module's cpp-active splices, in source order.
pub(super) fn extract_cpp_splices(module: &crate::semantic::Module) -> CppSplices {
    use crate::grammar::SpliceKind;
    let active: Vec<&crate::semantic::types::Splice> =
        module.splices_for(crate::Backend::Cpp).collect();
    let slot = |kind: SpliceKind, definition: bool| {
        let picked: Vec<_> = active
            .iter()
            .copied()
            .filter(|s| s.kind == kind && s.definition == definition)
            .collect();
        join_slot(&picked)
    };
    CppSplices {
        prologue: slot(SpliceKind::Prologue, false),
        epilogue: slot(SpliceKind::Epilogue, false),
        prologue_def: slot(SpliceKind::Prologue, true),
        epilogue_def: slot(SpliceKind::Epilogue, true),
    }
}

/// Render every item, free function, and extern value in the module
/// into intermediate buffers. The intra-module FullDef edges are
/// topologically sorted here so by-value references appear after
/// their target's full definition.
pub(super) fn render_module_body<'a>(
    key: &ItemPath,
    module: &'a Module,
    registry: &TypeRegistry,
    bindings: &std::collections::BTreeMap<ItemPath, CppExternBinding>,
    ctx: render::RenderCtx,
) -> Result<ModuleBody<'a>> {
    let mut body = String::new();
    let mut post_cpp = String::new();
    let mut wrote_anything = false;

    let raw_items: Vec<_> = module
        .definitions(registry)
        .filter(|item| ctx.cfg_passes(&item.cfg))
        .filter(|item| {
            // Skip nested items (whose parent is a type, not a module).
            // They are rendered inside their parent type's struct body.
            item.path.parent().is_some_and(|parent| &parent == key)
        })
        .collect();
    let sorted_items = deps::topo_sort_module_items(key, raw_items, registry, bindings)?;
    // Standalone module-level constants: collect here (before the consuming
    // `for` loop) and emit as a single contiguous block after all types and
    // free functions. The parent==key check is defensive — `sorted_items`
    // already only contains module-direct items (filtered at collection
    // time above) — but makes the intent explicit. `render_const` returns
    // empty `post_header`/`post_cpp`, so the contiguous const block only
    // needs `decl` — no out-of-line definitions to carry over.
    let standalone_consts: Vec<_> = sorted_items
        .iter()
        .filter(|item| {
            item.resolved()
                .is_some_and(|r| matches!(r.inner, ItemDefinitionInner::Constant(_)))
                && item.path.parent().is_some_and(|parent| &parent == key)
        })
        .copied()
        .collect();
    for item in sorted_items {
        let is_standalone_const = item
            .resolved()
            .is_some_and(|r| matches!(r.inner, ItemDefinitionInner::Constant(_)))
            && item.path.parent().is_some_and(|parent| &parent == key);
        if is_standalone_const {
            continue;
        }
        let Some(rendered) = render::render_item(item, ctx)? else {
            continue;
        };
        // Emit post_header (out-of-class definitions like deferred constants
        // and template method definitions) immediately after the decl so they
        // stay adjacent to their type, rather than being deferred to a
        // separate block at the end of the namespace.
        let combined = if rendered.post_header.is_empty() {
            rendered.decl
        } else {
            let mut s = rendered.decl;
            if !s.ends_with('\n') {
                s.push('\n');
            }
            s.push_str(&rendered.post_header);
            s
        };
        if !combined.is_empty() {
            if wrote_anything {
                writeln!(body)?;
            }
            body.push_str(&combined);
            wrote_anything = true;
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

    // Emit standalone module-level constants as a single contiguous block
    // (no blank lines between them) after all types and free functions.
    if !standalone_consts.is_empty() {
        if wrote_anything {
            writeln!(body)?;
        }
        for item in standalone_consts {
            let Some(rendered) = render::render_item(item, ctx)? else {
                continue;
            };
            body.push_str(&rendered.decl);
        }
        wrote_anything = true;
    }

    Ok(ModuleBody {
        body,
        post_cpp,
        wrote_anything,
        public_functions,
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
    let module_path = &module.path;
    for item in module.definitions(registry) {
        if item.is_predefined()
            || matches!(item.category, crate::semantic::types::ItemCategory::Extern)
        {
            continue;
        }
        // Skip nested items — their parent is a type, not this module.
        if item
            .path
            .parent()
            .is_some_and(|parent| &parent != module_path)
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
            ItemDefinitionInner::Union(_) => format!("union {leaf};"),
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
            ItemDefinitionInner::Constant(_) => continue,
            ItemDefinitionInner::ExternValue(_) => continue,
        };
        out.push(line);
    }
    out.sort();
    out.dedup();
    out
}

/// Render `using` aliases for a module's `pub use` re-exports. Each alias
/// makes `<module>::<local_name>` a valid C++ name for the re-export's
/// canonical target, mirroring the pyxis-level re-export. Targets that no
/// longer resolve (e.g. dangling paths) are skipped.
fn render_reexport_aliases(
    module: &Module,
    registry: &TypeRegistry,
    ctx: render::RenderCtx,
) -> Result<Vec<String>> {
    use crate::semantic::types::Type;
    // Force fully-qualified target names: a same-module bare-name render
    // could produce a degenerate `using Bar = Bar;`, and member-shadow
    // rewriting must never touch the alias's right-hand side.
    let empty = ItemPath::empty();
    let qualified_ctx = render::RenderCtx {
        module_path: &empty,
        shadowed_members: None,
        ..ctx
    };
    let mut aliases = Vec::new();
    for (local_name, target) in module.reexports() {
        let canonical = registry.canonicalize(&target);
        if !registry.contains(&canonical) {
            continue;
        }
        let rendered = render::render_type(&Type::Raw(canonical), qualified_ctx)?;
        let name = render::cpp_ident(&local_name);
        aliases.push(format!("using {name} = {rendered};"));
    }
    Ok(aliases)
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
/// block containing intra-module forward decls, every item body, and
/// the user's epilogue.
#[allow(clippy::too_many_arguments)]
pub(super) fn assemble_header(
    key: &ItemPath,
    semantic_state: &SemanticOutput,
    module: &Module,
    registry: &TypeRegistry,
    ctx: render::RenderCtx,
    module_deps: &deps::ModuleDeps,
    body: &str,
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
            let path = format!(
                "\"{}\"",
                super::write::module_to_relative_include(dep_module)
            );
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
            super::write::open_namespace(&mut out, dep_module)?;
            for item_path in items {
                let line = super::write::forward_decl_line(item_path, semantic_state, ctx);
                writeln!(out, "    {line}")?;
            }
            super::write::close_namespace(&mut out, dep_module)?;
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
            if let Some(arg) = super::write::parse_include_arg(line)
                && emitted_includes.contains(arg)
            {
                continue;
            }
            if let Some(arg) = super::write::parse_include_arg(line) {
                emitted_includes.insert(arg.to_string());
            }
            writeln!(out, "{line}")?;
        }
    }

    let reexport_aliases = render_reexport_aliases(module, registry, ctx)?;
    let has_namespace_body =
        !body.is_empty() || !reexport_aliases.is_empty() || !splices.epilogue.is_empty();
    if has_namespace_body {
        writeln!(out)?;
        super::write::open_namespace(&mut out, key)?;
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
        // Re-export `using` aliases land after every same-module type is
        // fully declared, so an alias whose target lives in this module
        // refers to an already-declared name.
        if !reexport_aliases.is_empty() {
            writeln!(out)?;
            for line in &reexport_aliases {
                writeln!(out, "    {line}")?;
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
        super::write::close_namespace(&mut out, key)?;
    }
    Ok(out)
}

/// Assemble the full `.cpp` text: header include + optional source-
/// private prologue + namespace block containing free-function
/// definitions, hoisted out-of-class member definitions, and the
/// user's source-side epilogue.
pub(super) fn assemble_source(
    key: &ItemPath,
    ctx: render::RenderCtx,
    body: &ModuleBody<'_>,
    splices: &CppSplices,
    module_deps: &deps::ModuleDeps,
) -> Result<String> {
    let mut cpp = String::new();
    writeln!(cpp, "// @generated by pyxis — do not edit")?;
    writeln!(cpp)?;
    let header_include = super::write::module_to_relative_include(key);
    writeln!(cpp, "#include \"{header_include}\"")?;

    // The header only forward-declares its FwdOnly deps (pointer / function
    // signature uses). A function *defined* here may use such a type by
    // value (e.g. a by-value parameter or return), which needs the complete
    // type — so pull in those headers in the .cpp. A .cpp is a leaf, so the
    // extra includes can't create a cycle the way they could in the header.
    let mut extra_includes: Vec<String> = module_deps
        .forward_decls
        .keys()
        .map(super::write::module_to_relative_include)
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
    super::write::open_namespace(&mut cpp, key)?;
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
    super::write::close_namespace(&mut cpp, key)?;
    Ok(cpp)
}
