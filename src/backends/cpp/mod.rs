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

use std::path::Path;

use crate::{backends::Result, config::Project, grammar::ItemPath, semantic::SemanticOutput};

mod assemble;
mod cmake;
mod deps;
mod extern_bindings;
mod render;
mod runtime;
mod write;

pub use write::{write_cmake, write_runtime_header};

use extern_bindings::build_cpp_extern_bindings;
use write::write_module;

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
