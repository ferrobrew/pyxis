//! C++ backend.
//!
//! Emits one `.hpp` + one `.cpp` per pyxis module, plus a top-level
//! `CMakeLists.txt`, `pyxis_runtime.hpp`, and `cmake-toolchains/xwin-x86.cmake`
//! so the project compiles into a static library targeting MSVC ABI.
//!
//! Phase 0: scaffolding only — entry points exist but produce nothing.

use std::path::Path;

use crate::{
    backends::Result,
    config::Project,
    grammar::ItemPath,
    semantic::{Module, ResolvedSemanticState},
};

mod cmake;
mod deps;
mod render;

/// Emit `<out_dir>/include/<module>/...hpp` + `<out_dir>/src/<module>/...cpp`
/// for a single module.
pub fn write_module(
    _out_dir: &Path,
    _key: &ItemPath,
    _semantic_state: &ResolvedSemanticState,
    _module: &Module,
) -> Result<()> {
    // TODO(phase 1): emit headers/sources.
    Ok(())
}

/// Emit `<out_dir>/include/pyxis_runtime.hpp` — shared typedefs / utility
/// templates used by every generated module.
pub fn write_runtime_header(_out_dir: &Path) -> Result<()> {
    // TODO(phase 2/3): emit runtime header.
    Ok(())
}

/// Emit `<out_dir>/CMakeLists.txt` and `<out_dir>/cmake-toolchains/xwin-x86.cmake`.
pub fn write_cmake(_out_dir: &Path, _project: &Project) -> Result<()> {
    // TODO(phase 4): emit CMake glue.
    Ok(())
}
