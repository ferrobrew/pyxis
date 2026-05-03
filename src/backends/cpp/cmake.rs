//! CMake / runtime-header generation for the C++ backend.
//!
//! Produces a top-level `CMakeLists.txt` configured for an MSVC-ABI static
//! library, a `cmake-toolchains/xwin-x86.cmake` toolchain file that drives
//! `clang-cl` against an xwin-provisioned MSVC SDK on Linux dev hosts, and
//! `include/pyxis_runtime.hpp` (shared typedefs / utility templates).
//!
//! Phase 0: scaffolding only.
