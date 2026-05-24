# Toolchain for building pyxis-emitted MSVC-ABI code on Linux dev hosts via
# clang-cl + xwin-provisioned MSVC headers/libs. Not part of pyxis codegen
# output — checked into the pyxis repo as a dev-loop convenience.
#
# Prerequisites:
#   1. xwin (https://github.com/Jake-Shadle/xwin):
#        xwin --arch x86,x86_64 --accept-license splat --output ~/.xwin
#   2. clang-cl, lld-link, llvm-lib, llvm-rc from a recent LLVM (>= 16).
#
# Usage:
#   cmake -S . -B build \
#     -DCMAKE_TOOLCHAIN_FILE=<pyxis-repo>/tools/cmake-toolchains/xwin-x86.cmake \
#     -DXWIN_ROOT=$HOME/.xwin
#   cmake --build build -j

set(CMAKE_SYSTEM_NAME Windows)
set(CMAKE_SYSTEM_PROCESSOR x86)

# Resolve XWIN_ROOT from -D, the cache, or the env (in that order). Cache it
# so try_compile() sub-projects (which re-include this file in a fresh CMake
# context without command-line vars) still see it. Forward it via
# CMAKE_TRY_COMPILE_PLATFORM_VARIABLES too as a belt-and-braces measure.
if(NOT DEFINED XWIN_ROOT AND DEFINED ENV{XWIN_ROOT})
    set(XWIN_ROOT $ENV{XWIN_ROOT})
endif()
if(NOT DEFINED XWIN_ROOT)
    message(FATAL_ERROR "XWIN_ROOT not set. Pass -DXWIN_ROOT=<path> or export XWIN_ROOT=<path>; see this file's header for setup.")
endif()
set(XWIN_ROOT "${XWIN_ROOT}" CACHE PATH "xwin splat root" FORCE)
list(APPEND CMAKE_TRY_COMPILE_PLATFORM_VARIABLES XWIN_ROOT)

# Static-library try_compile target type avoids manifest/RC machinery during
# CMake's compiler-validation phase, which tends to invoke a host preprocessor
# and trip nix-wrapped-cc heuristics with -fPIC injection on Linux.
set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)

# xwin's default splat ships only the release MSVC runtime (`msvcrt.lib`),
# not the debug variant (`msvcrtd.lib`). Pin every config to the release
# DLL CRT so Debug builds don't try to link against a lib that isn't there.
set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreadedDLL" CACHE STRING "" FORCE)

set(CMAKE_C_COMPILER clang-cl)
set(CMAKE_CXX_COMPILER clang-cl)
set(CMAKE_LINKER lld-link)
set(CMAKE_AR llvm-lib)
# Use a no-op RC compiler. CMake on MSVC-like platforms threads `llvm-rc`
# into the `vs_link_exe` wrapper to embed a Win32 manifest in executables;
# `llvm-rc` then preprocesses through `cc`, which on Nix resolves to the
# wrapped gcc and trips `-fPIC` injection. The cross-compile target doesn't
# need the manifest, so swap RC for a stub that just creates the .res file.
get_filename_component(_pyxis_toolchain_dir "${CMAKE_CURRENT_LIST_FILE}" DIRECTORY)
set(CMAKE_RC_COMPILER "${_pyxis_toolchain_dir}/no-op-rc.sh" CACHE FILEPATH "RC compiler" FORCE)

set(CLANG_TARGET "--target=i686-pc-windows-msvc")
set(_XWIN_INC "/imsvc;${XWIN_ROOT}/crt/include;/imsvc;${XWIN_ROOT}/sdk/include/ucrt;/imsvc;${XWIN_ROOT}/sdk/include/um;/imsvc;${XWIN_ROOT}/sdk/include/shared")
string(REPLACE ";" " " _XWIN_INC_FLAGS "${_XWIN_INC}")

set(CMAKE_C_FLAGS_INIT "${CLANG_TARGET} ${_XWIN_INC_FLAGS}")
set(CMAKE_CXX_FLAGS_INIT "${CLANG_TARGET} ${_XWIN_INC_FLAGS}")

set(_XWIN_LIB "/libpath:${XWIN_ROOT}/crt/lib/x86 /libpath:${XWIN_ROOT}/sdk/lib/ucrt/x86 /libpath:${XWIN_ROOT}/sdk/lib/um/x86")
set(CMAKE_EXE_LINKER_FLAGS_INIT "${_XWIN_LIB}")
set(CMAKE_STATIC_LINKER_FLAGS_INIT "${_XWIN_LIB}")
set(CMAKE_MODULE_LINKER_FLAGS_INIT "${_XWIN_LIB}")
set(CMAKE_SHARED_LINKER_FLAGS_INIT "${_XWIN_LIB}")

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)

