# Contributing to pyxis

## Backends

Pyxis emits one of three target languages:

- `rust` (default) — `.rs` files inside a Cargo crate.
- `json` — a single JSON document for tooling that wants to consume the
  semantic IR directly.
- `cpp` — `.hpp` + `.cpp` per module plus a normative `CMakeLists.txt`,
  for C++ tooling that wants to link a static library. Targets the MSVC
  ABI directly (composition-only structs, `__thiscall`/`__stdcall`/etc.
  on `#[address]`-bound functions); generated code is portable to any
  MSVC-ABI consumer (real Windows + MSVC, or `clang-cl`).

```sh
cargo run -p pyxis-driver -- build --backend cpp <input-dir> <output-dir>
```

## Building emitted C++ on Linux dev hosts

The cpp output is normative — it does not bake xwin or clang-cl
assumptions into `CMakeLists.txt`. To build it on Linux, point CMake at
the dev-only toolchain in `tools/cmake-toolchains/xwin-x86.cmake`, which
wires `clang-cl` against the MSVC SDK provisioned by
[`xwin`](https://github.com/Jake-Shadle/xwin).

One-time setup:

```sh
# 1. Get xwin (cargo install xwin) and pull the SDK. Both x86 and x86_64
#    are needed if you target both; pyxis's emitted cpp is 32-bit by
#    default.
xwin --arch x86,x86_64 --accept-license splat --output ~/.xwin

# 2. Install a recent LLVM. Anything >= 16 should work; the toolchain
#    needs clang-cl, lld-link, llvm-lib, and llvm-rc on PATH.
#    On Nix, `nix-shell -p llvmPackages_21.clang` is one way.
```

Per-build:

```sh
cmake -S <emitted-cpp-tree> -B <build-dir> \
  -DCMAKE_TOOLCHAIN_FILE=<pyxis-repo>/tools/cmake-toolchains/xwin-x86.cmake \
  -DXWIN_ROOT=$HOME/.xwin
cmake --build <build-dir> -j
```

The toolchain pins `MultiThreadedDLL` (release CRT) for every config —
xwin doesn't ship `msvcrtd.lib`, so Debug builds can't link the debug
CRT. Treat `Debug` and `Release` as differing only in optimization /
debug-info, not CRT.

On native Windows, no toolchain file is needed — point CMake at a
regular MSVC install or `clang-cl` and build normally.

## Tests

```sh
python test.py
```

runs the full test suite: clippy, fmt, the parser/semantic unit tests,
and `cargo run --example codegen_tests`, which emits the test corpus
through every backend and rebuilds the emitted output. The cpp test
corpus uses a regular host C++17 compiler (no MSVC ABI required) so
CI doesn't need xwin.
