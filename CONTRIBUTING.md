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
  MSVC-ABI consumer (real Windows + MSVC, or `clang-cl`). See
  [`docs/cpp_backend.md`](docs/cpp_backend.md) for the design
  decisions and ABI tradeoffs.

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

## Changing the language

When you change the language (new attributes, syntax, types, etc.), you
must audit every surface that consumes it — not just the compiler. Most
surfaces handle attributes generically and need no changes, but you must
verify each one. Here is the full checklist:

| Surface | Path | When it needs updating |
|---------|------|----------------------|
| **Compiler frontend** | `src/` | Always. Parser, semantic IR, and all backends (Rust/C++/JSON). |
| **Parser test helpers** | `src/parser/attributes.rs` | New `Attribute` variants or test constructors (e.g. `Attribute::pinned()`). The grammar parses `#[ident]` attributes generically, so simple ident attributes need no grammar change. |
| **`AttributeName` enum** | `src/semantic/error.rs` | Only if the new attribute participates in conflicting-attribute validation (e.g. `#[packed]` + `#[align]`). Most attributes don't need an entry. |
| **Tree-sitter grammar** | `tooling/tree-sitter-pyxis/grammar.js` | Only for new syntax forms or keywords. Ident attributes (`#[foo]`) are already parsed generically as `attribute_ident → $.identifier`. |
| **Highlights query** | `tooling/tree-sitter-pyxis/queries/highlights.scm` | Rarely. Attributes are highlighted generically via `(attribute) @attribute`. Only change if a new node type needs a capture. |
| **Zed extension** | `tooling/zed-pyxis/` | Rarely. It just consumes the grammar and highlights query. No attribute-specific logic. |
| **LSP hover** | `tooling/lsp/src/handlers/hover_format.rs` | New attributes need a description string in the `attribute_description()` match table so hovering shows documentation. |
| **LSP completion** | `tooling/lsp/src/handlers/completion.rs` | Only if adding new keywords (not attributes). The completion handler lists keyword tokens, not attribute names. |
| **JSON types** | `types/json.ts` | After changing JSON backend structs (which derive `specta::Type`). Regenerate with `cargo run -p pyxis-driver -- gen-types`. |
| **Viewer** | `viewer/src/components/Attributes.tsx` | New attributes need a badge entry in `ItemAttributes` to be visible in the docs viewer. |
| **Codegen test corpus** | `codegen_tests/input/`, `codegen_tests/output/` | Add a test input exercising the new feature, then regenerate output with `cargo run --example codegen_tests`. |
| **C++ backend docs** | `docs/cpp_backend.md` | If the attribute affects C++ codegen (most don't — `copyable`/`cloneable` are Rust-only). |
| **Pretty-printer** | `src/pretty_print.rs` | Usually no change needed (attributes print generically). Add a round-trip test to confirm. |

## Tests

```sh
python test.py
```

runs the full test suite: clippy, fmt, the parser/semantic unit tests,
`cargo run --example codegen_tests` (which emits the test corpus
through every backend and rebuilds the emitted output), and
`cargo doc --no-deps -p codegen_tests` (which catches unresolved doc
link references in the generated Rust output). The cpp test
corpus uses a regular host C++17 compiler (no MSVC ABI required) so
CI doesn't need xwin.

Formatting relies on a nightly-only rustfmt feature (`imports_granularity`,
configured in `rustfmt.toml`), so check it with nightly:

```sh
cargo +nightly fmt --all -- --check
```

## Editor tooling

Pyxis ships a tree-sitter grammar, a Zed extension, and a language
server. All live under `tooling/`.

The tree-sitter grammar lives in its own repository,
[`ferrobrew/tree-sitter-pyxis`](https://github.com/ferrobrew/tree-sitter-pyxis),
and is vendored here as a git **submodule** at `tooling/tree-sitter-pyxis`.
Clone with `git clone --recurse-submodules`, or in an existing checkout run:

```sh
git submodule update --init
```

To change the grammar: edit it in `tooling/tree-sitter-pyxis`, then run

```sh
python tooling/sync-grammar.py -m "Describe the grammar change"
```

which regenerates the parser, runs the grammar tests, commits and pushes
to the grammar repo's `main`, and re-pins **both** the submodule and
`tooling/zed-pyxis/extension.toml` at the resulting commit SHA. It stages
those two bumps in this repo and leaves the commit to you (pass
`--commit-parent` to commit them too). Run it with no `-m` any time to
re-pin against the submodule's current HEAD.

Two invariants the script maintains, which you must preserve if you ever
touch this by hand:

- `extension.toml` pins the grammar by full commit SHA, never a branch
  ref — Zed caches its compiled grammar by that string and won't
  re-resolve a branch.
- The parser is generated with `--abi 14` (wired into the grammar's
  `npm run generate`), the ABI Zed's bundled tree-sitter runtime loads.

After syncing, reinstall the Zed dev extension to pick up the new grammar.

### Architecture

The compiler uses a [Salsa](https://github.com/salsa-rs/salsa)-backed query
graph (`src/salsa/`). Both the batch compilation pipeline (`build_with_store_and_options`)
and the LSP server call the same Salsa queries — there is no separate
"imperative pipeline" and "LSP pipeline."

- `src/salsa/` — Salsa database, inputs, IR, and tracked functions
- `tooling/tree-sitter-pyxis/` — tree-sitter grammar for syntax highlighting (a submodule → [`ferrobrew/tree-sitter-pyxis`](https://github.com/ferrobrew/tree-sitter-pyxis))
- `tooling/zed-pyxis/` — Zed extension
- `tooling/lsp/` — LSP server binary (`pyxis-lsp`)

### Running the LSP

```sh
cargo build -p pyxis-lsp --release
```

The `pyxis-lsp` binary communicates over stdio. The Zed extension spawns it
automatically (see its README for installation instructions).
