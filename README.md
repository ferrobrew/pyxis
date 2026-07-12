# Pyxis

Pyxis is a domain-specific language for describing types and structures that already exist in a binary's memory. You write `.pyxis` files describing the layouts, vtables, and functions of an application's data structures, and the driver generates Rust, JSON, or C++ output from those descriptions.

This is useful for game modding, reverse engineering, and interop tooling - anywhere you need a structured, machine-readable description of types that live inside a compiled binary.

## Install

```sh
cargo install pyxis-driver
```

Or build from source:

```sh
cargo build -p pyxis-driver --release
```

The binary is `pyxis`.

## Quickstart

Create a project directory with a `pyxis.toml`:

```toml
[project]
name = "my_project"
pointer_size = 4
```

Write a `.pyxis` file:

```pyxis
// types.pyxis
#[size(0x18)]
pub type Player {
    vftable {
        pub fn get_name(&self) -> *const u8;
        pub fn get_health(&self) -> i32;
    },

    pub name: *const u8,
    pub health: i32,
    pub position: [f32; 3],
}

impl Player {
    #[address(0x401000)]
    pub fn heal(&mut self, amount: i32);
}
```

Build it:

```sh
pyxis build . --backend rust out/
```

The generated Rust code lands in `out/lib.rs` (and submodules for nested directories).

For real projects, the primary usage pattern is embedding the compiler in a `build.rs` script so the generated code is produced at build time. See the [Rust backend docs](docs/rust_backend.md) for details.

## Backends

Pyxis emits three target formats from the same semantic IR:

- **Rust** (default) - one `.rs` file per module. See [Rust backend docs](docs/rust_backend.md).
- **JSON** - a single JSON document for tooling that consumes the semantic IR directly. See [JSON backend docs](docs/json_backend.md).
- **C++** - `.hpp`/`.cpp` per module plus a normative `CMakeLists.txt`. Targets the MSVC ABI. See [C++ backend docs](docs/cpp_backend.md).

```sh
# Rust (default)
pyxis build . --backend rust out/

# JSON
pyxis build . --backend json out/

# C++
pyxis build . --backend cpp out/
```

## Checking definitions

You can check that your `.pyxis` definitions are valid - correct syntax, resolved types, passing validation - without generating any output:

```sh
# Check definitions for errors without generating output
pyxis check .
```

This runs the full semantic analysis pipeline and reports all parse and semantic errors it finds.

## Language reference

The full language reference covers every syntax form, type, attribute, cfg predicate, backend splice, module construct, and convention:

→ [docs/language.md](docs/language.md)

## pyxis-defs

[`pyxis-defs`](https://github.com/ferrobrew/pyxis-defs) is the canonical real-world Pyxis project and primary consumer. It contains memory-structure definitions for real applications (Just Cause 2, and others), with a build harness that compile-checks generated Rust against the matching Windows targets.

If you're learning Pyxis, reading the pyxis-defs source is the best way to see how the language is used in practice.

## Editor support

Pyxis has editor support for Zed, including syntax highlighting and a language server (diagnostics, hover, go-to-definition, completion, formatting, code lens, inlay hints, and rename).

### Building the language server

```sh
cargo build -p pyxis-lsp --release
```

Add the resulting `pyxis-lsp` binary to your `PATH`.

### Zed

Install the extension from `tooling/zed-pyxis/` (see its README for details). The language server is auto-discovered if `pyxis-lsp` is on your `PATH`.

### Tree-sitter grammar

The grammar lives in `tooling/tree-sitter-pyxis/` (a git submodule → [`ferrobrew/tree-sitter-pyxis`](https://github.com/ferrobrew/tree-sitter-pyxis)). Run `tree-sitter test` to verify, and `tree-sitter parse <file.pyxis>` to test against real input.
