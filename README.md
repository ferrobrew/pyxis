# Pyxis

A DSL for describing types for existing structures and classes in memory.

Write `.pyxis` files describing your structures, and then use the `driver`
to automatically generate Rust code for those structures. Very rudimentary
and primarily for my use only.

## Editor Support

Pyxis has editor support for Zed and VSCode, including syntax highlighting
and a language server (diagnostics, hover, go-to-definition, completion,
formatting, code lens, inlay hints, and rename).

### Building the language server

```sh
cargo build -p pyxis-lsp --release
```

Add the resulting `pyxis-lsp` binary to your `PATH`.

### Zed

Install the extension from `tooling/zed-pyxis/` (see its README for details).
The language server is auto-discovered if `pyxis-lsp` is on your `PATH`.

### VSCode

Install the extension from `tooling/vscode-pyxis/` (see its README for
details). Set `pyxis.languageServerPath` to the binary path if it's not on
your `PATH`.

### Tree-sitter grammar

The grammar lives in `tooling/tree-sitter-pyxis/`. Run `tree-sitter test`
to verify, and `tree-sitter parse <file.pyxis>` to test against real input.