# Pyxis extension for Zed

Syntax highlighting and language server support for the [Pyxis DSL](https://github.com/ferrobrew/pyxis) in [Zed](https://zed.dev).

## Features

- Syntax highlighting via a tree-sitter grammar
- Language server integration (diagnostics, hover, go-to-definition, completion, formatting, code lens, inlay hints, rename)

## Installation

### Prerequisites

1. Build the Pyxis language server:
   ```sh
   cargo build -p pyxis-lsp --release
   ```
   This produces the `pyxis-lsp` binary. Add it to your `PATH` (or note its full path).

2. Ensure `pyxis-lsp` is on your `PATH` (Zed launches it by name), or set the full path in Zed settings:
   ```json
   {
     "lsp": {
       "pyxis": {
         "binary": {
           "path": "/path/to/pyxis-lsp"
         }
       }
     }
   }
   ```

### Install the extension

1. Clone this repository.
2. In Zed, open the command palette → `zed: install dev extension`.
3. Select the `tooling/zed-pyxis` directory.

Alternatively, symlink `tooling/zed-pyxis` into `~/.local/share/zed/extensions/installed/pyxis`.
