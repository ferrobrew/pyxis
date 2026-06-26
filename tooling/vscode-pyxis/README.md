# Pyxis extension for VSCode

Syntax highlighting and language server support for the [Pyxis DSL](https://github.com/ferrobrew/pyxis) in [VSCode](https://code.visualstudio.com).

## Features

- Syntax highlighting via a TextMate grammar
- Language server integration (diagnostics, hover, go-to-definition, completion, formatting, code lens, inlay hints, rename)

## Installation

### Prerequisites

1. Build the Pyxis language server:
   ```sh
   cargo build -p pyxis-lsp --release
   ```
   This produces the `pyxis-lsp` binary. Add it to your `PATH` (or note its full path).

2. Set the path in VSCode settings (`Ctrl+,` → search "pyxis"):
   ```json
   {
     "pyxis.languageServerPath": "/path/to/pyxis-lsp"
   }
   ```
   Defaults to `pyxis-lsp` (searches `PATH`).

### Install the extension

1. Install dependencies and build:
   ```sh
   cd tooling/vscode-pyxis
   npm install
   npm run build
   ```

2. Package (optional, for `.vsix`):
   ```sh
   npx vsce package
   ```

3. Install in VSCode:
   - Command Palette → `Extensions: Install from VSIX...` → select the `.vsix`
   - Or for development: copy/symlink `tooling/vscode-pyxis` to `~/.vscode/extensions/pyxis`
