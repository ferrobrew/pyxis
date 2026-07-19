# JSON backend

The JSON backend emits a single `output.json` file containing the full semantic IR as structured data. It's designed for tooling that wants to consume the type system directly - documentation viewers, code generators, analysis tools - without parsing `.pyxis` source or linking against the Rust compiler crate.

## Output shape

The output is a `JsonDocumentation` struct serialized to JSON:

```json
{
  "schema_version": 11,
  "pyxis_version": "0.1.0",
  "pointer_size": 4,
  "project_name": "my_project",
  "items": { ... },
  "modules": { ... },
  "source_paths": [ ... ]
}
```

| Field | Type | Description |
|----|---|-------|
| `schema_version` | `u32` | Schema version (see below). Missing in pre-v2 documents; consumers should treat a missing value as 1. |
| `pyxis_version` | `string` | Version of the Pyxis compiler that generated this document (from `CARGO_PKG_VERSION`). |
| `pointer_size` | `usize` | Pointer size for the target platform (4 or 8). |
| `project_name` | `string` | Project name from `pyxis.toml`. |
| `items` | `map<string, JsonItem>` | Map of absolute item paths to item definitions. |
| `modules` | `map<string, JsonModule>` | Nested module hierarchy. |
| `source_paths` | `array<string>` | Source file paths indexed by file ID. |

## Schema versioning

The current schema version is **11** (`CURRENT_SCHEMA_VERSION` in `src/backends/json.rs`). The version is bumped on breaking shape changes to the JSON output. The version history is documented in the source file's comments.

Older documents (pre-v2) omit the `schema_version` field entirely. Consumers should treat a missing value as version 1. The `pyxis_version` field defaults to `"unknown"` when not available.

Notable version history: schema v10 retired the per-module `backends` map in favor of a flat `splices` array of standalone `prologue`/`epilogue` statements, each carrying its own cfg gate. Schema v11 added `c_string`, `struct`, `array`, and `const_ref` variants to `JsonConstValue` for C-string literals, structured initializers, and constant aliases.

## Item model

Each item is a `JsonItem` with a `kind` discriminator:

| Kind | Description |
|---|-------|
| `Type` | A struct/class definition with fields, vftable, and associated functions. |
| `Enum` | An enum with variants. |
| `Bitflags` | A bitflags definition with members. |
| `TypeAlias` | A type alias pointing at a target type. |
| `Constant` | A compile-time constant value. |
| `ExternValue` | A global at a fixed address. |

Every item carries:

- `path` - the absolute module path (e.g. `"math::Vector3"`)
- `visibility` - `public` or `private`
- `type_parameters` - generic type parameters (e.g. `["T"]` for `SharedPtr<T>`)
- `size` and `alignment` - in bytes
- `category` - `defined`, `predefined`, or `extern`
- `cfg` - the optional `#[cfg(...)]` predicate
- `source` - file and line number
- `cpp_name`, `cpp_header`, `rust_name` - extern type bindings (only on extern types)
- `kind` - the discriminator with kind-specific details (fields, variants, etc.)

Type definitions carry their fields as `JsonRegion` entries, each with a name, type, offset, visibility, and optional `#[base]` flag. Vftables carry their function slots as `JsonTypeVftable`.

## Cfg surfacing

The JSON backend deliberately does **not** filter by `#[cfg(backend = ...)]`. Every item is emitted with its cfg predicate attached as structured `JsonCfg` data, so downstream tooling can render or filter per their own rules.

```json
{
  "cfg": {
    "type": "key_value",
    "key": "backend",
    "value": "cpp"
  }
}
```

`JsonCfg` mirrors the parser's `CfgPredicate` shape:

| Variant | JSON form | Example |
|-----|------|-----|
| Ident | `{"type": "ident", "name": "test"}` | `cfg(test)` |
| KeyValue | `{"type": "key_value", "key": "backend", "value": "cpp"}` | `cfg(backend = "cpp")` |
| Any | `{"type": "any", "predicates": [...]}` | `cfg(any(...))` |
| All | `{"type": "all", "predicates": [...]}` | `cfg(all(...))` |
| Not | `{"type": "not", "predicate": ...}` | `cfg(not(...))` |

This means the JSON output is documentation, not a build target - it contains everything, and the consumer decides what to show.

## Splices

Backend splices (`prologue`/`epilogue` statements) are emitted as a flat `splices` array on each `JsonModule`. This replaced the per-module `backends` map in schema v10.

Each `JsonSplice` carries:

| Field | Type | Description |
|----|---|-------|
| `kind` | `"prologue"` or `"epilogue"` | Which end of the module's output the splice attaches to. |
| `cfg` | `JsonCfg?` | Optional cfg gate. `null` means every backend. |
| `definition` | `bool` | Whether this is a `definition` splice (C++ `.cpp` source). |
| `for_type` | `string?` | Resolved absolute item path when tagged with `for <Type>`. `null` for module-level splices. |
| `text` | `string` | The spliced code text. |

Splices tagged `for <Type>` are rendered on the type's documentation page by the viewer rather than the module page.

## Doc links

Doc comment intra-doc links (`[`Item`]`, `[`Item::Field`]`) are resolved to absolute paths and emitted as `doc_links` arrays on each item and module. Each `JsonDocLink` carries:

| Field | Type | Description |
|----|---|-------|
| `text` | `string` | The link path as written in the doc (e.g. `"Type::method"`, `"Action"`). |
| `target_kind` | `"item"` or `"module"` | Whether the link points at an item or a module. |
| `path` | `string` | Absolute path to the target. |
| `anchor` | `string?` | Anchor within the target page (e.g. `"field-m_Foo"`, `"variant-Bar"`), for member-level links. |

Anchors follow a convention: `field-<name>`, `variant-<name>`, `flag-<name>`, `func-<name>`, `vfunc-<name>`. Module-level extern values and nested constants get their own item pages (no anchor), so they link directly to the item path.

## TypeScript types

The JSON backend's structs derive `specta::Type`, which lets the driver generate TypeScript type definitions:

```sh
cargo run -p pyxis-driver - gen-types
```

This regenerates `types/json.ts` from the Rust struct definitions. The viewer consumes these types via npm workspaces (the `@pyxis/types` package symlinks to the generated file).

When you change a JSON backend struct, regenerate the TypeScript types and the viewer picks up the changes automatically.

## Viewer integration

The web viewer (`viewer/`) is a React application that loads `output.json` and renders a rustdoc-style documentation site.

### Features

- **Module tree navigation** - collapsible sidebar with the full module hierarchy
- **Search** - real-time search across all types, enums, bitflags, and functions with keyboard navigation
- **Type references** - clickable type references that navigate to the referenced item
- **Backend splice sections** - collapsible sections showing prologue/epilogue code per module and per type
- **Smart path resolution** - shows relative paths when possible to reduce clutter (same module shows just the name, sibling modules show relative paths)
- **Dark/light mode** - automatic theme detection with manual toggle
- **SPA routing** - client-side routing that avoids full page reloads

### Development

```bash
# From the repository root
npm install
cd viewer
npm run dev
```

The viewer uses Vite for development and builds to static files for production. It loads a JSON documentation file via a file picker in the UI.
