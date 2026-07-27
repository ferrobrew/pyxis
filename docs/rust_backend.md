# Rust backend

The Rust backend is the default backend. It emits one `.rs` file per module, producing a Cargo crate tree that mirrors the input `.pyxis` file tree.

The primary way to use the Rust backend is to embed the Pyxis compiler in a `build.rs` script. The compiler runs at build time, reads your `.pyxis` definition files, and writes the generated Rust source directly into your crate's `src` directory (or `OUT_DIR`). This keeps the generated code out of version control and ensures it stays in sync with the definitions.

## `build.rs` integration

Add `pyxis` as a build dependency in your `Cargo.toml`:

```toml
[build-dependencies]
pyxis = { git = "https://github.com/ferrobrew/pyxis" }
```

Then write a `build.rs` that calls `pyxis::build_script_with_options`:

```rust
// build.rs
use std::path::Path;

fn main() {
    pyxis::build_script_with_options(
        Path::new("pyxis-defs"),
        Some(Path::new("src")),
        pyxis::BuildOptions {
            public_addresses: true,
            ..Default::default()
        },
    )
    .expect("pyxis build failed");
}
```

The first argument is the directory containing `pyxis.toml` and the `.pyxis` file tree. The second is the output directory - `None` defaults to `OUT_DIR` (the Cargo-managed build directory), which is appropriate when you want to `include!` the output from `lib.rs`. Passing an explicit path like `Some(Path::new("src"))` writes the generated files directly into your crate's source tree, so Cargo picks them up as regular modules.

For the `OUT_DIR` pattern, include the generated root from your crate root:

```rust
// lib.rs
include!(concat!(env!("OUT_DIR"), "/lib.rs"));
```

For the `src` directory pattern, add a `mod` declaration or `include!` in your `lib.rs` pointing at the generated root file.

`build_script` is the simpler form without options:

```rust
pyxis::build_script(Path::new("defs"), None)?;
```

Both functions print `cargo:rerun-if-changed=<in_dir>` so Cargo rebuilds when the definitions change.

## Output layout

```
out/
├── lib.rs                    # root module (or mod.rs via --rust-root-file-name)
├── types.rs                  # leaf module "types"
├── math/
│   ├── mod.rs                # module "math" (has children)
│   ├── vector3.rs            # leaf module "math::vector3"
│   └── matrix4.rs            # leaf module "math::matrix4"
└── game_objects.rs           # leaf module "game_objects"
```

The root module (empty path key) becomes `lib.rs` by default. A module with child modules becomes `mod.rs`. A leaf module (no children) becomes `<name>.rs`. The root file name is configurable via `-rust-root-file-name` or `BuildOptions::rust_root_file_name`.

The root module also carries lint `allow`s that cascade to descendant modules:

```rust
#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals, clippy::missing_safety_doc, clippy::unnecessary_cast, clippy::module_inception)]
#![cfg_attr(any(), rustfmt::skip)]
```

The rustfmt skip prevents a stray `cargo fmt` from reformatting the prettyplease-formatted output.

## Module mounting

### `-rust-module-prefix`

When the generated tree is mounted as a submodule of a consuming crate rather than at the crate root, `-rust-module-prefix` prepends a path to every `crate::`-relative reference. For example, `-rust-module-prefix jc2` turns `crate::world::Weather` into `crate::jc2::world::Weather` so references resolve when the tree is mounted at `crate::jc2`.

This does not affect output file locations - those follow `out_dir`.

### `-rust-root-file-name`

Controls the root module's file name. Defaults to `lib.rs` (a crate root). Set to `mod.rs` when the generated tree is mounted as a submodule via `-rust-module-prefix`, so the tree can be included with `pub mod jc2;` from the host crate's `lib.rs`.

## `BuildOptions`

```rust
pub struct BuildOptions {
    pub public_addresses: bool,
    pub rust_module_prefix: Option<grammar::ItemPath>,
    pub rust_root_file_name: Option<String>,
}
```

| Field | Effect |
|----|----|
| `public_addresses` | Emits `pub const <Fn>_ADDRESS: usize` next to each address-bound function. Consumers can reference the address (e.g. to install a hook) without hardcoding it. The constant is always `pub`, even when the function wrapper is private. The function body transmutes the constant rather than a literal. |
| `rust_module_prefix` | Module path prepended to `crate::`-relative references. |
| `rust_root_file_name` | File name for the root module's output. |

## Generated derives

| Attribute | Rust derive |
|------|-------|
| `#[copyable]` | `Copy`, `Clone` |
| `#[cloneable]` | `Clone` only |
| `#[defaultable]` | `Default` |
| `#[pinned]` | Suppresses `Copy`/`Clone` (even if `#[copyable]` or `#[cloneable]` is present). Adds a `_pin: PhantomPinned` field. |

`#[pinned]` takes precedence over `#[copyable]` and `#[cloneable]` because a pinned type must not be moved - `Copy`/`Clone` would allow moving out from behind a `Pin`. The `PhantomPinned` marker makes the type `!Unpin`, forcing consumers to use `Pin<&mut T>` or `Box::pin`.

For enums and bitflags, the same attributes apply. `#[defaultable]` on an enum or bitflags requires a `#[default]` variant - the `Default` impl returns that variant.

## Unions

A pyxis `union` lowers to a real Rust `union`, with every member wrapped in `ManuallyDrop<T>`:

```rust
#[derive(Copy, Clone)]
#[repr(C, align(8))]
pub union Payload {
    pub as_int: ::core::mem::ManuallyDrop<i32>,
    pub as_float: ::core::mem::ManuallyDrop<f32>,
    pub as_ptr: ::core::mem::ManuallyDrop<*mut i32>,
}
```

`ManuallyDrop` is applied unconditionally. Rust requires it for any member whose type isn't `Copy`, and applying it only where needed would make a member's spelling depend on whether its type happened to be `#[copyable]`. It is `repr(transparent)` and derefs to its contents, so it costs nothing at runtime and little at the call site: `unsafe { *value.payload.as_int }`.

Two impls are written out rather than derived, because a union can derive neither - the compiler has no way to know which member is live:

| Impl | Behaviour |
|------|-------|
| `Debug` | Prints `Name { .. }`. There is nothing safe to print, since which member applies is a property of the surrounding data. |
| `Default` | Emitted only for `#[defaultable]` unions; returns `unsafe { core::mem::zeroed() }`. The `#[defaultable]` check already rejects members that aren't plain data, so an all-zero reading is valid. |

Emitting these anyway is what lets a *containing* struct keep its own `#[derive(Debug, Default)]` working.

`Copy` and `Clone` are derived normally from `#[copyable]`/`#[cloneable]` - `ManuallyDrop<T>: Copy` exactly when `T: Copy`, so the existing trait-constraint checks carry over unchanged. `#[pinned]` suppresses them as it does for structs, but adds no `PhantomPinned` member: an extra member would be another reading of the same bytes, not an extra field.

Inline unions (`pub payload: union { ... }`) are emitted as ordinary module-scope items named `{Type}{Field}Union`, flattened through the same `flatten_type_name` path as any other item.

## Function pointers

A pyxis `fn(A, B) -> R` lowers to `unsafe extern "<cc>" fn(A, B) -> R`. Every generated function pointer is `unsafe` — pyxis describes slots in a foreign binary, and nothing verifies the target.

```rust
pub struct Callbacks {
    pub on_tick: unsafe extern "system" fn(engine: *mut Engine, dt: f32),
    pub on_event: unsafe extern "system" fn(*mut Engine, u32) -> bool,
    pub table: [unsafe extern "C" fn(*mut Engine); 4],
}
```

Parameter names written in pyxis are carried into the emitted signature; a parameter written without one stays anonymous rather than getting a synthesized name.

The type is emitted bare, not wrapped in `Option`. A null slot is as expressible — and as unchecked — as a null `*mut T`, matching how pyxis treats every other pointer. Callers that need to test for null can `transmute` or compare the address themselves.

## `#[external_body]` handling

When a function has `#[external_body]`, the Rust backend skips code emission entirely. The function signature is not emitted as a Rust method - the user's epilogue `impl` block is the sole source.

Rust permits multiple `impl Foo` blocks, so the user's epilogue can host its own `impl Foo { fn bar(...) { ... } }` without conflict. The function still appears in the JSON backend's output (with `body: "external"`) and on the type's documentation page.

## Doc link rewriting

Rustdoc-style intra-doc links (`[`Item`]`, `[`Item::Field`]`, `[`Self::member`]`, `[`display text`](Path)`) are rewritten at emission time by rendering each link's *resolved target* as an absolute Rust path.

The process:

1. Semantic analysis resolves every doc link once, crate-wide, and stores the results per doc block (`doc_links::resolve_all`). Unresolvable links fail the build there.
2. The backend renders each resolved target as an absolute `crate::` path: nested items are flattened (`Outer::Inner` → `crate::module::Outer_Inner`), extern values become their `get_<name>` accessor path, and members append to their owner's rendered path. Links to predefined types (`u32`, ...) are left alone — rustdoc resolves primitives natively.
3. An inline link keeps its label and has its destination replaced; a code shortcut (`` [`Inner`] ``) becomes an inline link (`` [`Inner`](crate::module::Outer_Inner) ``) so the written label survives.
4. The rewritten links are emitted in the `#[doc = "..."]` attribute strings.

Because every destination is absolute, no doc-driven `use` imports or aliases are emitted, and rewriting never depends on how the link was written — only on what it resolved to. The generated root allows `rustdoc::redundant_explicit_links`, since the uniform rewrite is "redundant" for the subset of links rustdoc could have resolved from the label alone.

The link detection is shared between the compiler and the LSP via `scan_links`, so hover previews and generated docs see the same link resolution.

## Extern type bindings

An `extern type` with `#[rust_name = "path::to::Type"]` emits a `pub use` alias:

```rust
pub use ::core::sync::atomic::AtomicU32 as AtomicHandle;
```

This makes references to `AtomicHandle` resolve to the real Rust type without a hand-written prologue. Extern types without a `rust_name` binding emit nothing - the consumer must supply the type through some other channel (typically a prologue `use` statement).

## Nested item flattening

Rust can't represent true nested types (a type defined inside another type's body). The Rust backend flattens nested item names with `_`:

```
Outer::InnerEnum  →  Outer_InnerEnum
Outer::InnerType  →  Outer_InnerType
```

The flattening uses the full path, so deeply nested items get longer names. Doc links referencing nested items are rewritten to match.

## Bitflags implementation

The Rust backend emits a freestanding reimplementation of the `bitflags!` macro (`__bitflags!`) into the crate root whenever any `bitflags` definition exists in the crate. This avoids a dependency on the external `bitflags` crate.

The generated type is a `#[repr(transparent)]` newtype over the underlying integer, always `Copy + Clone`, with the standard bitflags API (`contains`, `insert`, `remove`, `|`, `&`, `^`, `!`, `from_bits`, `from_bits_truncate`, `is_empty`, `is_all`, etc.).

## Size checks

Every type with a non-zero size gets a `static_assert`-style size check:

```rust
fn _GameObject_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x30], GameObject>([0u8; 0x30]);
    }
    unreachable!()
}
```

This forces a compile error if the generated struct's size doesn't match the declared `#[size(N)]`. It catches layout mistakes early - if the field sizes and offsets don't sum to the right total, the transmute fails to compile.

## `AsRef` / `AsMut` for composition

Types with `#[base]` fields get `AsRef` and `AsMut` impls for each base type in their hierarchy. This is the Rust equivalent of C++'s implicit conversion - `&derived` coerces to `&base` via `as_ref()`.

When a type has multiple base fields of the same type, the `AsRef`/`AsMut` impls are skipped (they'd conflict) and a doc comment explains why.
