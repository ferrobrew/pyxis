# Pyxis language reference

Pyxis is a domain-specific language for describing types and structures that already exist in a binary's memory. You write `.pyxis` files that describe the layouts, vtables, and functions of an application's data structures, and the Pyxis driver generates Rust, JSON, or C++ output from those descriptions.

The mental model is the opposite of a type definition language. You aren't designing new types - you're documenting types that a compiled binary already contains. The sizes, offsets, vtable layouts, and function addresses are fixed by the binary. Pyxis gives you syntax to describe them so that tooling can consume a structured representation instead of reverse-engineering notes.

## Project structure

A Pyxis project is a directory containing a `pyxis.toml` file and a tree of `.pyxis` source files:

```
my_project/
├── pyxis.toml
├── types.pyxis           # module "types"
├── math/
│   ├── mod.pyxis          # module "math" (the folder's own items)
│   ├── vector3.pyxis      # module "math::vector3"
│   └── matrix4.pyxis      # module "math::matrix4"
└── game_objects.pyxis     # module "game_objects"
```

The `pyxis.toml` file configures the project:

```toml
[project]
name = "my_project"
pointer_size = 4
```

`pointer_size` is 4 or 8, matching the target binary's pointer width. It determines the size of `*const T`, `*mut T`, and function pointer types, and influences the default calling convention for member functions (thiscall on 32-bit, system on 64-bit).

The driver discovers source files with `glob("**/*.pyxis")` relative to the project root. Each file is a module. A folder with a `mod.pyxis` file gets its own items alongside its child modules; a folder without one is just a directory containing modules.

## Modules and imports

Each `.pyxis` file is a module. Modules contain item definitions, imports, extern types, functions, impl blocks, and backend splices.

### `use` imports

Imports bring types from other modules into scope. The syntax mirrors Rust:

```pyxis
use math::Vector3;
use math::{Matrix4, Vector3};
use math::{math::Aabb, rtti::Rtti};
```

Simple path imports (`use math::Vector3;`) bring a single item into scope. Braced imports (`use math::{Matrix4, Vector3};`) bring multiple items from the same path. Nested braced imports (`use math::{math::Aabb, rtti::Rtti};`) are supported.

### `pub use` re-exports

A `pub use` re-exports an imported item so other modules can reach it through this module:

```pyxis
pub use math::{Matrix4, Vector3};
```

Without `pub`, the import is module-private. With `pub`, the item becomes part of the module's public surface. In the Rust backend, `pub use` emits a `pub use` statement. In the C++ backend, it emits a `using` alias. In the JSON backend, re-exports appear in the module's `reexports` list.

### cfg-gated imports

`use` statements can be gated with `#[cfg(...)]`:

```pyxis
#[cfg(backend = "cpp")]
use types::math::Matrix4;
```

This is commonly used alongside backend splices when a backend needs an import that others don't.

## Type definitions

A type definition describes the memory layout of a struct or class:

```pyxis
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
```

This example uses primitives (`u8`, `i32`, `f32`), pointers (`*const u8`), arrays (`[f32; 3]`), and a `vftable` block. These are all explained in [Types](#types) below. For now, the key idea is that each field occupies a fixed region of memory at a computed offset, and the `#[size(N)]` attribute declares the total size of the type.

A more complex example with generics, `unknown<N>` padding, and `#[base]` inheritance appears later in this section.

### Fields

Fields are declared as `visibility name: Type,` inside the type body. The comma is the separator - trailing commas are optional. Each field occupies a memory region at a computed offset.

Pyxis computes field offsets automatically. The type's alignment is derived from `#[align(N)]` if present, otherwise from the largest alignment of its fields. Padding is inserted between fields to satisfy each field's alignment requirement. `#[packed]` suppresses all padding. `#[address(0xN)]` on a field pins it to an exact byte offset - use this when the binary's layout has a gap or an unusual alignment that automatic computation can't express. When you pin a field's offset, Pyxis inserts padding up to that offset if needed.

Field visibility (`pub` or private) controls cross-module access. In the Rust backend, private fields are emitted as private struct fields. In the C++ backend, all fields are public (struct scope).

Fields can carry attributes like `#[address(0x8)]` (fixed offset), `#[base]` (composition-based inheritance), or layout attributes.

### `vftable` blocks

A `vftable { ... }` block declares the virtual function table layout. Functions inside a vftable block are virtual function slots, not address-bound functions. They're called through the vtable pointer at runtime.

```pyxis
type IndexedVftable {
    #[size(4)]
    vftable {
        #[index(0)]
        pub fn first(&self) -> u32;

        #[index(2)]
        pub fn second(&self, x: i32) -> i32;

        #[index(3)]
        pub fn third(&self) -> u32;
    },
}
```

`#[index(N)]` sets the vtable slot index. Without it, slots are numbered sequentially. Use explicit indices to skip gaps - vtables in real binaries often have reserved slots for future use or platform-specific entries. The `#[size(N)]` attribute on the vftable block sets the total slot count, which must cover the highest index plus one.

### Opaque forward declarations

A type with no body is an opaque forward declaration:

```pyxis
pub type PropertyFile;
pub type ModelInstance;
```

This tells Pyxis the type exists but its layout is unknown. Pointers to opaque types are valid; embedding one by value requires `#[min_size(N)]` to give it a footprint.

### Nested items

Types, enums, bitflags, type aliases, constants, and extern values can be nested inside a type body:

```pyxis
pub type Outer {
    pub enum InnerEnum: u8 {
        A,
        B,
        C,
    },
    pub type InnerType {
        pub inner_field: u16,
    },
    pub bitflags InnerFlags: u32 {
        FLAG_A = 1,
        FLAG_B = 2,
    },
    pub type InnerAlias = u32,

    pub field: u32,
}
```

Nested items are accessed as `Outer::InnerEnum` etc. In the Rust backend, nested types are flattened to `Outer_InnerEnum` (rustdoc can't represent true nested types). In the JSON backend, they appear under the parent's `nested_items` with their full paths preserved.

### Generics

Types can have type parameters:

```pyxis
#[size(0x8)]
pub type Shared<T> {
    pub ptr: *mut T,
}

#[size(0x10)]
pub type MapEntry<K, V> {
    pub key: *mut K,
    pub value: *mut V,
}
```

Generic types are instantiated at use sites: `Shared<Vector3>`, `MapEntry<u32, Vector3>`. The type parameters don't have a known size until instantiated, so the semantic resolver computes concrete layouts for each instantiation.

Putting it together, here's a more complex type definition that uses generics, `unknown<N>` for padding, and `#[base]` for composition-based inheritance:

```pyxis
#[size(0x30)]
pub type GameObject {
    vftable {
        pub fn destructor(&mut self);
        pub fn get_type(&mut self) -> *const u32;
        pub fn is_type(&mut self, ty: *const u32) -> bool;
        pub fn initialise(&mut self);

        #[index(16)]
        pub fn init_transform(&mut self, mat: *const f32);
    },

    #[base]
    pub rtti: Rtti,
    pub child_objects: unknown<0x10>,
    pub parent_object: SharedPtr<GameObject>,
    pub object_id: u32,
}
```

## Enums

An enum describes a tagged integer with named variants:

```pyxis
#[defaultable]
pub enum TestEnum: u32 {
    #[default]
    None,
    Some,
    Value,
}
```

The `: RepType` annotation specifies the underlying integer type. Variants are assigned sequential values starting from 0 unless an explicit value is given:

```pyxis
pub enum RenderMode: u8 {
    Forward,
    Deferred,
}
```

`#[defaultable]` on the enum enables `Default` derivation in the Rust backend. `#[default]` on a variant marks it as the default value. These are separate attributes because not every defaultable enum's default is the first variant.

Impl blocks on enums work the same as on types:

```pyxis
impl TestEnum {
    #[address(0x123)]
    pub fn test();

    #[address(0x456)]
    pub fn another_test(&self) -> i32;
}
```

### Nested constants

Enums can contain nested constants:

```pyxis
pub enum Color: u8 {
    pub const DEFAULT: Color = Color::Red,

    Red,
    Green,
    Blue,
}
```

## Bitflags

Bitflags describe a flags-style enum where each variant is a bit:

```pyxis
pub bitflags TestBitflags: u32 {
    NONE = 0,
    FLAG_1 = 1,
    FLAG_2 = 2,
    FLAG_3 = 4,
}
```

Each member must have an explicit value. The `: RepType` annotation specifies the underlying integer type.

`#[defaultable]` and `#[default]` work the same as on enums:

```pyxis
#[defaultable]
pub bitflags TestBitflags2: u32 {
    NONE = 0,
    FLAG_1 = 1,
    #[default]
    FLAG_2 = 2,
    FLAG_3 = 4,
}
```

In the Rust backend, bitflags emit a `#[repr(transparent)]` newtype with the full bitflags API (`contains`, `insert`, `|`, `&`, `^`, `!`, `from_bits`, etc.) via a freestanding reimplementation of the `bitflags!` macro - no external dependency required.

Nested constants work inside bitflags the same way as in enums.

## Type aliases

A type alias gives a new name to an existing type:

```pyxis
pub type Vec3Ptr = *const Vector3;
pub type IntMutPtr = *mut i32;
```

Generic aliases take type parameters:

```pyxis
pub type Ptr<T> = *mut T;
pub type ConstPtr<T> = *const T;
pub type SharedRef<T> = Shared<T>;
pub type Entry<K, V> = MapEntry<K, V>;
```

Aliases can reference generic instantiations:

```pyxis
pub type VecSharedPtr = SharedPtr<Vector3>;
pub type VecEntry = Entry<u32, Vector3>;
```

Aliases participate in cross-module resolution. An alias imported from another module works as a re-export - visibility is checked at the alias definition site, not the use site.

## Constants

Constants are compile-time values declared at module level or nested inside types, enums, and bitflags:

```pyxis
pub const MAX_HEALTH: i32 = 100;
pub const SCALE_FACTOR: f32 = 1.5;
pub const GRAVITY: f64 = 9.81;
pub const GAME_NAME: str = "Pyxis";
pub const DEFAULT_COLOR: Color = Color::Red;
```

Constant expressions support integers (decimal and hex), floats, strings (the `str` type), and enum/bitflag variant paths.

Nested constants inside a type body use commas as separators:

```pyxis
pub type Player {
    pub const STARTING_GOLD: u32 = 500,
    pub const SPAWN_X: f32 = 0.0,

    pub type Inventory {
        pub const MAX_SLOTS: u32 = 30,

        pub slots: u32,
    },

    pub health: i32,
}
```

In the Rust backend, nested constants are emitted as associated `const` items inside the parent's `impl` block.

## Extern types and values

### Extern types

An extern type is an opaque type backed by a backend binding rather than a Pyxis layout:

```pyxis
#[rust_name = "::core::sync::atomic::AtomicU32", cpp_name = "::std::atomic<::std::uint32_t>"]
extern type AtomicHandle;
```

`extern type Name;` declares the type exists. The `#[rust_name]`, `#[cpp_name]`, and `#[cpp_header]` attributes tell each backend how to resolve it:

- `#[rust_name = "path::to::Type"]` - the Rust backend emits `pub use path::to::Type as Name;`, so references resolve without a hand-written prologue.
- `#[cpp_name = "std::atomic<int32_t>"]` - the C++ backend substitutes this verbatim at every use site.
- `#[cpp_header = "<atomic>"]` - the C++ backend adds this to the using module's `#include` list.

Extern types can have generics - the generic syntax is concatenated into the name string, so `extern type Foo<Bar>;` becomes the literal string `Foo<Bar>`.

### Extern values

An extern value is a global variable at a fixed address:

```pyxis
#[address(0x1_000)]
pub extern g_frame_count: u32;

#[address(0x2_000)]
pub extern g_engine: *mut Engine;
```

In the Rust backend, a module-level extern value emits a `get_<name>()` accessor that reads from the fixed address. A nested extern value (inside a type, enum, or bitflags) models a C++ class's static global, emitted as `Parent::get_<name>()`.

```pyxis
pub type Engine {
    #[address(0x3_000)]
    pub extern g_instance: *mut Engine,

    pub frame: u32,
}

pub enum RenderMode: u8 {
    #[address(0x4_000)]
    extern g_current: *mut RenderMode,

    Forward,
    Deferred,
}
```

## Functions

Functions describe callable entries in the binary. They can be freestanding (at module level) or associated with a type via an `impl` block.

### Freestanding functions

```pyxis
#[address(0x456)]
pub fn freestanding();

#[address(0x789)]
fn another_freestanding(arg1: i32) -> i32;
```

Freestanding functions must have an `#[address(...)]` attribute (or `#[external_body]`) - the semantic layer rejects a function with no implementation.

### Impl blocks

```pyxis
impl GameObject {
    #[address(0x917_A00)]
    pub fn init_transform_recursively(&mut self, mat: *const f32);
}
```

Impl blocks contain methods. Each method can take `&self` (const self), `&mut self` (mutable self), or named arguments:

```pyxis
impl PfxInstance {
    #[address(0x6B7C40)]
    pub fn set_game_object(&mut self, game_object: *mut PfxGameObject);
}
```

### Generic impl blocks

Generic impl blocks declare type parameters on the `impl` keyword:

```pyxis
impl<T> Pair<T> {
    #[external_body]
    pub fn first_ptr(&self) -> *mut T;
}

impl<T, Y> Pair<T> {
    #[external_body]
    pub fn cast_first(&self) -> *mut Y;
}
```

In the first form, `T` corresponds to the struct's own type parameter. In the second, `Y` is a method-level type parameter - it exists on the impl block but not on the struct. In C++, `Y` becomes a method-template parameter. In Rust, it becomes a function-level type parameter.

### Qualified impl paths

Impl blocks can target nested types using path qualification:

```pyxis
impl Outer::Inner {
    #[address(0x100)]
    pub fn do_thing(&self);
}
```

### Vftable functions

Functions inside a `vftable { ... }` block are virtual function slots. They're called through the vtable pointer, not bound to a direct address. The function body kind is `Vftable`, meaning the generated code fetches the function pointer from the vtable at the computed slot index and calls through it.

### `#[external_body]`

`#[external_body]` declares a function whose body is supplied by a backend prologue or epilogue rather than bound to a binary address:

```pyxis
impl Counter {
    #[external_body]
    pub fn make() -> Counter;
    #[external_body]
    pub fn bump(&mut self) -> u32;
    #[external_body]
    pub fn read(&self) -> u32;
}
```

The function signature is still emitted so it appears on the type's documentation page, but the Rust backend skips code emission entirely - the user's epilogue `impl` block is the sole source. The C++ backend routes the declaration to the header and expects the body in an epilogue.

Why `#[external_body]` exists: some methods can't be expressed in the function grammar (trait impls, `unsafe fn`, by-value `self`, `where`-clauses, extension traits). `#[external_body]` lets you declare the signature in Pyxis - so it surfaces on the type page and in the JSON - while the body lives in an epilogue as opaque text.

## Types

### Primitives

| Type | Size (bytes) | Notes |
|---|-------|----|
| `bool` | 1 | |
| `u8` | 1 | |
| `u16` | 2 | |
| `u32` | 4 | |
| `u64` | 8 | |
| `u128` | 16 | |
| `i8` | 1 | |
| `i16` | 2 | |
| `i32` | 4 | |
| `i64` | 8 | |
| `i128` | 16 | |
| `f32` | 4 | |
| `f64` | 8 | |
| `c_char` | 1 | C-ABI char. Backend-mapped: Rust → `::std::ffi::c_char`, C++ → `char`. Distinct from Rust's 4-byte `char`. |
| `void` | 0 | Backend-mapped: Rust → `::std::ffi::c_void`. |
| `str` | 0 | Const-only. Appears in `const` declarations, not field types. Size 0/alignment 1 are sentinels - it has no runtime layout. |
| `AtomicBool` | 1 | |
| `AtomicU8` | 1 | |
| `AtomicU16` | 2 | |
| `AtomicU32` | 4 | |
| `AtomicU64` | 8 | |
| `AtomicI8` | 1 | |
| `AtomicI16` | 2 | |
| `AtomicI32` | 4 | |
| `AtomicI64` | 8 | |

### Pointers

`*const T` and `*mut T` are pointer types. Their size is the project's `pointer_size` (4 or 8 bytes).

```pyxis
pub type Pointers {
    pub ptr: *const Vector3,
    pub mut_ptr: *mut GameObject,
    pub double_ptr: *mut *mut u8,
}
```

Function pointers are not expressible as field types. If a struct contains a raw function pointer, model it as a `vftable { ... }` block (which generates a typed function-pointer layout) or as an opaque `unknown<N>` field of the appropriate size.

### Arrays

`[T; N]` is a fixed-size array of `N` elements of type `T`. The size is `sizeof(T) * N`.

```pyxis
pub type Arrays {
    pub items: [Shared<Vector3>; 4],
    pub matrix: [f32; 16],
    pub ptr_array: [*mut u8; 8],
}
```

### Generic types

`Foo<T, U>` instantiates a generic type with type arguments. Arguments can themselves be generic:

```pyxis
pub type Container {
    pub entity: Shared<Vector3>,
    pub weak_entity: WeakPtr<Vector3>,
    pub entry: MapEntry<u32, Vector3>,
    pub items: [Shared<Vector3>; 4],
    pub ptr: *mut Shared<Vector3>,
}
```

### `unknown<N>`

`unknown<N>` is an untyped region of `N` bytes. Use it for padding, reserved fields, or data whose layout you haven't reverse-engineered yet:

```pyxis
pub type Padding {
    pub child_objects: unknown<0x10>,
    pub padding: unknown<4>,
}
```

The size is `N` bytes and alignment is 1. It has no type - you can't take a pointer to it or access its contents. It exists so you can fill gaps in a struct layout without inventing a fake type.

### Reserved keywords

`meta` and `functions` are tokenized as keywords but have no current meaning in the language. Avoid them as identifiers to stay forward-compatible with future features.

The full keyword list is: `pub`, `type`, `enum`, `bitflags`, `impl`, `fn`, `extern`, `use`, `meta`, `functions`, `vftable`, `unknown`, `prologue`, `epilogue`, `mut`, `const`, `self`, `Self`, `_`.

## Attributes

Attributes apply to items, fields, and functions. They're written as `#[name]`, `#[name(args)]`, or `#[name = "value"]`. Multiple attributes can be stacked: `#[size(0x10), align(4)]`.

### Layout attributes

| Attribute | Applies to | Effect |
|------|------|----|
| `#[size(N)]` | Types | Sets the exact size in bytes. The semantic layer verifies that fields sum to this size. |
| `#[align(N)]` | Types | Sets alignment in bytes. Mutually exclusive with `#[packed]`. |
| `#[min_size(N)]` | Types, opaque types | Sets a minimum size. Useful for opaque forward declarations that need a footprint: `#[min_size(8)] pub type Marker;` gives the type 8 bytes and alignment 1. |
| `#[packed]` | Types | Removes padding between fields. Mutually exclusive with `#[align]`. |

`#[min_size(N)]` exists because opaque types (`pub type Foo;`) have size 0 by default, and embedding a zero-size type by value is rejected. `#[min_size]` gives the opaque type a real footprint so it can appear as a field without a known layout. If the computed size is smaller than `min_size`, it's rounded up.

### Location attributes

| Attribute | Applies to | Effect |
|------|------|----|
| `#[address(0x...)]` | Fields, functions, extern values | On a field: sets the byte offset within the type. On a function: sets the binary address to call. On an extern value: sets the memory address of the global. |
| `#[singleton(0x...)]` | Types | The type is a singleton at the given address. The Rust backend emits a `get()` method that dereferences a pointer-to-pointer at the address. |
| `#[index(N)]` | Vftable functions | Sets the vtable slot index for this virtual function. |

`#[singleton(0x...)]` marks a type as a singleton at a fixed address. The address points to a pointer-to-pointer: the first dereference gets the pointer to the instance, the second gets the instance itself. This matches the common C++ pattern where a global pointer variable holds the address of the singleton. The Rust backend emits a `get()` method that performs the double-dereference and returns `Option<&'static mut Self>`:

```pyxis
#[singleton(0x13_377_331)]
pub type TestType {
    pub field_1: u32,
}
```

### Inheritance

| Attribute | Applies to | Effect |
|------|------|----|
| `#[base]` | Fields | Marks the field as a composition-based base. The field's type becomes a base class that this type inherits layout from. |

Pyxis uses composition, not C++ inheritance. A `#[base]` field is embedded by value at the same offsets as its type. The Rust backend generates `AsRef`/`AsMut` impls so `&derived` coerces to `&base`. The C++ backend generates implicit conversion operators.

Why composition: Pyxis describes layouts that already exist in a binary. Real C++ inheritance brings layout decisions (base-class offsets, padding, vtable layout, adjustor thunks) that the compiler chose. We can't reliably match a specific binary's layout while letting the compiler make those decisions. Composition with an explicit `Base` field keeps the IR's layout authoritative - `Derived` is guaranteed identical to `Base { ... extra fields }` because that's literally what the struct is.

### Function attributes

| Attribute | Applies to | Effect |
|------|------|----|
| `#[calling_convention(...)]` | Functions | Sets the calling convention. Valid values: `C`, `cdecl`, `stdcall`, `fastcall`, `thiscall`, `vectorcall`, `system`. |
| `#[external_body]` | Functions (not vftable functions) | The function body is supplied by a backend epilogue, not bound to an address. |

The default calling convention is `thiscall` on 32-bit (when the function has `&self` or `&mut self`) or `system` otherwise. On 64-bit, member functions default to `system`.

`#[external_body]` is rejected on vftable functions - virtual function slots always call through the vtable pointer, so there's no "body" to externalize.

### Trait attributes

| Attribute | Applies to | Effect |
|------|------|----|
| `#[copyable]` | Types, enums, bitflags | Rust backend: derives `Copy` + `Clone`. Suppressed by `#[pinned]`. |
| `#[cloneable]` | Types, enums, bitflags | Rust backend: derives `Clone` only. Suppressed by `#[pinned]`. |
| `#[defaultable]` | Types, enums, bitflags | Rust backend: derives `Default`. On enums/bitflags, requires a `#[default]` variant. |
| `#[default]` | Enum/bitflag variants | Marks the default variant for `Default` derivation. |
| `#[pinned]` | Types, enums, bitflags | Rust backend: adds a `PhantomPinned` field and suppresses `Copy`/`Clone`. The type must not be relocated in memory. |

`#[pinned]` exists because some types have addresses that must not change - the target binary passes pointers to `this` or its fields around, and moving the object would invalidate those pointers. `PhantomPinned` makes the type `!Unpin`, forcing consumers to use `Pin<&mut T>` or `Box::pin`.

On enums and bitflags, `#[pinned]` is an IR/JSON-only attribute - it has no effect on Rust or C++ output because enums and bitflags are value types that don't have addresses in the same sense.

### Backend binding attributes

| Attribute | Applies to | Effect |
|------|------|----|
| `#[rust_name = "path::to::Type"]` | Extern types | Rust backend: emits `pub use path::to::Type as Name;`. |
| `#[cpp_name = "..."]` | Extern types | C++ backend: substitutes this verbatim at every use site. |
| `#[cpp_header = "<header>"]` | Extern types | C++ backend: adds the header to the using module's `#include` list. |

These attributes are the bridge between Pyxis's opaque `extern type` declarations and the concrete types each backend uses. An extern type without any binding emits nothing - the consumer must supply the type some other way (typically via a hand-written prologue).

### Inner attributes

`#![...]` applies to the enclosing module, not an item:

```pyxis
#![rust(example_flag)]
```

Inner attributes are parsed generically - any attribute form can appear inside `#![...]`. They're collected on the module and passed through to the IR. No built-in inner attributes have defined semantics yet; this is an extension point for future use.

## Conditional compilation

`#[cfg(...)]` gates which items, functions, impl blocks, splices, and imports are emitted by each backend. Predicates are evaluated at emission time against a context that carries the current backend name.

### Predicates

| Form | Meaning |
|---|-----|
| `cfg(backend = "rust")` | True when the active backend is Rust. |
| `cfg(backend = "cpp")` | True when the active backend is C++. |
| `cfg(backend = "json")` | True when the active backend is JSON. |
| `cfg(any(...))` | True if any inner predicate is true. |
| `cfg(all(...))` | True if all inner predicates are true. |
| `cfg(not(...))` | True if the inner predicate is false. |

### Closed-world semantics

Unknown atoms evaluate to false. `cfg(test)` is false under every backend because `test` isn't a known axis. `not(unknown)` is therefore true. This means `#[cfg(not(backend = "rust"))]` correctly emits on cpp, json, and any future backend - an unknown backend is just "not rust."

The only axis currently modeled is `backend`. Bare identifier atoms (`cfg(test)`) are always false because no built-in axis is a bare flag.

### Where cfg applies

Cfg predicates can be attached to:

- Type, enum, bitflags, alias, and const definitions
- Impl blocks (applies to all methods in the block)
- Individual functions within an impl block
- Freestanding functions
- Backend splices (`prologue`/`epilogue`)
- `use` statements

When an impl block has a cfg and a method within it also has a cfg, the method's effective cfg is the conjunction (`all(...)`) of both.

### Disjoint-cfg method declarations

Two methods with the same name can be declared under disjoint cfgs:

```pyxis
#[cfg(backend = "cpp")]
impl Probe {
    #[external_body]
    pub fn cpp_only(&self) -> u32;
}

#[cfg(backend = "rust")]
impl Probe {
    #[external_body]
    pub fn rust_only(&self) -> u32;
}
```

Pyxis treats cfg-disjoint declarations as distinct - at most one is ever emitted per build, so there's no conflict. Two methods whose cfgs could both be active in one build (e.g. two ungated, or both `backend = "cpp"`) are rejected as duplicates.

## Backend splices

`prologue` and `epilogue` statements splice raw backend code into a module's generated output. They use raw string syntax to avoid escaping issues:

```pyxis
#[cfg(backend = "rust")]
epilogue r#"
    impl Counter {
        pub fn make() -> Counter { Counter { value: 0 } }
        pub fn bump(&mut self) -> u32 { self.value += 1; self.value }
        pub fn read(&self) -> u32 { self.value }
    }
"#;
```

`prologue` code lands at the top of the module's output (before item definitions). `epilogue` code lands at the bottom (after item definitions). In the C++ backend, prologue lands above the namespace, epilogue lands at the bottom of the namespace.

### `definition` modifier

The C++ backend distinguishes header (`.hpp`) from source (`.cpp`) files. The `definition` modifier routes a splice to the source file instead of the header:

```pyxis
#[cfg(backend = "cpp")]
prologue definition r#"
    #include <cstdio>
"#;

#[cfg(backend = "cpp")]
epilogue definition r#"
    ::std::uint32_t Logger::record() {
        seen += 1;
        std::printf("Logger seen=%u\n", seen);
        return seen;
    }
"#;
```

The `definition` modifier is cpp-only. The semantic layer rejects it for non-cpp backends or ungated splices - only cpp distinguishes header from source, so the modifier is meaningless elsewhere.

### `for <Type>` attribution

A splice can be attributed to a specific type using `for <Type>`:

```pyxis
#[cfg(backend = "rust")]
epilogue for Widget r#"
    impl Widget {
        pub fn make() -> Widget { Widget { id: 0 } }
        pub fn get_id(&self) -> u32 { self.id }
    }
"#;
```

Tagged splices render on the type's documentation page rather than the module page. The type must be in the same module as the splice. This is purely a documentation concern - the emitted code position is unchanged.

### cfg-gated splices

Splices are commonly gated with `#[cfg(backend = "...")]` so each backend gets its own code:

```pyxis
#[cfg(backend = "rust")]
epilogue r#"
    pub fn module_hello() -> u32 { 42 }
"#;

#[cfg(backend = "cpp")]
epilogue r#"
inline ::std::uint32_t module_hello() { return 42; }
"#;
```

### Plain string syntax

Splices accept plain string literals as well as raw strings:

```pyxis
prologue "use std::sync::atomic::Ordering;";
```

Raw strings (`r#" ... "#`) are preferred for multi-line code blocks because they don't require escape sequences.

## Doc comments

`///` is an outer doc comment - it attaches to the item that follows:

```pyxis
/// A shared pointer to a type.
/// The inner pointer is mutable.
#[size(0x8)]
pub type Shared<T> {
    pub ptr: *mut T,
}
```

`//!` is an inner doc comment - it attaches to the enclosing module:

```pyxis
//! This module provides shared pointer types.
```

### Doc link syntax

Doc comments support rustdoc-style intra-doc links:

```pyxis
/// A type with nested declarations.
///
/// See [`InnerEnum`], [`InnerType`], and [`InnerAlias`].
///
/// You can also qualify them: [`Outer::InnerEnum`], [`Outer::InnerType`].
pub type Outer {
    /// An enum nested inside [`Outer`].
    ///
    /// Variants: [`InnerEnum::A`], [`InnerEnum::B`], [`InnerEnum::C`].
    pub enum InnerEnum: u8 {
        A,
        B,
        C,
    },
}
```

Link forms:
- `[`Item`]` - shortcut link to an item in scope.
- `[`Item::Field`]` - link to a field, variant, or method.
- `[`display text`](Path)` - inline link with custom display text.

The Rust backend rewrites links to fully-qualified `crate::` paths at emission time, adding cross-module imports as needed. The JSON backend resolves links to absolute paths and attaches them as structured `doc_links` data so the viewer can render clickable references.

### Unicode

Doc comments support Unicode:

```pyxis
//! Module-level doc - café & naïve.

/// A type - with an emdash - and café.
pub type Unicode {
    /// field - naïve
    pub field_1: u64,
}
```

## Visibility

`pub` makes an item visible to other modules. Without `pub`, the item is module-private.

Visibility controls:
- Cross-module access: a private type can't be referenced from another module.
- Re-export eligibility: only `pub use` can re-export an item.

In the Rust backend, `pub` maps to Rust's `pub`. In the C++ backend, everything is public at struct scope - the `pub`/private distinction is Rust-only. In the JSON backend, visibility is surfaced as structured data on each item.

## Conventions

These conventions are drawn from [`pyxis-defs`](https://github.com/ferrobrew/pyxis-defs), the canonical real-world Pyxis project.

### Strip engine-specific type prefixes

Many engines prefix struct types (`SVector3`) and classes (`CGameObject`). Drop these prefixes in Pyxis: `Vector3`, `GameObject`, `SharedPtr`. Keep the prefix only when stripping would collide with another type.

### Module structure mirrors the source application

One `.pyxis` file per module; folders nest. Use `use` to import cross-module types. A folder that needs its own items gets a `mod.pyxis`.

### Opaque forward declarations for unknown-layout types

When a type's layout is unknown, declare it as an opaque forward declaration:

```pyxis
pub type PropertyFile;
pub type ModelInstance;
```

Pointers to opaque types are valid. If you need to embed one by value, use `#[min_size(N)]` to give it a footprint.

### `#[index(N)]` for vtable gaps

Real vtables have gaps - reserved slots, platform-specific entries, or functions that were added in later versions. Use `#[index(N)]` to skip slots:

```pyxis
pub type GameObject {
    vftable {
        pub fn destructor(&mut self);
        pub fn get_type(&mut self) -> *const u32;

        #[index(16)]
        pub fn init_transform(&mut self, mat: *const f32);
    },
}
```

### `#[external_body]` for backend-provided methods

When a method's body is supplied by a backend epilogue rather than bound to a binary address, declare it with `#[external_body]`. Gate the impl block with `#[cfg(backend = "...")]` so each backend only sees its own declarations:

```pyxis
#[cfg(backend = "rust")]
impl Widget {
    #[external_body]
    pub fn spin(&self) -> u32;
}

#[cfg(backend = "rust")]
epilogue for Widget r#"
    impl Widget {
        pub fn spin(&self) -> u32 { self.value * 2 }
    }
"#;
```

When a method exists for more than one backend, declare it once per backend under its own cfg-gated impl block. Pyxis treats cfg-disjoint declarations as distinct methods, so both surface on the type page.

### Extension traits in Rust epilogues

The function grammar models plain inherent methods - `&self`/`&mut self`/named args, return type, method type params. It can't express trait impls (`impl Drop for Foo`), `unsafe fn`, by-value `self`, `where`-clauses, or extension traits.

For those, put the body in a Rust epilogue tagged `for <Type>` so it renders on the type's page:

```pyxis
#[cfg(backend = "rust")]
epilogue for GameObject r#"
    pub trait GameObjectExt {
        unsafe fn get_matrix(&self) -> Option<crate::types::math::Matrix4>;
    }
    impl<T: std::convert::AsRef<crate::game_objects::GameObject>> GameObjectExt for T {
        unsafe fn get_matrix(&self) -> Option<crate::types::math::Matrix4> {
            let mut out = crate::types::math::Matrix4::default();
            let set = unsafe { self.as_ref().get_matrix_raw(out.as_mut_ptr()) };
            set.then_some(out)
        }
    }
"#;
```

### Cross-backend analogs in doc comments

When a method exists in one backend but has an analog in another, document the relationship in a doc comment:

```pyxis
#[cfg(backend = "cpp")]
impl GameObject {
    /// Cpp analog of rust's `GameObjectExt::get_matrix`.
    #[external_body]
    pub fn try_get_matrix(&self, out: *mut Matrix4) -> bool;
}
```

### `unknown<N>` for untyped regions

Use `unknown<N>` for padding, reserved fields, or data whose layout you haven't mapped yet:

```pyxis
pub type GameObject {
    pub child_objects: unknown<0x10>,
}
```
