# C++ backend design notes

The cpp backend emits one `.hpp` per pyxis module (with a matching `.cpp`
when there are out-of-line definitions to host), plus a shared
`include/pyxis_runtime.hpp` and a normative top-level `CMakeLists.txt`.
This document records the non-obvious design decisions and their
trade-offs — things you'd notice when binding the output to a real
binary that you wouldn't pick up by reading the code in isolation.

Output layout looks like this:

```
out/
├── CMakeLists.txt
├── include/
│   ├── pyxis_runtime.hpp
│   ├── <module>.hpp
│   └── <nested>/<module>.hpp
└── src/
    ├── <module>.cpp        (only when needed)
    └── <nested>/<module>.cpp
```

The `.hpp/.cpp` split is opinionated: pyxis hoists every non-template
out-of-line method definition into the `.cpp` so the header stays a
declaration surface. Template members stay header-visible (the compiler
needs them at every instantiation site). The `CMakeLists.txt` uses
`GLOB_RECURSE` so adding or removing modules is transparent.

## ABI: composition-based inheritance

`#[base]` regions become **composition**, not C++ inheritance. A type
`Derived` with `#[base] base: Base` lays out as:

```cpp
struct Derived {
    Base base;       // by-value, occupies the same offsets as `Base`
    // ...derived-only fields below...
};
```

Plus an implicit conversion operator on the derived type:

```cpp
struct Derived {
    operator Base&() { return this->base; }
    operator const Base&() const { return this->base; }
};
```

### Why composition

Pyxis's job is to describe layouts that *already exist* in a binary.
Real C++ inheritance brings layout decisions that aren't ours to make:
the compiler chooses base-class offsets, may insert padding for
alignment, picks a vtable layout, and on multiple inheritance generally
inserts adjustor thunks. We can't reliably match a specific binary's
layout while letting the compiler "help."

Composition + an explicit `Base` field keeps the IR's layout
authoritative. `Derived` is guaranteed identical to `Base { ... extra
fields }` because that's literally what the struct is.

### The ergonomic tradeoff

`Derived` and `Base` are **not implicitly convertible** at the pointer
level. `Derived*` does not coerce to `Base*`; only `Derived&` coerces
to `Base&` (via the `operator Base&()` we emit).

Any binary-bound API typed as `Base*` requires `&derived->base` at the
call site. Example:

```cpp
void some_engine_api(Base* b);

Derived* d = ...;
some_engine_api(d);           // ❌ no implicit Derived*→Base* conversion
some_engine_api(&d->base);    // ✅ explicit upcast through the field
```

In Rust the equivalent is `d.as_ref()` (which pyxis generates via
`AsRef`/`AsMut` impls). The C++ side picks ergonomics over magic.

## Vftables

Each type with a `vftable { ... }` block in pyxis gets:

1. A separate `<Type>Vftable` struct describing the function-pointer
   layout.
2. A `vftable: const <Type>Vftable*` field (added implicitly if the type
   doesn't already inherit one through a base region).
3. An `_vftable_ptr()` accessor on the type.
4. Per-vfunc wrapper methods that fetch the pointer through
   `_vftable_ptr()`, call through it, and return the result.

### `void*` receivers in vftable slots

The function-pointer signatures in the generated `<Type>Vftable`
struct take `void*` (or `const void*`) for the receiver, not
`Type*`:

```cpp
struct BaseVftable {
    void (*destructor)(void*);                // not BaseA*, BaseB*, or Base*
    ::std::int32_t (*foo)(const void*, int);
};
```

That lets a derived type's `this` flow into the slot without explicit
casts through the base chain. We can't express "any pointer to this
type or one of its bases" in C++'s type system without inheritance
(which we've already given up — see above), so `void*` is the
ABI-compatible escape hatch.

Inside the generated wrappers this means you're trusting the slot
index rather than getting a type-check on the call site. Wrappers are
mechanical and the layout is checked by `static_assert(sizeof)`, so
the type-system gap is bounded; just don't expect the compiler to
catch a mistake if you write a wrapper by hand.

### `_vftable_ptr()` and the reinterpret chain

For a `Derived` whose vftable lives in a `Base` field, the accessor
walks the chain via `reinterpret_cast`:

```cpp
const DerivedVftable* Derived::_vftable_ptr() const {
    return reinterpret_cast<const DerivedVftable*>(this->base._vftable_ptr());
}
```

This relies on the derived vftable's *layout* sharing its prefix with
the base vftable's — exactly how MSVC lays out vtables for genuine
inheritance — so the cast is safe in practice on the MSVC ABI we
target. Strict-aliasing pedants will note this is a `reinterpret_cast`
between unrelated pointer types; in our setting that's fine because
the underlying bytes are guaranteed to be the same vftable layout
extended at the end.

## Calling conventions

We always target MSVC ABI. The `PYXIS_*` macro shims defined in
`pyxis_runtime.hpp` (driven by `src/backends/cpp/runtime.rs`) expand
to `__cdecl`/`__stdcall`/etc. on MSVC, to `__attribute__((cdecl))`-
style attributes on clang/GCC i386, and to nothing on every other
host. The non-MSVC branches exist only so generated headers
compile-check during the dev loop — anything actually calling through
to a `__stdcall` binary entrypoint needs an MSVC-compatible build.

`PYXIS_VECTORCALL` is empty on the clang/GCC i386 branch (there's no
real vectorcall on that target). Binaries that actually depend on
vectorcall semantics must build against the MSVC arm.

## Splices: `prologue` / `epilogue` and `definition`

`backend cpp { prologue ...; epilogue ...; }` blocks splice user-written
C++ into the emitted module. The cpp backend uniquely supports a
`definition` modifier:

```pyxis
backend cpp {
    prologue r#" ... "#;             // lands in the .hpp above the namespace
    epilogue r#" ... "#;             // lands in the .hpp at the bottom of the namespace
    prologue definition r#" ... "#;  // lands in the .cpp above the namespace
    epilogue definition r#" ... "#;  // lands in the .cpp at the bottom of the namespace
}
```

Common use cases:

- `prologue` for `#include <foo>` directives, template specializations,
  or `using` aliases that need to be visible to downstream consumers.
- `prologue definition` for source-private `#include`s (e.g.
  `<windows.h>`, `<d3d10.h>`) that would otherwise leak into every
  consumer of the header.
- `epilogue` for header-side inline method bodies (paired with pyxis's
  `#[external_body]` attribute, which declares the signature in the
  IR and leaves the body to the splice).
- `epilogue definition` for method bodies that don't need to be
  header-visible.

The semantic layer rejects `definition` for non-cpp backends — only
cpp distinguishes header from source, so the modifier is meaningless
elsewhere.

## `#[cpp_header]` / `#[cpp_name]` extern bindings

`extern type Foo;` declarations can carry `#[cpp_header = "<atomic>"]`
and `#[cpp_name = "std::atomic<int32_t>"]` attributes. The cpp backend
walks them once up front and:

- Adds the `cpp_header` value to the using module's `#include` list.
- Substitutes the `cpp_name` verbatim at every use site (we don't emit
  a `using Foo = std::atomic<int32_t>;` alias because the pyxis leaf
  name may contain generic syntax — `Foo<Bar<u32>>` — that's invalid
  on the LHS of `using`).

## Dependency analysis & forward declarations

Two distinct kinds of edges:

- **FullDef**: by-value field, base, array element, FullDef-typed
  template arg. Needs an `#include` of the defining module.
- **FwdOnly**: pointer field, function param/return. A forward
  declaration is enough.

`src/backends/cpp/deps.rs` walks the graph for every module and
classifies edges. The header includes the FullDef deps and forward-
declares the FwdOnly ones.

Inside the namespace, every non-alias, non-extern type also gets an
intra-module forward decl up front so peer items can refer to each
other regardless of declaration order.

### Layout cycle detection

A strongly-connected component on FullDef edges is a real value-cycle
that no amount of forward-decling can resolve (e.g. `A { B b; }` and
`B { A a; }`). The backend runs Tarjan's SCC over both the intra-
module item graph and the cross-module aggregate graph and errors out
with `CppBackendError::LayoutCycle` if it finds one. Break the cycle
by introducing a pointer indirection or moving a type into a separate
module.

## Building on Linux

The emitted `CMakeLists.txt` is normative — no toolchain assumptions.
On Linux:

1. `cargo install xwin` and `xwin --arch x86,x86_64 --accept-license splat --output ~/.xwin`.
2. Install LLVM ≥ 16 (`clang-cl`, `lld-link`, `llvm-lib`, `llvm-rc`).
3. `cmake -S out -B build -DCMAKE_TOOLCHAIN_FILE=tools/cmake-toolchains/xwin-x86.cmake -DXWIN_ROOT=$HOME/.xwin`.

The toolchain pins `MultiThreadedDLL` (release CRT) for every config
because xwin doesn't ship `msvcrtd.lib`. Treat `Debug` and `Release`
as differing only in optimization / debug-info, not CRT selection.

On native Windows no toolchain file is needed; point CMake at MSVC or
`clang-cl` and build normally.

## What this backend deliberately does not do

- **No virtual inheritance** — composition only. Diamond inheritance
  shows up as two base fields.
- **No RTTI** — there's no `typeid`, `dynamic_cast`, or exception
  hierarchy. Binary-bound APIs typically don't use these and adding
  them would require us to commit to a vtable layout.
- **No copy/move semantics** — copy/move constructors/assignment are
  implicit defaults. `#[copyable]` / `#[cloneable]` only inform the
  Rust backend; the cpp side is always trivially-copyable when the
  fields are.
- **No member access control** — pyxis's `pub`/private distinction is
  rust-only. In cpp every method and field is emitted at struct scope
  with default visibility (public for `struct`). Backend epilogues
  need to call into rust-private methods by name.
