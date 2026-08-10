# Contributing to pyxis

## What This Is

Pyxis is a domain-specific language for describing the types and structures that already exist in a binary's memory. Developers write `.pyxis` files that describe layouts, vtables, and functions, and the driver generates Rust, JSON, or C++ output from those descriptions. The repository is a Rust workspace: the compiler's Salsa-backed query graph lives in `src/`, the driver in `driver/`, a macro crate in `pyxis_macros/`, editor tooling (tree-sitter grammar, Zed extension, language server) under `tooling/`, and a React + TypeScript + Tailwind docs viewer under `viewer/`. The sections below this opening document this project, and the template sections at the end record the shared conventions that `contributing-update` refreshes from upstream.

<!-- contributing-templates: files=general.md,rust.md,typescript.md,react.md,tailwind.md @ 57e7c55206a089254c37b212bb3ad43574cde20c -->

## Backends

Pyxis emits one of three target languages:

- `rust` (default): `.rs` files inside a Cargo crate. The primary usage pattern is embedding the compiler in a `build.rs` to generate a `src` folder within a crate. See [`docs/rust_backend.md`](docs/rust_backend.md) for output layout, module mounting, `BuildOptions`, and `build.rs` integration.
- `json`: a single JSON document for tooling that wants to consume the semantic IR directly. See [`docs/json_backend.md`](docs/json_backend.md) for the schema, item model, and viewer integration.
- `cpp`: `.hpp` + `.cpp` per module plus a normative `CMakeLists.txt`, for C++ tooling that wants to link a static library. Targets the MSVC ABI directly (composition-only structs, `__thiscall`/`__stdcall`/etc. on `#[address]`-bound functions); generated code is portable to any MSVC-ABI consumer (real Windows + MSVC, or `clang-cl`). See [`docs/cpp_backend.md`](docs/cpp_backend.md) for the design decisions and ABI tradeoffs.

The full language reference covers every syntax form, type, attribute, cfg predicate, backend splice, and convention: [`docs/language.md`](docs/language.md).

```sh
cargo run -p pyxis-driver -- build --backend cpp <input-dir> <output-dir>
```

## Keeping files small

Keep source files under 1,000 lines. When a module approaches that, split it: convert `foo.rs` into a `foo/` directory, move cohesive clusters into submodules, and preserve the public API with re-exports from `foo/mod.rs`. Methods on one type can be spread across multiple `impl` blocks in different files of the same module, so no visibility or trait plumbing is needed. Large inline `#[cfg(test)] mod tests` blocks count too; extracting them to a sibling `tests.rs` is often the whole fix.

## Building emitted C++ on Linux dev hosts

The cpp output is normative; it does not bake xwin or clang-cl assumptions into `CMakeLists.txt`. To build it on Linux, point CMake at the dev-only toolchain in `tools/cmake-toolchains/xwin-x86.cmake`, which wires `clang-cl` against the MSVC SDK provisioned by [`xwin`](https://github.com/Jake-Shadle/xwin).

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

The toolchain pins `MultiThreadedDLL` (release CRT) for every config; xwin does not ship `msvcrtd.lib`, so Debug builds cannot link the debug CRT. Treat `Debug` and `Release` as differing only in optimization and debug-info, not in CRT.

On native Windows, no toolchain file is needed: point CMake at a regular MSVC install or `clang-cl` and build normally.

## Tests

`python test.py` runs the full test suite: clippy, fmt, the parser and semantic unit tests, `cargo run --example codegen_tests` (which emits the test corpus through every backend and rebuilds the emitted output), `cargo doc --no-deps -p codegen_tests` (which catches unresolved doc link references in the generated Rust output), and a doxygen pass over the emitted C++ corpus (which catches unresolvable `@ref`s in the rewritten C++ doc links). The doxygen pass is skipped with a warning if doxygen is not installed; `nix-shell` provides it via `shell.nix`. The cpp test corpus uses a regular host C++17 compiler (no MSVC ABI required), so CI does not need xwin.

Formatting relies on a nightly-only rustfmt feature (`imports_granularity`, configured in `rustfmt.toml`), so check it with nightly:

```sh
cargo +nightly fmt --all -- --check
```

## Changing the language

A language change (new attributes, syntax, types, and so on) is complete only when every surface that consumes the language has been audited, not just the compiler. Most surfaces handle attributes generically and need no changes, but each one must be verified. Here is the full checklist:

| Surface | Path | When it needs updating |
|---------|------|------------------------|
| Compiler frontend | `src/` | Always. Parser, semantic IR, and all backends (Rust/C++/JSON). |
| Parser test helpers | `src/parser/attributes.rs` | New `Attribute` variants or test constructors (e.g. `Attribute::pinned()`). The grammar parses `#[ident]` attributes generically, so simple ident attributes need no grammar change. |
| New item kinds | `src/parser/items/mod.rs`, `src/semantic/types/item.rs` | Adding a whole item kind (like `union`) means a variant on both `ItemDefinitionInner` enums. Both are matched exhaustively in about 30 places across the compiler, LSP, and backends; let the compiler enumerate them rather than grepping. Also extend `SigKind` (`src/semantic/name_index.rs`) and `ItemKind` (`src/semantic/error/context.rs`). |
| `AttributeName` enum | `src/semantic/error.rs` | Only if the new attribute participates in conflicting-attribute validation (e.g. `#[packed]` + `#[align]`). Most attributes do not need an entry. |
| Tree-sitter grammar | `tooling/tree-sitter-pyxis/grammar.js` | Only for new syntax forms or keywords. Ident attributes (`#[foo]`) are already parsed generically as `attribute_ident -> $.identifier`. |
| Highlights query | `tooling/tree-sitter-pyxis/queries/highlights.scm` | Rarely. Attributes are highlighted generically via `(attribute) @attribute`. Only change if a new node type needs a capture. |
| Zed extension | `tooling/zed-pyxis/` | Rarely. It just consumes the grammar and highlights query. No attribute-specific logic. |
| LSP hover | `tooling/lsp/src/handlers/hover_format.rs` | New attributes need a description string in the `attribute_description()` match table so hovering shows documentation. |
| LSP completion | `tooling/lsp/src/handlers/completion.rs` | Only if adding new keywords (not attributes). The completion handler lists keyword tokens, not attribute names. |
| JSON types | `types/json.ts` | After changing JSON backend structs (which derive `specta::Type`). Regenerate with `cargo run -p pyxis-driver -- gen-types`. |
| Viewer | `viewer/src/components/Attributes.tsx` | New attributes need a badge entry in `ItemAttributes` to be visible in the docs viewer. |
| Codegen test corpus | `codegen_tests/input/`, `codegen_tests/output/` | Add a test input exercising the new feature, then regenerate output with `cargo run --example codegen_tests`. |
| C++ backend docs | `docs/cpp_backend.md` | If the attribute affects C++ codegen (most do not; `copyable`/`cloneable` are Rust-only). |
| Language reference | `docs/language.md` | If the language feature is documented in the language reference. Most new syntax/types/attributes need a section update here. |
| Rust backend docs | `docs/rust_backend.md` | If the attribute or feature affects Rust codegen output shape. |
| JSON backend docs | `docs/json_backend.md` | If the JSON schema or item model changes. Bump `CURRENT_SCHEMA_VERSION` and update the version history. |
| Pretty-printer | `src/pretty_print.rs` | Usually no change needed (attributes print generically). Add a round-trip test to confirm. |

## Editor tooling

Pyxis ships a tree-sitter grammar, a Zed extension, and a language server. All live under `tooling/`.

The tree-sitter grammar lives in its own repository, [`ferrobrew/tree-sitter-pyxis`](https://github.com/ferrobrew/tree-sitter-pyxis), and is vendored here as a git submodule at `tooling/tree-sitter-pyxis`. Clone with `git clone --recurse-submodules`, or in an existing checkout run:

```sh
git submodule update --init
```

To change the grammar, edit it in `tooling/tree-sitter-pyxis`, then run

```sh
python tooling/sync-grammar.py -m "Describe the grammar change"
```

The script regenerates the parser, runs the grammar tests, commits and pushes to the grammar repo's `main`, and re-pins both the submodule and `tooling/zed-pyxis/extension.toml` at the resulting commit SHA. It stages those two bumps in this repo and leaves the commit to the contributor; pass `--commit-parent` to commit them too. Run it with no `-m` any time to re-pin against the submodule's current HEAD.

Two invariants the script maintains must be preserved if the workflow is ever touched by hand:

- `extension.toml` pins the grammar by full commit SHA, never a branch ref: Zed caches its compiled grammar by that string and will not re-resolve a branch.
- The parser is generated with `--abi 14` (wired into the grammar's `npm run generate`), the ABI Zed's bundled tree-sitter runtime loads.

After syncing, reinstall the Zed dev extension to pick up the new grammar.

### Architecture

The compiler uses a [Salsa](https://github.com/salsa-rs/salsa)-backed query graph (`src/salsa/`). Both the batch compilation pipeline (`build_with_store_and_options`) and the LSP server call the same Salsa queries; there is no separate imperative pipeline and LSP pipeline.

- `src/salsa/`: Salsa database, inputs, IR, and tracked functions
- `tooling/tree-sitter-pyxis/`: tree-sitter grammar for syntax highlighting (a submodule pointing at [`ferrobrew/tree-sitter-pyxis`](https://github.com/ferrobrew/tree-sitter-pyxis))
- `tooling/zed-pyxis/`: Zed extension
- `tooling/lsp/`: LSP server binary (`pyxis-lsp`)

### Documentation

- [`docs/language.md`](docs/language.md): language reference (syntax, types, attributes, cfg, splices, conventions)
- [`docs/rust_backend.md`](docs/rust_backend.md): Rust backend output shape, `BuildOptions`, `build.rs` integration, generated derives
- [`docs/json_backend.md`](docs/json_backend.md): JSON schema, item model, viewer integration
- [`docs/cpp_backend.md`](docs/cpp_backend.md): C++ backend ABI design, composition-based inheritance, vftables

### Running the LSP

```sh
cargo build -p pyxis-lsp --release
```

The `pyxis-lsp` binary communicates over stdio. The Zed extension spawns it automatically (see its README for installation instructions).

## General conventions

### Correctness over convenience

- Model the full error space—no shortcuts or simplified error handling.
- Handle all edge cases, including race conditions, signal timing, and platform differences.
- Use the type system to encode correctness constraints: newtypes, builder patterns, type states, lifetimes. Never use a bare string or integer where the domain has a meaning for it; a recognised closed set of values rides as an enum, not as bare strings.
- Prefer compile-time guarantees over runtime checks where possible.
- Use message passing or the actor model to avoid data races in concurrent code.
- **Never silently drop content you can't represent.** When adapting data between formats, return an error for anything the target can't express. A caller may then choose to ignore it; silent data loss is a correctness bug, not a convenience.
- Validate at each layer the data crosses, not only where the bug happened to surface. One check is bypassed by the next code path, refactor, or test double; the goal is to make the bad state structurally impossible, not locally absent.
- Getting the details right is really important!

### User experience as a primary driver

- Provide structured, helpful error messages that can be rendered with an appropriate library at a later stage.
- Make progress reporting responsive and informative.
- Maintain consistency across platforms even when underlying OS capabilities differ. Use OS-native logic rather than trying to emulate Unix on Windows (or vice versa).
- Write user-facing messages in clear, present tense: "Frobnicator now supports..." not "Frobnicator now supported..."

### Pragmatic incrementalism

- "Not overly generic"—prefer specific, composable logic over abstract frameworks.
- Evolve the design incrementally rather than attempting perfect upfront architecture.
- **The rule of three**: don't abstract until you've seen the pattern three times. Three similar lines beat a premature abstraction, which is harder to remove than it was to add.
- Don't build for hypothetical future requirements.

### Dependencies

- Neither reflex is right: reaching for a dependency for something trivial and handrolling something with a long correctness tail are both mistakes, and the second is the more expensive one.
- Handroll it when it's small, self-contained, and you can see the whole problem — a builder, a wrapper, a couple of pure functions.
- Take the dependency when the problem has a tail you'd otherwise discover in production: dates and timezones, Unicode, text encodings, compression, TLS, anything cryptographic. Never handroll cryptography.
- Prefer an existing dependency (including one of its feature flags) over a new one, and a focused library over a framework you'd use 5% of.
- Where the project documents a chosen stack, use it rather than silently introducing an alternative to something already covered.
- When it's genuinely unclear, ask rather than picking silently. Either direction is cheap to change early and annoying to change late.

### Boundaries and compatibility

- A library never reads environment variables. Configuration — keys, URLs, feature flags, paths — arrives as parameters, and only the application entry point reads the environment. Otherwise the library can't be tested without manipulating the environment, and it's silently coupled to a deployment.
- When changing anything serialised to disk or sent over a wire, walk the whole version matrix: old reader with new data, new reader with old data, and **old writer with new data**.
  - That third case is the one that gets missed. A default lets an old reader parse new data, but an old writer then drops the fields it doesn't know about on write-back — which corrupts the file rather than failing on it.
  - Bump the format version when you add the field, not when you first depend on it.

### Functional core, imperative shell

- Keep decision logic in pure functions that take data in and return data out. Keep I/O, concurrency primitives, and orchestration in a thin shell at the edges.
- The shape is gather, then process, then persist: the shell collects the inputs, the core decides, the shell writes the result. A core function that reaches out to read something mid-decision is the thing this is meant to prevent.
- Isolate coupling to the outside world — filesystem, clock, network, subprocesses, devices — behind a small seam: a trait, an interface, a dependency struct, with a production implementation and a test fake. Tests are deterministic because they substitute the fake, not because they clean up after the real thing.
- The payoff is testability. A pure core needs no fakes at all, and a thin shell has little logic left worth mocking. When a test needs elaborate setup to reach the behaviour it's checking, that's usually the code's shape talking, not the test's.

### Code organisation

The language-specific files set the file-size threshold and the naming conventions; these apply everywhere.

- Name a file for what it holds, not for a category. No `utils`, `helpers`, `common`, or `misc` — they become dumping grounds, and nothing stops unrelated code being added to a file whose name claims nothing. A file named for string formatting, or date arithmetic, or API error handling is one it's hard to put the wrong thing in. (Each language file gives the casing to use.)
- Within a file, put the public API first, then the private implementation below it: constants, helpers, and internal types. Order the private items by use, so each appears roughly in the order the public API reaches for it (topological order).
- Split a file along natural seams — distinct data types, feature groups, functional areas — not arbitrarily at the line limit. A cohesive single-concern file that slightly exceeds the threshold beats a fragmented one.
- Group a wide folder into subfolders by domain or role. A flat folder of 20+ files is a signal that subfolders are wanted.
- Test files follow the same thresholds as the code they test.

### Testing

- Test comprehensively, including edge cases, race conditions, and stress tests.
- Pay attention to what facilities already exist for testing, and aim to reuse them.
- When fixing a bug, add the failing regression test first, then make it pass.
- **Use real instances of what you control, and fakes for what you don't.** Your own database or filesystem is a managed dependency: talking to it is an implementation detail you can refactor freely, so test against the real thing. A third-party API, an SMTP server, a message bus is unmanaged: that conversation is observable behaviour, so put it behind a fake.
- **Don't mock what you don't own.** Wrap the third-party library in your own thin interface and substitute that, rather than mocking the library's own surface. It makes the test simpler and the design better.
- Never add a method to production code that only tests call. Cleanup and inspection helpers belong in test utilities.
- Wait for conditions, not for durations. Poll for the state you're expecting with a timeout, rather than sleeping long enough that it's *probably* ready — the latter passes locally and fails in CI. A fixed sleep is right only when the thing under test is itself about timing, like a debounce, and then the comment says why that duration.
- Clean up long-lived resources: containers, VMs, processes, cloud objects. Don't bother scrubbing database rows and log entries — perfect data cleanup is a fool's errand that makes multi-step integration tests nearly impossible. A test that needs pristine state should mint unique identifiers instead of depending on an empty table.
- Don't write a test that only exercises serialisation or a derived implementation. A round-trip earns its place only when it guards a real wire: a versioned payload, a public API, or a file format.
- No personal names in fixtures, and never the author's own identity. Anonymise every test and fixture to invented placeholders; do not seed one from any real person's name, handles, or biographical details, even when real data reproduces the behaviour under test. Reproduce the *shape* of what you observed, never the actual content.

Where a function has a property worth stating, prefer a property-based test over a handful of examples. Reach for the strongest property that applies — roughly, in increasing order of strength: doesn't crash, preserves the type, holds an invariant, is idempotent (`f(f(x)) == f(x)`), round-trips (`decode(encode(x)) == x`). An oracle property, where a new implementation must agree with the old one, is the tool for a rewrite.

Two ways a property test can look fine and test nothing. It can be tautological, comparing an expression against itself. Or it can restate the function's own logic in the assertion, in which case a bug in your reasoning appears in both halves and cancels out. Include the degenerate cases — empty, single element — explicitly rather than trusting the generator to find them.

### Documentation

- Use inline comments to explain "why," not just "what".
- Don't add narrative comments in function bodies. Only add a comment if what you're doing is non-obvious or special in some way, or if something needs a deeper "why" explanation.
- Module-level documentation should explain purpose and responsibilities.
- Comments and docs describe the present state. Reserve past-tense narration for the rare case where history explains a standing "why".
- Keep the user-facing docs in sync with the code. Where a document restates something the code owns — a config default, an example config file, a generated reference, an API surface — name the code as the source of truth and update the document in the same change. A code change that lands without its doc update isn't finished.
- Better still, generate the document from the code and check it in CI. Don't rely on anyone remembering.
- **Always** use periods at the end of code comments.
- **Never** use title case in headings and titles. Always use sentence case.
- Always use the Oxford comma.
- Don't omit articles ("a", "an", "the"). Write "the file has a newer version" not "file has newer version".

## Code style

### Rust edition and linting

- Use Rust 2024 edition.
- Format with **nightly rustfmt**: `cargo +nightly fmt --all`. `rustfmt.toml` opts into `imports_granularity = "Crate"` and `group_imports = "StdExternalCrate"`, which are nightly-only — stable rustfmt prints a warning about each and then silently skips them, so a stable-formatted file is *not* equivalent to a nightly-formatted one.
- Ensure the following checks pass at the end of each complete task (you don't need to do this for intermediate steps):
  - `cargo +nightly fmt --all -- --check`
  - `cargo clippy --workspace --all-targets --all-features -- -D warnings`
  - `cargo clippy --workspace --all-targets --no-default-features -- -D warnings`
  - `cargo test --workspace`
  - `cargo test --workspace --no-default-features`
- Configure clippy's restriction lints in `[workspace.lints.clippy]` in the workspace root's `Cargo.toml`, with each crate opting in via `[lints] workspace = true`:

  ```toml
  [workspace.lints.clippy]
  unwrap_used = "warn"
  expect_used = "warn"
  ```

- The end-of-task and CI clippy commands pass `-- -D warnings`, so any warning fails those checks; a plain `cargo clippy` run reports warnings but doesn't fail on them.

- Use `cargo clippy` in place of `cargo build` — it typechecks and lints in one pass, so a separate build buys nothing.
- Iterate in debug. Reach for `--release` only when benchmarking or packaging.
- No `unwrap()` or `expect()` in production code; tests are fine.
- Never silence a lint without a concrete reason documented in a comment above it. In almost all cases the right move is to restructure the code.
- When you do suppress one, prefer `#[expect(...)]` to `#[allow(...)]`. `expect` warns once the suppression is no longer needed, so stale suppressions can't quietly accumulate as the code changes around them.

### Type system patterns

- **Builder patterns** for complex construction (e.g. `TestRunnerBuilder`).
- **Type states** encoded in generics when state transitions matter.
- **Lifetimes** used extensively to avoid cloning (e.g. `TestInstance<'a>`).
- **Newtypes** for domain types, per the general rule against bare primitives.
- **`#[non_exhaustive]`** on public types in a library crate with a stable API, so a new variant or field isn't a breaking change. Internal crates don't need it.
- **Parameter structs over long argument lists**: when a function approaches the `clippy::too_many_arguments` threshold, bundle the cohesive parameters into a struct rather than threading more positional arguments. A request struct, or a shared seam like `Engine { store, graph, clock }` that several call shapes pass along.
  - **Never** silence that lint with an `allow`. The lint firing means a struct is wanted. The one exception is a signature you don't own — an FFI shim, or a hook mirroring a foreign ABI — where the `allow` is the honest annotation.

### Error handling

- Do not use `thiserror`. Instead, manually implement `std::fmt::Display` and `std::error::Error` for a given error `struct` or `enum`. `Error::source` returns the wrapped cause where there is one, so the chain stays walkable.
- Group errors by category with an `ErrorKind` enum when appropriate.
- Provide rich error context using structured error types.
- Two-tier error model:
  - `ExpectedError`: User/external errors with semantic exit codes.
  - Internal errors: Programming errors that may panic or use internal error types.
- Every error's `Display` leads with a `<context>:` prefix naming the subsystem or operation it belongs to, then the cause: `event store: …`, `lua: block commit failed: …`, `could not open the event log at /path: …`. Messages stay lowercase, so they compose.
  - An aggregating error prefixes its own layer's context and delegates to the inner error. A chained error then reads as nested context: `turn: lua: block commit failed: event store: …`.
  - Add resource context — a path, an id — at the layer that has it. Avoid bare "failed to {x}" glue; name the operation instead.

### Async patterns

- Do not introduce async to a project without async.
- Use `tokio` for async runtime (multi-threaded).
- Use async for I/O and concurrency, keep other code synchronous.
- Use `parking_lot::Mutex` for synchronous locks (the default); its guard is non-poisoning and must never be held across an `.await`. Reserve `tokio::sync::Mutex` for the rare guard that must survive an `.await`, since most locks are acquired, used, and dropped within a synchronous span.
- Use bounded `mpsc` channels; an unbounded channel hides backpressure.

### Logging

- Use `tracing` for diagnostic and operational logging throughout, emitting at meaningful points, not noisily.
- Install the subscriber only in binaries, and send logs to stderr.
- Operator and diagnostic programs route their output through `tracing` too. Reserve `stdout`/`println!` for genuine machine-readable command output.

### Module organisation

- Use `mod.rs` files to re-export public items; nontrivial logic lives in submodules, not in `mod.rs` itself.
- Keep module boundaries strict with restricted visibility, but prefer `pub(crate)` and `pub(super)` over `pub(in <path>)`. The `pub(in …)` form scopes to a named ancestor, which is precise but reads as a smell; reach for it only when neither `pub(crate)` nor `pub(super)` expresses the intended scope.
- Use `#[cfg(unix)]` and `#[cfg(windows)]` for conditional compilation.
- **Always** import types or functions at the very top of the module, with the one exception being `cfg()`-gated functions. Never import types or modules within function contexts, other than this `cfg()`-gated exception.
- It is okay to import enum variants for pattern matching, though.
- Re-exports follow the same rule: a `pub use` belongs at the top of the module with the imports, not beside the item it re-exports. In a `mod.rs`, the `mod` declarations come first, then the `pub use` block, so the module's public surface reads as one list.
- A path used more than once in a module gets imported at the top — the specific items, not the module — rather than repeated in full at each call site.
  - A path used once may stay fully-qualified. Unless it's unwieldy, meaning more than three module segments deep, in which case import it anyway.
  - And when the module already imports a sibling from the same parent, import the new item alongside it rather than writing it inline.
- **Always** anchor intra-crate paths at `crate::`, never `super::`. Write `crate::graph::Graph`, not `super::Graph` or `super::super::Graph`. The one exception is a test module, where `use super::*;` (pulling the parent module into the `#[cfg(test)]` block) is the idiomatic form and stays.
- Prefer a single grouped `use` statement per crate root rather than several siblings under it, collapsing shared prefixes: `use axum::{extract::State, http::StatusCode, routing::get};`, not three separate lines. Group imports into three blocks separated by blank lines: `std`, external crates, then `crate`/`super`/`self`. `imports_granularity = "Crate"` and `group_imports = "StdExternalCrate"` in `rustfmt.toml` enforce both automatically under nightly rustfmt.

### Code organisation

The general code-organisation rules apply; these are the Rust specifics.

- **The file hierarchy is the architecture diagram.** A newcomer should be able to read the directory listing and infer what the project does.
  - A subsystem with a public entry point is a folder module whose `mod.rs` states the boundary, with private submodules inside. Only `lib.rs`, `main.rs`, and genuinely cross-cutting types like `errors.rs` stay as top-level single files.
  - Avoid a top-level single file where a folder grouping is natural. If several files are semantically related, or file A is only consumed by file B, merge them.
- **The file-size threshold is around 1000 lines.** Split into a folder and re-export the public items from `mod.rs`, so consumers see a stable API.
  - Splitting is cheaper than it looks: methods on one type can spread across several `impl` blocks in different files of the same module, so there's no visibility or trait plumbing to do.
  - A large inline `#[cfg(test)] mod tests` counts toward the total. Extracting it to a sibling `tests.rs` is often the whole fix.
  - Existing oversized files are grandfathered. Split one when a change touches it substantially, not in drive-by churn.
- Shared helpers for a split test module live in its `mod.rs`.

### Control flow and state machines

- Avoid rightward drift. If a function is nesting three `select!` blocks or four levels of `match`/`if let`, extract each arm into a named function that takes a context struct. The control flow at the top level should read like an outline.
- Model each state machine as an explicit `enum` with named variants, even if only one field differs between them. Favour an exhaustive `match` plus a `transition` helper over scattered `if let` chains.
- Where a subsystem transitions through phases that own different local state (e.g. `Idle`, `Starting`, `Running`, `Draining`), extract each phase body into its own function and pass a typed context struct. This is the pragmatic version of the typestate pattern for actor-style loops; it keeps invariants local without requiring full type-parameterised phases.
- Invalid transitions should be unrepresentable at the boundary where they're consumed. If `transition()` returns `Option<State>`, the caller should never `.unwrap()` it in production — either enumerate the legal inputs ahead of time, or make the caller total.

### Platform coupling

- A file that depends on platform-specific facilities says so on the first line of its module-level docstring: `//! Linux-only: reads /proc/{pid}/cmdline.` The convention is explicit enough that a port to a second platform — or a reviewer — knows exactly what the contract is.
- The outside-world seams take the form of traits — `Fs`, `ProcessSpawner`, `GpuProbe`, `Clock` — bundled into one dependency struct that production and test construct differently. A second OS implementation then falls out of the same shape the fakes already needed.
- When a second platform does land, gate the existing impl with `#[cfg(target_os = "…")]` and add the alternative under a sibling gate; the trait definition stays platform-neutral.

### Reaching through smart pointers

- To borrow the value inside a lock guard, a `Box`, or an `Arc`, prefer `.as_ref()` / `.as_mut()` over a manual double-deref: write `state.lock().as_ref()`, not `&**state.lock()`. The named form reads as "borrow the value" rather than as deref bookkeeping. The same applies to an `Arc<dyn Trait>`: `model.as_ref()`, not `&**model`.

### Serde

- Use `#[serde(deny_unknown_fields)]` on config types, and `#[serde(default)]` on new fields so they stay backwards-compatible.
- Use `serde_ignored` when deserialising config, so a typo'd or stale field is reported rather than ignored.
- Avoid `#[serde(untagged)]` when deserialising — the error messages it produces are useless. Write a custom visitor instead.
- Reserve `#[serde(flatten)]` for the case it is genuinely for: extending a shared struct with local fields, or a `toml::Table` catch-all that preserves unknown fields across a round-trip. Note that it interacts badly with `serde_ignored` and with `deny_unknown_fields`.

### Memory and performance

- Be deliberate about when you clone and when you borrow. Share immutable data behind `Arc`/`Rc` or borrow it; where the data has a natural tree or graph shape, share the nodes rather than cloning subtrees.
- Use `smol_str` for efficient small string storage.
- Use `smallvec` for collections that are usually small, to avoid heap allocations in the common case.
- Stream data (e.g. iterators) where possible rather than buffering.

### Dependency versions

- **Applications, binaries, and workspace-internal crates: pin exactly.** Cargo's default caret range lets resolution drift between `cargo update` runs; `serde = "=1.0.219"` makes a bump a reviewable change instead of an ambient one.
- **Crates published to a registry: use the narrowest range that works.** An exact pin in a published library breaks diamond-dependency unification — if two of a consumer's dependencies pin different patch versions of the same crate, they get two copies or a hard resolution failure. Publish a range and let the consumer's lockfile do the pinning.
- Manage shared versions in the workspace root's `[workspace.dependencies]` and reference them with `{ workspace = true }`.
- Check the current version before adding or bumping a crate rather than writing one from memory.
- Comment on a non-obvious dependency choice, including a feature-flag choice made for a reason.

## Testing

### Testing tools

- **test-case**: For parameterised tests.
- **proptest**: For property-based testing.
- **insta**: For snapshot testing.
- **libtest-mimic**: For custom test harnesses.
- **pretty_assertions**: For better assertion output.
- Use `cargo nextest run` in place of `cargo test` where it's available.

### Testing conventions

- Unit tests live in the same file as the code under test, in a `#[cfg(test)] mod tests` block. Integration tests live under `tests/`.
- Never `#[ignore]` a test, and never let one silently skip. A test that cannot run in an environment is gated by a `cfg` or a feature, so its absence is visible.
- Tests must be deterministic: no real subprocesses, no real filesystem, no wall-clock sleeps, no network. Route each of those through the seam traits above and substitute the fake. Gate a fake behind `#[cfg(any(test, feature = "test-fakes"))]` when integration tests need it too.
- Time is the narrow exception when everything already runs on `tokio::time`: `#[tokio::test(start_paused = true)]` plus `tokio::time::advance` gives virtual time without needing another seam.

## TypeScript code style

The same correctness-first mindset applies here as anywhere else: TypeScript's type system is strong enough to encode most of the same invariants, and it should be pushed to do so. "Just cast it" is not an acceptable answer.

### Tooling and workflow

- These checks must pass at the end of each complete task, and CI enforces them:
  - `npm run typecheck` — `tsc -b` with no errors.
  - `npm run lint` — ESLint.
  - `npm run format:check` — Prettier. `npm run format` writes the fixes.
  - `npm run build` — the production build, where the project produces one.
- Run them frequently during development, not just at the end of a task. A clean lint is cheap to maintain and expensive to recover.

### Compiler settings

- Keep the project on the strictest practical settings: `strict`, `noUnusedLocals`, `noUnusedParameters`, `noFallthroughCasesInSwitch`, `erasableSyntaxOnly`, and `verbatimModuleSyntax`. Do not relax these.
- Prefer `import type { … }` for type-only imports, as `verbatimModuleSyntax` requires.
- Do not disable a rule or a flag to make a specific piece of code compile. Fix the code instead.

### Type system patterns

Treat these as the TypeScript analogues of the Rust patterns. The goal is the same: make illegal states unrepresentable.

- **Discriminated unions** for modelling state machines and result types — the equivalent of Rust enums. Always include a `kind` (or similar) tag and narrow on it.
- **Exhaustiveness checking** via a `never`-typed default branch in switches and `if`/`else` chains, so adding a new variant becomes a compile error everywhere it is handled.
- **Branded (nominal) types** for values that share a representation but not a meaning (e.g. `UserId` vs. `ProjectId` both being `string`). This is the parallel of Rust newtypes.
- **`readonly`** on arrays, tuples, and object properties by default, and `Readonly<T>` on reference-type parameters. Reach for mutability only when it is genuinely needed.
- **`as const`** for literal data that should be inferred as narrowly as possible, and **`satisfies`** to check a value against a type without widening its inferred type.
- **Template literal types** and mapped/conditional types to encode constraints at the type level where it pays off.
- **Prefer `unknown` over `any`**. If you reach for `any`, stop and reconsider; if it is truly unavoidable, isolate it behind a narrow boundary and document why.
- **Avoid type assertions** (`as SomeType`) and non-null assertions (`!`). Use type guards, discriminated unions, or restructured code instead. A type assertion is a claim the compiler cannot verify, so it is a liability.
- **Validate at boundaries**. Data from the network, `localStorage`, URL parameters, or any other untyped source must be parsed and validated before being treated as typed. Do not trust a `JSON.parse` result.
- Use `type` for object shapes rather than `interface`. Reserve `interface` for the cases that need declaration merging or a framework convention that expects it.
- **String literal unions over `enum`s.** A union of literals narrows properly, needs no import at the use site, and doesn't generate a runtime object. `type Status = "pending" | "active" | "failed"`.
- **Name booleans with a prefix** — `is`, `has`, `can`, `should`, `will` — or as a plain adjective on a data field (`active`, `visible`). Avoid negative names: `isEnabled` beats `isDisabled`, because `!isDisabled` is one negation too many.
- Be consistent about which of `null` and `undefined` means "absent", and convert at the boundary rather than letting both flow through the same field. `undefined` is the ecosystem default (`field?: T`, optional props, default parameters); prefer it unless a wire format or a backend makes `null` the natural choice.

### Errors

- Model the full error space. Prefer a discriminated union result type (`{ kind: "ok"; value: T } | { kind: "err"; error: E }`) or similar over throwing for expected failure modes.
- Exceptions are for genuinely exceptional, programmer-error situations.
- User-facing error messages follow the same rules as the rest of the project: present tense, sentence case, with periods.

### Module organisation

- Import types and values at the top of the file. No inline `require`/`import()` inside function bodies except for genuinely dynamic imports (code-splitting).
- Use named exports. Reserve default exports for cases where a framework or tool requires them (e.g. route modules, some bundler entry points).
- Use function declarations for top-level functions and arrow functions for inline callbacks. Annotate return types explicitly on exported functions, including `Promise<T>`.
- The file-size threshold here is around 400 lines.
- Group a `lib/` (or equivalent) folder by concern — `api/`, `model/`, `format/`, `nav/` — rather than letting it go flat.
- Use lowerCamelCase filenames for modules that export values and functions.

### Generated bindings across a language boundary

Where the frontend talks to a backend in another language, that backend owns the wire contract. The TypeScript side of it is generated — never hand-edited, and generally not committed.

Wire the generation into the backend's own build so it can't go stale, and have CI run that build before the frontend checks. A change to a wire type is then validated in the same commit that makes it. Lint and format skip the generated directories.

Regenerating makes new fields *typed*; it doesn't make them *visible*. When a backend change adds or restructures state that the frontend displays, update the display code in the same change — a field that arrives typed but unrendered is a gap, where the UI shows stale behaviour while the backend has moved on.

### Testing tools

- **Vitest**: Unit and component tests.
- **Playwright**: For end-to-end flows against a running backend, if and when one is justified. Do not reach for this for what a unit test can cover.

## React

- Write function components and hooks. No class components.
- Follow the rules of hooks strictly, and keep `eslint-plugin-react-hooks` warnings at zero. Add the plugin's rules to the ESLint config and include them in the `npm run lint` check.
- Where the React Compiler is enabled, don't add `useMemo`, `useCallback`, or `React.memo` preemptively — prefer plain derivation. Reach for one only when the compiler demonstrably can't handle a case, and say so in a comment.
- That's also why the hooks lint is load-bearing rather than advisory: the compiler's guarantees hold only while the code stays within the Rules of React.
- Keep components small and focused. Lift state only as far as it needs to go.
- Type component props explicitly. Do not rely on inference for the public shape of a component.
  - Annotate short prop sets inline on the component: `function Badge({ label }: { label: string })`.
  - When props run long — more than about six properties — extract a named exported type and annotate the component with it: `export type BadgeProps = { … }`.
  - Extend props via intersection with `&`: `type IconButtonProps = ButtonProps & { icon: Icon }`.
- Prefer composition over configuration — a few focused components beat one component with a dozen boolean props.

### File and folder layout

- One main component per file, with its co-located sub-components. Non-component utilities (hooks, constants, pure functions) go in separate files so hot-reload boundaries stay clean.
- Use PascalCase for `.tsx` component files and lowerCamelCase for `.ts` utility files. Where a component and its utilities would collide, give the utilities a `Utilities` suffix (e.g. `channelUtilities.ts`).
- A `components/` folder is for shared components only — those used by two or more views. A component used by exactly one view lives in that view's folder; a component used by exactly one application shell lives with that shell.

### Testing

- **React Testing Library** for component tests — query by user-visible semantics, not implementation details.

## Styling: Tailwind first, CSS last

- Styling is done with Tailwind utility classes in the markup.
- **Do not write custom CSS unless it truly, genuinely cannot be expressed in Tailwind.** This is a hard rule, not a soft preference. "It would be slightly cleaner in CSS" is not sufficient justification; neither is "I'm more comfortable with CSS". If you think you need custom CSS, first check whether an arbitrary value (`[…]`), a variant, a theme extension, or a small component abstraction solves it.
- When custom CSS is genuinely required (e.g. a keyframe animation, or a selector Tailwind cannot express), keep it minimal, colocated, and leave a comment explaining why Tailwind wasn't sufficient.
- Define the design tokens — colours, spacing, type scale — in the Tailwind theme, and reach for those tokens rather than ad-hoc values, so the visual system stays coherent and design changes stay centralised. Name them for their role in the design, not for the colour they currently are.

### Linting

Class order, duplicate and conflicting utilities, typo'd class names, and shorthand collapsing are all machine-checkable — so let the linter own them rather than spending review on them. Use [`eslint-plugin-better-tailwindcss`](https://github.com/schoero/eslint-plugin-better-tailwindcss) with its `recommended` config, and include it in the `npm run lint` check. Two things to get right when setting it up:

- It reports stylistic rules as warnings and correctness rules as errors by default. Treat both as errors, so lint stays a binary signal.
- Point it at the theme, or every custom token you define will be reported as an unknown class: `entryPoint` for a v4 CSS-based config, `tailwindConfig` for v3.

```js
settings: { "better-tailwindcss": { entryPoint: "src/app.css" } }
```

Compose conditional classes with `clsx` (plus `tailwind-merge` where later classes must override earlier ones), or `cva` for a component with variants. Never build a class name by interpolating fragments into a string — it defeats both the linter and Tailwind's own class extraction.
