//! Salsa tracked functions — the query graph.
//!
//! The pipeline:
//!   `tokenize_file` → `parse_file` (per-file leaf queries)
//!   → `name_index` (body/location-free index of names, scopes, and arity — its
//!      output backdates across edits that don't change the project's shape)
//!   → `placeholder_base` (predefined + extern + every item as a placeholder,
//!      built once and shared via `TypeRegistry::with_base`)
//!   → `resolve_item` (per-item, **incremental**: depends on `name_index`, the
//!      single file that declares the item, `placeholder_base`, and
//!      `resolve_item` for the item's value dependencies — an edit to an
//!      unrelated file leaves it cached)
//!   → `analyze` (root query for diagnostics + the batch compiler: drives
//!      resolution through `resolve_item` (so it too is incremental — only the
//!      edited items and their dependents re-resolve), then runs the
//!      post-resolution passes — extern values, functions, associated
//!      functions, doc links)
//!
//! `resolve_item` depends on the leaner, body/location-free `name_index` rather
//! than the full `DeclarationRegistry` — that's what makes it incremental.
//! Overlaying the shared `placeholder_base` keeps a clean whole-program analyze
//! O(edges), not O(n²). `collect_declarations` still builds the location-full
//! registry the LSP uses for per-request name resolution.
//!
//! The queries are split across cohesive submodules:
//!   - `leaf` — `tokenize_file` / `parse_file`
//!   - `index` — `collect_declarations` / `name_index` / `placeholder_base`
//!   - `source_map` — `file_type_references` and its span helpers
//!   - `resolve` — `resolve_item` (+ `grammar_def_for`)
//!   - `root` — `analyze` / `compute_associated_functions` + post-resolution passes
//!   - `helpers` — shared `TypeRegistry`/`ItemDefinition` construction

mod helpers;
mod index;
mod leaf;
mod resolve;
mod root;
mod source_map;

pub use index::{collect_declarations, name_index, placeholder_base};
pub use leaf::{parse_file, tokenize_file};
pub use resolve::resolve_item;
pub use root::{AssociatedFunctionsResult, analyze, compute_associated_functions};
pub use source_map::file_type_references;
