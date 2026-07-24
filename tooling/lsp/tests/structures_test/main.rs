//! Regression tests for precise, structure-aware hover/go-to-definition:
//! type names, fields, vftable entries, impl methods, impl targets, cfg-gated
//! `use`s — and robustness when a type has a semantic error (mid-edit `#[size]`).

mod helpers;
mod hover;
mod imports_completion;
mod navigation_features;
mod references_rename;
