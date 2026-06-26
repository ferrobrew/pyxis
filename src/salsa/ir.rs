//! Salsa tracked structs — the intermediate representation.
//!
//! ## `Update` / `Hash` strategy
//!
//! Salsa 0.27 tracked struct fields must implement `salsa::Update`, which
//! requires `Hash + Eq`. The existing Pyxis types (`grammar::Module`,
//! `TypeRegistry`, etc.) implement `Eq` but NOT `Hash`. There are two options:
//!
//! 1. Add `Hash` derive to all Pyxis types (invasive — many types).
//! 2. Use `interned` structs for values that need to be stored, OR use
//!    tracked functions that return `Arc<T>` where `T: Update`.
//!
//! For the spike, we test pattern 1: adding `Hash` to the types we need
//! to store in tracked structs. If this is too invasive, we fall back to
//! interning.

use std::sync::Arc;

/// A parsed file (grammar::Module + parse errors).
#[salsa::tracked]
pub struct ParsedFile<'db> {
    pub source: super::SourceFile,
    #[returns(ref)]
    pub module: Arc<crate::grammar::Module>,
}

/// The full resolved semantic state — the root query result.
/// For the spike, we only test that this compiles with Arc<T> fields
/// where T: Hash + Eq + Clone.
#[salsa::tracked]
pub struct SemanticAnalysis<'db> {
    #[returns(ref)]
    pub errors: Arc<Vec<crate::semantic::SemanticError>>,
}
