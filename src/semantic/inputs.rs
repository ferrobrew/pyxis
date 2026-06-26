//! Salsa input structs — the base inputs to the query graph.

use std::sync::Arc;

/// A source file. The LSP updates `contents` on didChange;
/// Salsa invalidates downstream queries automatically.
#[salsa::input]
pub struct SourceFile {
    /// Relative path, e.g. "world/weather.pyxis"
    pub path: String,
    /// Stable FileId for ItemLocation stamping (the `u32` from `FileId::as_u32()`)
    pub file_id: u32,
    /// File contents (source text)
    #[returns(ref)]
    pub contents: String,
}

/// Project configuration (from `pyxis.toml`)
#[salsa::input]
pub struct ProjectConfig {
    /// Pointer size in bytes (4 or 8), from the project config
    pub pointer_size: usize,
    /// Project name (used for crate name generation)
    #[returns(ref)]
    pub project_name: String,
}

/// An interned set of source files. This wraps `Vec<SourceFile>` so it can
/// be passed as a parameter to tracked functions (salsa requires the second
/// parameter to be a salsa struct, and `Vec<SourceFile>` doesn't implement
/// `SalsaStructInDb`).
#[salsa::interned]
pub struct SourceSet {
    #[returns(ref)]
    pub sources: Vec<SourceFile>,
}
