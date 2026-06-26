//! Salsa input structs — the base inputs to the query graph.

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
