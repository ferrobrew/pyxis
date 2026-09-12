//! Output-writer abstraction for backend codegen.
//!
//! Backends generate and write one file at a time through an injected
//! [`OutputWriter`] instead of calling `std::fs` directly. Production uses
//! [`DiskWriter`] (streaming — one file in memory at a time, peak memory is
//! just the largest generated file); tests use [`MemoryWriter`], which buffers
//! generated files for inspection without touching the filesystem.
//!
//! The trait is a write sink: backends hand each generated file to the
//! writer, and the disk impl owns directory creation and io-error mapping in
//! one place.

use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

use crate::backends::BackendError;

/// A sink for generated code. Backends write each output file through this
/// abstraction; the writer decides where the bytes actually land.
///
/// Implementations must write `contents` to `path` atomically-enough for
/// codegen purposes. The production [`DiskWriter`] additionally creates all
/// parent directories of `path` before writing; in-memory implementations
/// have no directories to create.
pub trait OutputWriter {
    /// Write `contents` to `path`, creating parent directories as needed.
    fn write(&mut self, path: &Path, contents: &str) -> Result<(), BackendError>;
}

/// The production writer: streams each file to disk as it is generated.
///
/// A zero-sized type — no output is buffered, so peak memory matches a
/// hand-rolled `std::fs::write` call per file.
pub struct DiskWriter;

impl OutputWriter for DiskWriter {
    fn write(&mut self, path: &Path, contents: &str) -> Result<(), BackendError> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| BackendError::Io {
                error: e,
                context: format!("failed to create directory {}", parent.display()),
            })?;
        }
        std::fs::write(path, contents).map_err(|e| BackendError::Io {
            error: e,
            context: format!("failed to write {}", path.display()),
        })
    }
}

/// A test writer: buffers generated files in memory, keyed by path.
///
/// Deterministic (`files()` iterates in sorted path order) and inspectable,
/// so tests can assert on generated contents without a filesystem.
#[derive(Debug, Default)]
pub struct MemoryWriter {
    files: BTreeMap<PathBuf, String>,
}

impl OutputWriter for MemoryWriter {
    fn write(&mut self, path: &Path, contents: &str) -> Result<(), BackendError> {
        self.files.insert(path.to_path_buf(), contents.to_string());
        Ok(())
    }
}

impl MemoryWriter {
    /// The contents written for `path`, if any.
    pub fn file(&self, path: &Path) -> Option<&str> {
        self.files.get(path).map(|s| s.as_str())
    }

    /// All written files as `(path, contents)` pairs, sorted by path.
    pub fn files(&self) -> impl Iterator<Item = (&Path, &str)> {
        self.files
            .iter()
            .map(|(path, contents)| (path.as_path(), contents.as_str()))
    }

    /// Consume the writer, returning all written files keyed by path.
    pub fn into_files(self) -> BTreeMap<PathBuf, String> {
        self.files
    }

    /// Whether no files have been written yet.
    pub fn is_empty(&self) -> bool {
        self.files.is_empty()
    }
}
