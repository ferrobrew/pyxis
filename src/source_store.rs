use std::path::PathBuf;

use crate::span::FileId;

/// Entry in the file store representing a source file.
#[derive(Debug)]
enum FileEntry {
    /// A file on the filesystem - content is read on demand.
    Path { filename: String, path: PathBuf },
    /// An in-memory file (for tests or internal use).
    InMemory { filename: String, content: String },
}

impl FileEntry {
    fn filename(&self) -> &str {
        match self {
            FileEntry::Path { filename, .. } => filename,
            FileEntry::InMemory { filename, .. } => filename,
        }
    }

    fn source(&self) -> Option<String> {
        match self {
            FileEntry::Path { path, .. } => std::fs::read_to_string(path).ok(),
            FileEntry::InMemory { content, .. } => Some(content.clone()),
        }
    }
}

/// A store that maps file IDs to filenames and provides access to source content.
///
/// Files are registered during compilation and assigned sequential IDs.
/// The store is shared across all compilation phases and used for error reporting.
#[derive(Debug)]
pub struct FileStore {
    files: Vec<FileEntry>,
}

impl Default for FileStore {
    fn default() -> Self {
        Self::new()
    }
}

impl FileStore {
    /// Create a new file store with predefined entries.
    pub fn new() -> Self {
        let mut store = Self { files: Vec::new() };

        // Register predefined files at known indices
        // FileId::INTERNAL = 0
        store.files.push(FileEntry::InMemory {
            filename: "<internal>".to_string(),
            content: String::new(),
        });

        // FileId::TEST = 1 (only meaningful in tests, but we always reserve the slot)
        store.files.push(FileEntry::InMemory {
            filename: "<test>".to_string(),
            content: String::new(),
        });

        store
    }

    /// Register a filesystem file and return its ID.
    ///
    /// The `filename` is the display name (e.g., relative path shown in errors),
    /// while `path` is the actual filesystem path to read content from.
    pub fn register_path(&mut self, filename: String, path: PathBuf) -> FileId {
        let id = FileId::new(self.files.len() as u32);
        self.files.push(FileEntry::Path { filename, path });
        id
    }

    /// Register an in-memory file and return its ID.
    ///
    /// This is useful for tests or dynamically generated content.
    pub fn register_in_memory(&mut self, filename: String, content: String) -> FileId {
        let id = FileId::new(self.files.len() as u32);
        self.files.push(FileEntry::InMemory { filename, content });
        id
    }

    /// Get the filename for a file ID.
    ///
    /// # Panics
    /// Panics if the file ID is invalid.
    pub fn filename(&self, id: FileId) -> &str {
        self.files[id.index()].filename()
    }

    /// Get the source content for a file ID.
    ///
    /// For filesystem files, this reads the file on demand.
    /// Returns `None` if the file cannot be read.
    pub fn source(&self, id: FileId) -> Option<String> {
        self.files.get(id.index()).and_then(|entry| entry.source())
    }
}
