use std::collections::BTreeMap;

/// Trait for providing source code for error reporting
pub trait SourceStore {
    /// Get the source code for a given filename
    fn get(&mut self, filename: &str) -> Option<&str>;
}

/// In-memory source store
#[derive(Debug, Default)]
pub struct InMemorySourceStore {
    sources: BTreeMap<String, String>,
}

impl InMemorySourceStore {
    pub fn new() -> Self {
        Self {
            sources: BTreeMap::new(),
        }
    }

    pub fn insert(&mut self, filename: String, source: String) {
        self.sources.insert(filename, source);
    }
}

impl SourceStore for InMemorySourceStore {
    fn get(&mut self, filename: &str) -> Option<&str> {
        self.sources.get(filename).map(|s| s.as_str())
    }
}

/// Filesystem source store that reads files on demand and caches them
#[derive(Debug, Default)]
pub struct FilesystemSourceStore {
    cache: BTreeMap<String, String>,
}

impl FilesystemSourceStore {
    pub fn new() -> Self {
        Self {
            cache: BTreeMap::new(),
        }
    }
}

impl SourceStore for FilesystemSourceStore {
    fn get(&mut self, filename: &str) -> Option<&str> {
        // Check if already cached
        if self.cache.contains_key(filename) {
            return self.cache.get(filename).map(|s| s.as_str());
        }

        // Try to read from filesystem
        if let Ok(content) = std::fs::read_to_string(filename) {
            self.cache.insert(filename.to_string(), content);
            self.cache.get(filename).map(|s| s.as_str())
        } else {
            None
        }
    }
}

impl SourceStore for BTreeMap<String, String> {
    fn get(&mut self, filename: &str) -> Option<&str> {
        BTreeMap::get(self, filename).map(|s| s.as_str())
    }
}
