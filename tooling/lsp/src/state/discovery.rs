use super::*;

impl ServerState {
    /// Extract workspace root paths from initialize params.
    /// Checks workspace_folders first, then falls back to root_uri.
    pub(super) fn extract_root_paths(
        params: &lsp_types::InitializeParams,
    ) -> Vec<std::path::PathBuf> {
        let mut roots = Vec::new();

        if let Some(folders) = &params.workspace_folders {
            for folder in folders {
                if let Some(path) = uri_to_file_path(&folder.uri) {
                    roots.push(path);
                }
            }
        }

        // `root_uri` is deprecated in favour of `workspace_folders`, but older
        // clients still only send it, so fall back to it.
        #[allow(deprecated)]
        if roots.is_empty()
            && let Some(root_uri) = &params.root_uri
            && let Some(path) = uri_to_file_path(root_uri)
        {
            roots.push(path);
        }

        roots
    }

    /// Scan a workspace root for pyxis.toml files. Each pyxis.toml defines a
    /// project; all .pyxis files under it (but not under a deeper pyxis.toml)
    /// belong to that project.
    pub(super) fn discover_projects(&mut self, root: &std::path::Path) {
        // Find all pyxis.toml files under the workspace root
        let mut config_files = Vec::new();
        Self::collect_files(root, "pyxis.toml", &mut config_files);

        for config_path in config_files {
            let project_root = config_path.parent().unwrap_or(root).to_path_buf();

            // Read pointer_size from pyxis.toml
            let pointer_size = if let Ok(config) = pyxis::config::Config::load(&config_path) {
                config.project.pointer_size
            } else {
                4
            };
            self.projects.insert(project_root.clone(), pointer_size);

            // Discover all .pyxis files under this project root
            let mut pyxis_files = Vec::new();
            Self::collect_pyxis_files(&project_root, &mut pyxis_files);
            for path in pyxis_files {
                self.register_discovered_file(&path, &project_root);
            }
        }
    }

    /// Register a discovered .pyxis file in the Salsa db.
    fn register_discovered_file(&mut self, path: &std::path::Path, project_root: &std::path::Path) {
        let Ok(source) = std::fs::read_to_string(path) else {
            return;
        };
        self.register_file(path.to_path_buf(), project_root.to_path_buf(), source);
    }

    /// Register a file's content into the db and document map. The single
    /// registration path shared by filesystem discovery and the in-memory
    /// (test) constructor — nothing here touches the disk.
    pub(super) fn register_file(
        &mut self,
        abs_path: std::path::PathBuf,
        project_root: std::path::PathBuf,
        source: String,
    ) {
        let relative_path = abs_path
            .strip_prefix(&project_root)
            .unwrap_or(&abs_path)
            .display()
            .to_string();

        // Skip if already registered (dedup by absolute path, not relative —
        // different projects can have the same relative path e.g. "types/math.pyxis").
        // Compared canonically so symlinked roots / `..` don't register a file twice.
        if self.find_document_by_abs_path(&abs_path).is_some() {
            return;
        }

        let file_id = self
            .file_store
            .register_in_memory(relative_path.clone(), source.clone());
        let source_file =
            SourceFile::new(&self.db, relative_path, file_id.as_u32(), source.clone());
        let uri = file_uri(&abs_path);

        self.documents.insert(
            uri.clone(),
            Document {
                source_file,
                file_id,
                content: source,
                project_root: Some(project_root),
                abs_path: Some(abs_path),
            },
        );
        self.file_id_to_uri.insert(file_id, uri);
    }

    /// Build a server entirely from in-memory project files — no filesystem
    /// access, for tests. Each project is `(root, pointer_size, files)` where
    /// `files` are `(relative_path, content)` pairs. Use [`ServerState::document_uri`]
    /// to address a registered file.
    #[allow(clippy::type_complexity)] // compact tuple shape is clearer than a named alias here
    pub fn in_memory(projects: &[(&str, usize, &[(&str, &str)])]) -> Self {
        let mut state = Self {
            db: PyxisDatabaseImpl::default(),
            file_store: FileStore::new(),
            documents: HashMap::new(),
            file_id_to_uri: HashMap::new(),
            projects: HashMap::new(),
        };
        for (root, pointer_size, files) in projects {
            let root_path = std::path::PathBuf::from(root);
            state.projects.insert(root_path.clone(), *pointer_size);
            for (rel, content) in *files {
                let abs = root_path.join(rel);
                state.register_file(abs, root_path.clone(), content.to_string());
            }
        }
        state
    }

    /// The URI a file registered via [`ServerState::in_memory`] (a project root
    /// plus a relative path) maps to.
    pub fn document_uri(root: &str, rel: &str) -> Uri {
        file_uri(&std::path::PathBuf::from(root).join(rel))
    }

    /// Find an already-tracked document whose absolute path refers to the same
    /// physical file as `abs_path`, comparing canonically. Discovery keys
    /// documents by a canonicalized URI, so a client's verbatim didOpen URI
    /// (symlinked root, `..`, different percent-encoding) may not match the key
    /// even though it's the same file — this dedup prevents a duplicate
    /// Document/FileId/SourceFile (and the spurious duplicate-definition errors
    /// that would follow).
    pub(super) fn find_document_by_abs_path(&self, abs_path: &std::path::Path) -> Option<Uri> {
        let target = canonical_path(abs_path);
        self.documents
            .iter()
            .find(|(_, d)| {
                d.abs_path
                    .as_deref()
                    .is_some_and(|p| canonical_path(p) == target)
            })
            .map(|(uri, _)| uri.clone())
    }

    /// Find the project root for a file by walking up the directory tree
    /// looking for a pyxis.toml file.
    pub(super) fn find_project_root(&self, path: &std::path::Path) -> Option<std::path::PathBuf> {
        // Pick the *innermost* known project root that is an ancestor of this
        // path — i.e. the longest matching prefix. `self.projects` is a HashMap,
        // so iterating without this would non-deterministically return an outer
        // project for nested layouts.
        if let Some(best) = self
            .projects
            .keys()
            .filter(|root| path.starts_with(root))
            .max_by_key(|root| root.components().count())
        {
            return Some(best.clone());
        }

        // Walk up the directory tree looking for pyxis.toml
        let mut current = path.parent();
        while let Some(dir) = current {
            if dir.join("pyxis.toml").exists() {
                return Some(dir.to_path_buf());
            }
            current = dir.parent();
        }
        None
    }

    /// Recursively collect all .pyxis file paths under a directory.
    fn collect_pyxis_files(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
        let entries = match std::fs::read_dir(dir) {
            Ok(e) => e,
            Err(_) => return,
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
                if name.starts_with('.') || name == "target" || name == "node_modules" {
                    continue;
                }
                Self::collect_pyxis_files(&path, out);
            } else if path.extension().and_then(|e| e.to_str()) == Some("pyxis") {
                out.push(path);
            }
        }
    }

    /// Recursively collect all files with a given name under a directory.
    fn collect_files(dir: &std::path::Path, filename: &str, out: &mut Vec<std::path::PathBuf>) {
        let entries = match std::fs::read_dir(dir) {
            Ok(e) => e,
            Err(_) => return,
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
                if name.starts_with('.') || name == "target" || name == "node_modules" {
                    continue;
                }
                Self::collect_files(&path, filename, out);
            } else if path.file_name().and_then(|n| n.to_str()) == Some(filename) {
                out.push(path);
            }
        }
    }
}
