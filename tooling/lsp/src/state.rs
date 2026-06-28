//! Server state — holds the Salsa database, FileStore, and open documents.

use std::{collections::HashMap, str::FromStr};

use lsp_server::Notification;
use lsp_types::{Diagnostic, PublishDiagnosticsParams, Uri};
use pyxis::{
    semantic::{self, PyxisDatabaseImpl, SourceFile},
    source_store::FileStore,
    span::{FileId, HasLocation},
};

pub struct ServerState {
    /// Salsa database — the query graph
    pub(crate) db: PyxisDatabaseImpl,
    /// Long-lived FileStore — the sole authority for FileId creation
    file_store: FileStore,
    /// URI → (SourceFile, FileId, content)
    pub(crate) documents: HashMap<Uri, Document>,
    /// FileId → URI, for converting diagnostic ItemLocations to LSP Locations
    file_id_to_uri: HashMap<FileId, Uri>,
    /// Map from project root path → project config (pointer_size)
    projects: HashMap<std::path::PathBuf, usize>,
}

pub(crate) struct Document {
    pub(crate) source_file: SourceFile,
    pub(crate) file_id: FileId,
    pub(crate) content: String,
    /// The project root this file belongs to (for determining pointer_size
    /// and grouping sources for analyze())
    pub(crate) project_root: Option<std::path::PathBuf>,
    /// The absolute filesystem path of this file (for dedup)
    pub(crate) abs_path: Option<std::path::PathBuf>,
}

impl ServerState {
    pub fn new(initialize_params: &serde_json::Value) -> Result<Self, Box<dyn std::error::Error>> {
        let params: lsp_types::InitializeParams =
            serde_json::from_value(initialize_params.clone())?;

        // Determine workspace root paths
        let root_paths = Self::extract_root_paths(&params);

        let mut state = Self {
            db: PyxisDatabaseImpl::default(),
            file_store: FileStore::new(),
            documents: HashMap::new(),
            file_id_to_uri: HashMap::new(),
            projects: HashMap::new(),
        };

        // Discover all pyxis.toml files and their associated .pyxis files
        for root in &root_paths {
            state.discover_projects(root);
        }

        Ok(state)
    }

    /// Get the pointer_size for a given file URI by finding its project root.
    pub(crate) fn pointer_size_for(&self, uri: &Uri) -> usize {
        let Some(doc) = self.documents.get(uri) else {
            return 4;
        };
        if let Some(project_root) = &doc.project_root
            && let Some(&ps) = self.projects.get(project_root)
        {
            return ps;
        }
        4
    }

    /// Extract workspace root paths from initialize params.
    /// Checks workspace_folders first, then falls back to root_uri.
    fn extract_root_paths(params: &lsp_types::InitializeParams) -> Vec<std::path::PathBuf> {
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
    fn discover_projects(&mut self, root: &std::path::Path) {
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
    fn register_file(
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
        // different projects can have the same relative path e.g. "types/math.pyxis")
        if self
            .documents
            .values()
            .any(|d| d.abs_path.as_ref() == Some(&abs_path))
        {
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

    /// Find the project root for a file by walking up the directory tree
    /// looking for a pyxis.toml file.
    fn find_project_root(&self, path: &std::path::Path) -> Option<std::path::PathBuf> {
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

    /// Handle textDocument/didOpen
    pub fn handle_did_open(
        &mut self,
        notif: Notification,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let params: lsp_types::DidOpenTextDocumentParams = serde_json::from_value(notif.params)?;
        let uri = params.text_document.uri.clone();
        let text = params.text_document.text;

        if let Some(doc) = self.documents.get_mut(&uri) {
            // File was already discovered during workspace scan — update its
            // content in place (the editor's version may differ from disk).
            use pyxis::semantic::Setter;
            doc.source_file.set_contents(&mut self.db).to(text.clone());
            doc.content = text;
            self.file_store
                .update_in_memory(doc.file_id, doc.content.clone());
        } else {
            let fs_path = uri_to_file_path(&uri);

            // Try to find the project root for this file
            let project_root = fs_path
                .as_ref()
                .and_then(|path| self.find_project_root(path));

            // Compute the project-relative path (e.g. "world/weather.pyxis")
            // so collect_declarations derives the correct module path.
            // Falls back to the filename if no project root is known.
            let relative_path = match (&fs_path, &project_root) {
                (Some(path), Some(root)) => path
                    .strip_prefix(root)
                    .unwrap_or(path)
                    .display()
                    .to_string(),
                _ => uri_to_filename(&uri),
            };

            let file_id = self
                .file_store
                .register_in_memory(relative_path.clone(), text.clone());

            let source_file =
                SourceFile::new(&self.db, relative_path, file_id.as_u32(), text.clone());

            self.documents.insert(
                uri.clone(),
                Document {
                    source_file,
                    file_id,
                    content: text,
                    project_root,
                    abs_path: fs_path,
                },
            );
            self.file_id_to_uri.insert(file_id, uri);
        }
        Ok(())
    }

    /// Handle textDocument/didChange (FULL sync)
    pub fn handle_did_change(
        &mut self,
        notif: Notification,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let params: lsp_types::DidChangeTextDocumentParams = serde_json::from_value(notif.params)?;
        let uri = params.text_document.uri.clone();

        if let Some(doc) = self.documents.get_mut(&uri)
            && let Some(change) = params.content_changes.into_iter().last()
        {
            doc.content = change.text;
            let new_content = doc.content.clone();
            use pyxis::semantic::Setter;
            doc.source_file.set_contents(&mut self.db).to(new_content);
            self.file_store
                .update_in_memory(doc.file_id, doc.content.clone());
        }
        Ok(())
    }

    /// Handle textDocument/didSave
    ///
    /// We deliberately do NOT update the document's content from `params.text`,
    /// even though we request `include_text` in the save capability.
    ///
    /// Root cause of issue 4 (save clearing a live parse error): with FULL text
    /// sync, `textDocument/didChange` is the *authoritative* and most up-to-date
    /// source of the buffer's content — it is applied synchronously as the user
    /// types. The `text` included on a save is the on-disk snapshot, which can
    /// lag or differ from the live buffer (clients may send a stale/normalized
    /// copy, or one captured before the corrupt edit was flushed to disk).
    ///
    /// If we let the save text overwrite the synced buffer, saving a corrupt
    /// file could clobber the (correct, corrupt) didChange content with stale
    /// clean text, making the parse-error diagnostic incorrectly disappear.
    ///
    /// So a save is a no-op for content; it only triggers a re-publish of
    /// diagnostics (done by the caller in main_loop), using the content the
    /// editor already kept in sync via didChange.
    pub fn handle_did_save(
        &mut self,
        notif: Notification,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // Parse to validate the payload, but intentionally ignore `text`.
        let _params: lsp_types::DidSaveTextDocumentParams = serde_json::from_value(notif.params)?;
        Ok(())
    }

    /// Handle textDocument/didClose
    ///
    /// We do NOT remove the document from the database. The file still exists
    /// on disk and is needed for cross-file analysis (go-to-definition, hover,
    /// diagnostics). Removing it would cause other files' use-statements and
    /// type references to fail resolution. The editor will re-send did_open
    /// if the file is opened again.
    pub fn handle_did_close(
        &mut self,
        notif: Notification,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let _params: lsp_types::DidCloseTextDocumentParams = serde_json::from_value(notif.params)?;
        // Intentionally a no-op — keep the file in the db.
        Ok(())
    }

    /// React to on-disk changes the editor reports (created/changed/deleted
    /// `.pyxis` files and `pyxis.toml`). Important for agent-driven edits, where
    /// files are rewritten on disk outside the editor's open buffers.
    pub fn handle_did_change_watched_files(
        &mut self,
        notif: Notification,
    ) -> Result<(), Box<dyn std::error::Error>> {
        use lsp_types::FileChangeType;
        let params: lsp_types::DidChangeWatchedFilesParams = serde_json::from_value(notif.params)?;
        for change in params.changes {
            let Some(path) = uri_to_file_path(&change.uri) else {
                continue;
            };
            let is_toml = path.file_name().and_then(|n| n.to_str()) == Some("pyxis.toml");
            let is_pyxis = path.extension().and_then(|e| e.to_str()) == Some("pyxis");
            if !is_toml && !is_pyxis {
                continue;
            }

            // Locate any document we already track for this path (match by
            // absolute path; the client's URI form may differ from our key).
            let existing = self
                .documents
                .iter()
                .find(|(_, d)| d.abs_path.as_deref() == Some(path.as_path()))
                .map(|(u, _)| u.clone());

            if change.typ == FileChangeType::DELETED {
                if let Some(uri) = existing
                    && let Some(doc) = self.documents.remove(&uri)
                {
                    self.file_id_to_uri.remove(&doc.file_id);
                }
                continue;
            }

            // CREATED or CHANGED.
            if is_toml {
                // A new/changed project config — (re)discover its project.
                if let Some(dir) = path.parent() {
                    self.discover_projects(dir);
                }
                continue;
            }

            let Ok(source) = std::fs::read_to_string(&path) else {
                continue;
            };
            if let Some(uri) = existing {
                if let Some(doc) = self.documents.get_mut(&uri)
                    && doc.content != source
                {
                    use pyxis::semantic::Setter;
                    doc.source_file
                        .set_contents(&mut self.db)
                        .to(source.clone());
                    doc.content = source.clone();
                    self.file_store.update_in_memory(doc.file_id, source);
                }
            } else if let Some(root) = self.find_project_root(&path) {
                self.register_file(path, root, source);
            }
        }
        Ok(())
    }

    /// Collect sources belonging to the same project as the given URI.
    /// This ensures cross-module features (hover, go-to-def, etc.) only
    /// analyze files within the same project — important for monorepos
    /// where multiple projects share module paths like `types::math`.
    pub(crate) fn sources_for(&self, uri: &Uri) -> Vec<SourceFile> {
        let target_root = self
            .documents
            .get(uri)
            .and_then(|d| d.project_root.as_ref());

        self.documents
            .values()
            .filter(|d| d.project_root.as_ref() == target_root)
            .map(|d| d.source_file)
            .collect()
    }

    /// Compute the module path for a document from its SourceFile.path
    /// (which is project-relative, e.g. "world/weather.pyxis").
    /// This mirrors collect_declarations' ItemPath::from_path derivation.
    pub(crate) fn module_path_for(&self, uri: &Uri) -> Option<pyxis::grammar::ItemPath> {
        let doc = self.documents.get(uri)?;
        let path_str = doc.source_file.path(&self.db);
        Some(pyxis::grammar::ItemPath::from_path(std::path::Path::new(
            path_str.as_str(),
        )))
    }

    /// Run the Salsa analyze query and collect diagnostics.
    /// Groups documents by project root so each project is analyzed
    /// independently with its own pointer_size.
    pub fn collect_diagnostics(&self) -> Vec<Notification> {
        if self.documents.is_empty() {
            return vec![];
        }

        // Group sources by project root (None → default project)
        let mut project_groups: HashMap<Option<&std::path::PathBuf>, (Vec<SourceFile>, usize)> =
            HashMap::new();
        for doc in self.documents.values() {
            let entry = project_groups
                .entry(doc.project_root.as_ref())
                .or_insert_with(|| (Vec::new(), 4));
            entry.0.push(doc.source_file);
        }

        // Set pointer_size for each group based on the project config
        for (project_root, (_, pointer_size)) in project_groups.iter_mut() {
            if let Some(root) = project_root
                && let Some(&ps) = self.projects.get(*root)
            {
                *pointer_size = ps;
            }
        }

        let mut notifications = Vec::new();

        for (_project_root, (sources, pointer_size)) in project_groups {
            if sources.is_empty() {
                continue;
            }
            let source_set = semantic::SourceSet::new(&self.db, sources);
            let analysis = semantic::analyze(&self.db, pointer_size, source_set);

            // Collect parse errors
            for parse_err in analysis.parse_errors(&self.db).iter() {
                let loc = parse_err.location();
                if let Some(notif) = self.error_to_notification(loc, &parse_err.to_string()) {
                    notifications.push(notif);
                }
            }

            // Collect semantic errors
            for sem_err in analysis.errors(&self.db).iter() {
                // TypeResolutionStalled may contain multiple unresolved references;
                // emit one diagnostic per reference for better UX.
                if let pyxis::semantic::SemanticError::TypeResolutionStalled {
                    unresolved_references,
                    ..
                } = sem_err
                {
                    for r in unresolved_references {
                        let msg = format!("{}: Type not found: `{}`", r.location, r.type_name);
                        if let Some(notif) = self.error_to_notification(&r.location, &msg) {
                            notifications.push(notif);
                        }
                    }
                } else if let Some(loc) = sem_err.location()
                    && let Some(notif) = self.error_to_notification(loc, &sem_err.to_string())
                {
                    notifications.push(notif);
                }
            }
        }

        // If no errors, publish empty diagnostics for all open documents
        if notifications.is_empty() {
            for uri in self.documents.keys() {
                notifications.push(make_publish_diagnostics(uri.clone(), vec![]));
            }
        }

        notifications
    }

    /// Convert a Pyxis error location to an LSP publishDiagnostics notification
    fn error_to_notification(
        &self,
        location: &pyxis::span::ItemLocation,
        message: &str,
    ) -> Option<Notification> {
        let uri = self.file_id_to_uri.get(&location.file_id)?;
        let content = self.documents.get(uri)?.content.clone();
        let range = crate::span::pyxis_span_to_lsp_range(&content, &location.span);
        // Parse/lexer errors are often zero-width (EOF, unexpected char); widen
        // so editors draw an inline squiggle rather than only listing them.
        let range = crate::span::widen_empty_range(&content, range);
        let diagnostic = Diagnostic {
            range,
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            message: message.to_string(),
            source: Some("pyxis".to_string()),
            ..Default::default()
        };
        Some(make_publish_diagnostics(uri.clone(), vec![diagnostic]))
    }

    /// Get the document content for a URI
    pub fn get_content(&self, uri: &Uri) -> Option<&str> {
        self.documents.get(uri).map(|d| d.content.as_str())
    }

    /// Get the cached token stream for a URI (reuses the Salsa `tokenize_file`
    /// query that `parse_file` already ran — no re-tokenizing).
    pub fn tokens_for(&self, uri: &Uri) -> Option<std::sync::Arc<Vec<pyxis::tokenizer::Token>>> {
        let doc = self.documents.get(uri)?;
        Some(
            semantic::tokenize_file(&self.db, doc.source_file)
                .tokens(&self.db)
                .clone(),
        )
    }

    /// Get the parsed module for a URI
    pub fn get_parsed_module(&self, uri: &Uri) -> Option<pyxis::grammar::Module> {
        let doc = self.documents.get(uri)?;
        let parsed = semantic::parse_file(&self.db, doc.source_file);
        Some(parsed.module(&self.db).as_ref().clone())
    }
}

fn make_publish_diagnostics(uri: Uri, diagnostics: Vec<Diagnostic>) -> Notification {
    Notification::new(
        "textDocument/publishDiagnostics".into(),
        PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: None,
        },
    )
}

fn uri_to_filename(uri: &Uri) -> String {
    // Strip the file:// prefix and return a simple relative path
    let s = uri.as_str();
    let path = s.strip_prefix("file://").unwrap_or(s);
    // For test URIs like "file:///test.pyxis", return just "test.pyxis"
    path.rsplit('/').next().unwrap_or(path).to_string()
}

/// Convert a `file://` URI to a filesystem path.
fn uri_to_file_path(uri: &Uri) -> Option<std::path::PathBuf> {
    let s = uri.as_str();
    let path_str = s.strip_prefix("file://")?;
    // On Unix, the path is already absolute (e.g. /home/user/project)
    // On Windows, file:// URIs use /C:/... format
    let path_str =
        if path_str.starts_with('/') && path_str.len() > 2 && path_str.as_bytes()[2] == b':' {
            // Windows: /C:/... → C:/...
            &path_str[1..]
        } else {
            path_str
        };
    // Decode percent-encoding for spaces etc.
    let decoded = percent_decode(path_str);
    let path = std::path::PathBuf::from(decoded);
    if path.exists() || path.parent().is_some() {
        Some(path)
    } else {
        None
    }
}

/// Convert a filesystem path to a `file://` URI.
fn file_path_to_uri(path: &std::path::Path) -> Option<Uri> {
    let absolute = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let s = absolute.to_string_lossy();
    let encoded = percent_encode(&s);
    let uri_str = format!("file://{}", encoded);
    Uri::from_str(&uri_str).ok()
}

/// The `file://` URI for a path. Never panics: `percent_encode` guarantees
/// ASCII output, so the formatted string always parses as a `Uri`. The
/// fallback covers synthetic in-memory test paths that don't canonicalize.
fn file_uri(path: &std::path::Path) -> Uri {
    if let Some(uri) = file_path_to_uri(path) {
        return uri;
    }
    let encoded = percent_encode(&path.display().to_string());
    let uri_str = format!("file:///{}", encoded.trim_start_matches('/'));
    Uri::from_str(&uri_str)
        .unwrap_or_else(|_| Uri::from_str("file:///").expect("`file:///` is a valid URI"))
}

/// Percent-decoding for file paths. Decodes `%XX` escapes to raw bytes and
/// reassembles them as UTF-8 — accumulating bytes (not chars) so a multi-byte
/// sequence like `%C3%A9` round-trips to `é` rather than mojibake.
fn percent_decode(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out: Vec<u8> = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%'
            && i + 2 < bytes.len()
            && let Ok(decoded) = std::str::from_utf8(&bytes[i + 1..i + 3])
                .ok()
                .and_then(|hex| u8::from_str_radix(hex, 16).ok())
                .ok_or(())
        {
            out.push(decoded);
            i += 3;
        } else {
            out.push(bytes[i]);
            i += 1;
        }
    }
    String::from_utf8_lossy(&out).into_owned()
}

/// Percent-encoding for file paths. Always produces ASCII output: besides the
/// reserved/unsafe ASCII characters, every control byte and every non-ASCII
/// (UTF-8 continuation/lead) byte is encoded. This is essential — a `Uri` must
/// be ASCII, and emitting `byte as char` for a multi-byte UTF-8 sequence would
/// both corrupt the path and produce a string `Uri::from_str` rejects.
fn percent_encode(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for byte in s.bytes() {
        match byte {
            b' ' | b'<' | b'>' | b'#' | b'%' | b'"' | b'{' | b'}' | b'|' | b'\\' | b'^' | b'['
            | b']' | b'`' => result.push_str(&format!("%{byte:02X}")),
            // Control bytes (incl. DEL) and all non-ASCII bytes.
            0x00..=0x1F | 0x7F..=0xFF => result.push_str(&format!("%{byte:02X}")),
            _ => result.push(byte as char),
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn percent_encode_is_ascii_and_encodes_non_ascii() {
        let encoded = percent_encode("/tmp/café/日本語.pyxis");
        assert!(
            encoded.is_ascii(),
            "encoded output must be ASCII: {encoded}"
        );
        assert!(
            encoded.contains("%C3%A9"),
            "é should be percent-encoded UTF-8"
        );
        // Round-trips back to the original bytes.
        assert_eq!(percent_decode(&encoded), "/tmp/café/日本語.pyxis");
    }

    #[test]
    fn file_uri_does_not_panic_on_non_ascii_paths() {
        // Regression: file_uri previously panicked (Uri::from_str(...).unwrap())
        // for any path with a non-ASCII component.
        let uri = file_uri(std::path::Path::new("/tmp/工程/café.pyxis"));
        assert!(uri.as_str().starts_with("file:///"));
        assert!(uri.as_str().is_ascii());
    }
}
