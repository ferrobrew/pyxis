//! Server state — holds the Salsa database, FileStore, and open documents.

use std::{collections::HashMap, str::FromStr};

use lsp_server::Notification;
use lsp_types::{Diagnostic, PublishDiagnosticsParams, Uri};
use pyxis::semantic::{self, PyxisDatabaseImpl, SourceFile};
use pyxis::source_store::FileStore;
use pyxis::span::FileId;
use pyxis::span::HasLocation;

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
    /// The project root this file belongs to (for determining pointer_size)
    pub(crate) project_root: Option<std::path::PathBuf>,
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
        if let Some(project_root) = &doc.project_root {
            if let Some(&ps) = self.projects.get(project_root) {
                return ps;
            }
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

        if roots.is_empty() {
            if let Some(root_uri) = &params.root_uri {
                if let Some(path) = uri_to_file_path(root_uri) {
                    roots.push(path);
                }
            }
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
        let source = match std::fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => return,
        };

        let relative_path = path
            .strip_prefix(project_root)
            .unwrap_or(path)
            .display()
            .to_string();

        // Skip if already registered
        if self
            .documents
            .values()
            .any(|d| self.file_store.filename(d.file_id) == relative_path)
        {
            return;
        }

        let file_id = self
            .file_store
            .register_in_memory(relative_path.clone(), source.clone());
        let source_file = SourceFile::new(
            &self.db,
            relative_path,
            file_id.as_u32(),
            source.clone(),
        );

        let uri = file_path_to_uri(path).unwrap_or_else(|| {
            let uri_str = format!("file:///{}", path.display());
            Uri::from_str(&uri_str).unwrap()
        });

        self.documents.insert(
            uri.clone(),
            Document {
                source_file,
                file_id,
                content: source,
                project_root: Some(project_root.to_path_buf()),
            },
        );
        self.file_id_to_uri.insert(file_id, uri);
    }

    /// Find the project root for a file by walking up the directory tree
    /// looking for a pyxis.toml file.
    fn find_project_root(&self, path: &std::path::Path) -> Option<std::path::PathBuf> {
        // First check if any known project root is an ancestor of this path
        for project_root in self.projects.keys() {
            if path.starts_with(project_root) {
                return Some(project_root.clone());
            }
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
    pub fn handle_did_open(&mut self, notif: Notification) -> Result<(), Box<dyn std::error::Error>> {
        let params: lsp_types::DidOpenTextDocumentParams =
            serde_json::from_value(notif.params)?;
        let uri = params.text_document.uri.clone();
        let text = params.text_document.text;

        if let Some(doc) = self.documents.get_mut(&uri) {
            // File was already discovered during workspace scan — update its
            // content in place (the editor's version may differ from disk).
            use pyxis::semantic::Setter;
            doc.source_file.set_contents(&mut self.db).to(text.clone());
            doc.content = text;
            self.file_store.update_in_memory(doc.file_id, doc.content.clone());
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

            let file_id = self.file_store.register_in_memory(relative_path.clone(), text.clone());

            let source_file = SourceFile::new(
                &self.db,
                relative_path,
                file_id.as_u32(),
                text.clone(),
            );

            self.documents.insert(
                uri.clone(),
                Document {
                    source_file,
                    file_id,
                    content: text,
                    project_root,
                },
            );
            self.file_id_to_uri.insert(file_id, uri);
        }
        Ok(())
    }

    /// Handle textDocument/didChange (FULL sync)
    pub fn handle_did_change(&mut self, notif: Notification) -> Result<(), Box<dyn std::error::Error>> {
        let params: lsp_types::DidChangeTextDocumentParams =
            serde_json::from_value(notif.params)?;
        let uri = params.text_document.uri.clone();

        if let Some(doc) = self.documents.get_mut(&uri) {
            if let Some(change) = params.content_changes.into_iter().last() {
                doc.content = change.text;
                let new_content = doc.content.clone();
                use pyxis::semantic::Setter;
                doc.source_file.set_contents(&mut self.db).to(new_content);
                self.file_store.update_in_memory(doc.file_id, doc.content.clone());
            }
        }
        Ok(())
    }

    /// Handle textDocument/didSave
    pub fn handle_did_save(&mut self, notif: Notification) -> Result<(), Box<dyn std::error::Error>> {
        let params: lsp_types::DidSaveTextDocumentParams =
            serde_json::from_value(notif.params)?;
        let uri = params.text_document.uri;

        if let Some(text) = params.text {
            if let Some(doc) = self.documents.get_mut(&uri) {
                doc.content = text.clone();
                use pyxis::semantic::Setter;
                doc.source_file.set_contents(&mut self.db).to(text);
                self.file_store.update_in_memory(doc.file_id, doc.content.clone());
            }
        }
        Ok(())
    }

    /// Handle textDocument/didClose
    pub fn handle_did_close(&mut self, notif: Notification) -> Result<(), Box<dyn std::error::Error>> {
        let params: lsp_types::DidCloseTextDocumentParams =
            serde_json::from_value(notif.params)?;
        self.documents.remove(&params.text_document.uri);
        Ok(())
    }

    /// Collect all sources for the analyze() query
    pub(crate) fn sources(&self) -> Vec<SourceFile> {
        self.documents.values().map(|d| d.source_file).collect()
    }

    /// Compute the module path for a document from its SourceFile.path
    /// (which is project-relative, e.g. "world/weather.pyxis").
    /// This mirrors collect_declarations' ItemPath::from_path derivation.
    pub(crate) fn module_path_for(&self, uri: &Uri) -> Option<pyxis::grammar::ItemPath> {
        let doc = self.documents.get(uri)?;
        let path_str = doc.source_file.path(&self.db);
        Some(pyxis::grammar::ItemPath::from_path(std::path::Path::new(path_str.as_str())))
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
            if let Some(root) = project_root {
                if let Some(&ps) = self.projects.get(*root) {
                    *pointer_size = ps;
                }
            }
        }

        let mut notifications = Vec::new();

        for (_, (sources, pointer_size)) in project_groups {
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
                } else if let Some(loc) = sem_err.location() {
                    if let Some(notif) = self.error_to_notification(loc, &sem_err.to_string()) {
                        notifications.push(notif);
                    }
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
        PublishDiagnosticsParams { uri, diagnostics, version: None },
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
    let path_str = if path_str.starts_with('/') && path_str.len() > 2 && path_str.as_bytes()[2] == b':' {
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

/// Simple percent-decoding for file paths.
fn percent_decode(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '%' {
            let h1 = chars.next();
            let h2 = chars.next();
            if let (Some(h1), Some(h2)) = (h1, h2) {
                if let Ok(byte) = u8::from_str_radix(&format!("{h1}{h2}"), 16) {
                    result.push(byte as char);
                    continue;
                }
                result.push('%');
                result.push(h1);
                result.push(h2);
            } else {
                result.push('%');
            }
        } else {
            result.push(c);
        }
    }
    result
}

/// Simple percent-encoding for file paths.
fn percent_encode(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for byte in s.bytes() {
        match byte {
            b' ' | b'<' | b'>' | b'#' | b'%' | b'"' | b'{' | b'}' | b'|' | b'\\' | b'^' | b'['
            | b']' | b'`' => {
                result.push_str(&format!("%{:02X}", byte));
            }
            _ => result.push(byte as char),
        }
    }
    result
}
