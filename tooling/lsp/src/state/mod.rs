//! Server state — holds the Salsa database, FileStore, and open documents.

use std::collections::HashMap;

use lsp_types::Uri;
use pyxis::{
    semantic::{PyxisDatabaseImpl, SourceFile},
    source_store::FileStore,
    span::FileId,
};

mod diagnostics;
mod discovery;
mod lifecycle;
mod uri;

use uri::{canonical_path, file_uri, make_publish_diagnostics, uri_to_file_path, uri_to_filename};

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

    /// Get the document content for a URI
    pub fn get_content(&self, uri: &Uri) -> Option<&str> {
        self.documents.get(uri).map(|d| d.content.as_str())
    }

    /// Get the cached token stream for a URI (reuses the Salsa `tokenize_file`
    /// query that `parse_file` already ran — no re-tokenizing).
    pub fn tokens_for(&self, uri: &Uri) -> Option<std::sync::Arc<Vec<pyxis::tokenizer::Token>>> {
        let doc = self.documents.get(uri)?;
        Some(
            pyxis::semantic::tokenize_file(&self.db, doc.source_file)
                .tokens(&self.db)
                .clone(),
        )
    }

    /// Get the parsed module for a URI
    pub fn get_parsed_module(&self, uri: &Uri) -> Option<pyxis::grammar::Module> {
        let doc = self.documents.get(uri)?;
        let parsed = pyxis::semantic::parse_file(&self.db, doc.source_file);
        Some(parsed.module(&self.db).as_ref().clone())
    }
}
