//! Server state — holds the Salsa database, FileStore, and open documents.

use std::collections::HashMap;

use lsp_server::Notification;
use lsp_types::{Diagnostic, PublishDiagnosticsParams, Uri};
use pyxis::salsa::{self, PyxisDatabaseImpl, SourceFile};
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
    /// Pointer size (default 4, could be config-derived in future)
    pub(crate) pointer_size: usize,
}

pub(crate) struct Document {
    pub(crate) source_file: SourceFile,
    pub(crate) file_id: FileId,
    pub(crate) content: String,
}

impl ServerState {
    pub fn new() -> Self {
        Self {
            db: PyxisDatabaseImpl::default(),
            file_store: FileStore::new(),
            documents: HashMap::new(),
            file_id_to_uri: HashMap::new(),
            pointer_size: 4,
        }
    }

    /// Handle textDocument/didOpen
    pub fn handle_did_open(&mut self, notif: Notification) -> Result<(), Box<dyn std::error::Error>> {
        let params: lsp_types::DidOpenTextDocumentParams =
            serde_json::from_value(notif.params)?;
        let uri = params.text_document.uri.clone();
        let text = params.text_document.text;
        let filename = uri_to_filename(&uri);
        let file_id = self.file_store.register_in_memory(filename, text.clone());

        let source_file = SourceFile::new(
            &self.db,
            uri_to_filename(&uri),
            file_id.as_u32(),
            text.clone(),
        );

        self.documents.insert(
            uri.clone(),
            Document {
                source_file,
                file_id,
                content: text,
            },
        );
        self.file_id_to_uri.insert(file_id, uri);
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
                use pyxis::salsa::Setter;
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
                use pyxis::salsa::Setter;
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

    /// Run the Salsa analyze query and collect diagnostics
    pub fn collect_diagnostics(&self) -> Vec<Notification> {
        let sources = self.sources();
        if sources.is_empty() {
            return vec![];
        }

        let source_set = salsa::SourceSet::new(&self.db, sources);
        let analysis = salsa::analyze(&self.db, self.pointer_size, source_set);

        let mut notifications = Vec::new();

        // Collect parse errors
        for parse_err in analysis.parse_errors(&self.db).iter() {
            let loc = parse_err.location();
            if let Some(notif) = self.error_to_notification(loc, &parse_err.to_string()) {
                notifications.push(notif);
            }
        }

        // Collect semantic errors
        for sem_err in analysis.errors(&self.db).iter() {
            if let Some(loc) = sem_err.location() {
                if let Some(notif) = self.error_to_notification(loc, &sem_err.to_string()) {
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
        let parsed = salsa::parse_file(&self.db, doc.source_file);
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
