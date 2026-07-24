use lsp_server::Notification;

use super::*;

impl ServerState {
    /// Handle textDocument/didOpen
    pub fn handle_did_open(
        &mut self,
        notif: Notification,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let params: lsp_types::DidOpenTextDocumentParams = serde_json::from_value(notif.params)?;
        let uri = params.text_document.uri.clone();
        let text = params.text_document.text;

        let fs_path = uri_to_file_path(&uri);

        // The file may already be tracked under a different URI key — discovery
        // canonicalizes paths, so the client's verbatim URI can differ. Match by
        // exact URI first, then by canonical absolute path, to avoid inserting a
        // duplicate Document/FileId/SourceFile for the same physical file.
        let existing_uri = if self.documents.contains_key(&uri) {
            Some(uri.clone())
        } else {
            fs_path
                .as_deref()
                .and_then(|path| self.find_document_by_abs_path(path))
        };

        if let Some(doc_uri) = existing_uri {
            // File was already discovered/opened — update its content in place
            // (the editor's version may differ from disk).
            let doc = self
                .documents
                .get_mut(&doc_uri)
                .expect("existing_uri came from documents");
            use pyxis::semantic::Setter;
            doc.source_file.set_contents(&mut self.db).to(text.clone());
            doc.content = text;
            self.file_store
                .update_in_memory(doc.file_id, doc.content.clone());
        } else {
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
}
