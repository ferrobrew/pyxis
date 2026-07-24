use std::collections::HashMap;

use lsp_server::Notification;
use lsp_types::{Diagnostic, Uri};
use pyxis::{semantic, span::HasLocation};

use super::*;

impl ServerState {
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

        // Accumulate every diagnostic by URI across all projects. `publishDiagnostics`
        // replaces the whole set for a URI, so we must aggregate per file rather than
        // emit one notification per error (which would only show the last one) and
        // would leave already-fixed files showing stale squiggles.
        // (`Uri`'s interior mutability is a cache that doesn't affect hashing, so
        // it's a sound map key — same as the `documents` field above.)
        #[allow(clippy::mutable_key_type)]
        let mut by_uri: HashMap<Uri, Vec<Diagnostic>> = HashMap::new();

        for (_project_root, (sources, pointer_size)) in project_groups {
            if sources.is_empty() {
                continue;
            }
            let source_set = semantic::SourceSet::new(&self.db, sources);
            let analysis = semantic::analyze(&self.db, pointer_size, source_set);

            // Collect parse errors
            for parse_err in analysis.parse_errors(&self.db).iter() {
                let loc = parse_err.location();
                if let Some((uri, diag)) = self.error_to_diagnostic(loc, &parse_err.to_string()) {
                    by_uri.entry(uri).or_default().push(diag);
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
                        if let Some((uri, diag)) = self.error_to_diagnostic(&r.location, &msg) {
                            by_uri.entry(uri).or_default().push(diag);
                        }
                    }
                } else if let Some(loc) = sem_err.location()
                    && let Some((uri, diag)) = self.error_to_diagnostic(loc, &sem_err.to_string())
                {
                    by_uri.entry(uri).or_default().push(diag);
                }
            }
        }

        // Emit exactly one publishDiagnostics per tracked document — including
        // documents with no diagnostics, so a now-clean file's stale squiggles
        // are cleared. Sort documents by URI and each file's diagnostics by range
        // so the output is deterministic for tests/snapshots.
        let mut uris: Vec<&Uri> = self.documents.keys().collect();
        uris.sort_by(|a, b| a.as_str().cmp(b.as_str()));

        let mut notifications = Vec::with_capacity(uris.len());
        for uri in uris {
            let mut diagnostics = by_uri.remove(uri).unwrap_or_default();
            diagnostics.sort_by_key(|d| {
                (
                    d.range.start.line,
                    d.range.start.character,
                    d.range.end.line,
                    d.range.end.character,
                )
            });
            notifications.push(make_publish_diagnostics(uri.clone(), diagnostics));
        }

        notifications
    }

    /// Convert a Pyxis error location to a `(Uri, Diagnostic)` pair so the caller
    /// can aggregate multiple diagnostics per URI before publishing.
    fn error_to_diagnostic(
        &self,
        location: &pyxis::span::ItemLocation,
        message: &str,
    ) -> Option<(Uri, Diagnostic)> {
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
        Some((uri.clone(), diagnostic))
    }
}
