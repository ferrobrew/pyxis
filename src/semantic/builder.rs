//! SemanticBuilder — a thin convenience wrapper around the Salsa query graph.
//!
//! This is used by tests and batch compilation. Instead of building a mutable
//! TypeRegistry incrementally, it pretty-prints each added module to source
//! text and feeds it through the Salsa-backed `analyze` query. This means all
//! semantic analysis goes through the same incremental query graph that the
//! LSP uses.
//!
//! ```ignore
//! let builder = SemanticBuilder::new(pointer_size);
//! builder.add_module(&module, &ItemPath::from("test"))?;
//! let resolved = builder.build()?;
//! ```

use crate::{
    grammar::{self, ItemPath},
    pretty_print::pretty_print,
    semantic::{self, PyxisDatabaseImpl, SemanticError, SemanticOutput, SourceFile, SourceSet},
    span::HasLocation,
};

/// Pending source registration — the pretty-printed source + its module path.
struct PendingSource {
    path: String,
    source: String,
}

/// A builder that constructs a Salsa-backed semantic analysis from
/// programmatic AST modules. Each `add_module` call pretty-prints the
/// grammar module to source text; `build` feeds all sources through the
/// Salsa `analyze` query and projects the result to `SemanticOutput`.
pub struct SemanticBuilder {
    pointer_size: usize,
    pending: Vec<PendingSource>,
    /// Counter for synthesizing unique file IDs for each added module.
    file_id_counter: u32,
}

impl SemanticBuilder {
    /// Create a new builder with the given pointer size.
    pub fn new(pointer_size: usize) -> Self {
        Self {
            pointer_size,
            pending: Vec::new(),
            // File IDs 0 and 1 are reserved (internal/test); start at 2.
            file_id_counter: 2,
        }
    }

    /// Add a grammar module to the analysis.
    ///
    /// The module is pretty-printed to source text and stored; the actual
    /// semantic analysis happens when `build()` is called.
    pub fn add_module(
        &mut self,
        module: &grammar::Module,
        path: &ItemPath,
    ) -> crate::semantic::error::Result<()> {
        use crate::semantic::{
            attribute,
            error::{AttributeName, ExternKind},
        };

        // Reject two source files mapping to the same module path
        let path_str = path
            .iter()
            .map(|s| s.as_str().to_string())
            .collect::<Vec<_>>()
            .join("/");
        let source_path = format!("{path_str}.pyxis");
        if !path.is_empty() && self.pending.iter().any(|p| p.path == source_path) {
            let location = module
                .items
                .first()
                .map(|item| *item.location())
                .unwrap_or_else(crate::span::ItemLocation::internal);
            return Err(SemanticError::DuplicateModule {
                path: path.clone(),
                location,
            });
        }

        // Validate extern types have required attributes (size + align),
        // matching the batch compiler's eager validation.
        for extern_type in module.extern_types() {
            if let grammar::ModuleItem::ExternType {
                name: extern_name,
                attributes,
                location: extern_location,
                ..
            } = extern_type
            {
                let mut size = None;
                let mut alignment = None;
                for attribute in attributes {
                    let Some((ident, items)) = attribute.function() else {
                        continue;
                    };
                    let loc = attribute.location();
                    if let Ok(Some(s)) = attribute::parse_size(ident, items, loc) {
                        size = Some(s);
                    } else if let Ok(Some(a)) = attribute::parse_align(ident, items, loc) {
                        alignment = Some(a);
                    }
                }
                if size.is_none() {
                    return Err(SemanticError::MissingExternAttribute {
                        attribute_name: AttributeName::Size,
                        extern_kind: ExternKind::Type,
                        type_name: extern_name.as_str().into(),
                        module_name: path.to_string(),
                        location: *extern_location,
                    });
                }
                if alignment.is_none() {
                    return Err(SemanticError::MissingExternAttribute {
                        attribute_name: AttributeName::Align,
                        extern_kind: ExternKind::Type,
                        type_name: extern_name.as_str().into(),
                        module_name: path.to_string(),
                        location: *extern_location,
                    });
                }
            }
        }

        let source = pretty_print(module);
        self.pending.push(PendingSource {
            path: source_path,
            source,
        });
        self.file_id_counter += 1;
        Ok(())
    }

    /// Run the Salsa-backed analysis and return the resolved semantic state.
    ///
    /// Returns the first error (if any) to preserve the `Result` contract
    /// that callers expect.
    pub fn build(self) -> crate::semantic::error::Result<SemanticOutput> {
        let db = PyxisDatabaseImpl::default();
        let sources: Vec<SourceFile> = self
            .pending
            .into_iter()
            .enumerate()
            .map(|(i, ps)| {
                let file_id = (i as u32) + 2; // Start at 2 (0 and 1 reserved)
                SourceFile::new(&db, ps.path, file_id, ps.source)
            })
            .collect();

        let source_set = SourceSet::new(&db, sources);
        let analysis = semantic::analyze(&db, self.pointer_size, source_set);

        // Parse errors should never occur here — we pretty-print valid
        // grammar::Module ASTs, which always parse back. If this fires,
        // there's a bug in pretty_print.
        if let Some(first_parse_err) = analysis.parse_errors(&db).first() {
            panic!("SemanticBuilder: pretty-printed source failed to parse: {first_parse_err}");
        }

        // Dual-path error model: collect all errors, merge TypeResolutionStalled
        // errors into one (to match the batch compiler's behavior),
        // and return the first as Err.
        let errors = analysis.errors(&db);
        if errors.is_empty() {
            return analysis
                .to_semantic_output(&db)
                .ok_or(SemanticError::TypeResolutionStalled {
                    unresolved_types: vec![],
                    resolved_types: vec![],
                    unresolved_references: vec![],
                });
        }

        // Merge all TypeResolutionStalled errors into one, preserving
        // all unresolved types and references.
        let mut stalled_types = Vec::new();
        let mut stalled_refs = Vec::new();
        let mut first_other: Option<SemanticError> = None;
        for err in errors.iter() {
            if let SemanticError::TypeResolutionStalled {
                unresolved_types,
                unresolved_references,
                ..
            } = err
            {
                stalled_types.extend(unresolved_types.iter().cloned());
                stalled_refs.extend(unresolved_references.iter().cloned());
            } else if first_other.is_none() {
                first_other = Some(err.clone());
            }
        }

        // If we have any stalled types/refs, return a merged stalled error.
        // Otherwise return the first non-stalled error.
        if !stalled_types.is_empty() || !stalled_refs.is_empty() {
            Err(SemanticError::TypeResolutionStalled {
                unresolved_types: stalled_types,
                resolved_types: vec![],
                unresolved_references: stalled_refs,
            })
        } else {
            Err(first_other.unwrap_or_else(|| errors[0].clone()))
        }
    }
}
