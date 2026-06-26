//! Salsa-backed incremental compilation query layer.
//!
//! This module replaces the imperative `SemanticState` builder with a
//! Salsa query graph. Both the CLI `driver` and the `lsp` binary call the
//! same Salsa queries — there is no separate "imperative pipeline" and
//! "LSP pipeline."
//!
//! See the plan (`Phase 2`) for the full architecture.

pub mod db;
pub mod inputs;
pub mod ir;
pub mod queries;

pub use db::{Db, PyxisDatabaseImpl};
pub use inputs::{ProjectConfig, SourceFile};
pub use ir::{ParsedFile, SemanticAnalysis};
pub use queries::{analyze, parse_file};

// Re-export salsa's Setter trait for downstream crates (LSP)
pub use salsa::Setter;

#[cfg(test)]
mod tests {
    use super::*;
    use salsa::Setter;

    #[test]
    fn spike_parse_file_works() {
        let db = PyxisDatabaseImpl::default();
        let source = SourceFile::new(
            &db,
            "test.pyxis".to_string(),
            1, // file_id
            "pub type Test { pub field: u32, }".to_string(),
        );
        let parsed = parse_file(&db, source);
        let module = parsed.module(&db);
        let defs: Vec<_> = module.definitions().collect();
        assert!(!defs.is_empty(), "should parse a type def");
    }

    #[test]
    fn spike_analyze_returns_no_errors() {
        let db = PyxisDatabaseImpl::default();
        let source = SourceFile::new(
            &db,
            "test.pyxis".to_string(),
            1,
            "pub type Test { pub field: u32, }".to_string(),
        );
        let analysis = analyze(&db, 4, vec![source]);
        let errors = analysis.errors(&db);
        let parse_errors = analysis.parse_errors(&db);
        assert!(errors.is_empty(), "should have no semantic errors");
        assert!(parse_errors.is_empty(), "should have no parse errors");
    }

    #[test]
    fn spike_incremental_invalidation() {
        let mut db = PyxisDatabaseImpl::default();
        let source = SourceFile::new(
            &db,
            "test.pyxis".to_string(),
            1,
            "pub type A { pub x: u32, }".to_string(),
        );

        let parsed1 = parse_file(&db, source);
        let module1 = parsed1.module(&db);
        let defs1: Vec<_> = module1.definitions().collect();
        assert_eq!(defs1.len(), 1);

        // Change the content — Salsa should invalidate the cached parse
        source.set_contents(&mut db).to(
            "pub type A { pub x: u32, } pub type B { pub y: u32, }".to_string(),
        );

        let parsed2 = parse_file(&db, source);
        let module2 = parsed2.module(&db);
        let defs2: Vec<_> = module2.definitions().collect();
        assert_eq!(defs2.len(), 2, "should re-parse with new content");
    }
}
