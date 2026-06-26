pub mod error;
pub mod types;

pub mod builder;
pub mod db;
pub mod declaration_registry;
pub(crate) mod attribute;
pub(crate) mod bitflags_definition;
pub mod doc_links;
pub mod inputs;
pub(crate) mod enum_definition;
pub(crate) mod function;
pub mod ir;
pub mod module;
pub mod queries;
pub mod resolution_context;
pub mod semantic_state;
pub(crate) mod type_alias_definition;
pub(crate) mod type_definition;
pub mod type_registry;

#[cfg(test)]
mod tests;

pub use error::{Result, SemanticError};
pub use module::{ExternBindings, Module};
pub use semantic_state::{ResolvedSemanticState, SemanticState};
pub use type_registry::TypeRegistry;

pub use builder::SemanticBuilder;

// Salsa-backed incremental compilation query layer.
//
// This replaces the imperative `SemanticState` builder with a Salsa query
// graph. Both the CLI `driver` and the `lsp` binary call the same Salsa
// queries — there is no separate "imperative pipeline" and "LSP pipeline."
pub use db::{Db, PyxisDatabaseImpl};
pub use inputs::{ProjectConfig, SourceFile, SourceSet};
pub use ir::{DeclarationSet, ParsedFile, ResolvedItem, SemanticAnalysis};
pub use queries::{analyze, collect_declarations, parse_file, resolve_item};

// Re-export salsa's Setter trait for downstream crates (LSP)
pub use salsa::Setter;
