pub mod error;
pub mod types;

pub(crate) mod attribute;
pub(crate) mod bitflags_definition;
pub mod doc_links;
pub(crate) mod enum_definition;
pub(crate) mod function;
pub mod module;
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
