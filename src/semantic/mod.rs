pub mod error;
pub mod types;

mod bitflags_definition;
mod enum_definition;
mod function;
mod module;
mod semantic_state;
mod type_definition;
mod type_registry;

#[cfg(test)]
mod tests;

pub use error::{Result, SemanticError};
pub use module::Module;
pub use semantic_state::{ResolvedSemanticState, SemanticState};
pub use type_registry::TypeRegistry;
