#[cfg(test)]
mod tests;

pub(crate) mod module;
pub(crate) mod semantic_state;
pub(crate) mod type_registry;
pub(crate) mod types;

mod function;
mod type_definition;

pub use semantic_state::SemanticState;
