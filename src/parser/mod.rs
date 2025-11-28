pub mod attributes;
pub mod expressions;
pub mod external;
pub mod functions;
pub mod items;
pub mod module;
pub mod paths;
pub mod types;

pub use core::Parser;
pub use error::ParseError;
pub use module::Module;

mod core;
mod error;

use std::sync::Arc;

#[cfg(test)]
/// Parse a Pyxis module from a string for tests, with the spans stripped out
pub fn parse_str_for_tests(input: &str) -> Result<Module, ParseError> {
    parse_str_with_filename(input, "<test>")
}

/// Parse a Pyxis module from a string with a specific filename for error reporting
pub fn parse_str_with_filename(input: &str, filename: &str) -> Result<Module, ParseError> {
    // First tokenize
    let tokens = crate::tokenizer::tokenize_with_filename(input.to_string(), filename.to_string())?;

    // Then parse
    let filename_arc: Arc<str> = filename.into();
    let mut parser = Parser::new(tokens, filename_arc, input.to_string());
    let module = parser.parse_module()?;

    Ok(module)
}
