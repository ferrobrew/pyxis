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

use crate::span::FileId;

#[cfg(test)]
/// Parse a Pyxis module from a string for tests, with the spans stripped out
pub fn parse_str_for_tests(input: &str) -> Result<Module, ParseError> {
    parse_str_with_file_id(input, FileId::TEST)
}

/// Parse a Pyxis module from a string with a specific file ID for error reporting
pub fn parse_str_with_file_id(input: &str, file_id: FileId) -> Result<Module, ParseError> {
    // First tokenize
    let tokens = crate::tokenizer::tokenize_with_file_id(input.to_string(), file_id)?;

    // Then parse
    let mut parser = Parser::new(tokens, file_id, input.to_string());
    let module = parser.parse_module()?;

    Ok(module)
}
