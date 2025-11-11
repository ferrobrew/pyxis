mod recursive_descent;
pub mod strip_spans;

use crate::grammar::Module;
use crate::tokenizer::tokenize;
use recursive_descent::Parser;

#[cfg(test)]
mod tests;

/// Parse a Pyxis module from a string
pub fn parse_str(input: &str) -> anyhow::Result<Module> {
    // First tokenize
    let tokens = tokenize(input.to_string())?;

    // Then parse
    let mut parser = Parser::new(tokens);
    let module = parser.parse_module()?;

    Ok(module)
}

// Re-export for convenience
pub use recursive_descent::ParseError as Error;
