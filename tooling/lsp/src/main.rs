//! Pyxis Language Server
//!
//! An LSP server for the Pyxis DSL, built on the Salsa-backed compiler.
//! Provides diagnostics, hover, go-to-definition, completion, document symbols,
//! formatting, code lens, inlay hints, and rename.

mod main_loop;
mod state;
mod span;
mod handlers;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    main_loop::run()
}
