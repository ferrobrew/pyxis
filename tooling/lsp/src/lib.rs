//! Pyxis Language Server library.
//!
//! Provides LSP server functionality for the Pyxis DSL, built on the
//! Salsa-backed compiler.

pub mod handlers;
pub mod main_loop;
pub mod span;
pub mod state;
