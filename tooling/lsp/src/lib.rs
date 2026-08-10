//! Pyxis Language Server library.
//!
//! Provides LSP server functionality for the Pyxis DSL, built on the
//! Salsa-backed compiler.
//!
//! The workspace restriction lints (unwrap_used/expect_used/panic/unreachable)
//! target production code. Test code is explicitly exempt per the contributing
//! guidelines, so allow them under `cfg(test)` only.
#![cfg_attr(
    test,
    allow(
        clippy::unwrap_used,
        clippy::expect_used,
        clippy::panic,
        clippy::unreachable
    )
)]

pub mod handlers;
pub mod main_loop;
pub mod span;
pub mod state;
