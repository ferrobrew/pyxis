//! Infallible formatting macros for `std::fmt::Write` sinks that cannot fail.
//!
//! Writing to a `String` (or any `String`-backed buffer) via
//! [`std::fmt::Write`] is infallible in practice: `str` targets never return
//! an error for the `fmt` calls this crate makes. These macros drop that
//! `fmt::Result` at exactly one place — the macro body — with the
//! justification documented here, so call sites stay free of
//! `.unwrap()`/`.expect()` noise.
//!
//! Only use these for sinks that provably cannot fail (in-memory `String`
//! buffers). If a writer can genuinely fail, propagate the `Result` instead.

/// Write formatted text into a [`std::fmt::Write`] sink that cannot fail.
///
/// The sink is a `String`-backed buffer, whose `write_fmt` only errors on
/// out-of-memory or a broken `Formatter` — neither occurs for the in-memory
/// formatting this crate does. The result is deliberately dropped here, once,
/// rather than unwrapped at every call site.
#[macro_export]
macro_rules! infallible_write {
    ($dst:expr $(, $($arg:tt)*)?) => {{
        use ::std::fmt::Write as _;
        let _ = write!($dst $(, $($arg)*)?);
    }};
}

/// Write a formatted line (with trailing newline) into a [`std::fmt::Write`]
/// sink that cannot fail.
///
/// The sink is a `String`-backed buffer, whose `write_fmt` only errors on
/// out-of-memory or a broken `Formatter` — neither occurs for the in-memory
/// formatting this crate does. The result is deliberately dropped here, once,
/// rather than unwrapped at every call site.
#[macro_export]
macro_rules! infallible_writeln {
    ($dst:expr $(, $($arg:tt)*)?) => {{
        use ::std::fmt::Write as _;
        let _ = writeln!($dst $(, $($arg)*)?);
    }};
}
