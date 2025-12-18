//! Semantic analysis tests.
//!
//! This module contains tests for the semantic analysis phase of the compiler,
//! organized by category:
//!
//! - `basic_resolution` - Basic struct and type resolution
//! - `error_messages` - Error message quality tests
//! - `imports` - Module imports and use statements
//! - `extern_types` - Extern type resolution
//! - `extern_values` - Extern value definitions
//! - `vftable` - Virtual function table generation
//! - `enums` - Enum type resolution
//! - `copyable_cloneable` - Copyable and cloneable trait resolution
//! - `defaultable` - Defaultable trait resolution
//! - `doc_comments` - Documentation comment propagation
//! - `bitflags` - Bitflags type resolution
//! - `functions` - Freestanding function resolution
//! - `type_aliases` - Type alias resolution
//! - `generics` - Generic type resolution

mod alignment;
mod basic_resolution;
mod bitflags;
mod copyable_cloneable;
mod defaultable;
mod doc_comments;
mod enums;
mod error_messages;
mod extern_types;
mod extern_values;
mod functions;
mod generics;
mod imports;
mod inheritance;
mod min_size;
mod type_aliases;
mod util;
mod vftable;
mod visibility;
