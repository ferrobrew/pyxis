#![allow(
    dead_code,
    non_snake_case,
    non_upper_case_globals,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
pub mod atomics;
pub mod bitflags;
pub mod braced_imports;
pub mod cfg;
pub mod definition_body;
pub mod diamond_inheritance;
pub mod doc_comments;
pub mod enums;
pub mod external_body;
pub mod freestanding_functions;
pub mod generics;
pub mod generics_cross_module;
pub mod math;
pub mod min_size;
pub mod multiple_levels;
pub mod self_referential_generics;
pub mod singleton;
pub mod two_base_classes;
pub mod type_alias_reexport;
pub mod type_aliases;
pub mod world;
pub mod world_consumer;
