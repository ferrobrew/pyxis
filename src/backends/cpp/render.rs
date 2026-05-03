//! Item → C++ text rendering for the C++ backend.
//!
//! Owns the conversion of resolved IR items (structs, enums, bitflags, type
//! aliases, vftables, generic templates, extern-type bindings) into the textual
//! output for `.hpp` and `.cpp` files. Splices `backend cpp prologue/epilogue`
//! blocks around the generated content.
//!
//! Phase 0: scaffolding only.
