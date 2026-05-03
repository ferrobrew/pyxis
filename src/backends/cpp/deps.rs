//! Dependency-graph construction and SCC-based cycle detection for the C++
//! backend.
//!
//! Each item in a project references other items via either a **FullDef** edge
//! (by-value field, base, array element, FullDef-typed template arg — needs
//! `#include`) or a **FwdOnly** edge (pointer or function param/return — a
//! forward declaration is enough). The semantic resolver does not guarantee
//! that the FullDef graph is acyclic, so we run SCC analysis ourselves and
//! emit a clear diagnostic if a true value-cycle is found.
//!
//! Phase 0: scaffolding only.
