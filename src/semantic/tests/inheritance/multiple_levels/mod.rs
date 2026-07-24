//! Multiple levels of inheritance
//! ------------------------------
//! We set up three types: Base, Derived and DerivedDerived.
//! Derived derives from Base, and DerivedDerived derives from Derived.
//!
//! We need to test, where 'x' marks the presence of a vftable:
//!
//!  Base | Drved | Drv2d
//! ----------------------
//!       |       |
//!       |       |   x
//!       |   x   |
//!       |   x   |   x
//!   x   |       |
//!   x   |       |   x
//!   x   |   x   |
//!   x   |   x   |   x

mod base_vftable;
mod no_base_vftable;
