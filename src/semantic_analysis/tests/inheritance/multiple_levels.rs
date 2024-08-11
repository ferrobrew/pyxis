//! Multiple levels of inheritance
//! ------------------------------
//! We set up three types: BaseA, Derived and DerivedDerived.
//! Derived derives from BaseA, and DerivedDerived derives from Derived.
//!
//! We need to test, where 'x' marks the presence of a vftable:
//!
//! BaseA | Drved | Drv2d
//! ----------------------
//!       |       |
//!       |       |   x
//!       |   x   |
//!       |   x   |   x
//!   x   |       |
//!   x   |       |   x
//!   x   |   x   |
//!   x   |   x   |   x
