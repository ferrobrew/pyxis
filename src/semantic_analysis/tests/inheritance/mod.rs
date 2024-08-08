//! Tests for inheritance of types with optional vftables.
//!
//! One base class
//! --------------
//! We set up two types: BaseA and Derived.
//! Derived derives from BaseA.
//!
//! We need to test, where 'x' marks the presence of a vftable:
//!
//! BaseA | Drved
//! --------------
//!       |
//!       |   x
//!   x   |
//!   x   |   x
//!
//! Two base classes
//! ----------------
//! We set up three types: BaseA, BaseB and Derived.
//! Derived derives from both BaseA and BaseB.
//!
//! However, note that [/layout/msvc2022/output.txt] demonstrates that
//! a compiler will rearrange structs to put an inherited-from type
//! with a vftable at the start of the type, which means we don't need
//! to test the BaseA no-vftable BaseB vftable cases, as these are isomorphic
//! to BaseA vftable BaseB no-vftable.
//!
//! We need to test, where 'x' marks the presence of a vftable:
//!
//! BaseA | BaseB | Drved
//! ----------------------
//!       |       |
//!       |       |   x
//!   x   |       |
//!   x   |       |   x
//!   x   |   x   |
//!   x   |   x   |   x
//!
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

use super::*;

mod dsl;
mod one_base_class;
mod two_base_classes;
