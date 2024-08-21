#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#[repr(C, align(8))]
struct Base {
    vftable: *const crate::diamond_inheritance::BaseVftable,
}
fn _Base_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 8usize], Base>([0u8; 8usize]);
    }
    unreachable!()
}
impl Base {
    pub fn vftable(&self) -> *const crate::diamond_inheritance::BaseVftable {
        self.vftable as *const crate::diamond_inheritance::BaseVftable
    }
    pub unsafe fn destructor(&mut self) {
        let f = std::ptr::addr_of!((* self.vftable()).destructor).read();
        f(self as *mut Self as _)
    }
}
#[repr(C, align(8))]
struct BaseA {
    pub base: crate::diamond_inheritance::Base,
}
fn _BaseA_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 8usize], BaseA>([0u8; 8usize]);
    }
    unreachable!()
}
impl BaseA {
    pub fn vftable(&self) -> *const crate::diamond_inheritance::BaseAVftable {
        self.base.vftable() as *const crate::diamond_inheritance::BaseAVftable
    }
    pub unsafe fn destructor(&mut self) {
        let f = std::ptr::addr_of!((* self.vftable()).destructor).read();
        f(self as *mut Self as _)
    }
}
impl std::convert::AsRef<crate::diamond_inheritance::Base> for BaseA {
    fn as_ref(&self) -> &crate::diamond_inheritance::Base {
        &self.base
    }
}
impl std::convert::AsMut<crate::diamond_inheritance::Base> for BaseA {
    fn as_mut(&mut self) -> &mut crate::diamond_inheritance::Base {
        &mut self.base
    }
}
#[repr(C, align(8))]
struct BaseAVftable {
    pub destructor: unsafe extern "thiscall" fn(
        this: *mut crate::diamond_inheritance::BaseA,
    ),
}
fn _BaseAVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 8usize], BaseAVftable>([0u8; 8usize]);
    }
    unreachable!()
}
impl BaseAVftable {}
#[repr(C, align(8))]
struct BaseB {
    pub base: crate::diamond_inheritance::Base,
}
fn _BaseB_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 8usize], BaseB>([0u8; 8usize]);
    }
    unreachable!()
}
impl BaseB {
    pub fn vftable(&self) -> *const crate::diamond_inheritance::BaseBVftable {
        self.base.vftable() as *const crate::diamond_inheritance::BaseBVftable
    }
    pub unsafe fn destructor(&mut self) {
        let f = std::ptr::addr_of!((* self.vftable()).destructor).read();
        f(self as *mut Self as _)
    }
}
impl std::convert::AsRef<crate::diamond_inheritance::Base> for BaseB {
    fn as_ref(&self) -> &crate::diamond_inheritance::Base {
        &self.base
    }
}
impl std::convert::AsMut<crate::diamond_inheritance::Base> for BaseB {
    fn as_mut(&mut self) -> &mut crate::diamond_inheritance::Base {
        &mut self.base
    }
}
#[repr(C, align(8))]
struct BaseBVftable {
    pub destructor: unsafe extern "thiscall" fn(
        this: *mut crate::diamond_inheritance::BaseB,
    ),
}
fn _BaseBVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 8usize], BaseBVftable>([0u8; 8usize]);
    }
    unreachable!()
}
impl BaseBVftable {}
#[repr(C, align(8))]
struct BaseVftable {
    pub destructor: unsafe extern "thiscall" fn(
        this: *mut crate::diamond_inheritance::Base,
    ),
}
fn _BaseVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 8usize], BaseVftable>([0u8; 8usize]);
    }
    unreachable!()
}
impl BaseVftable {}
#[repr(C, align(8))]
struct Derived {
    pub base_a: crate::diamond_inheritance::BaseA,
    pub base_b: crate::diamond_inheritance::BaseB,
}
fn _Derived_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 16usize], Derived>([0u8; 16usize]);
    }
    unreachable!()
}
impl Derived {
    pub fn vftable(&self) -> *const crate::diamond_inheritance::DerivedVftable {
        self.base_a.vftable() as *const crate::diamond_inheritance::DerivedVftable
    }
    pub unsafe fn destructor(&mut self) {
        self.base_b.destructor()
    }
    pub unsafe fn destructor(&mut self) {
        let f = std::ptr::addr_of!((* self.vftable()).destructor).read();
        f(self as *mut Self as _)
    }
}
impl std::convert::AsRef<crate::diamond_inheritance::BaseA> for Derived {
    fn as_ref(&self) -> &crate::diamond_inheritance::BaseA {
        &self.base_a
    }
}
impl std::convert::AsMut<crate::diamond_inheritance::BaseA> for Derived {
    fn as_mut(&mut self) -> &mut crate::diamond_inheritance::BaseA {
        &mut self.base_a
    }
}
///`AsRef` and `AsMut` implementations were not generated for `Derived` to `crate :: diamond_inheritance :: Base`,
///as there are multiple implementations of the same type in the hierarchy:
///  - `base_a.base`
///  - `base_b.base`
const _CONFLICTING_DERIVED_BASE_A_BASE: () = ();
impl std::convert::AsRef<crate::diamond_inheritance::BaseB> for Derived {
    fn as_ref(&self) -> &crate::diamond_inheritance::BaseB {
        &self.base_b
    }
}
impl std::convert::AsMut<crate::diamond_inheritance::BaseB> for Derived {
    fn as_mut(&mut self) -> &mut crate::diamond_inheritance::BaseB {
        &mut self.base_b
    }
}
///`AsRef` and `AsMut` implementations were not generated for `Derived` to `crate :: diamond_inheritance :: Base`,
///as there are multiple implementations of the same type in the hierarchy:
///  - `base_a.base`
///  - `base_b.base`
const _CONFLICTING_DERIVED_BASE_B_BASE: () = ();
#[repr(C, align(8))]
struct DerivedVftable {
    pub destructor: unsafe extern "thiscall" fn(
        this: *mut crate::diamond_inheritance::Derived,
    ),
}
fn _DerivedVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 8usize], DerivedVftable>([0u8; 8usize]);
    }
    unreachable!()
}
impl DerivedVftable {}
