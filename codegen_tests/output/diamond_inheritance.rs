#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
struct Base {
    vftable: *const crate::diamond_inheritance::BaseVftable,
}
fn _Base_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Base>([0u8; 0x8]);
    }
    unreachable!()
}
impl Base {
    pub fn vftable(&self) -> *const crate::diamond_inheritance::BaseVftable {
        self.vftable as *const crate::diamond_inheritance::BaseVftable
    }
    pub unsafe fn destructor(&mut self) {
        let f = (&raw const (*self.vftable()).destructor).read();
        f(self as *mut Self as _)
    }
}
impl std::convert::AsRef<Base> for Base {
    fn as_ref(&self) -> &Base {
        self
    }
}
impl std::convert::AsMut<Base> for Base {
    fn as_mut(&mut self) -> &mut Base {
        self
    }
}
#[repr(C, align(8))]
struct BaseA {
    pub base: crate::diamond_inheritance::Base,
}
fn _BaseA_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], BaseA>([0u8; 0x8]);
    }
    unreachable!()
}
impl BaseA {
    pub fn vftable(&self) -> *const crate::diamond_inheritance::BaseAVftable {
        self.base.vftable() as *const crate::diamond_inheritance::BaseAVftable
    }
    pub unsafe fn associated(&mut self) {
        let f: unsafe extern "thiscall" fn(this: *mut Self) = ::std::mem::transmute(
            0x123 as usize,
        );
        f(self as *mut Self as _)
    }
    pub unsafe fn destructor(&mut self) {
        let f = (&raw const (*self.vftable()).destructor).read();
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
impl std::convert::AsRef<BaseA> for BaseA {
    fn as_ref(&self) -> &BaseA {
        self
    }
}
impl std::convert::AsMut<BaseA> for BaseA {
    fn as_mut(&mut self) -> &mut BaseA {
        self
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
        ::std::mem::transmute::<[u8; 0x8], BaseAVftable>([0u8; 0x8]);
    }
    unreachable!()
}
impl BaseAVftable {}
impl std::convert::AsRef<BaseAVftable> for BaseAVftable {
    fn as_ref(&self) -> &BaseAVftable {
        self
    }
}
impl std::convert::AsMut<BaseAVftable> for BaseAVftable {
    fn as_mut(&mut self) -> &mut BaseAVftable {
        self
    }
}
#[repr(C, align(8))]
struct BaseB {
    pub base: crate::diamond_inheritance::Base,
}
fn _BaseB_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], BaseB>([0u8; 0x8]);
    }
    unreachable!()
}
impl BaseB {
    pub fn vftable(&self) -> *const crate::diamond_inheritance::BaseBVftable {
        self.base.vftable() as *const crate::diamond_inheritance::BaseBVftable
    }
    pub unsafe fn associated(&mut self) {
        let f: unsafe extern "thiscall" fn(this: *mut Self) = ::std::mem::transmute(
            0x123 as usize,
        );
        f(self as *mut Self as _)
    }
    pub unsafe fn destructor(&mut self) {
        let f = (&raw const (*self.vftable()).destructor).read();
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
impl std::convert::AsRef<BaseB> for BaseB {
    fn as_ref(&self) -> &BaseB {
        self
    }
}
impl std::convert::AsMut<BaseB> for BaseB {
    fn as_mut(&mut self) -> &mut BaseB {
        self
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
        ::std::mem::transmute::<[u8; 0x8], BaseBVftable>([0u8; 0x8]);
    }
    unreachable!()
}
impl BaseBVftable {}
impl std::convert::AsRef<BaseBVftable> for BaseBVftable {
    fn as_ref(&self) -> &BaseBVftable {
        self
    }
}
impl std::convert::AsMut<BaseBVftable> for BaseBVftable {
    fn as_mut(&mut self) -> &mut BaseBVftable {
        self
    }
}
#[repr(C, align(8))]
struct BaseVftable {
    pub destructor: unsafe extern "thiscall" fn(
        this: *mut crate::diamond_inheritance::Base,
    ),
}
fn _BaseVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], BaseVftable>([0u8; 0x8]);
    }
    unreachable!()
}
impl BaseVftable {}
impl std::convert::AsRef<BaseVftable> for BaseVftable {
    fn as_ref(&self) -> &BaseVftable {
        self
    }
}
impl std::convert::AsMut<BaseVftable> for BaseVftable {
    fn as_mut(&mut self) -> &mut BaseVftable {
        self
    }
}
#[repr(C, align(8))]
struct Derived {
    pub base_a: crate::diamond_inheritance::BaseA,
    pub base_b: crate::diamond_inheritance::BaseB,
}
fn _Derived_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], Derived>([0u8; 0x10]);
    }
    unreachable!()
}
impl Derived {
    pub fn vftable(&self) -> *const crate::diamond_inheritance::DerivedVftable {
        self.base_a.vftable() as *const crate::diamond_inheritance::DerivedVftable
    }
    pub unsafe fn associated(&mut self) {
        self.base_a.associated()
    }
    pub unsafe fn base_b_associated(&mut self) {
        self.base_b.associated()
    }
    pub unsafe fn base_b_destructor(&mut self) {
        self.base_b.destructor()
    }
    pub unsafe fn destructor(&mut self) {
        let f = (&raw const (*self.vftable()).destructor).read();
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
impl std::convert::AsRef<Derived> for Derived {
    fn as_ref(&self) -> &Derived {
        self
    }
}
impl std::convert::AsMut<Derived> for Derived {
    fn as_mut(&mut self) -> &mut Derived {
        self
    }
}
#[repr(C, align(8))]
struct DerivedVftable {
    pub destructor: unsafe extern "thiscall" fn(
        this: *mut crate::diamond_inheritance::Derived,
    ),
}
fn _DerivedVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], DerivedVftable>([0u8; 0x8]);
    }
    unreachable!()
}
impl DerivedVftable {}
impl std::convert::AsRef<DerivedVftable> for DerivedVftable {
    fn as_ref(&self) -> &DerivedVftable {
        self
    }
}
impl std::convert::AsMut<DerivedVftable> for DerivedVftable {
    fn as_mut(&mut self) -> &mut DerivedVftable {
        self
    }
}
