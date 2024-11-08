#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
struct BaseA {
    vftable: *const crate::two_base_classes::BaseAVftable,
    pub field_a: u64,
}
fn _BaseA_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], BaseA>([0u8; 0x10]);
    }
    unreachable!()
}
impl BaseA {
    pub fn vftable(&self) -> *const crate::two_base_classes::BaseAVftable {
        self.vftable as *const crate::two_base_classes::BaseAVftable
    }
    pub unsafe fn base_a_associated(&self, a: i32) -> i32 {
        let f: unsafe extern "thiscall" fn(this: *const Self, a: i32) -> i32 = ::std::mem::transmute(
            0x123 as usize,
        );
        f(self as *const Self as _, a)
    }
    pub unsafe fn base_a_vfunc(&self, a: i32) -> i32 {
        let f = (&raw const (*self.vftable()).base_a_vfunc).read();
        f(self as *const Self as _, a)
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
    pub base_a_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::two_base_classes::BaseA,
        a: i32,
    ) -> i32,
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
    vftable: *const crate::two_base_classes::BaseBVftable,
    pub field_b: u64,
}
fn _BaseB_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], BaseB>([0u8; 0x10]);
    }
    unreachable!()
}
impl BaseB {
    pub fn vftable(&self) -> *const crate::two_base_classes::BaseBVftable {
        self.vftable as *const crate::two_base_classes::BaseBVftable
    }
    pub unsafe fn base_b_associated(&self, a: i32) -> i32 {
        let f: unsafe extern "thiscall" fn(this: *const Self, a: i32) -> i32 = ::std::mem::transmute(
            0x456 as usize,
        );
        f(self as *const Self as _, a)
    }
    pub unsafe fn base_b_vfunc(&self, a: i32) -> i32 {
        let f = (&raw const (*self.vftable()).base_b_vfunc).read();
        f(self as *const Self as _, a)
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
    pub base_b_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::two_base_classes::BaseB,
        a: i32,
    ) -> i32,
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
struct Derived {
    pub base_a: crate::two_base_classes::BaseA,
    pub base_b: crate::two_base_classes::BaseB,
}
fn _Derived_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x20], Derived>([0u8; 0x20]);
    }
    unreachable!()
}
impl Derived {
    pub fn vftable(&self) -> *const crate::two_base_classes::DerivedVftable {
        self.base_a.vftable() as *const crate::two_base_classes::DerivedVftable
    }
    pub unsafe fn base_a_associated(&self, a: i32) -> i32 {
        self.base_a.base_a_associated(a)
    }
    pub unsafe fn base_b_associated(&self, a: i32) -> i32 {
        self.base_b.base_b_associated(a)
    }
    pub unsafe fn base_b_vfunc(&self, a: i32) -> i32 {
        self.base_b.base_b_vfunc(a)
    }
    pub unsafe fn derived_associated(&self, a: i32) -> i32 {
        let f: unsafe extern "thiscall" fn(this: *const Self, a: i32) -> i32 = ::std::mem::transmute(
            0x789 as usize,
        );
        f(self as *const Self as _, a)
    }
    pub unsafe fn base_a_vfunc(&self, a: i32) -> i32 {
        let f = (&raw const (*self.vftable()).base_a_vfunc).read();
        f(self as *const Self as _, a)
    }
    pub unsafe fn derived_vfunc(&self, a: i32) -> i32 {
        let f = (&raw const (*self.vftable()).derived_vfunc).read();
        f(self as *const Self as _, a)
    }
}
impl std::convert::AsRef<crate::two_base_classes::BaseA> for Derived {
    fn as_ref(&self) -> &crate::two_base_classes::BaseA {
        &self.base_a
    }
}
impl std::convert::AsMut<crate::two_base_classes::BaseA> for Derived {
    fn as_mut(&mut self) -> &mut crate::two_base_classes::BaseA {
        &mut self.base_a
    }
}
impl std::convert::AsRef<crate::two_base_classes::BaseB> for Derived {
    fn as_ref(&self) -> &crate::two_base_classes::BaseB {
        &self.base_b
    }
}
impl std::convert::AsMut<crate::two_base_classes::BaseB> for Derived {
    fn as_mut(&mut self) -> &mut crate::two_base_classes::BaseB {
        &mut self.base_b
    }
}
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
    pub base_a_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::two_base_classes::Derived,
        a: i32,
    ) -> i32,
    pub derived_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::two_base_classes::Derived,
        a: i32,
    ) -> i32,
}
fn _DerivedVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], DerivedVftable>([0u8; 0x10]);
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
