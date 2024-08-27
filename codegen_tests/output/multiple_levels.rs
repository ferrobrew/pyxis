#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
struct Base {
    vftable: *const crate::multiple_levels::BaseVftable,
    pub base_field: u64,
}
fn _Base_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 16usize], Base>([0u8; 16usize]);
    }
    unreachable!()
}
impl Base {
    pub fn vftable(&self) -> *const crate::multiple_levels::BaseVftable {
        self.vftable as *const crate::multiple_levels::BaseVftable
    }
    pub unsafe fn base_associated(&self, a: i32) -> i32 {
        let f: unsafe extern "thiscall" fn(this: *const Self, a: i32) -> i32 = ::std::mem::transmute(
            291usize,
        );
        f(self as *const Self as _, a)
    }
    pub unsafe fn base_vfunc(&self, a: i32) -> i32 {
        let f = std::ptr::addr_of!((* self.vftable()).base_vfunc).read();
        f(self as *const Self as _, a)
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
struct BaseVftable {
    pub base_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::multiple_levels::Base,
        a: i32,
    ) -> i32,
}
fn _BaseVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 8usize], BaseVftable>([0u8; 8usize]);
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
    pub base: crate::multiple_levels::Base,
    pub derived_field: u64,
}
fn _Derived_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 24usize], Derived>([0u8; 24usize]);
    }
    unreachable!()
}
impl Derived {
    pub fn vftable(&self) -> *const crate::multiple_levels::DerivedVftable {
        self.base.vftable() as *const crate::multiple_levels::DerivedVftable
    }
    pub unsafe fn base_associated(&self, a: i32) -> i32 {
        self.base.base_associated(a)
    }
    pub unsafe fn derived_associated(&self, a: i32) -> i32 {
        let f: unsafe extern "thiscall" fn(this: *const Self, a: i32) -> i32 = ::std::mem::transmute(
            1110usize,
        );
        f(self as *const Self as _, a)
    }
    pub unsafe fn base_vfunc(&self, a: i32) -> i32 {
        let f = std::ptr::addr_of!((* self.vftable()).base_vfunc).read();
        f(self as *const Self as _, a)
    }
    pub unsafe fn derived_vfunc(&self, a: i32) -> i32 {
        let f = std::ptr::addr_of!((* self.vftable()).derived_vfunc).read();
        f(self as *const Self as _, a)
    }
}
impl std::convert::AsRef<crate::multiple_levels::Base> for Derived {
    fn as_ref(&self) -> &crate::multiple_levels::Base {
        &self.base
    }
}
impl std::convert::AsMut<crate::multiple_levels::Base> for Derived {
    fn as_mut(&mut self) -> &mut crate::multiple_levels::Base {
        &mut self.base
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
struct DerivedDerived {
    pub derived: crate::multiple_levels::Derived,
    pub derived_derived_field: u64,
}
fn _DerivedDerived_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 32usize], DerivedDerived>([0u8; 32usize]);
    }
    unreachable!()
}
impl DerivedDerived {
    pub fn vftable(&self) -> *const crate::multiple_levels::DerivedDerivedVftable {
        self.derived.vftable() as *const crate::multiple_levels::DerivedDerivedVftable
    }
    pub unsafe fn base_associated(&self, a: i32) -> i32 {
        self.derived.base_associated(a)
    }
    pub unsafe fn derived_associated(&self, a: i32) -> i32 {
        self.derived.derived_associated(a)
    }
    pub unsafe fn derived_derived_associated(&self, a: i32) -> i32 {
        let f: unsafe extern "thiscall" fn(this: *const Self, a: i32) -> i32 = ::std::mem::transmute(
            1929usize,
        );
        f(self as *const Self as _, a)
    }
    pub unsafe fn base_vfunc(&self, a: i32) -> i32 {
        let f = std::ptr::addr_of!((* self.vftable()).base_vfunc).read();
        f(self as *const Self as _, a)
    }
    pub unsafe fn derived_vfunc(&self, a: i32) -> i32 {
        let f = std::ptr::addr_of!((* self.vftable()).derived_vfunc).read();
        f(self as *const Self as _, a)
    }
    pub unsafe fn derived_derived_vfunc(&self, a: i32) -> i32 {
        let f = std::ptr::addr_of!((* self.vftable()).derived_derived_vfunc).read();
        f(self as *const Self as _, a)
    }
}
impl std::convert::AsRef<crate::multiple_levels::Derived> for DerivedDerived {
    fn as_ref(&self) -> &crate::multiple_levels::Derived {
        &self.derived
    }
}
impl std::convert::AsMut<crate::multiple_levels::Derived> for DerivedDerived {
    fn as_mut(&mut self) -> &mut crate::multiple_levels::Derived {
        &mut self.derived
    }
}
impl std::convert::AsRef<crate::multiple_levels::Base> for DerivedDerived {
    fn as_ref(&self) -> &crate::multiple_levels::Base {
        &self.derived.base
    }
}
impl std::convert::AsMut<crate::multiple_levels::Base> for DerivedDerived {
    fn as_mut(&mut self) -> &mut crate::multiple_levels::Base {
        &mut self.derived.base
    }
}
impl std::convert::AsRef<DerivedDerived> for DerivedDerived {
    fn as_ref(&self) -> &DerivedDerived {
        self
    }
}
impl std::convert::AsMut<DerivedDerived> for DerivedDerived {
    fn as_mut(&mut self) -> &mut DerivedDerived {
        self
    }
}
#[repr(C, align(8))]
struct DerivedDerivedDerived {
    pub derived_derived: crate::multiple_levels::DerivedDerived,
    pub derived_derived_derived_field: u64,
}
fn _DerivedDerivedDerived_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 40usize], DerivedDerivedDerived>([0u8; 40usize]);
    }
    unreachable!()
}
impl DerivedDerivedDerived {
    pub fn vftable(
        &self,
    ) -> *const crate::multiple_levels::DerivedDerivedDerivedVftable {
        self.derived_derived.vftable()
            as *const crate::multiple_levels::DerivedDerivedDerivedVftable
    }
    pub unsafe fn base_associated(&self, a: i32) -> i32 {
        self.derived_derived.base_associated(a)
    }
    pub unsafe fn derived_associated(&self, a: i32) -> i32 {
        self.derived_derived.derived_associated(a)
    }
    pub unsafe fn derived_derived_associated(&self, a: i32) -> i32 {
        self.derived_derived.derived_derived_associated(a)
    }
    pub unsafe fn derived_derived_derived_associated(&self, a: i32) -> i32 {
        let f: unsafe extern "thiscall" fn(this: *const Self, a: i32) -> i32 = ::std::mem::transmute(
            2748usize,
        );
        f(self as *const Self as _, a)
    }
    pub unsafe fn base_vfunc(&self, a: i32) -> i32 {
        let f = std::ptr::addr_of!((* self.vftable()).base_vfunc).read();
        f(self as *const Self as _, a)
    }
    pub unsafe fn derived_vfunc(&self, a: i32) -> i32 {
        let f = std::ptr::addr_of!((* self.vftable()).derived_vfunc).read();
        f(self as *const Self as _, a)
    }
    pub unsafe fn derived_derived_vfunc(&self, a: i32) -> i32 {
        let f = std::ptr::addr_of!((* self.vftable()).derived_derived_vfunc).read();
        f(self as *const Self as _, a)
    }
    pub unsafe fn derived_derived_derived_vfunc(&self, a: i32) -> i32 {
        let f = std::ptr::addr_of!((* self.vftable()).derived_derived_derived_vfunc)
            .read();
        f(self as *const Self as _, a)
    }
}
impl std::convert::AsRef<crate::multiple_levels::DerivedDerived>
for DerivedDerivedDerived {
    fn as_ref(&self) -> &crate::multiple_levels::DerivedDerived {
        &self.derived_derived
    }
}
impl std::convert::AsMut<crate::multiple_levels::DerivedDerived>
for DerivedDerivedDerived {
    fn as_mut(&mut self) -> &mut crate::multiple_levels::DerivedDerived {
        &mut self.derived_derived
    }
}
impl std::convert::AsRef<crate::multiple_levels::Derived> for DerivedDerivedDerived {
    fn as_ref(&self) -> &crate::multiple_levels::Derived {
        &self.derived_derived.derived
    }
}
impl std::convert::AsMut<crate::multiple_levels::Derived> for DerivedDerivedDerived {
    fn as_mut(&mut self) -> &mut crate::multiple_levels::Derived {
        &mut self.derived_derived.derived
    }
}
impl std::convert::AsRef<crate::multiple_levels::Base> for DerivedDerivedDerived {
    fn as_ref(&self) -> &crate::multiple_levels::Base {
        &self.derived_derived.derived.base
    }
}
impl std::convert::AsMut<crate::multiple_levels::Base> for DerivedDerivedDerived {
    fn as_mut(&mut self) -> &mut crate::multiple_levels::Base {
        &mut self.derived_derived.derived.base
    }
}
impl std::convert::AsRef<DerivedDerivedDerived> for DerivedDerivedDerived {
    fn as_ref(&self) -> &DerivedDerivedDerived {
        self
    }
}
impl std::convert::AsMut<DerivedDerivedDerived> for DerivedDerivedDerived {
    fn as_mut(&mut self) -> &mut DerivedDerivedDerived {
        self
    }
}
#[repr(C, align(8))]
struct DerivedDerivedDerivedVftable {
    pub base_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::multiple_levels::DerivedDerivedDerived,
        a: i32,
    ) -> i32,
    pub derived_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::multiple_levels::DerivedDerivedDerived,
        a: i32,
    ) -> i32,
    pub derived_derived_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::multiple_levels::DerivedDerivedDerived,
        a: i32,
    ) -> i32,
    pub derived_derived_derived_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::multiple_levels::DerivedDerivedDerived,
        a: i32,
    ) -> i32,
}
fn _DerivedDerivedDerivedVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<
            [u8; 32usize],
            DerivedDerivedDerivedVftable,
        >([0u8; 32usize]);
    }
    unreachable!()
}
impl DerivedDerivedDerivedVftable {}
impl std::convert::AsRef<DerivedDerivedDerivedVftable> for DerivedDerivedDerivedVftable {
    fn as_ref(&self) -> &DerivedDerivedDerivedVftable {
        self
    }
}
impl std::convert::AsMut<DerivedDerivedDerivedVftable> for DerivedDerivedDerivedVftable {
    fn as_mut(&mut self) -> &mut DerivedDerivedDerivedVftable {
        self
    }
}
#[repr(C, align(8))]
struct DerivedDerivedVftable {
    pub base_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::multiple_levels::DerivedDerived,
        a: i32,
    ) -> i32,
    pub derived_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::multiple_levels::DerivedDerived,
        a: i32,
    ) -> i32,
    pub derived_derived_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::multiple_levels::DerivedDerived,
        a: i32,
    ) -> i32,
}
fn _DerivedDerivedVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 24usize], DerivedDerivedVftable>([0u8; 24usize]);
    }
    unreachable!()
}
impl DerivedDerivedVftable {}
impl std::convert::AsRef<DerivedDerivedVftable> for DerivedDerivedVftable {
    fn as_ref(&self) -> &DerivedDerivedVftable {
        self
    }
}
impl std::convert::AsMut<DerivedDerivedVftable> for DerivedDerivedVftable {
    fn as_mut(&mut self) -> &mut DerivedDerivedVftable {
        self
    }
}
#[repr(C, align(8))]
struct DerivedVftable {
    pub base_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::multiple_levels::Derived,
        a: i32,
    ) -> i32,
    pub derived_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::multiple_levels::Derived,
        a: i32,
    ) -> i32,
}
fn _DerivedVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 16usize], DerivedVftable>([0u8; 16usize]);
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
