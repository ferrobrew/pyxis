#![allow(dead_code, non_snake_case, clippy::missing_safety_doc)]
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
