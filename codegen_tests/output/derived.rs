#![allow(dead_code, non_snake_case, clippy::missing_safety_doc)]
#[repr(C, align(8))]
struct BaseA {
    vftable: *const crate::derived::BaseAVftable,
    pub field_a: u64,
}
fn _BaseA_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 16usize], BaseA>([0u8; 16usize]);
    }
    unreachable!()
}
impl BaseA {
    pub fn vftable(&self) -> *const crate::derived::BaseAVftable {
        self.vftable as *const crate::derived::BaseAVftable
    }
    pub unsafe fn base_a_free(&self, a: i32) -> i32 {
        let f: unsafe extern "thiscall" fn(this: *const Self, a: i32) -> i32 = ::std::mem::transmute(
            291usize,
        );
        f(self as *const Self as _, a)
    }
    pub unsafe fn base_a_vfunc(&self, a: i32) -> i32 {
        let f = std::ptr::addr_of!((* self.vftable()).base_a_vfunc).read();
        f(self as *const Self as _, a)
    }
}
#[repr(C, align(8))]
struct BaseAVftable {
    pub base_a_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::derived::BaseA,
        a: i32,
    ) -> i32,
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
    vftable: *const crate::derived::BaseBVftable,
    pub field_b: u64,
}
fn _BaseB_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 16usize], BaseB>([0u8; 16usize]);
    }
    unreachable!()
}
impl BaseB {
    pub fn vftable(&self) -> *const crate::derived::BaseBVftable {
        self.vftable as *const crate::derived::BaseBVftable
    }
    pub unsafe fn base_b_free(&self, a: i32) -> i32 {
        let f: unsafe extern "thiscall" fn(this: *const Self, a: i32) -> i32 = ::std::mem::transmute(
            1110usize,
        );
        f(self as *const Self as _, a)
    }
    pub unsafe fn base_b_vfunc(&self, a: i32) -> i32 {
        let f = std::ptr::addr_of!((* self.vftable()).base_b_vfunc).read();
        f(self as *const Self as _, a)
    }
}
#[repr(C, align(8))]
struct BaseBVftable {
    pub base_b_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::derived::BaseB,
        a: i32,
    ) -> i32,
}
fn _BaseBVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 8usize], BaseBVftable>([0u8; 8usize]);
    }
    unreachable!()
}
impl BaseBVftable {}
#[repr(C, align(8))]
struct Derived {
    pub base_a: crate::derived::BaseA,
    pub base_b: crate::derived::BaseB,
}
fn _Derived_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 32usize], Derived>([0u8; 32usize]);
    }
    unreachable!()
}
impl Derived {
    pub fn vftable(&self) -> *const crate::derived::DerivedVftable {
        self.base_a.vftable as *const crate::derived::DerivedVftable
    }
    pub unsafe fn base_a_free(&self, a: i32) -> i32 {
        self.base_a.base_a_free(a)
    }
    pub unsafe fn base_b_free(&self, a: i32) -> i32 {
        self.base_b.base_b_free(a)
    }
    pub unsafe fn derived_free(&self, a: i32) -> i32 {
        let f: unsafe extern "thiscall" fn(this: *const Self, a: i32) -> i32 = ::std::mem::transmute(
            1929usize,
        );
        f(self as *const Self as _, a)
    }
    pub unsafe fn base_a_vfunc(&self, a: i32) -> i32 {
        let f = std::ptr::addr_of!((* self.vftable()).base_a_vfunc).read();
        f(self as *const Self as _, a)
    }
    pub unsafe fn derived_vfunc(&self, a: i32) -> i32 {
        let f = std::ptr::addr_of!((* self.vftable()).derived_vfunc).read();
        f(self as *const Self as _, a)
    }
}
#[repr(C, align(8))]
struct DerivedVftable {
    pub base_a_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::derived::Derived,
        a: i32,
    ) -> i32,
    pub derived_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::derived::Derived,
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
