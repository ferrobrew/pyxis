#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
//! This is a module doc comment
//!
//! The best of its kind
#[repr(C, align(8))]
/// This is a doc comment
pub struct TestType {
    vftable: *const crate::doc_comments::TestTypeVftable,
    /// This is a field doc comment
    pub field_1: u64,
}
fn _TestType_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 16usize], TestType>([0u8; 16usize]);
    }
    unreachable!()
}
impl TestType {
    pub fn vftable(&self) -> *const crate::doc_comments::TestTypeVftable {
        self.vftable as *const crate::doc_comments::TestTypeVftable
    }
    /// My test func!
    ///
    /// And its second line! :)
    pub unsafe fn test_func(&self) {
        let f: unsafe extern "thiscall" fn(this: *const Self) = ::std::mem::transmute(
            291usize,
        );
        f(self as *const Self as _)
    }
    /// My test vfunc!
    pub unsafe fn test_vfunc(&self) {
        let f = std::ptr::addr_of!((* self.vftable()).test_vfunc).read();
        f(self as *const Self as _)
    }
}
#[repr(C, align(8))]
pub struct TestTypeVftable {
    /// My test vfunc!
    pub test_vfunc: unsafe extern "thiscall" fn(
        this: *const crate::doc_comments::TestType,
    ),
}
fn _TestTypeVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 8usize], TestTypeVftable>([0u8; 8usize]);
    }
    unreachable!()
}
impl TestTypeVftable {}
