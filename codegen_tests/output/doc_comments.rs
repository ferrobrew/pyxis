#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
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
        ::std::mem::transmute::<[u8; 0x10], TestType>([0u8; 0x10]);
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
            0x123 as usize,
        );
        f(self as *const Self as _)
    }
    /// My test vfunc!
    pub unsafe fn test_vfunc(&self) {
        let f = std::ptr::addr_of!((* self.vftable()).test_vfunc).read();
        f(self as *const Self as _)
    }
}
impl std::convert::AsRef<TestType> for TestType {
    fn as_ref(&self) -> &TestType {
        self
    }
}
impl std::convert::AsMut<TestType> for TestType {
    fn as_mut(&mut self) -> &mut TestType {
        self
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
        ::std::mem::transmute::<[u8; 0x8], TestTypeVftable>([0u8; 0x8]);
    }
    unreachable!()
}
impl TestTypeVftable {}
impl std::convert::AsRef<TestTypeVftable> for TestTypeVftable {
    fn as_ref(&self) -> &TestTypeVftable {
        self
    }
}
impl std::convert::AsMut<TestTypeVftable> for TestTypeVftable {
    fn as_mut(&mut self) -> &mut TestTypeVftable {
        self
    }
}
