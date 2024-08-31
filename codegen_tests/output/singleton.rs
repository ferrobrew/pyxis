#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(4))]
pub struct TestType {
    pub field_1: u32,
}
fn _TestType_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], TestType>([0u8; 0x4]);
    }
    unreachable!()
}
impl TestType {
    pub unsafe fn get() -> Option<&'static mut Self> {
        unsafe {
            let ptr: *mut Self = *(322401073usize as *mut *mut Self);
            ptr.as_mut()
        }
    }
}
impl TestType {}
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
