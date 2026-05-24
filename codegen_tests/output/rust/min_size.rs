#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(16))]
/// Another type to test min_size rounding
pub struct RoundedType {
    pub value: u64,
    _field_8: [u8; 24],
}
fn _RoundedType_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x20], RoundedType>([0u8; 0x20]);
    }
    unreachable!()
}
impl RoundedType {}
impl std::convert::AsRef<RoundedType> for RoundedType {
    fn as_ref(&self) -> &RoundedType {
        self
    }
}
impl std::convert::AsMut<RoundedType> for RoundedType {
    fn as_mut(&mut self) -> &mut RoundedType {
        self
    }
}
#[repr(C, align(16))]
/// Test type with min_size attribute
pub struct TestType {
    pub field_1: i32,
    pub field_2: i32,
    _field_8: [u8; 24],
}
fn _TestType_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x20], TestType>([0u8; 0x20]);
    }
    unreachable!()
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
