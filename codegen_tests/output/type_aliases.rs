#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
/// Container using the alias
pub struct Container {
    pub ptr: *const crate::math::Vector3,
    pub int_ptr: *mut i32,
}
fn _Container_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], Container>([0u8; 0x10]);
    }
    unreachable!()
}
impl Container {}
impl std::convert::AsRef<Container> for Container {
    fn as_ref(&self) -> &Container {
        self
    }
}
impl std::convert::AsMut<Container> for Container {
    fn as_mut(&mut self) -> &mut Container {
        self
    }
}
/// Alias for a mutable i32 pointer.
pub type IntMutPtr = *mut i32;
/// Alias for a Vector3 pointer.
/// Demonstrates doc comments on type aliases.
pub type Vec3Ptr = *const crate::math::Vector3;
