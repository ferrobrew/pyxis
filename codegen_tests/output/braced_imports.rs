#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(4))]
/// Transform type that uses imported Matrix4 and Vector3
pub struct Transform {
    pub matrix: crate::math::Matrix4,
    pub position: crate::math::Vector3,
}
fn _Transform_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4C], Transform>([0u8; 0x4C]);
    }
    unreachable!()
}
impl Transform {}
impl std::convert::AsRef<Transform> for Transform {
    fn as_ref(&self) -> &Transform {
        self
    }
}
impl std::convert::AsMut<Transform> for Transform {
    fn as_mut(&mut self) -> &mut Transform {
        self
    }
}
