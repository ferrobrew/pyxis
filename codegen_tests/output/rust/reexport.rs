#![cfg_attr(any(), rustfmt::skip)]
pub use crate::math::Matrix4;
pub use crate::math::Vector3;
#[repr(C, align(4))]
/// A type that sits alongside the re-exports and uses them directly.
pub struct Bundle {
    pub matrix: crate::math::Matrix4,
    pub position: crate::math::Vector3,
}
fn _Bundle_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4C], Bundle>([0u8; 0x4C]);
    }
    unreachable!()
}
impl Bundle {}
impl std::convert::AsRef<Bundle> for Bundle {
    fn as_ref(&self) -> &Bundle {
        self
    }
}
impl std::convert::AsMut<Bundle> for Bundle {
    fn as_mut(&mut self) -> &mut Bundle {
        self
    }
}
