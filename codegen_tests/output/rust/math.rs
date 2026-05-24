#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(4))]
/// 4x4 matrix type for transformations
pub struct Matrix4 {
    pub data: [f32; 16],
}
fn _Matrix4_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x40], Matrix4>([0u8; 0x40]);
    }
    unreachable!()
}
impl Matrix4 {}
impl std::convert::AsRef<Matrix4> for Matrix4 {
    fn as_ref(&self) -> &Matrix4 {
        self
    }
}
impl std::convert::AsMut<Matrix4> for Matrix4 {
    fn as_mut(&mut self) -> &mut Matrix4 {
        self
    }
}
#[repr(C, align(4))]
/// 3D vector type
pub struct Vector3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}
fn _Vector3_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0xC], Vector3>([0u8; 0xC]);
    }
    unreachable!()
}
impl Vector3 {}
impl std::convert::AsRef<Vector3> for Vector3 {
    fn as_ref(&self) -> &Vector3 {
        self
    }
}
impl std::convert::AsMut<Vector3> for Vector3 {
    fn as_mut(&mut self) -> &mut Vector3 {
        self
    }
}
