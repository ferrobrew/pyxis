#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(4))]
/// Type with array alias
pub struct DataContainer {
    pub raw_data: crate::type_aliases::Matrix4Data,
    pub value: crate::type_aliases::MyInt,
}
fn _DataContainer_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x44], DataContainer>([0u8; 0x44]);
    }
    unreachable!()
}
impl DataContainer {}
impl std::convert::AsRef<DataContainer> for DataContainer {
    fn as_ref(&self) -> &DataContainer {
        self
    }
}
impl std::convert::AsMut<DataContainer> for DataContainer {
    fn as_mut(&mut self) -> &mut DataContainer {
        self
    }
}
/// Type alias to an array type
pub type Matrix4Data = [f32; 16];
/// Type alias to a mutable pointer
pub type MutableMatrixPtr = *mut crate::math::Matrix4;
/// Simple type alias to a primitive
pub type MyInt = i32;
/// Type alias to an unsigned integer
pub type MyU64 = u64;
#[repr(C, align(8))]
/// Type with pointer aliases (properly aligned)
pub struct PointerContainer {
    pub position_ptr: crate::type_aliases::VectorPtr,
    pub matrix_ptr: crate::type_aliases::MutableMatrixPtr,
}
fn _PointerContainer_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], PointerContainer>([0u8; 0x10]);
    }
    unreachable!()
}
impl PointerContainer {}
impl std::convert::AsRef<PointerContainer> for PointerContainer {
    fn as_ref(&self) -> &PointerContainer {
        self
    }
}
impl std::convert::AsMut<PointerContainer> for PointerContainer {
    fn as_mut(&mut self) -> &mut PointerContainer {
        self
    }
}
#[repr(C, align(4))]
/// Simple type that uses a primitive type alias
pub struct SimpleContainer {
    pub value1: crate::type_aliases::MyInt,
    pub value2: crate::type_aliases::MyInt,
}
fn _SimpleContainer_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], SimpleContainer>([0u8; 0x8]);
    }
    unreachable!()
}
impl SimpleContainer {}
impl std::convert::AsRef<SimpleContainer> for SimpleContainer {
    fn as_ref(&self) -> &SimpleContainer {
        self
    }
}
impl std::convert::AsMut<SimpleContainer> for SimpleContainer {
    fn as_mut(&mut self) -> &mut SimpleContainer {
        self
    }
}
/// Type alias to a pointer type
pub type VectorPtr = *const crate::math::Vector3;
