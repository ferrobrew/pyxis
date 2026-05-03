#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
/// Container using generic types
pub struct Container {
    pub entity: crate::generics::Shared<crate::math::Vector3>,
    pub weak_entity: crate::generics::WeakPtr<crate::math::Vector3>,
    pub entry: crate::generics::MapEntry<u32, crate::math::Vector3>,
}
fn _Container_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x20], Container>([0u8; 0x20]);
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
#[repr(C, align(8))]
/// Array of generic types
pub struct GenericArray {
    pub items: [crate::generics::Shared<crate::math::Vector3>; 4],
}
fn _GenericArray_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x20], GenericArray>([0u8; 0x20]);
    }
    unreachable!()
}
impl GenericArray {}
impl std::convert::AsRef<GenericArray> for GenericArray {
    fn as_ref(&self) -> &GenericArray {
        self
    }
}
impl std::convert::AsMut<GenericArray> for GenericArray {
    fn as_mut(&mut self) -> &mut GenericArray {
        self
    }
}
#[repr(C, align(8))]
/// Pointer to generic type
pub struct GenericPointer {
    pub ptr: *mut crate::generics::Shared<crate::math::Vector3>,
}
fn _GenericPointer_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], GenericPointer>([0u8; 0x8]);
    }
    unreachable!()
}
impl GenericPointer {}
impl std::convert::AsRef<GenericPointer> for GenericPointer {
    fn as_ref(&self) -> &GenericPointer {
        self
    }
}
impl std::convert::AsMut<GenericPointer> for GenericPointer {
    fn as_mut(&mut self) -> &mut GenericPointer {
        self
    }
}
#[repr(C, align(8))]
/// A key-value map entry.
pub struct MapEntry<K, V> {
    pub key: *mut K,
    pub value: *mut V,
}
impl<K, V> MapEntry<K, V> {}
#[repr(C, align(8))]
/// A shared pointer to a type.
/// The inner pointer is mutable.
pub struct Shared<T> {
    pub ptr: *mut T,
}
impl<T> Shared<T> {}
#[repr(C, align(8))]
/// A weak pointer to a type.
/// The inner pointer is const.
pub struct WeakPtr<T> {
    pub ptr: *const T,
}
impl<T> WeakPtr<T> {}
