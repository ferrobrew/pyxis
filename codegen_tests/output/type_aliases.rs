#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
/// Generic const pointer type alias
pub type ConstPtr<T> = *const T;
#[repr(C, align(8))]
/// Container using the non-generic type aliases
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
/// Alias for MapEntry with type parameters
pub type Entry<K, V> = crate::generics::MapEntry<K, V>;
#[repr(C, align(8))]
/// Container using generic aliases with concrete types
pub struct GenericAliasContainer {
    pub shared_vec: crate::generics::Shared<crate::math::Vector3>,
    pub weak_vec: crate::generics::WeakPtr<crate::math::Vector3>,
    pub entry: crate::generics::MapEntry<u32, crate::math::Vector3>,
    pub vec_ptr: *mut crate::math::Vector3,
    pub const_vec_ptr: *const crate::math::Vector3,
}
fn _GenericAliasContainer_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x30], GenericAliasContainer>([0u8; 0x30]);
    }
    unreachable!()
}
impl GenericAliasContainer {}
impl std::convert::AsRef<GenericAliasContainer> for GenericAliasContainer {
    fn as_ref(&self) -> &GenericAliasContainer {
        self
    }
}
impl std::convert::AsMut<GenericAliasContainer> for GenericAliasContainer {
    fn as_mut(&mut self) -> &mut GenericAliasContainer {
        self
    }
}
/// Alias for a mutable i32 pointer.
pub type IntMutPtr = *mut i32;
/// Generic pointer type alias
pub type Ptr<T> = *mut T;
/// Pointer to a generic type
pub type SharedPtr<T> = *mut crate::generics::Shared<T>;
/// Alias that wraps a generic type with a type parameter
pub type SharedRef<T> = crate::generics::Shared<T>;
/// Alias for a Vector3 pointer.
/// Demonstrates doc comments on type aliases.
pub type Vec3Ptr = *const crate::math::Vector3;
/// Alias for weak pointer to a type
pub type WeakRef<T> = crate::generics::WeakPtr<T>;
