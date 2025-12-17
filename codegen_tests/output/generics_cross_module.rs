#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
/// Entity type for testing
pub struct Entity {
    pub id: u32,
    pub flags: u32,
}
fn _Entity_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Entity>([0u8; 0x8]);
    }
    unreachable!()
}
impl Entity {}
impl std::convert::AsRef<Entity> for Entity {
    fn as_ref(&self) -> &Entity {
        self
    }
}
impl std::convert::AsMut<Entity> for Entity {
    fn as_mut(&mut self) -> &mut Entity {
        self
    }
}
#[repr(C, align(8))]
/// Uses MapEntry<K, V> from another module
pub struct EntityMap {
    pub entry: crate::generics::MapEntry<u32, crate::generics_cross_module::Entity>,
}
fn _EntityMap_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], EntityMap>([0u8; 0x10]);
    }
    unreachable!()
}
impl EntityMap {}
impl std::convert::AsRef<EntityMap> for EntityMap {
    fn as_ref(&self) -> &EntityMap {
        self
    }
}
impl std::convert::AsMut<EntityMap> for EntityMap {
    fn as_mut(&mut self) -> &mut EntityMap {
        self
    }
}
#[repr(C, align(8))]
/// Nested generic from another module
pub struct NestedShared {
    pub shared: crate::generics::Shared<
        crate::generics::Shared<crate::generics_cross_module::Entity>,
    >,
}
fn _NestedShared_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], NestedShared>([0u8; 0x8]);
    }
    unreachable!()
}
impl NestedShared {}
impl std::convert::AsRef<NestedShared> for NestedShared {
    fn as_ref(&self) -> &NestedShared {
        self
    }
}
impl std::convert::AsMut<NestedShared> for NestedShared {
    fn as_mut(&mut self) -> &mut NestedShared {
        self
    }
}
#[repr(C, align(8))]
/// Uses Shared<T> from another module with a local type
pub struct SharedEntity {
    pub shared: crate::generics::Shared<crate::generics_cross_module::Entity>,
}
fn _SharedEntity_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], SharedEntity>([0u8; 0x8]);
    }
    unreachable!()
}
impl SharedEntity {}
impl std::convert::AsRef<SharedEntity> for SharedEntity {
    fn as_ref(&self) -> &SharedEntity {
        self
    }
}
impl std::convert::AsMut<SharedEntity> for SharedEntity {
    fn as_mut(&mut self) -> &mut SharedEntity {
        self
    }
}
#[repr(C, align(8))]
/// Uses WeakPtr<T> from another module
pub struct WeakEntity {
    pub weak: crate::generics::WeakPtr<crate::generics_cross_module::Entity>,
}
fn _WeakEntity_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], WeakEntity>([0u8; 0x8]);
    }
    unreachable!()
}
impl WeakEntity {}
impl std::convert::AsRef<WeakEntity> for WeakEntity {
    fn as_ref(&self) -> &WeakEntity {
        self
    }
}
impl std::convert::AsMut<WeakEntity> for WeakEntity {
    fn as_mut(&mut self) -> &mut WeakEntity {
        self
    }
}
