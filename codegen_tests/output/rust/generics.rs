#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
/// Generic that embeds a concrete type by value (not behind a pointer), so
/// instantiating it by value must re-resolve `Payload` — the transitive value
/// dependency through a generic instantiation.
pub struct Boxed<T> {
    pub value: *mut T,
    pub payload: crate::generics::Payload,
    _field_c: [u8; 4],
}
impl<T> Boxed<T> {}
#[repr(C, align(8))]
/// Holds a generic instantiation by value. Regression guard:
/// `resolve_item` must resolve `Boxed`'s concrete field types, not just `Boxed`.
pub struct BoxedHolder {
    pub boxed: crate::generics::Boxed<crate::math::Vector3>,
}
fn _BoxedHolder_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], BoxedHolder>([0u8; 0x10]);
    }
    unreachable!()
}
impl BoxedHolder {}
impl std::convert::AsRef<BoxedHolder> for BoxedHolder {
    fn as_ref(&self) -> &BoxedHolder {
        self
    }
}
impl std::convert::AsMut<BoxedHolder> for BoxedHolder {
    fn as_mut(&mut self) -> &mut BoxedHolder {
        self
    }
}
#[repr(C, align(8))]
/// A vftable method takes a generic by value. Regression guard: a generic
/// referenced *only* in a vftable signature must still be resolved to build the
/// vftable.
pub struct BoxedRegistry {
    vftable: *const crate::generics::BoxedRegistryVftable,
}
fn _BoxedRegistry_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], BoxedRegistry>([0u8; 0x8]);
    }
    unreachable!()
}
impl BoxedRegistry {
    pub fn vftable(&self) -> *const crate::generics::BoxedRegistryVftable {
        self.vftable as *const crate::generics::BoxedRegistryVftable
    }
    pub unsafe fn add(&mut self, boxed: crate::generics::Boxed<crate::math::Vector3>) {
        unsafe {
            let f = (&raw const (*self.vftable()).add).read();
            f(self as *mut Self as _, boxed)
        }
    }
}
impl std::convert::AsRef<BoxedRegistry> for BoxedRegistry {
    fn as_ref(&self) -> &BoxedRegistry {
        self
    }
}
impl std::convert::AsMut<BoxedRegistry> for BoxedRegistry {
    fn as_mut(&mut self) -> &mut BoxedRegistry {
        self
    }
}
#[repr(C, align(8))]
pub struct BoxedRegistryVftable {
    pub add: unsafe extern "system" fn(
        this: *mut crate::generics::BoxedRegistry,
        boxed: crate::generics::Boxed<crate::math::Vector3>,
    ),
}
fn _BoxedRegistryVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], BoxedRegistryVftable>([0u8; 0x8]);
    }
    unreachable!()
}
impl BoxedRegistryVftable {}
impl std::convert::AsRef<BoxedRegistryVftable> for BoxedRegistryVftable {
    fn as_ref(&self) -> &BoxedRegistryVftable {
        self
    }
}
impl std::convert::AsMut<BoxedRegistryVftable> for BoxedRegistryVftable {
    fn as_mut(&mut self) -> &mut BoxedRegistryVftable {
        self
    }
}
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
#[repr(C, align(4))]
/// A concrete (non-generic) payload embedded *by value* inside a generic.
pub struct Payload {
    pub tag: u32,
}
fn _Payload_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Payload>([0u8; 0x4]);
    }
    unreachable!()
}
impl Payload {}
impl std::convert::AsRef<Payload> for Payload {
    fn as_ref(&self) -> &Payload {
        self
    }
}
impl std::convert::AsMut<Payload> for Payload {
    fn as_mut(&mut self) -> &mut Payload {
        self
    }
}
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
