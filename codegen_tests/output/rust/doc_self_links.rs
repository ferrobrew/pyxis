#![cfg_attr(any(), rustfmt::skip)]
//! Self-link testing module. Its own doc links [`Container`](crate::doc_self_links::Container) — the module's
//! doc block must keep its links separate from its first item's, since the
//! module's source location is a proxy borrowed from that item.
#[repr(C, align(4))]
/// Test `Self::` links in type docs.
///
/// Link to a field as [`Self::field`](crate::doc_self_links::Container::field), to a method as
/// [`Self::method`](crate::doc_self_links::Container::method), to a nested type as
/// [`Self::Nested`](crate::doc_self_links::Container_Nested), and to a nested type's member as
/// [`Self::Nested::nested_field`](crate::doc_self_links::Container_Nested::nested_field).
pub struct Container {
    /// A field.
    pub field: u32,
}
fn _Container_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Container>([0u8; 0x4]);
    }
    unreachable!()
}
impl Container {
    /// A method.
    pub unsafe fn method(&self) {
        unsafe {
            let f: unsafe extern "system" fn(this: *const Self) = ::std::mem::transmute(
                0x10 as usize,
            );
            f(self as *const Self as _)
        }
    }
}
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
#[repr(C, align(2))]
/// A nested type. Its field is [`Self::nested_field`](crate::doc_self_links::Container_Nested::nested_field).
pub struct Container_Nested {
    pub nested_field: u16,
}
fn _Container_Nested_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x2], Container_Nested>([0u8; 0x2]);
    }
    unreachable!()
}
impl Container_Nested {}
impl std::convert::AsRef<Container_Nested> for Container_Nested {
    fn as_ref(&self) -> &Container_Nested {
        self
    }
}
impl std::convert::AsMut<Container_Nested> for Container_Nested {
    fn as_mut(&mut self) -> &mut Container_Nested {
        self
    }
}
#[repr(C, align(8))]
/// Test `Self::` links written in vftable-function docs. Those docs are
/// copied onto the generated `VirtualContainerVftable`'s function-pointer
/// fields, where `Self` must still mean the declaring type — not the
/// generated vftable struct.
pub struct VirtualContainer {
    vftable: *const crate::doc_self_links::VirtualContainerVftable,
    /// A field a vftable-function doc links to via `Self::`.
    pub counter: u32,
    pub padding: u32,
}
fn _VirtualContainer_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], VirtualContainer>([0u8; 0x10]);
    }
    unreachable!()
}
impl VirtualContainer {
    pub fn vftable(&self) -> *const crate::doc_self_links::VirtualContainerVftable {
        self.vftable as *const crate::doc_self_links::VirtualContainerVftable
    }
    /// Reads [`counter`](crate::doc_self_links::VirtualContainer::counter) and calls
    /// [`get_counter`](crate::doc_self_links::VirtualContainer::get_counter).
    pub unsafe fn get_counter(&self) -> u32 {
        unsafe {
            let f = (&raw const (*self.vftable()).get_counter).read();
            f(self as *const Self as _)
        }
    }
}
impl std::convert::AsRef<VirtualContainer> for VirtualContainer {
    fn as_ref(&self) -> &VirtualContainer {
        self
    }
}
impl std::convert::AsMut<VirtualContainer> for VirtualContainer {
    fn as_mut(&mut self) -> &mut VirtualContainer {
        self
    }
}
#[repr(C, align(8))]
pub struct VirtualContainerVftable {
    /// Reads [`counter`](crate::doc_self_links::VirtualContainer::counter) and calls
    /// [`get_counter`](crate::doc_self_links::VirtualContainer::get_counter).
    pub get_counter: unsafe extern "system" fn(
        this: *const crate::doc_self_links::VirtualContainer,
    ) -> u32,
}
fn _VirtualContainerVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], VirtualContainerVftable>([0u8; 0x8]);
    }
    unreachable!()
}
impl VirtualContainerVftable {}
impl std::convert::AsRef<VirtualContainerVftable> for VirtualContainerVftable {
    fn as_ref(&self) -> &VirtualContainerVftable {
        self
    }
}
impl std::convert::AsMut<VirtualContainerVftable> for VirtualContainerVftable {
    fn as_mut(&mut self) -> &mut VirtualContainerVftable {
        self
    }
}
