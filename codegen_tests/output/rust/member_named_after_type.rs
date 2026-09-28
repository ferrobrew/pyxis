#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
/// A type with a nested type named after it, referenced by a field.
pub struct Catalogue {
    pub inner: crate::member_named_after_type::Catalogue_Catalogue,
    pub shelf: crate::member_named_after_type::Catalogue_Shelf,
    pub kind: crate::member_named_after_type::Catalogue_Kind,
    pub reserved: u32,
}
fn _Catalogue_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], Catalogue>([0u8; 0x10]);
    }
    unreachable!()
}
impl Catalogue {}
impl std::convert::AsRef<Catalogue> for Catalogue {
    fn as_ref(&self) -> &Catalogue {
        self
    }
}
impl std::convert::AsMut<Catalogue> for Catalogue {
    fn as_mut(&mut self) -> &mut Catalogue {
        self
    }
}
#[repr(C, align(4))]
/// A nested type named after its enclosing type. Its field is
/// [`Catalogue::Catalogue::entries`](crate::member_named_after_type::Catalogue_Catalogue::entries).
pub struct Catalogue_Catalogue {
    pub entries: u32,
}
fn _Catalogue_Catalogue_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Catalogue_Catalogue>([0u8; 0x4]);
    }
    unreachable!()
}
impl Catalogue_Catalogue {}
impl std::convert::AsRef<Catalogue_Catalogue> for Catalogue_Catalogue {
    fn as_ref(&self) -> &Catalogue_Catalogue {
        self
    }
}
impl std::convert::AsMut<Catalogue_Catalogue> for Catalogue_Catalogue {
    fn as_mut(&mut self) -> &mut Catalogue_Catalogue {
        self
    }
}
#[repr(u32)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
/// A nested enum with a variant named after it:
/// [`Kind::Kind`](crate::member_named_after_type::Catalogue_Kind::Kind).
pub enum Catalogue_Kind {
    Kind = 0isize as _,
    Other = 1isize as _,
}
fn _Catalogue_Kind_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Catalogue_Kind>([0u8; 0x4]);
    }
    unreachable!()
}
crate::__bitflags! {
    #[doc = " Nested bitflags with a flag named after them:"] #[doc =
    " [`Shelf::Shelf`](crate::member_named_after_type::Catalogue_Shelf::Shelf)."] pub
    struct Catalogue_Shelf : u32 { const Shelf = 1usize as _; const Other = 2usize as _;
    }
}
fn _Catalogue_Shelf_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Catalogue_Shelf>([0u8; 0x4]);
    }
    unreachable!()
}
#[repr(C, align(4))]
/// A union with a member named after it: [`Choice::Choice`](crate::member_named_after_type::Choice::Choice).
pub union Choice {
    pub Choice: ::core::mem::ManuallyDrop<u32>,
    pub other: ::core::mem::ManuallyDrop<f32>,
}
fn _Choice_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Choice>([0u8; 0x4]);
    }
    unreachable!()
}
impl ::core::fmt::Debug for Choice {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.write_str(concat!("Choice", " { .. }"))
    }
}
#[repr(C, align(4))]
/// A pinned type with a field named after it: [`Counter::Counter`](crate::member_named_after_type::Counter::Counter).
pub struct Counter {
    pub Counter: u32,
    #[doc(hidden)]
    _pin: ::std::marker::PhantomPinned,
}
fn _Counter_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Counter>([0u8; 0x4]);
    }
    unreachable!()
}
impl Counter {}
impl std::convert::AsRef<Counter> for Counter {
    fn as_ref(&self) -> &Counter {
        self
    }
}
impl std::convert::AsMut<Counter> for Counter {
    fn as_mut(&mut self) -> &mut Counter {
        self
    }
}
#[repr(C, align(8))]
/// A type with a vftable function named after it:
/// [`Dispatcher::Dispatcher`](crate::member_named_after_type::Dispatcher::Dispatcher).
pub struct Dispatcher {
    vftable: *const crate::member_named_after_type::DispatcherVftable,
}
fn _Dispatcher_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Dispatcher>([0u8; 0x8]);
    }
    unreachable!()
}
impl Dispatcher {
    pub fn vftable(&self) -> *const crate::member_named_after_type::DispatcherVftable {
        self.vftable as *const crate::member_named_after_type::DispatcherVftable
    }
    pub unsafe fn Dispatcher(&mut self, value: u32) -> u32 {
        unsafe {
            let f = (&raw const (*self.vftable()).Dispatcher).read();
            f(self as *mut Self as _, value)
        }
    }
}
impl std::convert::AsRef<Dispatcher> for Dispatcher {
    fn as_ref(&self) -> &Dispatcher {
        self
    }
}
impl std::convert::AsMut<Dispatcher> for Dispatcher {
    fn as_mut(&mut self) -> &mut Dispatcher {
        self
    }
}
#[repr(C, align(8))]
pub struct DispatcherVftable {
    pub Dispatcher: unsafe extern "system" fn(
        this: *mut crate::member_named_after_type::Dispatcher,
        value: u32,
    ) -> u32,
}
fn _DispatcherVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], DispatcherVftable>([0u8; 0x8]);
    }
    unreachable!()
}
impl DispatcherVftable {}
impl std::convert::AsRef<DispatcherVftable> for DispatcherVftable {
    fn as_ref(&self) -> &DispatcherVftable {
        self
    }
}
impl std::convert::AsMut<DispatcherVftable> for DispatcherVftable {
    fn as_mut(&mut self) -> &mut DispatcherVftable {
        self
    }
}
#[repr(C, align(4))]
/// A type whose nested items are named after it: [`Registry::Registry`](crate::member_named_after_type::Registry::Registry) is a
/// constant, and [`Catalogue::Catalogue`](crate::member_named_after_type::Catalogue_Catalogue) is a nested type.
pub struct Registry {
    pub size: u32,
}
fn _Registry_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Registry>([0u8; 0x4]);
    }
    unreachable!()
}
impl Registry {}
impl Registry {
    /// A nested constant named after its enclosing type.
    pub const Registry: u32 = 7;
}
impl std::convert::AsRef<Registry> for Registry {
    fn as_ref(&self) -> &Registry {
        self
    }
}
impl std::convert::AsMut<Registry> for Registry {
    fn as_mut(&mut self) -> &mut Registry {
        self
    }
}
#[repr(C, align(4))]
/// A type that forwards [`Teleporter::Teleporter`](crate::member_named_after_type::Teleporter::Teleporter) through its base field
/// [`Station::base`](crate::member_named_after_type::Station::base).
pub struct Station {
    pub base: crate::member_named_after_type::Teleporter,
}
fn _Station_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Station>([0u8; 0x4]);
    }
    unreachable!()
}
impl Station {
    /// Teleports.
    pub unsafe fn Teleporter(&mut self) -> bool {
        unsafe { self.base.Teleporter() }
    }
}
impl std::convert::AsRef<crate::member_named_after_type::Teleporter> for Station {
    fn as_ref(&self) -> &crate::member_named_after_type::Teleporter {
        &self.base
    }
}
impl std::convert::AsMut<crate::member_named_after_type::Teleporter> for Station {
    fn as_mut(&mut self) -> &mut crate::member_named_after_type::Teleporter {
        &mut self.base
    }
}
impl std::convert::AsRef<Station> for Station {
    fn as_ref(&self) -> &Station {
        self
    }
}
impl std::convert::AsMut<Station> for Station {
    fn as_mut(&mut self) -> &mut Station {
        self
    }
}
#[repr(C, align(4))]
/// A type with a method named after it: [`Teleporter::Teleporter`](crate::member_named_after_type::Teleporter::Teleporter).
pub struct Teleporter {
    pub state: u32,
}
fn _Teleporter_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Teleporter>([0u8; 0x4]);
    }
    unreachable!()
}
impl Teleporter {
    /// Teleports.
    pub unsafe fn Teleporter(&mut self) -> bool {
        unsafe {
            let f: unsafe extern "system" fn(this: *mut Self) -> bool = ::std::mem::transmute(
                0x3000 as usize,
            );
            f(self as *mut Self as _)
        }
    }
}
impl std::convert::AsRef<Teleporter> for Teleporter {
    fn as_ref(&self) -> &Teleporter {
        self
    }
}
impl std::convert::AsMut<Teleporter> for Teleporter {
    fn as_mut(&mut self) -> &mut Teleporter {
        self
    }
}
