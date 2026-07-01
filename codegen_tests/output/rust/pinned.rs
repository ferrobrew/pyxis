#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(4))]
/// A type that is pinned and copyable (Copy is suppressed in Rust)
pub struct PinnedCopyable {
    pub value: u32,
    #[doc(hidden)]
    _pin: ::std::marker::PhantomPinned,
}
fn _PinnedCopyable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], PinnedCopyable>([0u8; 0x4]);
    }
    unreachable!()
}
impl PinnedCopyable {}
impl std::convert::AsRef<PinnedCopyable> for PinnedCopyable {
    fn as_ref(&self) -> &PinnedCopyable {
        self
    }
}
impl std::convert::AsMut<PinnedCopyable> for PinnedCopyable {
    fn as_mut(&mut self) -> &mut PinnedCopyable {
        self
    }
}
#[repr(u32)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
/// A pinned enum (IR/JSON-only; no effect on Rust/C++ output)
pub enum PinnedEnum {
    Option1 = 0isize as _,
    Option2 = 1isize as _,
}
fn _PinnedEnum_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], PinnedEnum>([0u8; 0x4]);
    }
    unreachable!()
}
crate::__bitflags! {
    #[doc = " A pinned bitflags (IR/JSON-only; no effect on Rust/C++ output)"] pub struct
    PinnedFlags : u32 { const Flag1 = 1usize as _; const Flag2 = 2usize as _; }
}
fn _PinnedFlags_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], PinnedFlags>([0u8; 0x4]);
    }
    unreachable!()
}
#[repr(C, align(4))]
/// A type that must not be relocated
pub struct PinnedType {
    pub value: u32,
    #[doc(hidden)]
    _pin: ::std::marker::PhantomPinned,
}
fn _PinnedType_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], PinnedType>([0u8; 0x4]);
    }
    unreachable!()
}
impl PinnedType {}
impl std::convert::AsRef<PinnedType> for PinnedType {
    fn as_ref(&self) -> &PinnedType {
        self
    }
}
impl std::convert::AsMut<PinnedType> for PinnedType {
    fn as_mut(&mut self) -> &mut PinnedType {
        self
    }
}
