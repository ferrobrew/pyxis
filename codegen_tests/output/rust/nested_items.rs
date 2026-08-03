#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
/// A type with nested declarations.
///
/// See [`InnerEnum`](crate::nested_items::Outer_InnerEnum), [`InnerType`](crate::nested_items::Outer_InnerType), [`InnerFlags`](crate::nested_items::Outer_InnerFlags), and [`InnerAlias`](crate::nested_items::Outer_InnerAlias).
///
/// You can also qualify them: [`Outer::InnerEnum`](crate::nested_items::Outer_InnerEnum), [`Outer::InnerType`](crate::nested_items::Outer_InnerType).
pub struct Outer {
    /// Reference a nested item by its qualified name inside the parent's own
    /// body.
    pub inner: crate::nested_items::Outer_InnerFlags,
    pub field: u32,
}
fn _Outer_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Outer>([0u8; 0x8]);
    }
    unreachable!()
}
impl Outer {}
impl std::convert::AsRef<Outer> for Outer {
    fn as_ref(&self) -> &Outer {
        self
    }
}
impl std::convert::AsMut<Outer> for Outer {
    fn as_mut(&mut self) -> &mut Outer {
        self
    }
}
/// A type alias nested inside [`Outer`](crate::nested_items::Outer).
pub type Outer_InnerAlias = u32;
#[repr(u8)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
/// An enum nested inside [`Outer`](crate::nested_items::Outer).
///
/// Variants: [`InnerEnum::A`](crate::nested_items::Outer_InnerEnum::A), [`InnerEnum::B`](crate::nested_items::Outer_InnerEnum::B), [`InnerEnum::C`](crate::nested_items::Outer_InnerEnum::C).
pub enum Outer_InnerEnum {
    A = 0isize as _,
    B = 1isize as _,
    C = 2isize as _,
}
fn _Outer_InnerEnum_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x1], Outer_InnerEnum>([0u8; 0x1]);
    }
    unreachable!()
}
crate::__bitflags! {
    #[doc = " Bitflags nested inside [`Outer`](crate::nested_items::Outer)."] #[doc = ""]
    #[doc =
    " Members: [`InnerFlags::FLAG_A`](crate::nested_items::Outer_InnerFlags::FLAG_A), [`InnerFlags::FLAG_B`](crate::nested_items::Outer_InnerFlags::FLAG_B)."]
    pub struct Outer_InnerFlags : u32 { const FLAG_A = 1usize as _; const FLAG_B = 2usize
    as _; }
}
fn _Outer_InnerFlags_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Outer_InnerFlags>([0u8; 0x4]);
    }
    unreachable!()
}
#[repr(C, align(2))]
/// A type nested inside [`Outer`](crate::nested_items::Outer).
///
/// Its field is [`InnerType::inner_field`](crate::nested_items::Outer_InnerType::inner_field).
pub struct Outer_InnerType {
    pub inner_field: u16,
}
fn _Outer_InnerType_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x2], Outer_InnerType>([0u8; 0x2]);
    }
    unreachable!()
}
impl Outer_InnerType {}
impl std::convert::AsRef<Outer_InnerType> for Outer_InnerType {
    fn as_ref(&self) -> &Outer_InnerType {
        self
    }
}
impl std::convert::AsMut<Outer_InnerType> for Outer_InnerType {
    fn as_mut(&mut self) -> &mut Outer_InnerType {
        self
    }
}
