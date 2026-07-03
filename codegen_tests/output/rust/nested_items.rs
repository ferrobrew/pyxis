#![cfg_attr(any(), rustfmt::skip)]
#[allow(unused_imports)]
use Outer_InnerAlias as InnerAlias;
#[allow(unused_imports)]
use Outer_InnerEnum as InnerEnum;
#[allow(unused_imports)]
use Outer_InnerFlags as InnerFlags;
#[allow(unused_imports)]
use Outer_InnerType as InnerType;
#[repr(C, align(4))]
/// A type with nested declarations.
///
/// See [`Outer_InnerEnum`], [`Outer_InnerType`], [`Outer_InnerFlags`], and [`Outer_InnerAlias`].
///
/// You can also qualify them: [`Outer_InnerEnum`], [`Outer_InnerType`].
pub struct Outer {
    pub field: u32,
}
fn _Outer_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Outer>([0u8; 0x4]);
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
/// A type alias nested inside [`Outer`].
pub type Outer_InnerAlias = u32;
#[repr(u8)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
/// An enum nested inside [`Outer`].
///
/// Variants: [`Outer_InnerEnum::A`], [`Outer_InnerEnum::B`], [`Outer_InnerEnum::C`].
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
    #[doc = " Bitflags nested inside [`Outer`]."] #[doc = ""] #[doc =
    " Members: [`Outer_InnerFlags::FLAG_A`], [`Outer_InnerFlags::FLAG_B`]."] pub struct
    Outer_InnerFlags : u32 { const FLAG_A = 1usize as _; const FLAG_B = 2usize as _; }
}
fn _Outer_InnerFlags_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Outer_InnerFlags>([0u8; 0x4]);
    }
    unreachable!()
}
#[repr(C, align(2))]
/// A type nested inside [`Outer`].
///
/// Its field is [`Outer_InnerType::inner_field`].
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
