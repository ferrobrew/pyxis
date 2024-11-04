#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
bitflags::bitflags! {
    #[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Default)] pub struct TestBitflags :
    u32 { const NONE = 0isize as _; const FLAG_1 = 1isize as _; const FLAG_2 = 2isize as
    _; const FLAG_3 = 4isize as _; }
}
fn _TestBitflags_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], TestBitflags>([0u8; 0x4]);
    }
    unreachable!()
}
#[repr(u32)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
pub enum TestEnum {
    #[default]
    None = 0isize as _,
    Some = 1isize as _,
    Value = 2isize as _,
}
fn _TestEnum_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], TestEnum>([0u8; 0x4]);
    }
    unreachable!()
}
