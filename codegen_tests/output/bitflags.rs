#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
bitflags::bitflags! {
    #[derive(PartialEq, Eq, PartialOrd, Ord, Debug,)] pub struct TestBitflags : u32 {
    const NONE = 0usize as _; const FLAG_1 = 1usize as _; const FLAG_2 = 2usize as _;
    const FLAG_3 = 4usize as _; }
}
fn _TestBitflags_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], TestBitflags>([0u8; 0x4]);
    }
    unreachable!()
}
