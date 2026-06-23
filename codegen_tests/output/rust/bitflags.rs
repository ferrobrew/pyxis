#![cfg_attr(any(), rustfmt::skip)]
crate::__bitflags! {
    pub struct TestBitflags : u32 { const NONE = 0usize as _; const FLAG_1 = 1usize as _;
    const FLAG_2 = 2usize as _; const FLAG_3 = 4usize as _; }
}
fn _TestBitflags_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], TestBitflags>([0u8; 0x4]);
    }
    unreachable!()
}
crate::__bitflags! {
    pub struct TestBitflags2 : u32 { const NONE = 0usize as _; const FLAG_1 = 1usize as
    _; const FLAG_2 = 2usize as _; const FLAG_3 = 4usize as _; }
}
fn _TestBitflags2_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], TestBitflags2>([0u8; 0x4]);
    }
    unreachable!()
}
impl Default for TestBitflags2 {
    fn default() -> Self {
        Self::FLAG_2
    }
}
