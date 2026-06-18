#![allow(
    dead_code,
    non_snake_case,
    non_upper_case_globals,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(4))]
pub struct Probe {
    pub value: u32,
}
fn _Probe_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Probe>([0u8; 0x4]);
    }
    unreachable!()
}
impl Probe {}
impl std::convert::AsRef<Probe> for Probe {
    fn as_ref(&self) -> &Probe {
        self
    }
}
impl std::convert::AsMut<Probe> for Probe {
    fn as_mut(&mut self) -> &mut Probe {
        self
    }
}
impl Probe {
    pub fn read(&self) -> u32 {
        self.value
    }
    pub fn rust_only(&self) -> u32 {
        self.value + 1
    }
    pub fn rust_block_method(&self) -> u32 {
        self.value + 100
    }
}
