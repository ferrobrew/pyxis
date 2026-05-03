#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(4))]
pub struct Counter {
    pub value: u32,
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
impl Counter {
    pub fn bump(&mut self) -> u32 {
        self.value += 1;
        self.value
    }
    pub fn read(&self) -> u32 {
        self.value
    }
}
pub fn module_hello() -> u32 {
    42
}
