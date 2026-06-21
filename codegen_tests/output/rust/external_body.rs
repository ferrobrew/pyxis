#![allow(
    dead_code,
    non_snake_case,
    non_upper_case_globals,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast,
    clippy::module_inception
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
#[repr(C, align(8))]
pub struct Pair<T> {
    pub first: *mut T,
    pub second: *mut T,
}
impl<T> Pair<T> {}
impl Counter {
    pub fn make() -> Counter {
        Counter { value: 0 }
    }
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
impl<T> Pair<T> {
    pub fn first_ptr(&self) -> *mut T {
        self.first
    }
    pub fn cast_first<Y>(&self) -> *mut Y {
        self.first as *mut Y
    }
}
