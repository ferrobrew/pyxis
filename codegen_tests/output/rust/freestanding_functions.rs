#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(i32)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum A {
    None = 0isize as _,
}
fn _A_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], A>([0u8; 0x4]);
    }
    unreachable!()
}
impl A {
    unsafe fn test() {
        unsafe {
            let f: unsafe extern "system" fn() = ::std::mem::transmute(0x123 as usize);
            f()
        }
    }
}
pub unsafe fn freestanding() {
    unsafe {
        let f: unsafe extern "system" fn() = ::std::mem::transmute(0x456 as usize);
        f()
    }
}
unsafe fn another_freestanding(arg1: i32) -> i32 {
    unsafe {
        let f: unsafe extern "system" fn(arg1: i32) -> i32 = ::std::mem::transmute(
            0x789 as usize,
        );
        f(arg1)
    }
}
