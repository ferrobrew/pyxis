#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
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
impl TestEnum {
    pub unsafe fn test() {
        unsafe {
            let f: unsafe extern "system" fn() = ::std::mem::transmute(0x123 as usize);
            f()
        }
    }
    pub unsafe fn another_test(&self) -> i32 {
        unsafe {
            let f: unsafe extern "system" fn(this: *const Self) -> i32 = ::std::mem::transmute(
                0x456 as usize,
            );
            f(self as *const Self as _)
        }
    }
}
