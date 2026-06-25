#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(4))]
pub struct Cc {
    pub field: u32,
}
fn _Cc_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Cc>([0u8; 0x4]);
    }
    unreachable!()
}
impl Cc {
    pub unsafe fn c_method(&mut self, x: i32) -> i32 {
        unsafe {
            let f: unsafe extern "C" fn(this: *mut Self, x: i32) -> i32 = ::std::mem::transmute(
                0x1001 as usize,
            );
            f(self as *mut Self as _, x)
        }
    }
}
impl std::convert::AsRef<Cc> for Cc {
    fn as_ref(&self) -> &Cc {
        self
    }
}
impl std::convert::AsMut<Cc> for Cc {
    fn as_mut(&mut self) -> &mut Cc {
        self
    }
}
pub unsafe fn c_freestanding(x: i32, y: i32) -> i32 {
    unsafe {
        let f: unsafe extern "C" fn(x: i32, y: i32) -> i32 = ::std::mem::transmute(
            0x1000 as usize,
        );
        f(x, y)
    }
}
