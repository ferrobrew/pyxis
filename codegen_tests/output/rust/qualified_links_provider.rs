#![cfg_attr(any(), rustfmt::skip)]
/// An extern value.
pub unsafe fn get_shared_extern() -> &'static mut *mut u32 {
    unsafe { &mut *(0x200 as *mut *mut u32) }
}
/// A module with a freestanding function.
pub unsafe fn shared_function() {
    unsafe {
        let f: unsafe extern "system" fn() = ::std::mem::transmute(0x100 as usize);
        f()
    }
}
