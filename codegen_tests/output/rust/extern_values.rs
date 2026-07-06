#![cfg_attr(any(), rustfmt::skip)]
crate::__bitflags! {
    pub struct DebugFlags : u32 { const WIREFRAME = 1usize as _; const OVERDRAW = 2usize
    as _; }
}
fn _DebugFlags_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], DebugFlags>([0u8; 0x4]);
    }
    unreachable!()
}
impl DebugFlags {
    pub unsafe fn get_g_active() -> &'static mut *mut crate::extern_values::DebugFlags {
        unsafe { &mut *(0x5000 as *mut *mut crate::extern_values::DebugFlags) }
    }
}
#[repr(C, align(4))]
pub struct Engine {
    pub frame: u32,
}
fn _Engine_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Engine>([0u8; 0x4]);
    }
    unreachable!()
}
impl Engine {}
impl Engine {
    pub unsafe fn get_g_instance() -> &'static mut *mut crate::extern_values::Engine {
        unsafe { &mut *(0x3000 as *mut *mut crate::extern_values::Engine) }
    }
}
impl std::convert::AsRef<Engine> for Engine {
    fn as_ref(&self) -> &Engine {
        self
    }
}
impl std::convert::AsMut<Engine> for Engine {
    fn as_mut(&mut self) -> &mut Engine {
        self
    }
}
#[repr(u8)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum RenderMode {
    Forward = 0isize as _,
    Deferred = 1isize as _,
}
fn _RenderMode_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x1], RenderMode>([0u8; 0x1]);
    }
    unreachable!()
}
impl RenderMode {
    pub unsafe fn get_g_current() -> &'static mut *mut crate::extern_values::RenderMode {
        unsafe { &mut *(0x4000 as *mut *mut crate::extern_values::RenderMode) }
    }
}
/// The engine singleton, of type [`Engine`].
pub unsafe fn get_g_engine() -> &'static mut *mut crate::extern_values::Engine {
    unsafe { &mut *(0x2000 as *mut *mut crate::extern_values::Engine) }
}
pub unsafe fn get_g_frame_count() -> &'static mut u32 {
    unsafe { &mut *(0x1000 as *mut u32) }
}
