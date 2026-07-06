#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
pub struct MovieImpl {
    pub Viewport: crate::field_type_name_collision::Viewport,
}
fn _MovieImpl_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], MovieImpl>([0u8; 0x8]);
    }
    unreachable!()
}
impl MovieImpl {
    pub unsafe fn set_viewport(
        &mut self,
        desc: *const crate::field_type_name_collision::Viewport,
    ) {
        unsafe {
            let f: unsafe extern "system" fn(
                this: *mut Self,
                desc: *const crate::field_type_name_collision::Viewport,
            ) = ::std::mem::transmute(0x2000 as usize);
            f(self as *mut Self as _, desc)
        }
    }
}
impl std::convert::AsRef<MovieImpl> for MovieImpl {
    fn as_ref(&self) -> &MovieImpl {
        self
    }
}
impl std::convert::AsMut<MovieImpl> for MovieImpl {
    fn as_mut(&mut self) -> &mut MovieImpl {
        self
    }
}
#[repr(C, align(8))]
pub struct Viewport {
    pub width: u32,
    pub height: u32,
}
fn _Viewport_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Viewport>([0u8; 0x8]);
    }
    unreachable!()
}
impl Viewport {}
impl std::convert::AsRef<Viewport> for Viewport {
    fn as_ref(&self) -> &Viewport {
        self
    }
}
impl std::convert::AsMut<Viewport> for Viewport {
    fn as_mut(&mut self) -> &mut Viewport {
        self
    }
}
