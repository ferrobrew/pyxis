#![cfg_attr(any(), rustfmt::skip)]
type HandleRaw = *mut u8;
#[repr(C, align(8))]
pub struct Handle {
    pub raw: *mut u8,
}
fn _Handle_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Handle>([0u8; 0x8]);
    }
    unreachable!()
}
impl Handle {}
impl std::convert::AsRef<Handle> for Handle {
    fn as_ref(&self) -> &Handle {
        self
    }
}
impl std::convert::AsMut<Handle> for Handle {
    fn as_mut(&mut self) -> &mut Handle {
        self
    }
}
#[repr(C, align(4))]
pub struct Widget {
    pub id: u32,
}
fn _Widget_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Widget>([0u8; 0x4]);
    }
    unreachable!()
}
impl Widget {}
impl std::convert::AsRef<Widget> for Widget {
    fn as_ref(&self) -> &Widget {
        self
    }
}
impl std::convert::AsMut<Widget> for Widget {
    fn as_mut(&mut self) -> &mut Widget {
        self
    }
}
impl Widget {
    pub fn make() -> Widget {
        Widget { id: 0 }
    }
    pub fn get_id(&self) -> u32 {
        self.id
    }
}
pub fn module_init() -> u32 {
    1
}
impl Handle {
    pub fn null() -> Handle {
        Handle {
            raw: std::ptr::null_mut(),
        }
    }
}
