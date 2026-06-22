#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
/// Weather state for the world.
pub struct Weather {
    pub temperature: f32,
    pub humidity: f32,
}
fn _Weather_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Weather>([0u8; 0x8]);
    }
    unreachable!()
}
impl Weather {}
impl std::convert::AsRef<Weather> for Weather {
    fn as_ref(&self) -> &Weather {
        self
    }
}
impl std::convert::AsMut<Weather> for Weather {
    fn as_mut(&mut self) -> &mut Weather {
        self
    }
}
