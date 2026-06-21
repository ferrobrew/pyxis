#![allow(
    dead_code,
    non_snake_case,
    non_upper_case_globals,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast,
    clippy::module_inception
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
pub struct Atmosphere {
    pub weather: *mut crate::world::weather::Weather,
    pub temperature: f32,
    pub humidity: f32,
}
fn _Atmosphere_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], Atmosphere>([0u8; 0x10]);
    }
    unreachable!()
}
impl Atmosphere {}
impl std::convert::AsRef<Atmosphere> for Atmosphere {
    fn as_ref(&self) -> &Atmosphere {
        self
    }
}
impl std::convert::AsMut<Atmosphere> for Atmosphere {
    fn as_mut(&mut self) -> &mut Atmosphere {
        self
    }
}
