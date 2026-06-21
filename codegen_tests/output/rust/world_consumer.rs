#![allow(
    dead_code,
    non_snake_case,
    non_camel_case_types,
    non_upper_case_globals,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast,
    clippy::module_inception
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
pub struct WorldConsumer {
    pub weather: *mut crate::world::weather::Weather,
    pub world: *mut crate::world::World,
}
fn _WorldConsumer_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], WorldConsumer>([0u8; 0x10]);
    }
    unreachable!()
}
impl WorldConsumer {}
impl std::convert::AsRef<WorldConsumer> for WorldConsumer {
    fn as_ref(&self) -> &WorldConsumer {
        self
    }
}
impl std::convert::AsMut<WorldConsumer> for WorldConsumer {
    fn as_mut(&mut self) -> &mut WorldConsumer {
        self
    }
}
