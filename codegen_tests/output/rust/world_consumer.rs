#![cfg_attr(any(), rustfmt::skip)]
#[allow(unused_imports)]
use crate::world::deep::marker::DeepMarker;
#[repr(C, align(8))]
/// Consumes the world. Doc-links a type from a deeper, un-imported module
/// ([`DeepMarker`](world::deep::marker::DeepMarker)) to exercise the
/// doc-import path: the emitted `use` must split module and item segments at
/// the declaring module's prefix, not this module's depth.
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
