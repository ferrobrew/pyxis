#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
/// Consumes the world. Doc-links a type from a deeper, un-imported module
/// ([`DeepMarker`](crate::world::deep::marker::DeepMarker)) to exercise cross-module
/// doc-link resolution: the link destination must be rewritten to an absolute
/// crate path so rustdoc resolves it without a `use` import.
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
