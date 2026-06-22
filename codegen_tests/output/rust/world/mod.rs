#![cfg_attr(any(), rustfmt::skip)]
//! The world subsystem.
//! This module is backed by a `mod.pyxis`, so it exercises folder-level
//! modules: a module doc comment, an item defined directly in the folder
//! module, and `backend` glue - all spliced into the generated `mod.rs`
//! alongside the auto-generated child `mod` declarations.
pub mod atmosphere;
pub mod deep;
pub mod weather;
#[repr(C, align(8))]
/// A handle to the world, defined directly in the folder module.
pub struct World {
    pub id: u64,
}
fn _World_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], World>([0u8; 0x8]);
    }
    unreachable!()
}
impl World {}
impl std::convert::AsRef<World> for World {
    fn as_ref(&self) -> &World {
        self
    }
}
impl std::convert::AsMut<World> for World {
    fn as_mut(&mut self) -> &mut World {
        self
    }
}
impl World {
    pub fn from_id(id: u64) -> World {
        World { id }
    }
}
