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
pub use ::core::sync::atomic::AtomicU32 as AtomicHandle;
#[repr(C, align(4))]
pub struct UsesAtomicHandle {
    pub handle: crate::extern_bindings::AtomicHandle,
}
fn _UsesAtomicHandle_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], UsesAtomicHandle>([0u8; 0x4]);
    }
    unreachable!()
}
impl UsesAtomicHandle {}
impl std::convert::AsRef<UsesAtomicHandle> for UsesAtomicHandle {
    fn as_ref(&self) -> &UsesAtomicHandle {
        self
    }
}
impl std::convert::AsMut<UsesAtomicHandle> for UsesAtomicHandle {
    fn as_mut(&mut self) -> &mut UsesAtomicHandle {
        self
    }
}
