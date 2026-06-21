#![allow(
    dead_code,
    non_snake_case,
    non_upper_case_globals,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast,
    clippy::module_inception
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(4))]
pub struct DeepMarker {
    pub value: u32,
}
fn _DeepMarker_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], DeepMarker>([0u8; 0x4]);
    }
    unreachable!()
}
impl DeepMarker {}
impl std::convert::AsRef<DeepMarker> for DeepMarker {
    fn as_ref(&self) -> &DeepMarker {
        self
    }
}
impl std::convert::AsMut<DeepMarker> for DeepMarker {
    fn as_mut(&mut self) -> &mut DeepMarker {
        self
    }
}
