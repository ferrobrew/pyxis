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
/// Uses an alias from another module as a re-export.
/// This works because visibility is checked at alias definition time.
pub struct Wrapper {
    pub data: *const crate::math::Vector3,
}
fn _Wrapper_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Wrapper>([0u8; 0x8]);
    }
    unreachable!()
}
impl Wrapper {}
impl std::convert::AsRef<Wrapper> for Wrapper {
    fn as_ref(&self) -> &Wrapper {
        self
    }
}
impl std::convert::AsMut<Wrapper> for Wrapper {
    fn as_mut(&mut self) -> &mut Wrapper {
        self
    }
}
