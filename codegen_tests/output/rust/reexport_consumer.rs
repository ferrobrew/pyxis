#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(4))]
pub struct UsesReexport {
    pub position: crate::math::Vector3,
}
fn _UsesReexport_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0xC], UsesReexport>([0u8; 0xC]);
    }
    unreachable!()
}
impl UsesReexport {}
impl std::convert::AsRef<UsesReexport> for UsesReexport {
    fn as_ref(&self) -> &UsesReexport {
        self
    }
}
impl std::convert::AsMut<UsesReexport> for UsesReexport {
    fn as_mut(&mut self) -> &mut UsesReexport {
        self
    }
}
