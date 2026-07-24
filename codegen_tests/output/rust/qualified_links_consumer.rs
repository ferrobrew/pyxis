#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(1))]
/// Test module-qualified doc-links to functions and extern values.
///
/// See [`shared_function`](crate::qualified_links_provider::shared_function) and
/// [`shared_extern`](crate::qualified_links_provider::get_shared_extern).
pub struct Consumer {
    pub _marker: u8,
}
fn _Consumer_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x1], Consumer>([0u8; 0x1]);
    }
    unreachable!()
}
impl Consumer {}
impl std::convert::AsRef<Consumer> for Consumer {
    fn as_ref(&self) -> &Consumer {
        self
    }
}
impl std::convert::AsMut<Consumer> for Consumer {
    fn as_mut(&mut self) -> &mut Consumer {
        self
    }
}
