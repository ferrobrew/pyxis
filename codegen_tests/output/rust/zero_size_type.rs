#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
/// A type that embeds Marker. Both backends must agree on layout:
/// marker occupies 8 bytes (min_size, alignment 1).
pub struct Holder {
    pub marker: crate::zero_size_type::Marker,
    pub value: u32,
    _field_c: [u8; 4],
}
fn _Holder_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], Holder>([0u8; 0x10]);
    }
    unreachable!()
}
impl Holder {}
impl std::convert::AsRef<Holder> for Holder {
    fn as_ref(&self) -> &Holder {
        self
    }
}
impl std::convert::AsMut<Holder> for Holder {
    fn as_mut(&mut self) -> &mut Holder {
        self
    }
}
#[repr(C, align(1))]
/// A fieldless type given a nonzero footprint via `#[min_size]`.
/// Without this attribute, embedding it would be rejected.
pub struct Marker {
    _field_0: [u8; 8],
}
fn _Marker_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Marker>([0u8; 0x8]);
    }
    unreachable!()
}
impl Marker {}
impl std::convert::AsRef<Marker> for Marker {
    fn as_ref(&self) -> &Marker {
        self
    }
}
impl std::convert::AsMut<Marker> for Marker {
    fn as_mut(&mut self) -> &mut Marker {
        self
    }
}
