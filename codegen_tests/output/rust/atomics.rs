#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
/// Test type using atomic primitives
pub struct AtomicCounters {
    pub bool_flag: ::std::sync::atomic::AtomicBool,
    pub counter_u8: ::std::sync::atomic::AtomicU8,
    pub counter_u16: ::std::sync::atomic::AtomicU16,
    pub counter_u32: ::std::sync::atomic::AtomicU32,
    pub counter_u64: ::std::sync::atomic::AtomicU64,
    pub counter_i8: ::std::sync::atomic::AtomicI8,
    _field_11: [u8; 1],
    pub counter_i16: ::std::sync::atomic::AtomicI16,
    pub counter_i32: ::std::sync::atomic::AtomicI32,
    pub counter_i64: ::std::sync::atomic::AtomicI64,
}
fn _AtomicCounters_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x20], AtomicCounters>([0u8; 0x20]);
    }
    unreachable!()
}
impl AtomicCounters {}
impl std::convert::AsRef<AtomicCounters> for AtomicCounters {
    fn as_ref(&self) -> &AtomicCounters {
        self
    }
}
impl std::convert::AsMut<AtomicCounters> for AtomicCounters {
    fn as_mut(&mut self) -> &mut AtomicCounters {
        self
    }
}
