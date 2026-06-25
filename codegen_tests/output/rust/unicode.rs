#![cfg_attr(any(), rustfmt::skip)]
//! Module-level doc — café & naïve.
#[repr(C, align(8))]
/// A type — with an emdash — and café.
pub struct Unicode {
    vftable: *const crate::unicode::UnicodeVftable,
    /// field — naïve
    pub field_1: u64,
}
fn _Unicode_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], Unicode>([0u8; 0x10]);
    }
    unreachable!()
}
impl Unicode {
    pub fn vftable(&self) -> *const crate::unicode::UnicodeVftable {
        self.vftable as *const crate::unicode::UnicodeVftable
    }
    /// impl func — ☃ snowman
    ///
    /// second line — 愛
    pub unsafe fn test_func(&self) {
        unsafe {
            let f: unsafe extern "system" fn(this: *const Self) = ::std::mem::transmute(
                0x123 as usize,
            );
            f(self as *const Self as _)
        }
    }
    /// vfunc — 一期一会
    pub unsafe fn test_vfunc(&self) {
        unsafe {
            let f = (&raw const (*self.vftable()).test_vfunc).read();
            f(self as *const Self as _)
        }
    }
}
impl std::convert::AsRef<Unicode> for Unicode {
    fn as_ref(&self) -> &Unicode {
        self
    }
}
impl std::convert::AsMut<Unicode> for Unicode {
    fn as_mut(&mut self) -> &mut Unicode {
        self
    }
}
#[repr(C, align(8))]
pub struct UnicodeVftable {
    /// vfunc — 一期一会
    pub test_vfunc: unsafe extern "system" fn(this: *const crate::unicode::Unicode),
}
fn _UnicodeVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], UnicodeVftable>([0u8; 0x8]);
    }
    unreachable!()
}
impl UnicodeVftable {}
impl std::convert::AsRef<UnicodeVftable> for UnicodeVftable {
    fn as_ref(&self) -> &UnicodeVftable {
        self
    }
}
impl std::convert::AsMut<UnicodeVftable> for UnicodeVftable {
    fn as_mut(&mut self) -> &mut UnicodeVftable {
        self
    }
}
