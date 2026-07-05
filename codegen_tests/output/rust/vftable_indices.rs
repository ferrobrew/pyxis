#![cfg_attr(any(), rustfmt::skip)]
//! Vftable with explicit `#[index]` attributes and padding gaps.
//!
//! `first` is placed at index 0, `second` jumps to index 2 (leaving a padding
//! slot at index 1), and `third` continues at index 3. The `#[size(4)]` matches
//! the total slot count exactly. This verifies that ascending indices with
//! gaps produce the correct vftable layout.
#[repr(C, align(8))]
struct IndexedVftable {
    vftable: *const crate::vftable_indices::IndexedVftableVftable,
    pub field: u64,
}
fn _IndexedVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], IndexedVftable>([0u8; 0x10]);
    }
    unreachable!()
}
impl IndexedVftable {
    pub fn vftable(&self) -> *const crate::vftable_indices::IndexedVftableVftable {
        self.vftable as *const crate::vftable_indices::IndexedVftableVftable
    }
    pub unsafe fn first(&self) -> u32 {
        unsafe {
            let f = (&raw const (*self.vftable()).first).read();
            f(self as *const Self as _)
        }
    }
    pub unsafe fn second(&self, x: i32) -> i32 {
        unsafe {
            let f = (&raw const (*self.vftable()).second).read();
            f(self as *const Self as _, x)
        }
    }
    pub unsafe fn third(&self) -> u32 {
        unsafe {
            let f = (&raw const (*self.vftable()).third).read();
            f(self as *const Self as _)
        }
    }
}
impl std::convert::AsRef<IndexedVftable> for IndexedVftable {
    fn as_ref(&self) -> &IndexedVftable {
        self
    }
}
impl std::convert::AsMut<IndexedVftable> for IndexedVftable {
    fn as_mut(&mut self) -> &mut IndexedVftable {
        self
    }
}
#[repr(C, align(8))]
struct IndexedVftableVftable {
    pub first: unsafe extern "system" fn(
        this: *const crate::vftable_indices::IndexedVftable,
    ) -> u32,
    _vfunc_1: unsafe extern "system" fn(
        this: *mut crate::vftable_indices::IndexedVftable,
    ),
    pub second: unsafe extern "system" fn(
        this: *const crate::vftable_indices::IndexedVftable,
        x: i32,
    ) -> i32,
    pub third: unsafe extern "system" fn(
        this: *const crate::vftable_indices::IndexedVftable,
    ) -> u32,
}
fn _IndexedVftableVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x20], IndexedVftableVftable>([0u8; 0x20]);
    }
    unreachable!()
}
impl IndexedVftableVftable {}
impl std::convert::AsRef<IndexedVftableVftable> for IndexedVftableVftable {
    fn as_ref(&self) -> &IndexedVftableVftable {
        self
    }
}
impl std::convert::AsMut<IndexedVftableVftable> for IndexedVftableVftable {
    fn as_mut(&mut self) -> &mut IndexedVftableVftable {
        self
    }
}
