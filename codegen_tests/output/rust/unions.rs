#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
/// A pinned union can be neither copied nor moved.
pub union Anchored {
    pub value: ::core::mem::ManuallyDrop<u64>,
    pub halves: ::core::mem::ManuallyDrop<[u32; 2]>,
}
fn _Anchored_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Anchored>([0u8; 0x8]);
    }
    unreachable!()
}
impl ::core::fmt::Debug for Anchored {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.write_str(concat!("Anchored", " { .. }"))
    }
}
#[derive(Copy, Clone)]
#[repr(C, align(8))]
pub union Geometry {
    pub point: ::core::mem::ManuallyDrop<crate::unions::Vec2>,
    pub scalar: ::core::mem::ManuallyDrop<f64>,
    pub raw: ::core::mem::ManuallyDrop<[u8; 8]>,
}
fn _Geometry_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Geometry>([0u8; 0x8]);
    }
    unreachable!()
}
impl ::core::fmt::Debug for Geometry {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.write_str(concat!("Geometry", " { .. }"))
    }
}
#[repr(C, align(8))]
/// A union declared inline in field position. It lowers to a generated sibling
/// item named `InlineScratchDataUnion`.
pub struct InlineScratch {
    pub tag: u16,
    pub _reserved: [u8; 6],
    pub data: crate::unions::InlineScratchDataUnion,
}
fn _InlineScratch_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], InlineScratch>([0u8; 0x10]);
    }
    unreachable!()
}
impl InlineScratch {}
impl std::convert::AsRef<InlineScratch> for InlineScratch {
    fn as_ref(&self) -> &InlineScratch {
        self
    }
}
impl std::convert::AsMut<InlineScratch> for InlineScratch {
    fn as_mut(&mut self) -> &mut InlineScratch {
        self
    }
}
#[repr(C, align(8))]
pub union InlineScratchDataUnion {
    pub as_u64: ::core::mem::ManuallyDrop<u64>,
    pub as_bytes: ::core::mem::ManuallyDrop<[u8; 8]>,
}
fn _InlineScratchDataUnion_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], InlineScratchDataUnion>([0u8; 0x8]);
    }
    unreachable!()
}
impl ::core::fmt::Debug for InlineScratchDataUnion {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.write_str(concat!("InlineScratchDataUnion", " { .. }"))
    }
}
#[repr(C, align(4))]
/// A union nested inside another union, plus a nested item declaration in a
/// union body.
pub union Outer {
    pub raw: ::core::mem::ManuallyDrop<u32>,
    pub inner: ::core::mem::ManuallyDrop<crate::unions::OuterInnerUnion>,
}
fn _Outer_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Outer>([0u8; 0x4]);
    }
    unreachable!()
}
impl ::core::fmt::Debug for Outer {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.write_str(concat!("Outer", " { .. }"))
    }
}
#[repr(C, align(4))]
/// A type declared inside a union body. Nested declarations work here
/// exactly as they do in a `type` body.
pub struct Outer_Header {
    pub magic: u32,
}
fn _Outer_Header_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Outer_Header>([0u8; 0x4]);
    }
    unreachable!()
}
impl Outer_Header {}
impl std::convert::AsRef<Outer_Header> for Outer_Header {
    fn as_ref(&self) -> &Outer_Header {
        self
    }
}
impl std::convert::AsMut<Outer_Header> for Outer_Header {
    fn as_mut(&mut self) -> &mut Outer_Header {
        self
    }
}
#[repr(C, align(2))]
pub union OuterInnerUnion {
    pub lo: ::core::mem::ManuallyDrop<u16>,
    pub hi: ::core::mem::ManuallyDrop<[u8; 2]>,
}
fn _OuterInnerUnion_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x2], OuterInnerUnion>([0u8; 0x2]);
    }
    unreachable!()
}
impl ::core::fmt::Debug for OuterInnerUnion {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.write_str(concat!("OuterInnerUnion", " { .. }"))
    }
}
#[repr(C, align(8))]
/// `#[size]` alone asks for more room than any member needs, so the union gains
/// a whole-width `_padding` member. Rounding up to `#[align]` needs no such help
/// — see [`PaddedSlot`](crate::unions::PaddedSlot), which gets none.
pub union OversizedSlot {
    pub small: ::core::mem::ManuallyDrop<u32>,
    pub medium: ::core::mem::ManuallyDrop<u64>,
    _padding: ::core::mem::ManuallyDrop<[u8; 16]>,
}
fn _OversizedSlot_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], OversizedSlot>([0u8; 0x10]);
    }
    unreachable!()
}
impl ::core::fmt::Debug for OversizedSlot {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.write_str(concat!("OversizedSlot", " { .. }"))
    }
}
#[repr(C, packed)]
/// `#[packed]` drops the union's alignment to 1.
pub union PackedPair {
    pub word: ::core::mem::ManuallyDrop<u16>,
    pub bytes: ::core::mem::ManuallyDrop<[u8; 2]>,
}
fn _PackedPair_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x2], PackedPair>([0u8; 0x2]);
    }
    unreachable!()
}
impl ::core::fmt::Debug for PackedPair {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.write_str(concat!("PackedPair", " { .. }"))
    }
}
#[repr(C, align(16))]
/// `#[size]` pads a union out beyond its largest member, and `#[align]`
/// over-aligns it.
pub union PaddedSlot {
    pub small: ::core::mem::ManuallyDrop<u32>,
    pub medium: ::core::mem::ManuallyDrop<u64>,
}
fn _PaddedSlot_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], PaddedSlot>([0u8; 0x10]);
    }
    unreachable!()
}
impl ::core::fmt::Debug for PaddedSlot {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.write_str(concat!("PaddedSlot", " { .. }"))
    }
}
#[derive(Copy, Clone)]
#[repr(C, align(8))]
/// A value whose bytes have several competing readings. Which one applies is
/// decided by [`TaggedValue::kind`](crate::unions::TaggedValue::kind), not by the union itself.
pub union Payload {
    /// Read as a signed integer.
    pub as_int: ::core::mem::ManuallyDrop<i32>,
    /// Read as a float.
    pub as_float: ::core::mem::ManuallyDrop<f32>,
    /// Read as a pointer to something else.
    pub as_ptr: ::core::mem::ManuallyDrop<*mut i32>,
}
fn _Payload_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Payload>([0u8; 0x8]);
    }
    unreachable!()
}
impl ::core::fmt::Debug for Payload {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.write_str(concat!("Payload", " { .. }"))
    }
}
#[repr(C, align(8))]
/// A tagged value pairing a discriminant with a [`Payload`](crate::unions::Payload). When `kind` is 0,
/// the live member is [`Payload::as_int`](crate::unions::Payload::as_int).
pub struct TaggedValue {
    pub kind: u32,
    _field_4: [u8; 4],
    pub payload: crate::unions::Payload,
}
fn _TaggedValue_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], TaggedValue>([0u8; 0x10]);
    }
    unreachable!()
}
impl TaggedValue {}
impl std::convert::AsRef<TaggedValue> for TaggedValue {
    fn as_ref(&self) -> &TaggedValue {
        self
    }
}
impl std::convert::AsMut<TaggedValue> for TaggedValue {
    fn as_mut(&mut self) -> &mut TaggedValue {
        self
    }
}
#[derive(Copy, Clone)]
#[repr(C, align(8))]
/// Members can be structs, and the union takes the strictest alignment and the
/// largest size among them.
pub struct Vec2 {
    pub x: f32,
    pub y: f32,
}
fn _Vec2_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Vec2>([0u8; 0x8]);
    }
    unreachable!()
}
impl Vec2 {}
impl std::convert::AsRef<Vec2> for Vec2 {
    fn as_ref(&self) -> &Vec2 {
        self
    }
}
impl std::convert::AsMut<Vec2> for Vec2 {
    fn as_mut(&mut self) -> &mut Vec2 {
        self
    }
}
#[derive(Copy, Clone)]
#[repr(C, align(4))]
/// A defaultable union gets a hand-written `Default` in Rust — a union can't
/// derive one, because nothing knows which member is live.
pub union ZeroInit {
    pub count: ::core::mem::ManuallyDrop<u32>,
    pub flags: ::core::mem::ManuallyDrop<[u8; 4]>,
}
fn _ZeroInit_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], ZeroInit>([0u8; 0x4]);
    }
    unreachable!()
}
impl ::core::fmt::Debug for ZeroInit {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.write_str(concat!("ZeroInit", " { .. }"))
    }
}
impl ::core::default::Default for ZeroInit {
    fn default() -> Self {
        unsafe { ::core::mem::zeroed() }
    }
}
