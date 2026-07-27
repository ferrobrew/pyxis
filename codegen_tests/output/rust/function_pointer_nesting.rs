#![cfg_attr(any(), rustfmt::skip)]
//! Deeply nested function-pointer combinations, to check that all three
//! backends compose the type correctly rather than only handling one level.
//!
//! C++ declaration syntax nests inside-out, so every extra level here is a
//! chance for the declarator to bind `*`, `[]` and `()` in the wrong order.
//! The corpus compiles its own C++ and Rust output, so anything that composes
//! wrongly fails the build rather than sitting in a snapshot.
//!
//! `C` is the only non-default convention whose Rust (`extern "C"`) compiles
//! on every host the corpus builds against, so the convention cases below
//! distinguish positions by pairing it against the default, `system`.
#[repr(C, align(8))]
/// Passed around by pointer as the callback context.
pub struct Ctx {
    pub id: u64,
}
fn _Ctx_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Ctx>([0u8; 0x8]);
    }
    unreachable!()
}
impl Ctx {}
impl std::convert::AsRef<Ctx> for Ctx {
    fn as_ref(&self) -> &Ctx {
        self
    }
}
impl std::convert::AsMut<Ctx> for Ctx {
    fn as_mut(&mut self) -> &mut Ctx {
        self
    }
}
#[repr(C, align(8))]
/// Function pointers in vftable slots, in both argument and return position.
pub struct Dispatch {
    vftable: *const crate::function_pointer_nesting::DispatchVftable,
}
fn _Dispatch_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], Dispatch>([0u8; 0x8]);
    }
    unreachable!()
}
impl Dispatch {
    pub fn vftable(&self) -> *const crate::function_pointer_nesting::DispatchVftable {
        self.vftable as *const crate::function_pointer_nesting::DispatchVftable
    }
    pub unsafe fn install(
        &mut self,
        cb: unsafe extern "system" fn(*mut crate::function_pointer_nesting::Ctx) -> bool,
    ) -> unsafe extern "system" fn(*mut crate::function_pointer_nesting::Ctx) {
        unsafe {
            let f = (&raw const (*self.vftable()).install).read();
            f(self as *mut Self as _, cb)
        }
    }
    pub unsafe fn table(&self) -> *mut [unsafe extern "system" fn(); 4] {
        unsafe {
            let f = (&raw const (*self.vftable()).table).read();
            f(self as *const Self as _)
        }
    }
}
impl std::convert::AsRef<Dispatch> for Dispatch {
    fn as_ref(&self) -> &Dispatch {
        self
    }
}
impl std::convert::AsMut<Dispatch> for Dispatch {
    fn as_mut(&mut self) -> &mut Dispatch {
        self
    }
}
#[repr(C, align(8))]
pub struct DispatchVftable {
    pub install: unsafe extern "system" fn(
        this: *mut crate::function_pointer_nesting::Dispatch,
        cb: unsafe extern "system" fn(*mut crate::function_pointer_nesting::Ctx) -> bool,
    ) -> unsafe extern "system" fn(*mut crate::function_pointer_nesting::Ctx),
    pub table: unsafe extern "system" fn(
        this: *const crate::function_pointer_nesting::Dispatch,
    ) -> *mut [unsafe extern "system" fn(); 4],
}
fn _DispatchVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x10], DispatchVftable>([0u8; 0x10]);
    }
    unreachable!()
}
impl DispatchVftable {}
impl std::convert::AsRef<DispatchVftable> for DispatchVftable {
    fn as_ref(&self) -> &DispatchVftable {
        self
    }
}
impl std::convert::AsMut<DispatchVftable> for DispatchVftable {
    fn as_mut(&mut self) -> &mut DispatchVftable {
        self
    }
}
/// A signature that itself takes and returns signatures, reached through an
/// alias to check aliases carry the whole nested type.
pub type Handler = unsafe extern "system" fn(
    inner: unsafe extern "system" fn(*mut crate::function_pointer_nesting::Ctx) -> bool,
) -> unsafe extern "system" fn(*mut crate::function_pointer_nesting::Ctx);
#[derive(Copy, Clone)]
#[repr(C, align(8))]
/// A union whose members are competing readings of one pointer-sized slot.
pub union HookSlot {
    pub as_fn: ::core::mem::ManuallyDrop<
        unsafe extern "system" fn(*mut crate::function_pointer_nesting::Ctx) -> bool,
    >,
    pub as_address: ::core::mem::ManuallyDrop<u64>,
}
fn _HookSlot_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], HookSlot>([0u8; 0x8]);
    }
    unreachable!()
}
impl ::core::fmt::Debug for HookSlot {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.write_str(concat!("HookSlot", " { .. }"))
    }
}
#[repr(C, align(8))]
pub struct Nested {
    /// Three levels of parameter nesting.
    pub three: unsafe extern "system" fn(
        a: unsafe extern "system" fn(
            b: unsafe extern "system" fn(
                *mut crate::function_pointer_nesting::Ctx,
            ) -> u32,
        ) -> u32,
    ) -> u32,
    /// Three levels of return nesting, the hardest C++ declarator shape here.
    /// Doxygen mis-parses it (see DOXYGEN_IGNORED_WARNINGS in test.py); clang
    /// and gcc accept it, and this corpus compiles its own output.
    pub curried: unsafe extern "system" fn(
        *mut crate::function_pointer_nesting::Ctx,
    ) -> unsafe extern "system" fn(u32) -> unsafe extern "system" fn() -> bool,
    /// An array of arrays of function pointers.
    pub grid: [[unsafe extern "system" fn(
        *mut crate::function_pointer_nesting::Ctx,
    ); 2]; 3],
    /// A pointer to an array of function pointers. In C++ the subscript binds
    /// tighter than the star, so this needs parens where an array of pointers
    /// does not.
    pub table_ptr: *mut [unsafe extern "system" fn(
        *mut crate::function_pointer_nesting::Ctx,
    ); 4],
    /// A const pointer to a function pointer: the qualifier belongs to the
    /// pointee, not to the return type.
    pub locked: *const unsafe extern "system" fn(
        *mut crate::function_pointer_nesting::Ctx,
    ),
    /// An array by value alongside a pointer to a function pointer.
    pub bulk: unsafe extern "system" fn(
        values: [u32; 4],
        sink: *mut unsafe extern "system" fn(u32),
    ),
    /// A convention on the outer type only. The parameter and the return
    /// type must keep the default, not inherit it - so each of the three
    /// positions is exercised alone, with the other two left default.
    pub cc_outer: unsafe extern "C" fn(
        cb: unsafe extern "system" fn(u32) -> bool,
    ) -> unsafe extern "system" fn(),
    /// The same, on the return type only.
    pub cc_return: unsafe extern "system" fn(
        cb: unsafe extern "system" fn(u32) -> bool,
    ) -> unsafe extern "C" fn(),
    /// The same, on the parameter only.
    pub cc_param: unsafe extern "system" fn(
        cb: unsafe extern "C" fn(u32) -> bool,
    ) -> unsafe extern "system" fn(),
    /// All three annotated at once, so the attributes have to coexist in a
    /// single type expression rather than one per field. The parameter takes
    /// the other convention, so an outer one reaching it would still show.
    pub cc_all: unsafe extern "C" fn(
        cb: unsafe extern "system" fn(u32) -> bool,
    ) -> unsafe extern "C" fn(),
    /// Structs passed and returned by value through a function pointer. The
    /// parameter is the enclosing module's type, not a pointer to it.
    pub by_value: unsafe extern "system" fn(
        ctx: crate::function_pointer_nesting::Ctx,
    ) -> crate::function_pointer_nesting::Ctx,
    /// The alias above, which is itself a nested signature.
    pub aliased: unsafe extern "system" fn(
        inner: unsafe extern "system" fn(
            *mut crate::function_pointer_nesting::Ctx,
        ) -> bool,
    ) -> unsafe extern "system" fn(*mut crate::function_pointer_nesting::Ctx),
    /// A function pointer as a generic argument.
    pub slot: crate::function_pointer_nesting::Slot<
        unsafe extern "system" fn(*mut crate::function_pointer_nesting::Ctx) -> bool,
    >,
}
fn _Nested_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x90], Nested>([0u8; 0x90]);
    }
    unreachable!()
}
impl Nested {}
impl std::convert::AsRef<Nested> for Nested {
    fn as_ref(&self) -> &Nested {
        self
    }
}
impl std::convert::AsMut<Nested> for Nested {
    fn as_mut(&mut self) -> &mut Nested {
        self
    }
}
#[repr(C, align(8))]
/// Parameters named after the locals the generated call shims used to bind.
/// The Rust backend called the target through a local `f`, which a parameter
/// of the same name shadowed - the shim passed itself instead of the argument.
/// Both the `#[address]` and vftable shims are covered.
pub struct ShimNames {
    vftable: *const crate::function_pointer_nesting::ShimNamesVftable,
}
fn _ShimNames_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], ShimNames>([0u8; 0x8]);
    }
    unreachable!()
}
impl ShimNames {
    pub fn vftable(&self) -> *const crate::function_pointer_nesting::ShimNamesVftable {
        self.vftable as *const crate::function_pointer_nesting::ShimNamesVftable
    }
    pub unsafe fn via_address(
        &mut self,
        f: unsafe extern "system" fn(*mut crate::function_pointer_nesting::Ctx),
        this: u32,
    ) -> unsafe extern "system" fn(*mut crate::function_pointer_nesting::Ctx) {
        unsafe {
            (::std::mem::transmute::<
                usize,
                unsafe extern "system" fn(
                    this: *mut Self,
                    f: unsafe extern "system" fn(
                        *mut crate::function_pointer_nesting::Ctx,
                    ),
                    this: u32,
                ) -> unsafe extern "system" fn(*mut crate::function_pointer_nesting::Ctx),
            >(0x6000 as usize))(self as *mut Self as _, f, this)
        }
    }
    pub unsafe fn via_vftable(
        &mut self,
        f: *mut crate::function_pointer_nesting::Ctx,
        this: u32,
    ) -> *mut crate::function_pointer_nesting::Ctx {
        unsafe {
            ((&raw const (*self.vftable()).via_vftable)
                .read())(self as *mut Self as _, f, this)
        }
    }
}
impl std::convert::AsRef<ShimNames> for ShimNames {
    fn as_ref(&self) -> &ShimNames {
        self
    }
}
impl std::convert::AsMut<ShimNames> for ShimNames {
    fn as_mut(&mut self) -> &mut ShimNames {
        self
    }
}
#[repr(C, align(8))]
pub struct ShimNamesVftable {
    pub via_vftable: unsafe extern "system" fn(
        this: *mut crate::function_pointer_nesting::ShimNames,
        f: *mut crate::function_pointer_nesting::Ctx,
        this: u32,
    ) -> *mut crate::function_pointer_nesting::Ctx,
}
fn _ShimNamesVftable_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x8], ShimNamesVftable>([0u8; 0x8]);
    }
    unreachable!()
}
impl ShimNamesVftable {}
impl std::convert::AsRef<ShimNamesVftable> for ShimNamesVftable {
    fn as_ref(&self) -> &ShimNamesVftable {
        self
    }
}
impl std::convert::AsMut<ShimNamesVftable> for ShimNamesVftable {
    fn as_mut(&mut self) -> &mut ShimNamesVftable {
        self
    }
}
#[repr(C, align(8))]
/// A generic instantiated with a function pointer, exercising the type in
/// template-argument position. Sized explicitly, as any generic whose layout
/// can't be computed from its parameters must be.
pub struct Slot<T> {
    pub value: *mut T,
}
impl<T> Slot<T> {}
/// A global function pointer at a fixed address.
pub unsafe fn get_g_hook() -> &'static mut unsafe extern "system" fn(
    *mut crate::function_pointer_nesting::Ctx,
) -> bool {
    unsafe {
        &mut *(0x5000
            as *mut unsafe extern "system" fn(
                *mut crate::function_pointer_nesting::Ctx,
            ) -> bool)
    }
}
