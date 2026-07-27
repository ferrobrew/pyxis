#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
/// Callback slots. Nothing here is a vftable: the struct simply holds raw
/// function pointers the target installs itself.
pub struct Callbacks {
    /// Named parameters, default calling convention.
    pub on_tick: unsafe extern "system" fn(
        engine: *mut crate::function_pointers::Engine,
        dt: f32,
    ),
    /// Parameters may be left unnamed, and the signature can return a value.
    pub on_event: unsafe extern "system" fn(
        *mut crate::function_pointers::Engine,
        u32,
    ) -> bool,
    /// A no-argument slot.
    pub on_shutdown: unsafe extern "system" fn(),
    /// The convention is selected by an attribute on the type itself, so it
    /// works in nested positions too.
    pub on_alloc: unsafe extern "C" fn(
        size: u32,
    ) -> *mut crate::function_pointers::Engine,
    /// A dispatch table: an array of function pointers, each with its own
    /// convention.
    pub table: [unsafe extern "C" fn(*mut crate::function_pointers::Engine); 4],
    /// Via the alias.
    pub aliased: unsafe extern "system" fn(
        engine: *mut crate::function_pointers::Engine,
        dt: f32,
    ),
    /// Pointer-to-function-pointer, for a slot the target rewrites in place.
    pub indirect: *mut unsafe extern "system" fn(*mut crate::function_pointers::Engine),
    /// A signature that itself takes a callback.
    pub on_register: unsafe extern "system" fn(
        callback: unsafe extern "system" fn(*mut crate::function_pointers::Engine),
        user_data: *mut crate::function_pointers::Engine,
    ),
    /// A pointer to a const function-pointer slot. `const` qualifies the
    /// pointee, so it must land on the inner `*` in C++ (`void (*const *)()`),
    /// not on the return type.
    pub locked: *const unsafe extern "system" fn(*mut crate::function_pointers::Engine),
    /// `_` is a parameter name, as it is in field position.
    pub on_ignore: unsafe extern "system" fn(_: u32),
}
fn _Callbacks_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x68], Callbacks>([0u8; 0x68]);
    }
    unreachable!()
}
impl Callbacks {
    /// Function pointers work in argument and return position too.
    pub unsafe fn install(
        &mut self,
        tick: unsafe extern "system" fn(*mut crate::function_pointers::Engine, f32),
    ) -> unsafe extern "system" fn(*mut crate::function_pointers::Engine, f32) {
        unsafe {
            let f: unsafe extern "system" fn(
                this: *mut Self,
                tick: unsafe extern "system" fn(
                    *mut crate::function_pointers::Engine,
                    f32,
                ),
            ) -> unsafe extern "system" fn(*mut crate::function_pointers::Engine, f32) = ::std::mem::transmute(
                0x2000 as usize,
            );
            f(self as *mut Self as _, tick)
        }
    }
    /// An array parameter has to render identically in the declaration and in
    /// the function-pointer alias the body calls through, or the two disagree
    /// and the emitted C++ doesn't compile.
    pub unsafe fn seed(&mut self, values: [u32; 4]) {
        unsafe {
            let f: unsafe extern "system" fn(this: *mut Self, values: [u32; 4]) = ::std::mem::transmute(
                0x2001 as usize,
            );
            f(self as *mut Self as _, values)
        }
    }
}
impl std::convert::AsRef<Callbacks> for Callbacks {
    fn as_ref(&self) -> &Callbacks {
        self
    }
}
impl std::convert::AsMut<Callbacks> for Callbacks {
    fn as_mut(&mut self) -> &mut Callbacks {
        self
    }
}
#[repr(C, align(4))]
/// The engine a callback is handed a pointer to.
pub struct Engine {
    pub frame: u32,
}
fn _Engine_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Engine>([0u8; 0x4]);
    }
    unreachable!()
}
impl Engine {}
impl std::convert::AsRef<Engine> for Engine {
    fn as_ref(&self) -> &Engine {
        self
    }
}
impl std::convert::AsMut<Engine> for Engine {
    fn as_mut(&mut self) -> &mut Engine {
        self
    }
}
/// A named signature, reused below. A function-pointer type is an ordinary
/// type, so it aliases like any other.
pub type TickFn = unsafe extern "system" fn(
    engine: *mut crate::function_pointers::Engine,
    dt: f32,
);
