#![cfg_attr(any(), rustfmt::skip)]
#[allow(unused_imports)]
use Container_Nested as Nested;
#[repr(C, align(4))]
/// Test `Self::` links in type docs.
///
/// Link to a field as [`Self::field`](Self::field), to a method as
/// [`Self::method`](Self::method), to a nested type as
/// [`Self::Nested`](Container_Nested), and to a nested type's member as
/// [`Self::Nested::nested_field`](Container_Nested::nested_field).
pub struct Container {
    /// A field.
    pub field: u32,
}
fn _Container_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Container>([0u8; 0x4]);
    }
    unreachable!()
}
impl Container {
    /// A method.
    pub unsafe fn method(&self) {
        unsafe {
            let f: unsafe extern "system" fn(this: *const Self) = ::std::mem::transmute(
                0x10 as usize,
            );
            f(self as *const Self as _)
        }
    }
}
impl std::convert::AsRef<Container> for Container {
    fn as_ref(&self) -> &Container {
        self
    }
}
impl std::convert::AsMut<Container> for Container {
    fn as_mut(&mut self) -> &mut Container {
        self
    }
}
#[repr(C, align(2))]
/// A nested type. Its field is [`Self::nested_field`](Self::nested_field).
pub struct Container_Nested {
    pub nested_field: u16,
}
fn _Container_Nested_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x2], Container_Nested>([0u8; 0x2]);
    }
    unreachable!()
}
impl Container_Nested {}
impl std::convert::AsRef<Container_Nested> for Container_Nested {
    fn as_ref(&self) -> &Container_Nested {
        self
    }
}
impl std::convert::AsMut<Container_Nested> for Container_Nested {
    fn as_mut(&mut self) -> &mut Container_Nested {
        self
    }
}
