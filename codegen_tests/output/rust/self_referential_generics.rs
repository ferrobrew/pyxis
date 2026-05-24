#![allow(
    dead_code,
    non_snake_case,
    clippy::missing_safety_doc,
    clippy::unnecessary_cast
)]
#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(8))]
/// A game object that contains self-referential generic types.
/// This tests that the compiler can handle cycles where:
/// - GameObject contains SharedPtr<GameObject>
/// - SharedPtr<T> contains *mut T
/// - The pointer breaks the cycle, but only after generic substitution
pub struct GameObject {
    pub id: u32,
    _field_4: [u8; 4],
    pub parent_object: crate::self_referential_generics::SharedPtr<
        crate::self_referential_generics::GameObject,
    >,
    pub weak_this: crate::self_referential_generics::WeakPtr<
        crate::self_referential_generics::GameObject,
    >,
    pub first_child: crate::self_referential_generics::SharedPtr<
        crate::self_referential_generics::GameObject,
    >,
    _field_20: [u8; 8],
}
fn _GameObject_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x28], GameObject>([0u8; 0x28]);
    }
    unreachable!()
}
impl GameObject {}
impl std::convert::AsRef<GameObject> for GameObject {
    fn as_ref(&self) -> &GameObject {
        self
    }
}
impl std::convert::AsMut<GameObject> for GameObject {
    fn as_mut(&mut self) -> &mut GameObject {
        self
    }
}
#[repr(C, align(8))]
/// A shared pointer to a type.
pub struct SharedPtr<T> {
    pub px: *mut T,
}
impl<T> SharedPtr<T> {}
#[repr(C, align(8))]
/// A tree node with self-referential pointers.
pub struct TreeNode {
    pub value: u32,
    _field_4: [u8; 4],
    pub left: crate::self_referential_generics::SharedPtr<
        crate::self_referential_generics::TreeNode,
    >,
    pub right: crate::self_referential_generics::SharedPtr<
        crate::self_referential_generics::TreeNode,
    >,
}
fn _TreeNode_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x18], TreeNode>([0u8; 0x18]);
    }
    unreachable!()
}
impl TreeNode {}
impl std::convert::AsRef<TreeNode> for TreeNode {
    fn as_ref(&self) -> &TreeNode {
        self
    }
}
impl std::convert::AsMut<TreeNode> for TreeNode {
    fn as_mut(&mut self) -> &mut TreeNode {
        self
    }
}
#[repr(C, align(8))]
/// A weak pointer to a type.
pub struct WeakPtr<T> {
    pub px: *const T,
}
impl<T> WeakPtr<T> {}
