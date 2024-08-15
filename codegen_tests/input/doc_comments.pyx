//! This is a module doc comment
//!
//! The best of its kind

/// This is a doc comment
#[align(8)]
pub type TestType {
    vftable {
        /// My test vfunc!
        pub fn test_vfunc(&self);
    },
    /// This is a field doc comment
    #[address(8)]
    pub field_1: u64,
}
impl TestType {
    /// My test func!
    ///
    /// And its second line! :)
    #[address(0x123)]
    pub fn test_func(&self);
}