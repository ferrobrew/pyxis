#[align(8)]
type BaseA {
    vftable {
        pub fn base_a_vfunc(&self, a: i32) -> i32;
    },
    #[address(8)]
    pub field_a: u64,
}
impl BaseA {
    #[address(0x123)]
    pub fn base_a_associated(&self, a: i32) -> i32;
}

#[align(8)]
type BaseB {
    vftable {
        pub fn base_b_vfunc(&self, a: i32) -> i32;
    },
    #[address(8)]
    pub field_b: u64,
}
impl BaseB {
    #[address(0x456)]
    pub fn base_b_associated(&self, a: i32) -> i32;
}

#[align(8)]
type Derived {
    vftable {
        pub fn base_a_vfunc(&self, a: i32) -> i32;
        pub fn derived_vfunc(&self, a: i32) -> i32;
    },
    #[base]
    pub base_a: BaseA,
    #[base]
    pub base_b: BaseB,
}
impl Derived {
    #[address(0x789)]
    pub fn derived_associated(&self, a: i32) -> i32;
}
