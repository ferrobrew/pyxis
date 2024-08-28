#[align(8)]
type Base {
    vftable {
        pub fn base_vfunc(&self, a: i32) -> i32;
    },
    #[address(8)]
    pub base_field: u64,
}
impl Base {
    #[address(0x123)]
    pub fn base_associated(&self, a: i32) -> i32;
}

#[align(8)]
type Derived {
    vftable {
        pub fn base_vfunc(&self, a: i32) -> i32;
        pub fn derived_vfunc(&self, a: i32) -> i32;
    },
    #[base]
    pub base: Base,
    pub derived_field: u64,
}
impl Derived {
    #[address(0x456)]
    pub fn derived_associated(&self, a: i32) -> i32;
}

#[align(8)]
type DerivedDerived {
    vftable {
        pub fn base_vfunc(&self, a: i32) -> i32;
        pub fn derived_vfunc(&self, a: i32) -> i32;
        pub fn derived_derived_vfunc(&self, a: i32) -> i32;
    },
    #[base]
    pub derived: Derived,
    pub derived_derived_field: u64,
}
impl DerivedDerived {
    #[address(0x789)]
    pub fn derived_derived_associated(&self, a: i32) -> i32;
}

#[align(8)]
type DerivedDerivedDerived {
    vftable {
        pub fn base_vfunc(&self, a: i32) -> i32;
        pub fn derived_vfunc(&self, a: i32) -> i32;
        pub fn derived_derived_vfunc(&self, a: i32) -> i32;
        pub fn derived_derived_derived_vfunc(&self, a: i32) -> i32;
    },
    #[base]
    pub derived_derived: DerivedDerived,
    pub derived_derived_derived_field: u64,
}
impl DerivedDerivedDerived {
    #[address(0xabc)]
    pub fn derived_derived_derived_associated(&self, a: i32) -> i32;
}