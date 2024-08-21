type Base {
    vftable {
        pub fn destructor(&mut self);
    }
}

type BaseA {
    vftable {
        pub fn destructor(&mut self);
    },
    #[base]
    pub base: Base,
}

type BaseB {
    vftable {
        pub fn destructor(&mut self);
    },
    #[base]
    pub base: Base,
}

type Derived {
    vftable {
        pub fn destructor(&mut self);
    },
    #[base]
    pub base_a: BaseA,
    #[base]
    pub base_b: BaseB,
}