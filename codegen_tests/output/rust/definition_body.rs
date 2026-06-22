#![cfg_attr(any(), rustfmt::skip)]
#[repr(C, align(4))]
pub struct Logger {
    pub seen: u32,
}
fn _Logger_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Logger>([0u8; 0x4]);
    }
    unreachable!()
}
impl Logger {}
impl std::convert::AsRef<Logger> for Logger {
    fn as_ref(&self) -> &Logger {
        self
    }
}
impl std::convert::AsMut<Logger> for Logger {
    fn as_mut(&mut self) -> &mut Logger {
        self
    }
}
impl Logger {
    pub fn make() -> Logger {
        Logger { seen: 0 }
    }
    pub fn record(&mut self) -> u32 {
        self.seen += 1;
        self.seen
    }
}
