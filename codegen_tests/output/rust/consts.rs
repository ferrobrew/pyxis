#![cfg_attr(any(), rustfmt::skip)]
crate::__bitflags! {
    pub struct AccessFlags : u32 { const READ = 1usize as _; const WRITE = 2usize as _; }
}
fn _AccessFlags_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], AccessFlags>([0u8; 0x4]);
    }
    unreachable!()
}
impl AccessFlags {
    pub const DEFAULT_MASK: u32 = 3;
}
#[repr(u8)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Color {
    Red = 0isize as _,
    Green = 1isize as _,
    Blue = 2isize as _,
}
fn _Color_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x1], Color>([0u8; 0x1]);
    }
    unreachable!()
}
impl Color {
    pub const DEFAULT: crate::consts::Color = Color::Red;
}
pub const DEFAULT_COLOR: crate::consts::Color = Color::Red;
pub const GAME_NAME: &str = "Pyxis";
pub const GRAVITY: f64 = 9.81;
pub const MAX_HEALTH: i32 = 100;
#[repr(C, align(4))]
pub struct Player {
    pub health: i32,
}
fn _Player_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Player>([0u8; 0x4]);
    }
    unreachable!()
}
impl Player {}
impl Player {
    pub const SPAWN_X: f32 = 0.0;
    pub const STARTING_GOLD: u32 = 500;
}
impl std::convert::AsRef<Player> for Player {
    fn as_ref(&self) -> &Player {
        self
    }
}
impl std::convert::AsMut<Player> for Player {
    fn as_mut(&mut self) -> &mut Player {
        self
    }
}
#[repr(C, align(4))]
pub struct Player_Inventory {
    pub slots: u32,
}
fn _Player_Inventory_size_check() {
    unsafe {
        ::std::mem::transmute::<[u8; 0x4], Player_Inventory>([0u8; 0x4]);
    }
    unreachable!()
}
impl Player_Inventory {}
impl Player_Inventory {
    pub const MAX_SLOTS: u32 = 30;
}
impl std::convert::AsRef<Player_Inventory> for Player_Inventory {
    fn as_ref(&self) -> &Player_Inventory {
        self
    }
}
impl std::convert::AsMut<Player_Inventory> for Player_Inventory {
    fn as_mut(&mut self) -> &mut Player_Inventory {
        self
    }
}
pub const SCALE_FACTOR: f32 = 1.5;
