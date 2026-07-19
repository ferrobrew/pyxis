#![cfg_attr(any(), rustfmt::skip)]
#[allow(unused_imports)]
use Player_Inventory as Inventory;
crate::__bitflags! {
    #[doc = " The permissive baseline is [`DEFAULT_MASK`](AccessFlags::DEFAULT_MASK)."]
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
/// Defaults to [`DEFAULT`](Color::DEFAULT).
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
pub const DEFAULT_CAMERA: crate::math::Camera = crate::math::Camera {
    position: crate::math::Vector3 {
        x: 0.0,
        y: 0.0,
        z: 0.0,
    },
    transforms: [
        crate::math::Matrix4 {
            data: [
                1.0,
                0.0,
                0.0,
                0.0,
                0.0,
                1.0,
                0.0,
                0.0,
                0.0,
                0.0,
                1.0,
                0.0,
                0.0,
                0.0,
                0.0,
                1.0,
            ],
        },
        IDENTITY,
        crate::math::Matrix4 {
            data: [
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
            ],
        },
    ],
};
pub const DEFAULT_COLOR: crate::consts::Color = Color::Red;
pub const DEFAULT_HEALTH: i32 = MAX_HEALTH;
pub const DLL_NAME: &::std::ffi::CStr = c"MSVCP80";
pub const EMPTY_VEC: crate::math::Vector3 = crate::math::Vector3 {
    x: 0.0,
    y: 0.0,
    z: 0.0,
};
pub const GAME_NAME: &str = "Pyxis";
pub const GRAVITY: f64 = 9.81;
pub const IDENTITY: crate::math::Matrix4 = crate::math::Matrix4 {
    data: [
        1.0,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
    ],
};
pub const MAX_HEALTH: i32 = 100;
#[repr(C, align(4))]
pub struct Player {
    /// New players begin with [`STARTING_GOLD`](Player::STARTING_GOLD) gold,
    /// spawn at x=[`SPAWN_X`](Player::SPAWN_X), and can hold up to
    /// [`MAX_SLOTS`](Player_Inventory::MAX_SLOTS) items.
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
    pub const IDENTITY_MATRIX: crate::math::Matrix4 = crate::math::Matrix4 {
        data: [
            1.0,
            0.0,
            0.0,
            0.0,
            0.0,
            1.0,
            0.0,
            0.0,
            0.0,
            0.0,
            1.0,
            0.0,
            0.0,
            0.0,
            0.0,
            1.0,
        ],
    };
    pub const PLAYER_DLL: &::std::ffi::CStr = c"player.dll";
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
pub const SPAWN_POINT: crate::math::Vector3 = EMPTY_VEC;
pub const SYMBOL_NAME: &::std::ffi::CStr = c"??0?$basic_string@D";
pub const ZERO_MATRIX: crate::math::Matrix4 = crate::math::Matrix4 {
    data: [
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
    ],
};
