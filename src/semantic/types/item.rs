use crate::{
    grammar::{self, ItemPath},
    parser::cfg::CfgPredicate,
    span::{HasLocation, ItemLocation},
};

#[cfg(test)]
use crate::span::StripLocations;

use super::{
    BitflagsDefinition, ConstDefinition, EnumDefinition, ExternValueDefinition,
    TypeAliasDefinition, TypeDefinition, UnionDefinition, Visibility,
};

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub enum ItemDefinitionInner {
    Type(TypeDefinition),
    Enum(EnumDefinition),
    Bitflags(BitflagsDefinition),
    Union(UnionDefinition),
    TypeAlias(TypeAliasDefinition),
    Constant(ConstDefinition),
    ExternValue(ExternValueDefinition),
}
impl From<TypeDefinition> for ItemDefinitionInner {
    fn from(td: TypeDefinition) -> Self {
        ItemDefinitionInner::Type(td)
    }
}
impl From<EnumDefinition> for ItemDefinitionInner {
    fn from(ed: EnumDefinition) -> Self {
        ItemDefinitionInner::Enum(ed)
    }
}
impl From<UnionDefinition> for ItemDefinitionInner {
    fn from(ud: UnionDefinition) -> Self {
        ItemDefinitionInner::Union(ud)
    }
}
impl From<BitflagsDefinition> for ItemDefinitionInner {
    fn from(bd: BitflagsDefinition) -> Self {
        ItemDefinitionInner::Bitflags(bd)
    }
}
impl From<TypeAliasDefinition> for ItemDefinitionInner {
    fn from(ta: TypeAliasDefinition) -> Self {
        ItemDefinitionInner::TypeAlias(ta)
    }
}
impl From<ConstDefinition> for ItemDefinitionInner {
    fn from(cd: ConstDefinition) -> Self {
        ItemDefinitionInner::Constant(cd)
    }
}
impl From<ExternValueDefinition> for ItemDefinitionInner {
    fn from(ev: ExternValueDefinition) -> Self {
        ItemDefinitionInner::ExternValue(ev)
    }
}
impl ItemDefinitionInner {
    pub fn defaultable(&self) -> bool {
        match self {
            ItemDefinitionInner::Type(td) => td.defaultable,
            ItemDefinitionInner::Enum(ed) => ed.default.is_some(),
            ItemDefinitionInner::Bitflags(bd) => bd.default.is_some(),
            ItemDefinitionInner::Union(ud) => ud.defaultable,
            ItemDefinitionInner::TypeAlias(_) => false, // Type aliases don't have defaultable
            ItemDefinitionInner::Constant(_) => false,
            ItemDefinitionInner::ExternValue(_) => false,
        }
    }
    pub fn copyable(&self) -> bool {
        match self {
            ItemDefinitionInner::Type(td) => td.copyable,
            ItemDefinitionInner::Enum(ed) => ed.copyable,
            ItemDefinitionInner::Bitflags(bd) => bd.copyable,
            ItemDefinitionInner::Union(ud) => ud.copyable,
            ItemDefinitionInner::TypeAlias(_) => false, // Type aliases don't have copyable
            ItemDefinitionInner::Constant(_) => false,
            ItemDefinitionInner::ExternValue(_) => false,
        }
    }
    pub fn cloneable(&self) -> bool {
        match self {
            ItemDefinitionInner::Type(td) => td.cloneable,
            ItemDefinitionInner::Enum(ed) => ed.cloneable,
            ItemDefinitionInner::Bitflags(bd) => bd.cloneable,
            ItemDefinitionInner::Union(ud) => ud.cloneable,
            ItemDefinitionInner::TypeAlias(_) => false, // Type aliases don't have cloneable
            ItemDefinitionInner::Constant(_) => false,
            ItemDefinitionInner::ExternValue(_) => false,
        }
    }
    pub fn pinned(&self) -> bool {
        match self {
            ItemDefinitionInner::Type(td) => td.pinned,
            ItemDefinitionInner::Enum(ed) => ed.pinned,
            ItemDefinitionInner::Bitflags(bd) => bd.pinned,
            ItemDefinitionInner::Union(ud) => ud.pinned,
            ItemDefinitionInner::TypeAlias(_) => false, // Type aliases don't have pinned
            ItemDefinitionInner::Constant(_) => false,
            ItemDefinitionInner::ExternValue(_) => false,
        }
    }
    pub fn as_type(&self) -> Option<&TypeDefinition> {
        match self {
            Self::Type(v) => Some(v),
            _ => None,
        }
    }
    pub fn as_enum(&self) -> Option<&EnumDefinition> {
        match self {
            Self::Enum(v) => Some(v),
            _ => None,
        }
    }
    pub fn human_friendly_type(&self) -> &'static str {
        match self {
            ItemDefinitionInner::Type(_) => "a type",
            ItemDefinitionInner::Enum(_) => "an enum",
            ItemDefinitionInner::Bitflags(_) => "a bitflags",
            ItemDefinitionInner::Union(_) => "a union",
            ItemDefinitionInner::TypeAlias(_) => "a type alias",
            ItemDefinitionInner::Constant(_) => "a constant",
            ItemDefinitionInner::ExternValue(_) => "an extern value",
        }
    }
    pub fn as_union(&self) -> Option<&UnionDefinition> {
        match self {
            Self::Union(v) => Some(v),
            _ => None,
        }
    }
    pub fn as_type_alias(&self) -> Option<&TypeAliasDefinition> {
        match self {
            Self::TypeAlias(v) => Some(v),
            _ => None,
        }
    }
    pub fn as_constant(&self) -> Option<&ConstDefinition> {
        match self {
            Self::Constant(v) => Some(v),
            _ => None,
        }
    }
    pub fn as_extern_value(&self) -> Option<&ExternValueDefinition> {
        match self {
            Self::ExternValue(v) => Some(v),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct ItemStateResolved {
    pub size: usize,
    pub alignment: usize,
    pub inner: ItemDefinitionInner,
}
impl From<ItemStateResolved> for ItemState {
    fn from(isr: ItemStateResolved) -> Self {
        ItemState::Resolved(isr)
    }
}
impl ItemStateResolved {
    pub fn new((size, alignment): (usize, usize), inner: impl Into<ItemDefinitionInner>) -> Self {
        Self {
            size,
            alignment,
            inner: inner.into(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub enum ItemState {
    Unresolved(grammar::ItemDefinition),
    Resolved(ItemStateResolved),
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
#[cfg_attr(test, derive(StripLocations))]
#[cfg_attr(test, strip_locations(copy))]
pub enum ItemCategory {
    Defined,
    Predefined,
    Extern,
}
macro_rules! predefined_items {
    ($(($variant:ident, $name:expr, $size:expr)),* $(,)?) => {
        #[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
        pub enum PredefinedItem {
            $($variant),*
        }
        #[cfg(test)]
        impl StripLocations for PredefinedItem {
            fn strip_locations(&self) -> Self {
                *self
            }
        }
        impl PredefinedItem {
            pub const ALL: &'static [PredefinedItem] = &[
                $(Self::$variant),*
            ];
            pub fn name(&self) -> &'static str {
                match self {
                    $(Self::$variant => $name),*
                }
            }
            pub fn size(&self) -> usize {
                match self {
                    $(Self::$variant => $size),*
                }
            }
            pub fn is_unsigned_integer(&self) -> bool {
                matches!(self, Self::U8 | Self::U16 | Self::U32 | Self::U64 | Self::U128)
            }
            pub fn is_str(&self) -> bool {
                matches!(self, Self::Str)
            }
            pub fn is_cstr(&self) -> bool {
                matches!(self, Self::CStr)
            }
            pub fn is_integer(&self) -> bool {
                self.is_unsigned_integer()
                    || matches!(
                        self,
                        Self::I8 | Self::I16 | Self::I32 | Self::I64 | Self::I128
                    )
            }
            pub fn is_float(&self) -> bool {
                matches!(self, Self::F32 | Self::F64)
            }
        }
    }
}
predefined_items! {
    (Void, "void", 0),
    (Bool, "bool", 1),
    (U8, "u8", 1),
    (U16, "u16", 2),
    (U32, "u32", 4),
    (U64, "u64", 8),
    (U128, "u128", 16),
    (I8, "i8", 1),
    (I16, "i16", 2),
    (I32, "i32", 4),
    (I64, "i64", 8),
    (I128, "i128", 16),
    (F32, "f32", 4),
    (F64, "f64", 8),
    // C-ABI char. Backend-mapped: rust → `::std::ffi::c_char`, cpp → `char`.
    // Distinct from rust's `char` (4-byte unicode scalar) and pyxis's `i8`/`u8`
    // (signedness varies by platform on the C side, so we keep this an opaque
    // 1-byte type and let the backend pick the appropriate ABI alias).
    (CChar, "c_char", 1),
    // Atomic types
    (AtomicBool, "AtomicBool", 1),
    (AtomicU8, "AtomicU8", 1),
    (AtomicU16, "AtomicU16", 2),
    (AtomicU32, "AtomicU32", 4),
    (AtomicU64, "AtomicU64", 8),
    (AtomicI8, "AtomicI8", 1),
    (AtomicI16, "AtomicI16", 2),
    (AtomicI32, "AtomicI32", 4),
    (AtomicI64, "AtomicI64", 8),
    // `str` — const-only string type. Size 0 / alignment 1 are sentinels; it has
    // no runtime layout because it only appears in `const` declarations.
    (Str, "str", 0),
    // `cstr` — const-only C-string type (NUL-terminated). Like `str` it has no
    // runtime layout; it only appears in `const` declarations. Rust backend
    // maps to `&'static ::std::ffi::CStr`; C++ maps to `const char* const`.
    (CStr, "cstr", 0),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct ItemDefinition {
    pub visibility: Visibility,
    pub path: ItemPath,
    /// Type parameters for generic types (e.g., `["T", "U"]` for `type Map<T, U>`)
    pub type_parameters: Vec<String>,
    pub state: ItemState,
    pub category: ItemCategory,
    pub predefined: Option<PredefinedItem>,
    /// `#[cfg(...)]` predicate. `None` means "always emit"; otherwise each
    /// backend evaluates against its own context.
    pub cfg: Option<CfgPredicate>,
    /// Full span (incl. doc comments / attributes), used for diagnostics.
    pub location: ItemLocation,
    /// Position of the declaration itself, used for documentation source links.
    pub declaration_location: ItemLocation,
}
impl Default for ItemDefinition {
    fn default() -> Self {
        ItemDefinition {
            visibility: Visibility::Public,
            path: ItemPath::empty(),
            type_parameters: vec![],
            state: ItemState::Unresolved(grammar::ItemDefinition::default()),
            category: ItemCategory::Defined,
            predefined: None,
            cfg: None,
            location: ItemLocation::internal(),
            declaration_location: ItemLocation::internal(),
        }
    }
}
impl ItemDefinition {
    /// Test-only constructor for category_resolved that uses a synthetic location
    #[cfg(test)]
    pub fn category_resolved(
        (visibility, path): (Visibility, impl Into<ItemPath>),
        resolved: ItemStateResolved,
        category: ItemCategory,
    ) -> Self {
        ItemDefinition {
            visibility,
            path: path.into(),
            type_parameters: vec![],
            state: ItemState::Resolved(resolved),
            category,
            predefined: None,
            cfg: None,
            location: ItemLocation::test(),
            declaration_location: ItemLocation::test(),
        }
    }

    /// Test-only constructor for defined_resolved that uses a synthetic location
    #[cfg(test)]
    pub fn defined_resolved(
        (visibility, path): (Visibility, impl Into<ItemPath>),
        resolved: ItemStateResolved,
    ) -> Self {
        ItemDefinition {
            visibility,
            path: path.into(),
            type_parameters: vec![],
            state: ItemState::Resolved(resolved),
            category: ItemCategory::Defined,
            predefined: None,
            cfg: None,
            location: ItemLocation::test(),
            declaration_location: ItemLocation::test(),
        }
    }

    /// Test-only constructor for generic defined_resolved that uses a synthetic location
    #[cfg(test)]
    pub fn generic_defined_resolved(
        (visibility, path): (Visibility, impl Into<ItemPath>),
        type_params: impl IntoIterator<Item = impl Into<String>>,
        resolved: ItemStateResolved,
    ) -> Self {
        ItemDefinition {
            visibility,
            path: path.into(),
            type_parameters: type_params.into_iter().map(|s| s.into()).collect(),
            state: ItemState::Resolved(resolved),
            category: ItemCategory::Defined,
            predefined: None,
            cfg: None,
            location: ItemLocation::test(),
            declaration_location: ItemLocation::test(),
        }
    }

    /// Returns true if this is a generic type definition
    pub fn is_generic(&self) -> bool {
        !self.type_parameters.is_empty()
    }

    pub fn resolved(&self) -> Option<&ItemStateResolved> {
        match &self.state {
            ItemState::Resolved(tsr) => Some(tsr),
            _ => None,
        }
    }
    pub fn size(&self) -> Option<usize> {
        self.resolved().map(|r| r.size)
    }
    pub fn alignment(&self) -> Option<usize> {
        self.resolved().map(|r| r.alignment)
    }
    pub fn is_resolved(&self) -> bool {
        self.resolved().is_some()
    }
    pub fn is_predefined(&self) -> bool {
        self.category == ItemCategory::Predefined
    }
    pub fn category(&self) -> ItemCategory {
        self.category
    }
}
