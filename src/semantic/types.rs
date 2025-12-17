use std::fmt;

use crate::{
    grammar::{self, ItemPath},
    semantic::type_registry,
    span::{EqualsIgnoringLocations, HasLocation, ItemLocation},
};

#[cfg(test)]
use crate::span::StripLocations;

pub use crate::semantic::{
    bitflags_definition::{BitflagField, BitflagsDefinition},
    enum_definition::{EnumDefinition, EnumVariant},
    function::{Argument, CallingConvention, Function, FunctionBody},
    type_alias_definition::TypeAliasDefinition,
    type_definition::{Region, TypeDefinition, TypeVftable},
};

#[allow(dead_code, clippy::upper_case_acronyms)]
pub mod test_aliases {
    pub type SID = super::ItemDefinition;
    pub type STD = super::TypeDefinition;
    pub type SED = super::EnumDefinition;
    pub type SBFD = super::BitflagsDefinition;
    pub type STAD = super::TypeAliasDefinition;
    pub type ST = super::Type;
    pub type SAr = super::Argument;
    pub type SF = super::Function;
    pub type SIP = super::ItemPath;
    pub type SB = super::Backend;
    pub type SR = super::Region;
    pub type SIC = super::ItemCategory;
    pub type SIS = super::ItemState;
    pub type SISR = super::ItemStateResolved;
    pub type SCC = super::CallingConvention;
    pub type SV = super::Visibility;
    pub type SEV = super::ExternValue;
    pub type STV = super::TypeVftable;
    pub type SFB = super::FunctionBody;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Visibility {
    Public,
    Private,
}
impl EqualsIgnoringLocations for Visibility {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self == other
    }
}
#[cfg(test)]
impl StripLocations for Visibility {
    fn strip_locations(&self) -> Self {
        *self
    }
}
impl From<grammar::Visibility> for Visibility {
    fn from(v: grammar::Visibility) -> Self {
        match v {
            grammar::Visibility::Public => Visibility::Public,
            grammar::Visibility::Private => Visibility::Private,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum Type {
    Unresolved(grammar::Type),
    Raw(ItemPath),
    /// A generic type instantiation, e.g., `SharedPtr<GameObject>`
    /// The ItemPath is the base type (e.g., "SharedPtr") and the Vec contains the type arguments
    Generic(ItemPath, Vec<Type>),
    /// A type parameter reference (e.g., `T` inside a generic type definition)
    TypeParameter(String),
    ConstPointer(Box<Type>),
    MutPointer(Box<Type>),
    Array(Box<Type>, usize),
    Function(
        CallingConvention,
        Vec<(String, Box<Type>)>,
        Option<Box<Type>>,
    ),
}
impl EqualsIgnoringLocations for Type {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Unresolved(t), Type::Unresolved(t2)) => t.equals_ignoring_locations(t2),
            (Type::Raw(item_path), Type::Raw(item_path2)) => {
                item_path.equals_ignoring_locations(item_path2)
            }
            (Type::Generic(base, args), Type::Generic(base2, args2)) => {
                base.equals_ignoring_locations(base2) && args.equals_ignoring_locations(args2)
            }
            (Type::TypeParameter(name), Type::TypeParameter(name2)) => name == name2,
            (Type::ConstPointer(t), Type::ConstPointer(t2)) => t.equals_ignoring_locations(t2),
            (Type::MutPointer(t), Type::MutPointer(t2)) => t.equals_ignoring_locations(t2),
            (Type::Array(t, n), Type::Array(t2, n2)) => {
                t.equals_ignoring_locations(t2) && n.equals_ignoring_locations(n2)
            }
            (
                Type::Function(calling_convention, items, t),
                Type::Function(calling_convention2, items2, t2),
            ) => {
                calling_convention.equals_ignoring_locations(calling_convention2)
                    && items.equals_ignoring_locations(items2)
                    && t.equals_ignoring_locations(t2)
            }
            _ => false,
        }
    }
}
#[cfg(test)]
impl StripLocations for Type {
    fn strip_locations(&self) -> Self {
        match self {
            Type::Unresolved(located) => Type::Unresolved(located.strip_locations()),
            Type::Raw(item_path) => Type::Raw(item_path.strip_locations()),
            Type::Generic(base, args) => Type::Generic(
                base.strip_locations(),
                args.iter().map(|a| a.strip_locations()).collect(),
            ),
            Type::TypeParameter(name) => Type::TypeParameter(name.clone()),
            Type::ConstPointer(t) => Type::ConstPointer(t.strip_locations()),
            Type::MutPointer(t) => Type::MutPointer(t.strip_locations()),
            Type::Array(t, n) => Type::Array(t.strip_locations(), n.strip_locations()),
            Type::Function(calling_convention, items, return_type) => Type::Function(
                calling_convention.strip_locations(),
                items.strip_locations(),
                return_type.strip_locations(),
            ),
        }
    }
}
impl Type {
    /// Returns `None` if this type is unresolved or depends on unresolved type parameters
    pub(crate) fn size(&self, type_registry: &type_registry::TypeRegistry) -> Option<usize> {
        match self {
            Type::Unresolved(_) => None,
            Type::Raw(path) => type_registry
                .get(path, &ItemLocation::internal())
                .ok()
                .and_then(|t| t.size()),
            Type::Generic(base, _args) => {
                // For generic instantiations, we need to look up the base type
                // and compute size based on the instantiation
                // For now, return the base type's size (works for pointer-based generics)
                type_registry
                    .get(base, &ItemLocation::internal())
                    .ok()
                    .and_then(|t| t.size())
            }
            Type::TypeParameter(_) => {
                // Type parameters don't have a known size until instantiated
                None
            }
            Type::ConstPointer(_) => Some(type_registry.pointer_size()),
            Type::MutPointer(_) => Some(type_registry.pointer_size()),
            Type::Array(tr, count) => tr.size(type_registry).map(|s| s * count),
            Type::Function(_, _, _) => Some(type_registry.pointer_size()),
        }
    }
    pub(crate) fn alignment(&self, type_registry: &type_registry::TypeRegistry) -> Option<usize> {
        match self {
            Type::Unresolved(_) => None,
            Type::Raw(path) => type_registry
                .get(path, &ItemLocation::internal())
                .ok()
                .and_then(|t| t.alignment()),
            Type::Generic(base, _args) => {
                // For generic instantiations, use the base type's alignment
                type_registry
                    .get(base, &ItemLocation::internal())
                    .ok()
                    .and_then(|t| t.alignment())
            }
            Type::TypeParameter(_) => {
                // Type parameters don't have a known alignment until instantiated
                None
            }
            Type::ConstPointer(_) => Some(type_registry.pointer_size()),
            Type::MutPointer(_) => Some(type_registry.pointer_size()),
            Type::Array(tr, _) => Some(tr.alignment(type_registry)?),
            Type::Function(_, _, _) => Some(type_registry.pointer_size()),
        }
    }
    pub fn raw(path: impl Into<ItemPath>) -> Self {
        Type::Raw(path.into())
    }
    pub fn generic(path: impl Into<ItemPath>, args: impl IntoIterator<Item = Type>) -> Self {
        Type::Generic(path.into(), args.into_iter().collect())
    }
    pub fn type_parameter(name: impl Into<String>) -> Self {
        Type::TypeParameter(name.into())
    }
    pub fn const_pointer(self) -> Self {
        Type::ConstPointer(Box::new(self))
    }
    pub fn mut_pointer(self) -> Self {
        Type::MutPointer(Box::new(self))
    }
    pub fn array(self, size: usize) -> Self {
        Type::Array(Box::new(self), size)
    }
    pub fn function<'a>(
        calling_convention: CallingConvention,
        args: impl Into<Vec<(&'a str, Type)>>,
        return_type: impl Into<Option<Type>>,
    ) -> Self {
        Type::Function(
            calling_convention,
            args.into()
                .into_iter()
                .map(|(name, tr)| (name.to_string(), Box::new(tr)))
                .collect(),
            return_type.into().map(Box::new),
        )
    }
    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_, _))
    }
    pub fn boxed(self) -> Box<Type> {
        Box::new(self)
    }
    pub fn human_friendly_type(&self) -> &'static str {
        match self {
            Type::Unresolved(_) => "an unresolved type",
            Type::Raw(_) => "a type",
            Type::Generic(_, _) => "a generic type",
            Type::TypeParameter(_) => "a type parameter",
            Type::ConstPointer(_) => "a const pointer",
            Type::MutPointer(_) => "a mut pointer",
            Type::Array(_, _) => "an array",
            Type::Function(_, _, _) => "a function",
        }
    }
    pub fn as_raw(&self) -> Option<&ItemPath> {
        match self {
            Self::Raw(v) => Some(v),
            _ => None,
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unresolved(tr) => write!(f, "unresolved:{tr:?}"),
            Type::Raw(path) => write!(f, "{path}"),
            Type::Generic(base, args) => {
                write!(f, "{base}<")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    arg.fmt(f)?;
                }
                write!(f, ">")
            }
            Type::TypeParameter(name) => write!(f, "{name}"),
            Type::ConstPointer(tr) => {
                write!(f, "*const ")?;
                tr.fmt(f)
            }
            Type::MutPointer(tr) => {
                write!(f, "*mut ")?;
                tr.fmt(f)
            }
            Type::Array(tr, size) => {
                write!(f, "[")?;
                tr.fmt(f)?;
                write!(f, "; {size}]")
            }
            Type::Function(calling_convention, args, return_type) => {
                write!(f, "extern \"{calling_convention}\" fn (")?;
                for (index, (field, type_ref)) in args.iter().enumerate() {
                    write!(f, "{field}: ")?;
                    type_ref.fmt(f)?;
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")?;
                if let Some(type_ref) = return_type {
                    write!(f, " -> ")?;
                    type_ref.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum ItemDefinitionInner {
    Type(TypeDefinition),
    Enum(EnumDefinition),
    Bitflags(BitflagsDefinition),
    TypeAlias(TypeAliasDefinition),
}
#[cfg(test)]
impl StripLocations for ItemDefinitionInner {
    fn strip_locations(&self) -> Self {
        match self {
            ItemDefinitionInner::Type(td) => ItemDefinitionInner::Type(td.strip_locations()),
            ItemDefinitionInner::Enum(ed) => ItemDefinitionInner::Enum(ed.strip_locations()),
            ItemDefinitionInner::Bitflags(bd) => {
                ItemDefinitionInner::Bitflags(bd.strip_locations())
            }
            ItemDefinitionInner::TypeAlias(ta) => {
                ItemDefinitionInner::TypeAlias(ta.strip_locations())
            }
        }
    }
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
impl ItemDefinitionInner {
    pub fn defaultable(&self) -> bool {
        match self {
            ItemDefinitionInner::Type(td) => td.defaultable,
            ItemDefinitionInner::Enum(ed) => ed.default.is_some(),
            ItemDefinitionInner::Bitflags(bd) => bd.default.is_some(),
            ItemDefinitionInner::TypeAlias(_) => false, // Type aliases don't have defaultable
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
            ItemDefinitionInner::TypeAlias(_) => "a type alias",
        }
    }
    pub fn as_type_alias(&self) -> Option<&TypeAliasDefinition> {
        match self {
            Self::TypeAlias(v) => Some(v),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct ItemStateResolved {
    pub size: usize,
    pub alignment: usize,
    pub inner: ItemDefinitionInner,
}
#[cfg(test)]
impl StripLocations for ItemStateResolved {
    fn strip_locations(&self) -> Self {
        ItemStateResolved {
            size: self.size.strip_locations(),
            alignment: self.alignment.strip_locations(),
            inner: self.inner.strip_locations(),
        }
    }
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
pub enum ItemState {
    Unresolved(grammar::ItemDefinition),
    Resolved(ItemStateResolved),
}
#[cfg(test)]
impl StripLocations for ItemState {
    fn strip_locations(&self) -> Self {
        match self {
            ItemState::Unresolved(def) => ItemState::Unresolved(def.strip_locations()),
            ItemState::Resolved(resolved) => ItemState::Resolved(resolved.strip_locations()),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
pub enum ItemCategory {
    Defined,
    Predefined,
    Extern,
}
#[cfg(test)]
impl StripLocations for ItemCategory {
    fn strip_locations(&self) -> Self {
        *self
    }
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemDefinition {
    pub visibility: Visibility,
    pub path: ItemPath,
    /// Type parameters for generic types (e.g., `["T", "U"]` for `type Map<T, U>`)
    pub type_parameters: Vec<String>,
    pub state: ItemState,
    pub category: ItemCategory,
    pub predefined: Option<PredefinedItem>,
    pub location: ItemLocation,
}
impl HasLocation for ItemDefinition {
    fn location(&self) -> &ItemLocation {
        &self.location
    }
}
#[cfg(test)]
// StripSpans implementations for testing
impl StripLocations for ItemDefinition {
    fn strip_locations(&self) -> Self {
        ItemDefinition {
            visibility: self.visibility.strip_locations(),
            path: self.path.strip_locations(),
            type_parameters: self.type_parameters.clone(),
            state: self.state.strip_locations(),
            category: self.category.strip_locations(),
            predefined: self.predefined.strip_locations(),
            location: ItemLocation::test(),
        }
    }
}
impl ItemDefinition {
    /// Test-only constructor for category_resolved that uses a synthetic location
    #[cfg(test)]
    pub fn category_resolved(
        (visibility, path): (Visibility, impl Into<crate::grammar::ItemPath>),
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
            location: ItemLocation::test(),
        }
    }

    /// Test-only constructor for defined_resolved that uses a synthetic location
    #[cfg(test)]
    pub fn defined_resolved(
        (visibility, path): (Visibility, impl Into<crate::grammar::ItemPath>),
        resolved: ItemStateResolved,
    ) -> Self {
        ItemDefinition {
            visibility,
            path: path.into(),
            type_parameters: vec![],
            state: ItemState::Resolved(resolved),
            category: ItemCategory::Defined,
            predefined: None,
            location: ItemLocation::test(),
        }
    }

    /// Test-only constructor for generic defined_resolved that uses a synthetic location
    #[cfg(test)]
    pub fn generic_defined_resolved(
        (visibility, path): (Visibility, impl Into<crate::grammar::ItemPath>),
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
            location: ItemLocation::test(),
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

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Backend {
    pub prologue: Option<String>,
    pub epilogue: Option<String>,
    pub location: ItemLocation,
}
impl HasLocation for Backend {
    fn location(&self) -> &ItemLocation {
        &self.location
    }
}
#[cfg(test)]
impl Backend {
    pub fn new(prologue: impl Into<Option<String>>, epilogue: impl Into<Option<String>>) -> Self {
        Backend {
            prologue: prologue.into(),
            epilogue: epilogue.into(),
            location: ItemLocation::test(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ExternValue {
    pub visibility: Visibility,
    pub name: String,
    pub type_: Type,
    pub address: usize,
    pub location: ItemLocation,
}
impl HasLocation for ExternValue {
    fn location(&self) -> &ItemLocation {
        &self.location
    }
}
