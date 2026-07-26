use std::fmt;

use crate::{
    grammar::{self, ItemPath},
    semantic::{function::CallingConvention, type_registry},
    span::{EqualsIgnoringLocations, ItemLocation},
};

#[cfg(test)]
use crate::span::StripLocations;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
#[cfg_attr(test, strip_locations(copy))]
pub enum Visibility {
    Public,
    Private,
}
impl EqualsIgnoringLocations for Visibility {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self == other
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
#[cfg_attr(test, derive(StripLocations))]
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
    /// A function pointer: calling convention, parameters, optional return type.
    Function(CallingConvention, Vec<FunctionArg>, Option<Box<Type>>),
}

/// One parameter of a function-pointer type. `name` is `None` for a parameter
/// written without one (`fn(u32)`); backends that can emit an unnamed
/// parameter do, rather than inventing an identifier.
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct FunctionArg {
    pub name: Option<String>,
    pub type_: Box<Type>,
}
impl EqualsIgnoringLocations for FunctionArg {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self.name == other.name && self.type_.equals_ignoring_locations(&other.type_)
    }
}
impl FunctionArg {
    pub fn new(name: Option<String>, type_: Type) -> Self {
        FunctionArg {
            name,
            type_: Box::new(type_),
        }
    }
    /// A parameter with a name, as vftable-derived signatures always have.
    pub fn named(name: impl Into<String>, type_: Type) -> Self {
        FunctionArg::new(Some(name.into()), type_)
    }
    /// An anonymous parameter, as written in `fn(u32)`.
    pub fn unnamed(type_: Type) -> Self {
        FunctionArg::new(None, type_)
    }
}
impl From<(&str, Type)> for FunctionArg {
    fn from((name, type_): (&str, Type)) -> Self {
        FunctionArg::named(name, type_)
    }
}
impl From<Type> for FunctionArg {
    fn from(type_: Type) -> Self {
        FunctionArg::unnamed(type_)
    }
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
impl Type {
    /// Returns `None` if this type is unresolved or depends on unresolved type parameters
    pub(crate) fn size(&self, type_registry: &type_registry::TypeRegistry) -> Option<usize> {
        match self {
            Type::Unresolved(_) => None,
            Type::Raw(path) => type_registry
                .get(path, &ItemLocation::internal())
                .ok()
                .and_then(|t| t.size()),
            Type::Generic(base, args) => {
                // Compute size by substituting type parameters with concrete arguments
                type_registry.compute_generic_size(base, args)
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
            Type::Generic(base, args) => {
                // Compute alignment by substituting type parameters with concrete arguments
                type_registry.compute_generic_alignment(base, args)
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
    pub fn function(
        calling_convention: CallingConvention,
        args: impl IntoIterator<Item = impl Into<FunctionArg>>,
        return_type: impl Into<Option<Type>>,
    ) -> Self {
        Type::Function(
            calling_convention,
            args.into_iter().map(Into::into).collect(),
            return_type.into().map(Box::new),
        )
    }
    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_, _))
    }
    /// Returns `true` if this is the predefined single-segment `f32` type.
    /// Used by backends to decide float literal suffixing (`f32` vs `f64`)
    /// without threading the type registry through.
    pub fn is_f32(&self) -> bool {
        matches!(self, Type::Raw(p) if p.len() == 1 && p.iter().next().is_some_and(|s| s.as_str() == "f32"))
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
                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                    if let Some(name) = &arg.name {
                        write!(f, "{name}: ")?;
                    }
                    arg.type_.fmt(f)?;
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
