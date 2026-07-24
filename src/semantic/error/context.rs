use crate::{
    grammar::ItemPath,
    semantic::types::{ItemDefinitionInner, Type as SemanticType},
    span::ItemLocation,
};
#[cfg(test)]
use pyxis_macros::StripLocations;
use std::fmt;

/// Expected type kind for bitflags type validation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
#[cfg_attr(test, strip_locations(copy))]
pub enum BitflagsExpectedType {
    /// Expected a raw type (not a pointer, array, etc.)
    RawType,
    /// Expected a predefined type
    PredefinedType,
    /// Expected an unsigned integer
    UnsignedInteger,
}

impl fmt::Display for BitflagsExpectedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BitflagsExpectedType::RawType => write!(f, "a raw type"),
            BitflagsExpectedType::PredefinedType => write!(f, "a predefined type"),
            BitflagsExpectedType::UnsignedInteger => write!(f, "an unsigned integer"),
        }
    }
}

/// Context for attribute not supported errors
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub enum AttributeNotSupportedContext {
    /// Attribute not supported for a virtual function
    VirtualFunction { function_name: String },
    /// Attribute not supported for a non-virtual function
    NonVirtualFunction { function_name: String },
}

impl fmt::Display for AttributeNotSupportedContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AttributeNotSupportedContext::VirtualFunction { function_name } => {
                write!(f, "virtual function `{function_name}`")
            }
            AttributeNotSupportedContext::NonVirtualFunction { function_name } => {
                write!(f, "non-virtual function `{function_name}`")
            }
        }
    }
}

/// Kind of duplicate definition error
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
#[cfg_attr(test, strip_locations(copy))]
pub enum DuplicateDefinitionKind {
    /// Function already defined in type or base type
    FunctionInTypeOrBase,
}

impl fmt::Display for DuplicateDefinitionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DuplicateDefinitionKind::FunctionInTypeOrBase => {
                write!(f, "function already defined in type or base type")
            }
        }
    }
}

/// Reason why a field is not defaultable
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
#[cfg_attr(test, strip_locations(copy))]
pub enum DefaultableErrorKind {
    /// Field is a pointer or function type (never defaultable)
    PointerOrFunction,
    /// Field's type is not marked as defaultable
    TypeNotDefaultable,
}

impl fmt::Display for DefaultableErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DefaultableErrorKind::PointerOrFunction => {
                write!(f, "is not a defaultable type (pointer or function?)")
            }
            DefaultableErrorKind::TypeNotDefaultable => {
                write!(f, "is not a defaultable type")
            }
        }
    }
}

/// Kind of type reference (for error messages)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
#[cfg_attr(test, strip_locations(copy))]
pub enum TypeRefKind {
    Unresolved,
    Raw,
    Generic,
    TypeParameter,
    ConstPointer,
    MutPointer,
    Array,
    Function,
}

impl TypeRefKind {
    /// Create from a semantic Type
    pub fn from_type(ty: &SemanticType) -> Self {
        match ty {
            SemanticType::Unresolved(_) => TypeRefKind::Unresolved,
            SemanticType::Raw(_) => TypeRefKind::Raw,
            SemanticType::Generic(_, _) => TypeRefKind::Generic,
            SemanticType::TypeParameter(_) => TypeRefKind::TypeParameter,
            SemanticType::ConstPointer(_) => TypeRefKind::ConstPointer,
            SemanticType::MutPointer(_) => TypeRefKind::MutPointer,
            SemanticType::Array(_, _) => TypeRefKind::Array,
            SemanticType::Function(_, _, _) => TypeRefKind::Function,
        }
    }
}

impl fmt::Display for TypeRefKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeRefKind::Unresolved => write!(f, "an unresolved type"),
            TypeRefKind::Raw => write!(f, "a type"),
            TypeRefKind::Generic => write!(f, "a generic type"),
            TypeRefKind::TypeParameter => write!(f, "a type parameter"),
            TypeRefKind::ConstPointer => write!(f, "a const pointer"),
            TypeRefKind::MutPointer => write!(f, "a mut pointer"),
            TypeRefKind::Array => write!(f, "an array"),
            TypeRefKind::Function => write!(f, "a function"),
        }
    }
}

/// Kind of item definition (for error messages)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
#[cfg_attr(test, strip_locations(copy))]
pub enum ItemKind {
    Type,
    Enum,
    Bitflags,
    TypeAlias,
    Constant,
    ExternValue,
}

impl ItemKind {
    /// Create from an ItemDefinitionInner
    pub fn from_inner(inner: &ItemDefinitionInner) -> Self {
        match inner {
            ItemDefinitionInner::Type(_) => ItemKind::Type,
            ItemDefinitionInner::Enum(_) => ItemKind::Enum,
            ItemDefinitionInner::Bitflags(_) => ItemKind::Bitflags,
            ItemDefinitionInner::TypeAlias(_) => ItemKind::TypeAlias,
            ItemDefinitionInner::Constant(_) => ItemKind::Constant,
            ItemDefinitionInner::ExternValue(_) => ItemKind::ExternValue,
        }
    }
}

impl fmt::Display for ItemKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ItemKind::Type => write!(f, "a type"),
            ItemKind::Enum => write!(f, "an enum"),
            ItemKind::Bitflags => write!(f, "a bitflags"),
            ItemKind::TypeAlias => write!(f, "a type alias"),
            ItemKind::Constant => write!(f, "a constant"),
            ItemKind::ExternValue => write!(f, "an extern value"),
        }
    }
}

/// Known attribute names used in the semantic layer
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
#[cfg_attr(test, strip_locations(copy))]
pub enum AttributeName {
    /// The `address` attribute for specifying memory addresses
    Address,
    /// The `index` attribute for vftable function indices
    Index,
    /// The `calling_convention` attribute for function calling conventions
    CallingConvention,
    /// The `size` attribute for specifying type sizes
    Size,
    /// The `align` attribute for specifying type alignment
    Align,
    /// The `packed` attribute for packed structs
    Packed,
    /// The `min_size` attribute for specifying minimum type sizes
    MinSize,
    /// The `singleton` attribute for singleton types
    Singleton,
    /// The `external_body` attribute marks an `impl` method whose body
    /// is provided by the target backend's prologue/epilogue.
    ExternalBody,
}

impl fmt::Display for AttributeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AttributeName::Address => write!(f, "address"),
            AttributeName::Index => write!(f, "index"),
            AttributeName::CallingConvention => write!(f, "calling_convention"),
            AttributeName::Size => write!(f, "size"),
            AttributeName::Align => write!(f, "align"),
            AttributeName::Packed => write!(f, "packed"),
            AttributeName::MinSize => write!(f, "min_size"),
            AttributeName::Singleton => write!(f, "singleton"),
            AttributeName::ExternalBody => write!(f, "external_body"),
        }
    }
}

/// Kind of extern item (for error messages)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
#[cfg_attr(test, strip_locations(copy))]
pub enum ExternKind {
    /// An extern type declaration
    Type,
    /// An extern value declaration
    Value,
}

impl fmt::Display for ExternKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExternKind::Type => write!(f, "extern type"),
            ExternKind::Value => write!(f, "extern value"),
        }
    }
}

/// Context describing where an unresolved type was used
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub enum UnresolvedTypeContext {
    /// Base type of an enum definition
    EnumBaseType { enum_path: ItemPath },
    /// Base type of a bitflags definition
    BitflagsBaseType { bitflags_path: ItemPath },
    /// Target type of a type alias
    TypeAliasTarget { alias_path: ItemPath },
    /// Field type in a struct
    StructField {
        field_name: String,
        type_path: ItemPath,
    },
    /// Type annotation of a `const` declaration
    ConstType { const_path: ItemPath },
    /// Type annotation of an `extern` value declaration
    ExternValueType { extern_path: ItemPath },
}

impl fmt::Display for UnresolvedTypeContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnresolvedTypeContext::EnumBaseType { enum_path } => {
                write!(f, "base type of enum `{enum_path}`")
            }
            UnresolvedTypeContext::BitflagsBaseType { bitflags_path } => {
                write!(f, "base type of bitflags `{bitflags_path}`")
            }
            UnresolvedTypeContext::TypeAliasTarget { alias_path } => {
                write!(f, "target of type alias `{alias_path}`")
            }
            UnresolvedTypeContext::StructField {
                field_name,
                type_path,
            } => {
                write!(f, "field `{field_name}` of type `{type_path}`")
            }
            UnresolvedTypeContext::ConstType { const_path } => {
                write!(f, "type annotation of const `{const_path}`")
            }
            UnresolvedTypeContext::ExternValueType { extern_path } => {
                write!(f, "type annotation of extern value `{extern_path}`")
            }
        }
    }
}

/// Information about a type reference that couldn't be resolved
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct UnresolvedTypeReference {
    /// The type that couldn't be resolved (as written in source)
    pub type_name: String,
    /// Where it was referenced
    pub location: ItemLocation,
    /// Context describing where this type was used
    pub context: UnresolvedTypeContext,
}

impl fmt::Display for UnresolvedTypeReference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type `{}` in {}", self.type_name, self.context)
    }
}

/// Context for type resolution failures
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub enum TypeResolutionContext {
    /// Resolving alignment for base type of an enum
    EnumBaseTypeAlignment { enum_path: ItemPath },
    /// Resolving alignment for base type of a bitflags
    BitflagsBaseTypeAlignment { bitflags_path: ItemPath },
    /// Resolving type for an extern value
    ExternValue { extern_name: String },
    /// Resolving type for a function argument
    FunctionArgument {
        argument_name: String,
        function_name: String,
    },
}

impl fmt::Display for TypeResolutionContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeResolutionContext::EnumBaseTypeAlignment { enum_path } => {
                write!(f, "alignment for base type of enum `{enum_path}`")
            }
            TypeResolutionContext::BitflagsBaseTypeAlignment { bitflags_path } => {
                write!(f, "alignment for base type of bitflags `{bitflags_path}`")
            }
            TypeResolutionContext::ExternValue { extern_name } => {
                write!(f, "extern value `{extern_name}`")
            }
            TypeResolutionContext::FunctionArgument {
                argument_name,
                function_name,
            } => {
                write!(
                    f,
                    "argument `{argument_name}` in function `{function_name}`"
                )
            }
        }
    }
}
