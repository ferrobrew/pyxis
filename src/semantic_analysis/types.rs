use std::{fmt, str::FromStr};

use crate::{
    grammar::{self, ItemPath},
    semantic_analysis::type_registry,
};

#[allow(dead_code, clippy::upper_case_acronyms)]
pub mod test_aliases {
    pub type SID = super::ItemDefinition;
    pub type STD = super::TypeDefinition;
    pub type SED = super::EnumDefinition;
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Argument {
    ConstSelf,
    MutSelf,
    Field(String, Type),
}
impl Argument {
    pub fn field(name: impl Into<String>, type_ref: impl Into<Type>) -> Self {
        Argument::Field(name.into(), type_ref.into())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum CallingConvention {
    C,
    Cdecl,
    Stdcall,
    Fastcall,
    Thiscall,
    Vectorcall,
    System,
}
impl CallingConvention {
    pub fn as_str(&self) -> &'static str {
        match self {
            CallingConvention::C => "C",
            CallingConvention::Cdecl => "cdecl",
            CallingConvention::Stdcall => "stdcall",
            CallingConvention::Fastcall => "fastcall",
            CallingConvention::Thiscall => "thiscall",
            CallingConvention::Vectorcall => "vectorcall",
            CallingConvention::System => "system",
        }
    }
}
impl fmt::Display for CallingConvention {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
impl FromStr for CallingConvention {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "C" => Ok(CallingConvention::C),
            "cdecl" => Ok(CallingConvention::Cdecl),
            "stdcall" => Ok(CallingConvention::Stdcall),
            "fastcall" => Ok(CallingConvention::Fastcall),
            "thiscall" => Ok(CallingConvention::Thiscall),
            "vectorcall" => Ok(CallingConvention::Vectorcall),
            "system" => Ok(CallingConvention::System),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Visibility {
    Public,
    Private,
}
impl From<grammar::Visibility> for Visibility {
    fn from(v: grammar::Visibility) -> Self {
        match v {
            grammar::Visibility::Public => Visibility::Public,
            grammar::Visibility::Private => Visibility::Private,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub visibility: Visibility,
    pub name: String,
    pub address: Option<usize>,
    pub arguments: Vec<Argument>,
    pub return_type: Option<Type>,
    pub calling_convention: CallingConvention,
}
impl Function {
    pub fn new(visibility: Visibility, name: impl Into<String>) -> Self {
        Function {
            visibility,
            name: name.into(),
            address: None,
            arguments: Vec::new(),
            return_type: None,
            // ehh. This is not really always going to be true,
            // but I also don't want to specify it in all of the tests
            calling_convention: CallingConvention::Thiscall,
        }
    }
    pub fn with_address(mut self, address: usize) -> Self {
        self.address = Some(address);
        self
    }
    pub fn with_arguments(mut self, arguments: impl Into<Vec<Argument>>) -> Self {
        self.arguments = arguments.into();
        self
    }
    pub fn with_return_type(mut self, return_type: Type) -> Self {
        self.return_type = Some(return_type);
        self
    }
    pub fn with_calling_convention(mut self, calling_convention: CallingConvention) -> Self {
        self.calling_convention = calling_convention;
        self
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum Type {
    Unresolved(grammar::Type),
    Raw(ItemPath),
    ConstPointer(Box<Type>),
    MutPointer(Box<Type>),
    Array(Box<Type>, usize),
    Function(
        CallingConvention,
        Vec<(String, Box<Type>)>,
        Option<Box<Type>>,
    ),
}

impl Type {
    /// Returns `None` if this type is unresolved
    pub(crate) fn size(&self, type_registry: &type_registry::TypeRegistry) -> Option<usize> {
        match self {
            Type::Unresolved(_) => None,
            Type::Raw(path) => type_registry.get(path).and_then(|t| t.size()),
            Type::ConstPointer(_) => Some(type_registry.pointer_size()),
            Type::MutPointer(_) => Some(type_registry.pointer_size()),
            Type::Array(tr, count) => tr.size(type_registry).map(|s| s * count),
            Type::Function(_, _, _) => Some(type_registry.pointer_size()),
        }
    }

    pub(crate) fn alignment(&self, type_registry: &type_registry::TypeRegistry) -> Option<usize> {
        match self {
            Type::Unresolved(_) => None,
            Type::Raw(path) => type_registry.get(path).and_then(|t| t.alignment()),
            Type::ConstPointer(_) => Some(type_registry.pointer_size()),
            Type::MutPointer(_) => Some(type_registry.pointer_size()),
            Type::Array(tr, _) => Some(tr.alignment(type_registry)?),
            Type::Function(_, _, _) => Some(type_registry.pointer_size()),
        }
    }

    pub fn raw(path: impl Into<ItemPath>) -> Self {
        Type::Raw(path.into())
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
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unresolved(tr) => write!(f, "unresolved:{:?}", tr),
            Type::Raw(path) => write!(f, "{}", path),
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
                write!(f, "; {}]", size)
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
pub struct Region {
    pub visibility: Visibility,
    pub name: Option<String>,
    pub type_ref: Type,
}

impl Region {
    pub fn field(visibility: Visibility, name: impl Into<String>, type_ref: Type) -> Self {
        Region {
            visibility,
            name: Some(name.into()),
            type_ref,
        }
    }

    pub fn unnamed_field(type_ref: Type) -> Self {
        Region {
            visibility: Visibility::Private,
            name: None,
            type_ref,
        }
    }

    pub fn size(&self, type_registry: &type_registry::TypeRegistry) -> Option<usize> {
        self.type_ref.size(type_registry)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Default, Hash)]
pub struct TypeDefinition {
    pub regions: Vec<Region>,
    pub free_functions: Vec<Function>,
    pub vftable_functions: Option<Vec<Function>>,
    pub singleton: Option<usize>,
    pub copyable: bool,
    pub cloneable: bool,
    pub defaultable: bool,
    pub packed: bool,
}
impl TypeDefinition {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn with_regions(mut self, regions: impl Into<Vec<Region>>) -> Self {
        self.regions = regions.into();
        self
    }
    pub fn with_free_functions(mut self, free_functions: impl Into<Vec<Function>>) -> Self {
        self.free_functions = free_functions.into();
        self
    }
    pub fn with_vftable_functions(mut self, vftable_functions: impl Into<Vec<Function>>) -> Self {
        self.vftable_functions = Some(vftable_functions.into());
        self
    }
    pub fn with_singleton(mut self, singleton: usize) -> Self {
        self.singleton = Some(singleton);
        self
    }
    pub fn with_copyable(mut self, copyable: bool) -> Self {
        self.copyable = copyable;
        self
    }
    pub fn with_cloneable(mut self, cloneable: bool) -> Self {
        self.cloneable = cloneable;
        self
    }
    pub fn with_defaultable(mut self, defaultable: bool) -> Self {
        self.defaultable = defaultable;
        self
    }
    pub fn with_packed(mut self, packed: bool) -> Self {
        self.packed = packed;
        self
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct EnumDefinition {
    pub type_: Type,
    pub fields: Vec<(String, isize)>,
    pub singleton: Option<usize>,
    pub copyable: bool,
    pub cloneable: bool,
    pub defaultable: bool,
    pub default_index: Option<usize>,
}
impl EnumDefinition {
    pub fn new(type_: Type) -> Self {
        EnumDefinition {
            type_,
            fields: Vec::new(),
            singleton: None,
            copyable: false,
            cloneable: false,
            defaultable: false,
            default_index: None,
        }
    }
    pub fn with_fields<'a>(mut self, fields: impl IntoIterator<Item = (&'a str, isize)>) -> Self {
        self.fields = fields
            .into_iter()
            .map(|(n, v)| (n.to_string(), v))
            .collect();
        self
    }
    pub fn with_singleton(mut self, singleton: usize) -> Self {
        self.singleton = Some(singleton);
        self
    }
    pub fn with_copyable(mut self, copyable: bool) -> Self {
        self.copyable = copyable;
        self
    }
    pub fn with_cloneable(mut self, cloneable: bool) -> Self {
        self.cloneable = cloneable;
        self
    }
    pub fn with_defaultable(mut self, defaultable: bool) -> Self {
        self.defaultable = defaultable;
        self
    }
    pub fn with_default_index(mut self, default_index: usize) -> Self {
        self.default_index = Some(default_index);
        self
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum ItemDefinitionInner {
    Type(TypeDefinition),
    Enum(EnumDefinition),
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
impl ItemDefinitionInner {
    pub fn defaultable(&self) -> bool {
        match self {
            ItemDefinitionInner::Type(td) => td.defaultable,
            ItemDefinitionInner::Enum(ed) => ed.defaultable && ed.default_index.is_some(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
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

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum ItemState {
    Unresolved(grammar::ItemDefinition),
    Resolved(ItemStateResolved),
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
pub enum ItemCategory {
    Defined,
    Predefined,
    Extern,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct ItemDefinition {
    pub visibility: Visibility,
    pub path: ItemPath,
    pub state: ItemState,
    pub category: ItemCategory,
}
impl ItemDefinition {
    pub fn defined_resolved(
        visibility: Visibility,
        path: impl Into<ItemPath>,
        resolved: ItemStateResolved,
    ) -> Self {
        ItemDefinition {
            visibility,
            path: path.into(),
            state: ItemState::Resolved(resolved),
            category: ItemCategory::Defined,
        }
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
}
