use std::{collections::HashMap, fmt};

use crate::{
    grammar::{self, ItemPath},
    semantic_analysis::type_registry,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attribute {
    Address(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Argument {
    ConstSelf,
    MutSelf,
    Field(String, Type),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub attributes: Vec<Attribute>,
    pub arguments: Vec<Argument>,
    pub return_type: Option<Type>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Unresolved(grammar::TypeRef),
    Raw(ItemPath),
    ConstPointer(Box<Type>),
    MutPointer(Box<Type>),
    Array(Box<Type>, usize),
    Function(Vec<(String, Box<Type>)>, Option<Box<Type>>),
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
            Type::Function(_, _) => Some(type_registry.pointer_size()),
        }
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
            Type::Function(args, return_type) => {
                write!(f, "fn (")?;
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

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Region {
    Field(String, Type),
    Padding(usize),
}

impl Region {
    pub fn size(&self, type_registry: &type_registry::TypeRegistry) -> Option<usize> {
        match self {
            Region::Field(_, type_ref) => type_ref.size(type_registry),
            Region::Padding(size) => Some(*size),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum MetadataValue {
    Integer(isize),
}

#[derive(PartialEq, Eq, Debug, Clone, Default)]
pub struct TypeDefinition {
    pub regions: Vec<Region>,
    pub functions: HashMap<String, Vec<Function>>,
    pub metadata: HashMap<String, MetadataValue>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct EnumDefinition {
    pub ty: Type,
    pub fields: Vec<(String, isize)>,
    pub metadata: HashMap<String, MetadataValue>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
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

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ItemStateResolved {
    pub size: usize,
    pub inner: ItemDefinitionInner,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ItemState {
    Unresolved(grammar::ItemDefinition),
    Resolved(ItemStateResolved),
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum ItemCategory {
    Defined,
    Predefined,
    Extern,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ItemDefinition {
    pub path: ItemPath,
    pub state: ItemState,
    pub category: ItemCategory,
}

impl ItemDefinition {
    pub fn resolved(&self) -> Option<&ItemStateResolved> {
        match &self.state {
            ItemState::Resolved(tsr) => Some(tsr),
            _ => None,
        }
    }

    pub fn size(&self) -> Option<usize> {
        self.resolved().map(|r| r.size)
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
