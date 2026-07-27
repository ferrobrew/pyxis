use crate::grammar::ItemPath;

pub use crate::semantic::{
    bitflags_definition::{BitflagField, BitflagsDefinition},
    enum_definition::{EnumDefinition, EnumVariant},
    function::{Argument, CallingConvention, Function, FunctionBody},
    type_alias_definition::TypeAliasDefinition,
    type_definition::{Region, TypeDefinition, TypeVftable},
    union_definition::UnionDefinition,
};

mod const_value;
mod item;
mod splice;
mod type_;

pub use const_value::{ConstDefinition, ConstValue, ExternValueDefinition};
pub use item::{
    ItemCategory, ItemDefinition, ItemDefinitionInner, ItemState, ItemStateResolved, PredefinedItem,
};
pub use splice::Splice;
pub use type_::{FunctionArg, Type, Visibility};

#[allow(dead_code, clippy::upper_case_acronyms)]
pub mod test_aliases {
    pub type SID = super::ItemDefinition;
    pub type STD = super::TypeDefinition;
    pub type SUD = super::UnionDefinition;
    pub type SED = super::EnumDefinition;
    pub type SBFD = super::BitflagsDefinition;
    pub type STAD = super::TypeAliasDefinition;
    pub type ST = super::Type;
    pub type SAr = super::Argument;
    pub type SFA = super::FunctionArg;
    pub type SF = super::Function;
    pub type SIP = super::ItemPath;
    pub type SSp = super::Splice;
    pub type SR = super::Region;
    pub type SIC = super::ItemCategory;
    pub type SIS = super::ItemState;
    pub type SISR = super::ItemStateResolved;
    pub type SCC = super::CallingConvention;
    pub type SV = super::Visibility;
    pub type SEVD = super::ExternValueDefinition;
    pub type STV = super::TypeVftable;
    pub type SFB = super::FunctionBody;
}
