// Re-export all grammar types from the parser modules
pub use crate::parser::{
    attributes::{Attribute, AttributeItem, AttributeItems, Attributes, Visibility},
    expressions::{Expr, ExprField, IntFormat, StringFormat},
    external::{Backend, ExternValue},
    functions::{Argument, Function, FunctionBlock, ImplItem},
    items::{
        BitflagsDefItem, BitflagsDefinition, BitflagsStatement, Comment, EnumDefItem,
        EnumDefinition, EnumStatement, ItemDefinition, ItemDefinitionInner, TypeDefItem,
        TypeDefinition, TypeField, TypeStatement,
    },
    module::{Module, ModuleItem},
    paths::{ItemPath, ItemPathSegment},
    types::{Ident, Type},
};

#[cfg(test)]
pub mod test_aliases {
    use crate::span::ItemLocation;

    pub type M = super::Module;
    pub type ID = super::ItemDefinition;
    pub type TS = super::TypeStatement;
    pub type TD = super::TypeDefinition;
    pub type ES = super::EnumStatement;
    pub type ED = super::EnumDefinition;
    pub type BFS = super::BitflagsStatement;
    pub type BFD = super::BitflagsDefinition;
    pub type T = super::Type;
    pub type A = super::Attribute;
    pub type AI = super::AttributeItem;
    pub type AIs = super::AttributeItems;
    pub type As = super::Attributes;
    pub type Ar = super::Argument;
    pub type TF = super::TypeField;
    pub type E = super::Expr;
    pub type F = super::Function;
    pub type FB = super::FunctionBlock;
    pub type IP = super::ItemPath;
    pub type B = super::Backend;
    pub type V = super::Visibility;
    pub type EV = super::ExternValue;
    pub fn int_literal(value: isize) -> E {
        E::IntLiteral {
            value,
            format: super::IntFormat::Decimal,
            location: ItemLocation::test(),
        }
    }
    pub fn int_literal_with_format(value: isize, format: super::IntFormat) -> E {
        E::IntLiteral {
            value,
            format,
            location: ItemLocation::test(),
        }
    }
    pub fn string_literal(value: impl Into<String>) -> E {
        E::StringLiteral {
            value: value.into(),
            format: super::StringFormat::Regular,
            location: ItemLocation::test(),
        }
    }
    pub fn string_literal_with_format(value: impl Into<String>, format: super::StringFormat) -> E {
        E::StringLiteral {
            value: value.into(),
            format,
            location: ItemLocation::test(),
        }
    }
}
