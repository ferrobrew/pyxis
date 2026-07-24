use crate::grammar::ItemPath;

use super::Type;

#[cfg(test)]
use crate::span::StripLocations;

/// A compile-time constant value. `Float` stores the bit pattern (`f64::to_bits`)
/// so that `ConstValue` can derive `Eq`/`Hash` — `f64` does not implement either.
/// Two NaN values with the same bit pattern compare equal, which is acceptable for
/// a compile-time constant IR.
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub enum ConstValue {
    Int(isize),
    Float(u64),
    String(String),
    CString(String),
    EnumValue(ItemPath),
    /// A structured initializer for a POD type. `type_path` is the resolved
    /// type of the struct; `fields` are ordered to match the type definition's
    /// field declaration order (not source literal order), so the C++ backend
    /// can emit positional braced initialization.
    Struct {
        type_path: ItemPath,
        fields: Vec<(String, ConstValue)>,
    },
    /// An array initializer. Elements are in declaration order.
    Array(Vec<ConstValue>),
    /// A reference to another constant by path.
    ConstRef(ItemPath),
}

impl ConstValue {
    /// Parse the stored bit pattern back into an `f64`.
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            ConstValue::Float(bits) => Some(f64::from_bits(*bits)),
            _ => None,
        }
    }
}

/// Semantic representation of a `const` declaration.
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct ConstDefinition {
    pub type_: Type,
    pub value: ConstValue,
    pub doc: Vec<String>,
}

/// Semantic representation of an `extern` value declaration. Like
/// [`ConstDefinition`] it is a value item (size 0 / alignment 1); its
/// `visibility`, `name`, and path live on the enclosing [`ItemDefinition`](super::ItemDefinition).
/// The `#[address(...)]` attribute is resolved into `address`.
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct ExternValueDefinition {
    pub type_: Type,
    pub address: usize,
    pub doc: Vec<String>,
}
