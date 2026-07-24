use crate::{
    semantic::{
        type_registry::TypeRegistry,
        types::{Type, Visibility},
    },
    span::{HasLocation, ItemLocation},
};

#[cfg(test)]
use crate::span::StripLocations;

#[derive(PartialEq, Eq, Hash, Debug, Clone, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub struct Region {
    pub visibility: Visibility,
    pub name: Option<String>,
    pub doc: Vec<String>,
    pub type_ref: Type,
    pub is_base: bool,
    pub location: ItemLocation,
}
impl Region {
    #[cfg(test)]
    /// Test-only constructor for field that uses a synthetic location
    pub fn field((visibility, name): (Visibility, impl Into<String>), type_ref: Type) -> Self {
        let (visibility, name) = (visibility, name);
        Region {
            visibility,
            name: Some(name.into()),
            doc: vec![],
            type_ref,
            is_base: false,
            location: ItemLocation::test(),
        }
    }

    pub fn unnamed_field(type_ref: Type, location: ItemLocation) -> Self {
        Region {
            visibility: Visibility::Private,
            name: None,
            doc: vec![],
            type_ref,
            is_base: false,
            location,
        }
    }

    pub fn marked_as_base(mut self) -> Self {
        self.is_base = true;
        self
    }
    pub fn with_doc(mut self, doc: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.doc = doc.into_iter().map(|s| s.into()).collect();
        self
    }
    pub fn size(&self, type_registry: &TypeRegistry) -> Option<usize> {
        self.type_ref.size(type_registry)
    }
}
