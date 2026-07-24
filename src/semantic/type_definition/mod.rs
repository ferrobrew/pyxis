use crate::{
    grammar::ItemPath,
    semantic::{
        error::Result,
        type_registry::TypeRegistry,
        types::{Function, Type},
    },
};

#[cfg(test)]
use crate::span::StripLocations;

mod build;
mod region;
mod resolve;
mod vftable;

pub use build::build;
pub use region::Region;
pub(in crate::semantic) use resolve::get_region_name_and_type_definition;
pub use vftable::TypeVftable;

#[derive(PartialEq, Eq, Debug, Clone, Default, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct TypeDefinition {
    pub regions: Vec<Region>,
    pub doc: Vec<String>,
    pub associated_functions: Vec<Function>,
    pub vftable: Option<TypeVftable>,
    pub singleton: Option<usize>,
    pub copyable: bool,
    pub cloneable: bool,
    pub defaultable: bool,
    pub packed: bool,
    pub pinned: bool,
    /// Item paths of items declared inside this type body (nested items).
    pub nested_item_paths: Vec<ItemPath>,
}
#[cfg(test)]
impl TypeDefinition {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn with_regions(mut self, regions: impl IntoIterator<Item = Region>) -> Self {
        self.regions = regions.into_iter().collect();
        self
    }
    pub fn with_doc(mut self, doc: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.doc = doc.into_iter().map(|s| s.into()).collect();
        self
    }
    pub fn with_associated_functions(
        mut self,
        associated_functions: impl IntoIterator<Item = Function>,
    ) -> Self {
        self.associated_functions = associated_functions.into_iter().collect();
        self
    }
    pub fn with_vftable(mut self, vftable: TypeVftable) -> Self {
        self.vftable = Some(vftable);
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
    pub fn with_pinned(mut self, pinned: bool) -> Self {
        self.pinned = pinned;
        self
    }
    pub fn with_nested_item_paths(mut self, paths: impl IntoIterator<Item = ItemPath>) -> Self {
        self.nested_item_paths = paths.into_iter().collect();
        self
    }
}
impl TypeDefinition {
    /// Returns the fields and types of everything in this type's hierarchy, starting from the top
    pub fn dfs_hierarchy(
        &self,
        type_registry: &TypeRegistry,
        type_path: &ItemPath,
        fields: &[&str],
    ) -> Result<Vec<(Vec<String>, Type)>> {
        let mut output = vec![];
        for region in &self.regions {
            if !region.is_base {
                continue;
            }

            let Some((field_name, type_definition)) =
                get_region_name_and_type_definition(type_registry, type_path, region)?
            else {
                continue;
            };
            let field_path = fields
                .iter()
                .copied()
                .chain(Some(field_name.as_str()))
                .collect::<Vec<_>>();
            output.push((
                field_path.iter().map(|s| s.to_string()).collect(),
                region.type_ref.clone(),
            ));
            output.extend(type_definition.dfs_hierarchy(type_registry, type_path, &field_path)?);
        }

        Ok(output)
    }
    pub fn doc(&self) -> &[String] {
        &self.doc
    }
}
