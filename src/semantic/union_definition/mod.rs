use crate::{grammar::ItemPath, semantic::type_definition::Region};

#[cfg(test)]
use crate::span::StripLocations;

mod build;

pub use build::{InlineUnionRequest, build, build_inline_union};

/// A set of competing readings of the same bytes.
///
/// Every region starts at offset 0; the union's size is that of its largest
/// member (rounded up to its alignment) and its alignment is the strictest of
/// its members'. This is deliberately a separate struct from
/// [`crate::semantic::TypeDefinition`] rather than a flag on it: a
/// `TypeDefinition`'s `Vec<Region>` *is* its layout, with offsets implied by
/// accumulating sizes in insertion order. Several places rely on that. Giving
/// unions their own type turns every one of those places into a compile error
/// instead of a silently wrong offset.
#[derive(PartialEq, Eq, Debug, Clone, Default, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct UnionDefinition {
    /// The union's members. All of these start at offset 0.
    pub regions: Vec<Region>,
    pub doc: Vec<String>,
    pub copyable: bool,
    pub cloneable: bool,
    pub defaultable: bool,
    pub packed: bool,
    pub pinned: bool,
    /// Item paths of items declared inside this union body (nested items).
    pub nested_item_paths: Vec<ItemPath>,
}

#[cfg(test)]
impl UnionDefinition {
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

impl UnionDefinition {
    pub fn doc(&self) -> &[String] {
        &self.doc
    }
}

/// The generated item name for an inline union field: `Value` + `payload` →
/// `ValuePayloadUnion`.
///
/// Inline unions become module-scope siblings of their parent, mirroring the
/// generated `{Name}Vftable` structs. A genuinely nested path (`Value::Payload`)
/// is not an option: `ResolutionContext::add_item` resolves an item's parent
/// directly against the module map, and `declaring_module` only knows nested
/// paths through the grammar walks that populate `item_scopes` — so a generated
/// nested item would be dropped from every module and never reach a backend.
pub fn inline_union_name(parent_name: &str, field_name: &str) -> String {
    fn pascal_case(s: &str) -> String {
        s.split('_')
            .filter(|segment| !segment.is_empty())
            .map(|segment| {
                let mut chars = segment.chars();
                match chars.next() {
                    Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
                    None => String::new(),
                }
            })
            .collect()
    }
    format!("{parent_name}{}Union", pascal_case(field_name))
}
