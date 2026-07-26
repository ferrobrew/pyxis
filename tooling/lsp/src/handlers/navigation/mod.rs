use super::*;

use pyxis::grammar::ItemDefinitionInner;

mod definition;
mod hover;
mod type_hierarchy;

/// The nested item declarations (`const`/`type`/`enum`/… inside a body) of a
/// type, enum, or bitflags definition. These sit in each body's item list
/// alongside fields/variants, not in its `statements()`.
pub(crate) fn nested_items(definition: &ItemDefinition) -> Vec<&ItemDefinition> {
    match &definition.inner {
        ItemDefinitionInner::Union(ud) => ud
            .items
            .iter()
            .filter_map(|i| match i {
                TypeDefItem::Statement(s) => match &s.field {
                    TypeField::Item(it) => Some(&**it),
                    _ => None,
                },
                _ => None,
            })
            .collect(),
        ItemDefinitionInner::Type(td) => td
            .items
            .iter()
            .filter_map(|i| match i {
                TypeDefItem::Statement(s) => match &s.field {
                    TypeField::Item(it) => Some(&**it),
                    _ => None,
                },
                _ => None,
            })
            .collect(),
        ItemDefinitionInner::Enum(e) => e
            .items
            .iter()
            .filter_map(|i| match i {
                EnumDefItem::Item(it) => Some(&**it),
                _ => None,
            })
            .collect(),
        ItemDefinitionInner::Bitflags(b) => b
            .items
            .iter()
            .filter_map(|i| match i {
                BitflagsDefItem::Item(it) => Some(&**it),
                _ => None,
            })
            .collect(),
        ItemDefinitionInner::TypeAlias(_)
        | ItemDefinitionInner::Constant(_)
        | ItemDefinitionInner::ExternValue(_) => Vec::new(),
    }
}
