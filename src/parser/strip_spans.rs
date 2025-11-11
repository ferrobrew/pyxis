/// Trait for stripping span information from AST nodes for structural equality testing
pub trait StripSpans {
    /// Remove all span information, returning a version suitable for equality comparison
    fn strip_spans(&self) -> Self;
}

// For the grammar types, since we added comment nodes and other structures,
// we need to strip spans from Spanned<T> wrappers and remove comments for comparison

use crate::grammar::*;
use crate::span::Spanned;

/// Convert doc comments to doc attributes (prepends them to existing attributes)
fn doc_comments_to_attributes(doc_comments: &[String], attrs: Attributes) -> Attributes {
    let mut new_attrs = Vec::new();

    // Add doc comments first
    for comment in doc_comments {
        new_attrs.push(Attribute::doc(comment));
    }

    // Then add existing attributes
    new_attrs.extend(attrs.0);

    Attributes(new_attrs)
}

impl<T: Clone> StripSpans for Spanned<T> {
    fn strip_spans(&self) -> Self {
        // Return a synthetic version with the same value but no real span
        Spanned::new(self.value.clone(), crate::span::Span::synthetic())
    }
}

impl StripSpans for Module {
    fn strip_spans(&self) -> Self {
        Module {
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    ModuleItem::Comment(_) => None, // Filter out comments
                    _ => Some(item.strip_spans()),
                })
                .collect(),
            attributes: self.attributes.strip_spans(),
        }
    }
}

impl StripSpans for ModuleItem {
    fn strip_spans(&self) -> Self {
        match self {
            ModuleItem::Comment(c) => ModuleItem::Comment(c.strip_spans()),
            ModuleItem::Use(p) => ModuleItem::Use(p.clone()),
            ModuleItem::ExternType(n, a, d) => {
                ModuleItem::ExternType(n.clone(), a.strip_spans(), d.clone())
            }
            ModuleItem::Backend(b) => ModuleItem::Backend(b.clone()),
            ModuleItem::Definition(d) => ModuleItem::Definition(d.strip_spans()),
            ModuleItem::Impl(i) => ModuleItem::Impl(i.strip_spans()),
            ModuleItem::ExternValue(e) => ModuleItem::ExternValue(e.strip_spans()),
            ModuleItem::Function(f) => ModuleItem::Function(f.strip_spans()),
        }
    }
}

impl StripSpans for ItemDefinition {
    fn strip_spans(&self) -> Self {
        // Transfer doc_comments to the inner definition's attributes
        let mut inner = self.inner.strip_spans();
        match &mut inner {
            ItemDefinitionInner::Type(t) => {
                t.attributes = doc_comments_to_attributes(&self.doc_comments, t.attributes.clone());
            }
            ItemDefinitionInner::Enum(e) => {
                e.attributes = doc_comments_to_attributes(&self.doc_comments, e.attributes.clone());
            }
            ItemDefinitionInner::Bitflags(b) => {
                b.attributes = doc_comments_to_attributes(&self.doc_comments, b.attributes.clone());
            }
        }

        ItemDefinition {
            visibility: self.visibility,
            name: self.name.clone(),
            doc_comments: vec![], // Strip doc comments for comparison
            inner,
        }
    }
}

impl StripSpans for ItemDefinitionInner {
    fn strip_spans(&self) -> Self {
        match self {
            ItemDefinitionInner::Type(t) => ItemDefinitionInner::Type(t.strip_spans()),
            ItemDefinitionInner::Enum(e) => ItemDefinitionInner::Enum(e.strip_spans()),
            ItemDefinitionInner::Bitflags(b) => ItemDefinitionInner::Bitflags(b.strip_spans()),
        }
    }
}

impl StripSpans for TypeDefinition {
    fn strip_spans(&self) -> Self {
        TypeDefinition {
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    TypeDefItem::Comment(_) => None, // Filter out comments
                    TypeDefItem::Statement(s) => Some(TypeDefItem::Statement(s.strip_spans())),
                })
                .collect(),
            attributes: self.attributes.strip_spans(),
        }
    }
}

impl StripSpans for TypeStatement {
    fn strip_spans(&self) -> Self {
        TypeStatement {
            field: self.field.strip_spans(),
            attributes: doc_comments_to_attributes(
                &self.doc_comments,
                self.attributes.strip_spans(),
            ),
            doc_comments: vec![], // Strip doc comments for comparison
        }
    }
}

impl StripSpans for TypeField {
    fn strip_spans(&self) -> Self {
        match self {
            TypeField::Field(v, n, t) => TypeField::Field(*v, n.clone(), t.clone()),
            TypeField::Vftable(funcs) => {
                TypeField::Vftable(funcs.iter().map(|f| f.strip_spans()).collect())
            }
        }
    }
}

impl StripSpans for EnumDefinition {
    fn strip_spans(&self) -> Self {
        EnumDefinition {
            type_: self.type_.clone(),
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    EnumDefItem::Comment(_) => None, // Filter out comments
                    EnumDefItem::Statement(s) => Some(EnumDefItem::Statement(s.strip_spans())),
                })
                .collect(),
            attributes: self.attributes.strip_spans(),
        }
    }
}

impl StripSpans for EnumStatement {
    fn strip_spans(&self) -> Self {
        EnumStatement {
            name: self.name.clone(),
            expr: self.expr.clone(),
            attributes: doc_comments_to_attributes(
                &self.doc_comments,
                self.attributes.strip_spans(),
            ),
            doc_comments: vec![], // Strip doc comments for comparison
        }
    }
}

impl StripSpans for BitflagsDefinition {
    fn strip_spans(&self) -> Self {
        BitflagsDefinition {
            type_: self.type_.clone(),
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    BitflagsDefItem::Comment(_) => None, // Filter out comments
                    BitflagsDefItem::Statement(s) => {
                        Some(BitflagsDefItem::Statement(s.strip_spans()))
                    }
                })
                .collect(),
            attributes: self.attributes.strip_spans(),
        }
    }
}

impl StripSpans for BitflagsStatement {
    fn strip_spans(&self) -> Self {
        BitflagsStatement {
            name: self.name.clone(),
            expr: self.expr.clone(),
            attributes: doc_comments_to_attributes(
                &self.doc_comments,
                self.attributes.strip_spans(),
            ),
            doc_comments: vec![], // Strip doc comments for comparison
        }
    }
}

impl StripSpans for FunctionBlock {
    fn strip_spans(&self) -> Self {
        FunctionBlock {
            name: self.name.clone(),
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    ImplItem::Comment(_) => None, // Filter out comments
                    ImplItem::Function(f) => Some(ImplItem::Function(f.strip_spans())),
                })
                .collect(),
            attributes: self.attributes.strip_spans(),
        }
    }
}

impl StripSpans for Function {
    fn strip_spans(&self) -> Self {
        Function {
            visibility: self.visibility,
            name: self.name.clone(),
            attributes: doc_comments_to_attributes(
                &self.doc_comments,
                self.attributes.strip_spans(),
            ),
            doc_comments: vec![], // Strip doc comments for comparison
            arguments: self.arguments.clone(),
            return_type: self.return_type.clone(),
        }
    }
}

impl StripSpans for ExternValue {
    fn strip_spans(&self) -> Self {
        ExternValue {
            visibility: self.visibility,
            name: self.name.clone(),
            type_: self.type_.clone(),
            attributes: doc_comments_to_attributes(
                &self.doc_comments,
                self.attributes.strip_spans(),
            ),
            doc_comments: vec![], // Strip doc comments for comparison
        }
    }
}

impl StripSpans for Attributes {
    fn strip_spans(&self) -> Self {
        Attributes(self.0.clone())
    }
}
