//! A body- and location-free index of all declared names.
//!
//! This is the key to incremental resolution. Unlike [`DeclarationRegistry`],
//! which stores full grammar definitions (fields *and* source locations) and so
//! changes on every edit, `NameIndex` stores only what name resolution and
//! reference-registration need: each item's kind + generic arity, extern
//! sizes, module scopes, and which file declares each item. None of that
//! changes when a field's type is edited or lines shift, so the `name_index`
//! query backdates — and a `resolve_item` that depends on it (plus the single
//! file that declares the item) is invalidated only by edits that actually
//! affect it, not by edits anywhere in the project.
//!
//! [`DeclarationRegistry`]: crate::semantic::declaration_registry::DeclarationRegistry

use std::collections::{BTreeMap, BTreeSet};

use crate::{
    grammar::{self, ItemPath},
    semantic::attribute,
    span::HasLocation,
};

/// What kind of item a name denotes (no body, no location).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SigKind {
    Type,
    Enum,
    Bitflags,
    TypeAlias,
    Constant,
    ExternValue,
}

/// A declared item's stable signature: its kind and generic arity.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemSig {
    pub kind: SigKind,
    /// Number of generic type parameters.
    pub arity: usize,
}

/// An extern type's stable signature (size/alignment, no location).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExternSig {
    pub size: usize,
    pub alignment: usize,
}

/// Result of resolving a type name in a scope.
pub enum NameResolution {
    Found(ItemPath),
    FoundPredefined(ItemPath),
    FoundExtern(ItemPath),
    NotFound,
}

/// The stable name index — see the module docs.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct NameIndex {
    /// Declared items: path → signature.
    items: BTreeMap<ItemPath, ItemSig>,
    /// Extern types: path → size/alignment.
    extern_types: BTreeMap<ItemPath, ExternSig>,
    /// Predefined type paths (u8, u32, …).
    predefined: BTreeSet<ItemPath>,
    /// Module path → resolution scope (own path + `use` leaves).
    module_scopes: BTreeMap<ItemPath, Vec<ItemPath>>,
    /// Item path → index into the query's `sources` list (which file declares it).
    item_files: BTreeMap<ItemPath, usize>,
    /// Item path → declaring module path (for nested items whose parent is a type, not a module).
    item_scopes: BTreeMap<ItemPath, ItemPath>,
    /// Pointer size from project config.
    pointer_size: usize,
}

impl NameIndex {
    pub fn new(pointer_size: usize) -> Self {
        Self {
            pointer_size,
            ..Default::default()
        }
    }

    pub fn register_predefined(&mut self) {
        use crate::semantic::types::PredefinedItem;
        for predefined in PredefinedItem::ALL {
            self.predefined.insert(ItemPath::from(predefined.name()));
        }
    }

    /// Index one parsed module (declared at `module_path`, the `file_index`-th
    /// source). Records item signatures, extern sizes, and the module's scope.
    pub fn register_module(
        &mut self,
        module: &grammar::Module,
        module_path: &ItemPath,
        file_index: usize,
    ) {
        let scope = std::iter::once(module_path.clone())
            .chain(module.uses().flat_map(|u| {
                if let grammar::ModuleItem::Use { tree, .. } = u {
                    tree.flatten()
                } else {
                    vec![]
                }
            }))
            .collect::<Vec<_>>();
        self.module_scopes.insert(module_path.clone(), scope);

        for def in module.definitions() {
            let path = module_path.join(def.name.as_str().into());
            let kind = match &def.inner {
                grammar::ItemDefinitionInner::Type(_) => SigKind::Type,
                grammar::ItemDefinitionInner::Enum(_) => SigKind::Enum,
                grammar::ItemDefinitionInner::Bitflags(_) => SigKind::Bitflags,
                grammar::ItemDefinitionInner::TypeAlias(_) => SigKind::TypeAlias,
                grammar::ItemDefinitionInner::Constant(_) => SigKind::Constant,
                grammar::ItemDefinitionInner::ExternValue(_) => SigKind::ExternValue,
            };
            self.items.insert(
                path.clone(),
                ItemSig {
                    kind,
                    arity: def.type_parameters.len(),
                },
            );
            self.item_files.insert(path.clone(), file_index);

            // Recurse into type bodies to find nested items.
            // Only read TypeField::Item data (name, kind, arity) to preserve backdating.
            if let grammar::ItemDefinitionInner::Type(td) = &def.inner {
                for stmt in td.statements() {
                    if let grammar::TypeField::Item(inner) = &stmt.field {
                        let nested_path = path.join(inner.name.as_str().into());
                        let nested_kind = match &inner.inner {
                            grammar::ItemDefinitionInner::Type(_) => SigKind::Type,
                            grammar::ItemDefinitionInner::Enum(_) => SigKind::Enum,
                            grammar::ItemDefinitionInner::Bitflags(_) => SigKind::Bitflags,
                            grammar::ItemDefinitionInner::TypeAlias(_) => SigKind::TypeAlias,
                            grammar::ItemDefinitionInner::Constant(_) => SigKind::Constant,
                            grammar::ItemDefinitionInner::ExternValue(_) => SigKind::ExternValue,
                        };
                        self.items.insert(
                            nested_path.clone(),
                            ItemSig {
                                kind: nested_kind,
                                arity: inner.type_parameters.len(),
                            },
                        );
                        self.item_files.insert(nested_path.clone(), file_index);
                        self.item_scopes
                            .insert(nested_path.clone(), module_path.clone());
                        // Recurse into the nested item's body for doubly-nested items
                        self.register_nested_in_type(
                            &inner.inner,
                            &nested_path,
                            module_path,
                            file_index,
                        );
                        // Also handle enum/bitflags bodies of the nested item
                        if let grammar::ItemDefinitionInner::Enum(ed) = &inner.inner {
                            for item in &ed.items {
                                if let grammar::EnumDefItem::Item(nested2) = item {
                                    let nested2_path =
                                        nested_path.join(nested2.name.as_str().into());
                                    let nested2_kind = match &nested2.inner {
                                        grammar::ItemDefinitionInner::Type(_) => SigKind::Type,
                                        grammar::ItemDefinitionInner::Enum(_) => SigKind::Enum,
                                        grammar::ItemDefinitionInner::Bitflags(_) => {
                                            SigKind::Bitflags
                                        }
                                        grammar::ItemDefinitionInner::TypeAlias(_) => {
                                            SigKind::TypeAlias
                                        }
                                        grammar::ItemDefinitionInner::Constant(_) => {
                                            SigKind::Constant
                                        }
                                        grammar::ItemDefinitionInner::ExternValue(_) => {
                                            SigKind::ExternValue
                                        }
                                    };
                                    self.items.insert(
                                        nested2_path.clone(),
                                        ItemSig {
                                            kind: nested2_kind,
                                            arity: nested2.type_parameters.len(),
                                        },
                                    );
                                    self.item_files.insert(nested2_path.clone(), file_index);
                                    self.item_scopes
                                        .insert(nested2_path.clone(), module_path.clone());
                                    self.register_nested_in_type(
                                        &nested2.inner,
                                        &nested2_path,
                                        module_path,
                                        file_index,
                                    );
                                }
                            }
                        }
                        if let grammar::ItemDefinitionInner::Bitflags(bd) = &inner.inner {
                            for item in &bd.items {
                                if let grammar::BitflagsDefItem::Item(nested2) = item {
                                    let nested2_path =
                                        nested_path.join(nested2.name.as_str().into());
                                    let nested2_kind = match &nested2.inner {
                                        grammar::ItemDefinitionInner::Type(_) => SigKind::Type,
                                        grammar::ItemDefinitionInner::Enum(_) => SigKind::Enum,
                                        grammar::ItemDefinitionInner::Bitflags(_) => {
                                            SigKind::Bitflags
                                        }
                                        grammar::ItemDefinitionInner::TypeAlias(_) => {
                                            SigKind::TypeAlias
                                        }
                                        grammar::ItemDefinitionInner::Constant(_) => {
                                            SigKind::Constant
                                        }
                                        grammar::ItemDefinitionInner::ExternValue(_) => {
                                            SigKind::ExternValue
                                        }
                                    };
                                    self.items.insert(
                                        nested2_path.clone(),
                                        ItemSig {
                                            kind: nested2_kind,
                                            arity: nested2.type_parameters.len(),
                                        },
                                    );
                                    self.item_files.insert(nested2_path.clone(), file_index);
                                    self.item_scopes
                                        .insert(nested2_path.clone(), module_path.clone());
                                    self.register_nested_in_type(
                                        &nested2.inner,
                                        &nested2_path,
                                        module_path,
                                        file_index,
                                    );
                                }
                            }
                        }
                    }
                }
            }
            // Recurse into enum bodies to find nested items.
            if let grammar::ItemDefinitionInner::Enum(ed) = &def.inner {
                for item in &ed.items {
                    if let grammar::EnumDefItem::Item(inner) = item {
                        let nested_path = path.join(inner.name.as_str().into());
                        let nested_kind = match &inner.inner {
                            grammar::ItemDefinitionInner::Type(_) => SigKind::Type,
                            grammar::ItemDefinitionInner::Enum(_) => SigKind::Enum,
                            grammar::ItemDefinitionInner::Bitflags(_) => SigKind::Bitflags,
                            grammar::ItemDefinitionInner::TypeAlias(_) => SigKind::TypeAlias,
                            grammar::ItemDefinitionInner::Constant(_) => SigKind::Constant,
                            grammar::ItemDefinitionInner::ExternValue(_) => SigKind::ExternValue,
                        };
                        self.items.insert(
                            nested_path.clone(),
                            ItemSig {
                                kind: nested_kind,
                                arity: inner.type_parameters.len(),
                            },
                        );
                        self.item_files.insert(nested_path.clone(), file_index);
                        self.item_scopes
                            .insert(nested_path.clone(), module_path.clone());
                        self.register_nested_in_type(
                            &inner.inner,
                            &nested_path,
                            module_path,
                            file_index,
                        );
                    }
                }
            }
            // Recurse into bitflags bodies to find nested items.
            if let grammar::ItemDefinitionInner::Bitflags(bd) = &def.inner {
                for item in &bd.items {
                    if let grammar::BitflagsDefItem::Item(inner) = item {
                        let nested_path = path.join(inner.name.as_str().into());
                        let nested_kind = match &inner.inner {
                            grammar::ItemDefinitionInner::Type(_) => SigKind::Type,
                            grammar::ItemDefinitionInner::Enum(_) => SigKind::Enum,
                            grammar::ItemDefinitionInner::Bitflags(_) => SigKind::Bitflags,
                            grammar::ItemDefinitionInner::TypeAlias(_) => SigKind::TypeAlias,
                            grammar::ItemDefinitionInner::Constant(_) => SigKind::Constant,
                            grammar::ItemDefinitionInner::ExternValue(_) => SigKind::ExternValue,
                        };
                        self.items.insert(
                            nested_path.clone(),
                            ItemSig {
                                kind: nested_kind,
                                arity: inner.type_parameters.len(),
                            },
                        );
                        self.item_files.insert(nested_path.clone(), file_index);
                        self.item_scopes
                            .insert(nested_path.clone(), module_path.clone());
                        self.register_nested_in_type(
                            &inner.inner,
                            &nested_path,
                            module_path,
                            file_index,
                        );
                    }
                }
            }
        }

        for extern_type in module.extern_types() {
            if let grammar::ModuleItem::ExternType {
                name, attributes, ..
            } = extern_type
            {
                let mut size = None;
                let mut alignment = None;
                for attribute in attributes {
                    let Some((ident, items)) = attribute.function() else {
                        continue;
                    };
                    let loc = HasLocation::location(attribute);
                    if let Ok(Some(s)) = attribute::parse_size(ident, items, loc) {
                        size = Some(s);
                    } else if let Ok(Some(a)) = attribute::parse_align(ident, items, loc) {
                        alignment = Some(a);
                    }
                }
                if let (Some(size), Some(alignment)) = (size, alignment) {
                    self.extern_types.insert(
                        module_path.join(name.as_str().into()),
                        ExternSig { size, alignment },
                    );
                }
            }
        }
    }

    /// Recursively register nested items inside a type body.
    fn register_nested_in_type(
        &mut self,
        inner: &grammar::ItemDefinitionInner,
        parent_path: &ItemPath,
        module_path: &ItemPath,
        file_index: usize,
    ) {
        if let grammar::ItemDefinitionInner::Type(td) = inner {
            for stmt in td.statements() {
                if let grammar::TypeField::Item(nested) = &stmt.field {
                    let nested_path = parent_path.join(nested.name.as_str().into());
                    let nested_kind = match &nested.inner {
                        grammar::ItemDefinitionInner::Type(_) => SigKind::Type,
                        grammar::ItemDefinitionInner::Enum(_) => SigKind::Enum,
                        grammar::ItemDefinitionInner::Bitflags(_) => SigKind::Bitflags,
                        grammar::ItemDefinitionInner::TypeAlias(_) => SigKind::TypeAlias,
                        grammar::ItemDefinitionInner::Constant(_) => SigKind::Constant,
                        grammar::ItemDefinitionInner::ExternValue(_) => SigKind::ExternValue,
                    };
                    self.items.insert(
                        nested_path.clone(),
                        ItemSig {
                            kind: nested_kind,
                            arity: nested.type_parameters.len(),
                        },
                    );
                    self.item_files.insert(nested_path.clone(), file_index);
                    self.item_scopes
                        .insert(nested_path.clone(), module_path.clone());
                    // Recurse into the nested item's body too
                    self.register_nested_in_type(
                        &nested.inner,
                        &nested_path,
                        module_path,
                        file_index,
                    );
                    // Also recurse into enum/bitflags bodies of nested items
                    if let grammar::ItemDefinitionInner::Enum(ed) = &nested.inner {
                        for item in &ed.items {
                            if let grammar::EnumDefItem::Item(nested2) = item {
                                let nested2_path = nested_path.join(nested2.name.as_str().into());
                                let nested2_kind = match &nested2.inner {
                                    grammar::ItemDefinitionInner::Type(_) => SigKind::Type,
                                    grammar::ItemDefinitionInner::Enum(_) => SigKind::Enum,
                                    grammar::ItemDefinitionInner::Bitflags(_) => SigKind::Bitflags,
                                    grammar::ItemDefinitionInner::TypeAlias(_) => {
                                        SigKind::TypeAlias
                                    }
                                    grammar::ItemDefinitionInner::Constant(_) => SigKind::Constant,
                                    grammar::ItemDefinitionInner::ExternValue(_) => {
                                        SigKind::ExternValue
                                    }
                                };
                                self.items.insert(
                                    nested2_path.clone(),
                                    ItemSig {
                                        kind: nested2_kind,
                                        arity: nested2.type_parameters.len(),
                                    },
                                );
                                self.item_files.insert(nested2_path.clone(), file_index);
                                self.item_scopes
                                    .insert(nested2_path.clone(), module_path.clone());
                                self.register_nested_in_type(
                                    &nested2.inner,
                                    &nested2_path,
                                    module_path,
                                    file_index,
                                );
                            }
                        }
                    }
                    if let grammar::ItemDefinitionInner::Bitflags(bd) = &nested.inner {
                        for item in &bd.items {
                            if let grammar::BitflagsDefItem::Item(nested2) = item {
                                let nested2_path = nested_path.join(nested2.name.as_str().into());
                                let nested2_kind = match &nested2.inner {
                                    grammar::ItemDefinitionInner::Type(_) => SigKind::Type,
                                    grammar::ItemDefinitionInner::Enum(_) => SigKind::Enum,
                                    grammar::ItemDefinitionInner::Bitflags(_) => SigKind::Bitflags,
                                    grammar::ItemDefinitionInner::TypeAlias(_) => {
                                        SigKind::TypeAlias
                                    }
                                    grammar::ItemDefinitionInner::Constant(_) => SigKind::Constant,
                                    grammar::ItemDefinitionInner::ExternValue(_) => {
                                        SigKind::ExternValue
                                    }
                                };
                                self.items.insert(
                                    nested2_path.clone(),
                                    ItemSig {
                                        kind: nested2_kind,
                                        arity: nested2.type_parameters.len(),
                                    },
                                );
                                self.item_files.insert(nested2_path.clone(), file_index);
                                self.item_scopes
                                    .insert(nested2_path.clone(), module_path.clone());
                                self.register_nested_in_type(
                                    &nested2.inner,
                                    &nested2_path,
                                    module_path,
                                    file_index,
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn pointer_size(&self) -> usize {
        self.pointer_size
    }

    pub fn item_sig(&self, path: &ItemPath) -> Option<&ItemSig> {
        self.items.get(path)
    }

    pub fn extern_sig(&self, path: &ItemPath) -> Option<&ExternSig> {
        self.extern_types.get(path)
    }

    pub fn is_predefined(&self, path: &ItemPath) -> bool {
        self.predefined.contains(path)
    }

    pub fn item_paths(&self) -> impl Iterator<Item = &ItemPath> {
        self.items.keys()
    }

    pub fn extern_paths(&self) -> impl Iterator<Item = (&ItemPath, &ExternSig)> {
        self.extern_types.iter()
    }

    /// The source index of the file that declares `path`, if any.
    pub fn file_of(&self, path: &ItemPath) -> Option<usize> {
        self.item_files.get(path).copied()
    }

    /// The resolution scope for an item path (its declaring module's scope).
    /// For nested items (whose parent is a type, not a module), the scope is
    /// looked up via `item_scopes` which maps to the declaring module.
    pub fn scope_of(&self, item_path: &ItemPath) -> Option<&[ItemPath]> {
        // Check item_scopes first (for nested items whose parent is a type)
        if let Some(declaring_module) = self.item_scopes.get(item_path) {
            return self
                .module_scopes
                .get(declaring_module)
                .map(|s| s.as_slice());
        }
        let parent = item_path.parent()?;
        self.module_scopes.get(&parent).map(|s| s.as_slice())
    }

    /// The resolution scope of a module path directly.
    pub fn module_scope(&self, module_path: &ItemPath) -> Option<&[ItemPath]> {
        self.module_scopes.get(module_path).map(|s| s.as_slice())
    }

    /// Resolve a (possibly multi-segment) type path in a scope to its canonical
    /// `ItemPath`. Mirrors the LSP's `resolve_type_path`: a multi-segment path is
    /// likely absolute, else the leaf name is resolved through the scope.
    pub fn resolve_path(&self, scope: &[ItemPath], path: &ItemPath) -> Option<ItemPath> {
        if path.len() > 1 && (self.items.contains_key(path) || self.extern_types.contains_key(path))
        {
            return Some(path.clone());
        }
        match self.resolve_name(scope, path.last()?.as_str()) {
            NameResolution::Found(p)
            | NameResolution::FoundExtern(p)
            | NameResolution::FoundPredefined(p) => Some(p),
            NameResolution::NotFound => None,
        }
    }

    /// Resolve a type name in the given scope. Mirrors
    /// `DeclarationRegistry::resolve_name` but over the stable index.
    pub fn resolve_name(&self, scope: &[ItemPath], name: &str) -> NameResolution {
        // Explicitly `use`d / in-scope path whose leaf matches.
        let found = scope
            .iter()
            .rev()
            .find(|s| s.last().map(|i| i.as_str()) == Some(name))
            .cloned()
            .or_else(|| {
                // Else search each scope module for `<module>::name`.
                std::iter::once(&ItemPath::empty())
                    .chain(scope.iter())
                    .map(|ip| ip.join(name.into()))
                    .find(|candidate| {
                        self.items.contains_key(candidate)
                            || self.extern_types.contains_key(candidate)
                    })
            });

        if let Some(path) = found {
            if self.extern_types.contains_key(&path) {
                return NameResolution::FoundExtern(path);
            }
            if self.items.contains_key(&path) {
                return NameResolution::Found(path);
            }
        }

        let predefined_path = ItemPath::from(name);
        if self.predefined.contains(&predefined_path) {
            return NameResolution::FoundPredefined(predefined_path);
        }

        NameResolution::NotFound
    }
}
