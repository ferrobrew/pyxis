//! Resolution of parsed doc-link paths against a snapshot of the crate's
//! items, and the crate-wide pass ([`resolve_all`]) that drives it over every
//! doc comment.

use std::collections::{BTreeMap, BTreeSet};

use super::{
    scan::extract_links,
    types::{
        DocBlockKey, DocLinkMemberKind, DocLinkPath, DocLinkTarget, DocLinks, ModuleDocLinks,
        ResolvedDocLink,
    },
};
use crate::{
    grammar::{ItemPath, ItemPathSegment},
    semantic::{
        error::{Result, SemanticError},
        module::Module,
        type_registry::TypeRegistry,
        types::{ItemDefinitionInner, Visibility},
    },
    span::ItemLocation,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ItemMembers {
    Type {
        methods: Vec<String>,
        vftable_methods: Vec<String>,
        fields: Vec<String>,
        constants: Vec<String>,
        extern_values: Vec<String>,
    },
    Enum {
        variants: Vec<String>,
        methods: Vec<String>,
        constants: Vec<String>,
        extern_values: Vec<String>,
    },
    Bitflags {
        flags: Vec<String>,
        constants: Vec<String>,
        extern_values: Vec<String>,
    },
    Other,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ItemInfo {
    visibility: Visibility,
    members: ItemMembers,
}

/// A snapshot of resolvable items/members, decoupled from the registry so doc
/// links can be resolved while the resolved state is consumed elsewhere.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DocLinkResolver {
    items: BTreeMap<ItemPath, ItemInfo>,
    /// Paths of constants nested inside a type/enum/bitflags (i.e. whose parent
    /// is itself a registered item). These register in the type registry as
    /// full item paths, but must resolve through their parent as a
    /// [`DocLinkTarget::Member`] rather than a freestanding [`DocLinkTarget::Item`]:
    /// the Rust backend emits them as associated consts, not flattened free
    /// items, so importing the full path would name a nonexistent identifier.
    nested_constant_paths: BTreeSet<ItemPath>,
    /// Paths of every `extern` value item (module-level and nested). Skipped in
    /// the whole-path `Item` branch of [`Self::resolve`]: module-level externs
    /// resolve as a [`DocLinkTarget::ExternValue`] and nested ones as a
    /// [`DocLinkTarget::Member`], since the Rust backend emits both as `get_*`
    /// accessors rather than importable free items named after the value.
    extern_value_paths: BTreeSet<ItemPath>,
    module_functions: BTreeMap<ItemPath, Vec<String>>,
    module_extern_values: BTreeMap<ItemPath, Vec<String>>,
}

impl DocLinkResolver {
    pub fn build(type_registry: &TypeRegistry, modules: &BTreeMap<ItemPath, Module>) -> Self {
        // Collect the names of constants nested inside another item, keyed by
        // that parent item's path, plus the set of their full paths.
        let mut nested_constants_by_parent: BTreeMap<ItemPath, Vec<String>> = BTreeMap::new();
        let mut nested_constant_paths = BTreeSet::new();
        // Nested extern values, keyed by their parent item, plus the set of all
        // extern-value paths (module-level and nested).
        let mut nested_extern_values_by_parent: BTreeMap<ItemPath, Vec<String>> = BTreeMap::new();
        let mut extern_value_paths = BTreeSet::new();
        for (path, item) in type_registry.iter() {
            let inner = item.resolved().map(|r| &r.inner);
            let is_constant = matches!(inner, Some(ItemDefinitionInner::Constant(_)));
            let is_extern_value = matches!(inner, Some(ItemDefinitionInner::ExternValue(_)));
            if !is_constant && !is_extern_value {
                continue;
            }
            if is_extern_value {
                extern_value_paths.insert(path.clone());
            }
            let Some(parent) = path.parent() else {
                continue;
            };
            // Only values whose parent is itself an item (type/enum/bitflags) are
            // "nested"; module-level ones resolve as their own item / extern.
            if !type_registry.contains(&parent) {
                continue;
            }
            if let Some(name) = path.last() {
                let bucket = if is_constant {
                    &mut nested_constants_by_parent
                } else {
                    &mut nested_extern_values_by_parent
                };
                bucket
                    .entry(parent)
                    .or_default()
                    .push(name.as_str().to_string());
                if is_constant {
                    nested_constant_paths.insert(path.clone());
                }
            }
        }

        let mut items = BTreeMap::new();
        for (path, item) in type_registry.iter() {
            let constants = nested_constants_by_parent
                .get(path)
                .cloned()
                .unwrap_or_default();
            let extern_values = nested_extern_values_by_parent
                .get(path)
                .cloned()
                .unwrap_or_default();
            let members = match item.resolved().map(|r| &r.inner) {
                Some(ItemDefinitionInner::Type(td)) => ItemMembers::Type {
                    methods: td
                        .associated_functions
                        .iter()
                        .map(|f| f.name.clone())
                        .collect(),
                    vftable_methods: td
                        .vftable
                        .as_ref()
                        .map(|v| v.functions.iter().map(|f| f.name.clone()).collect())
                        .unwrap_or_default(),
                    fields: td.regions.iter().filter_map(|r| r.name.clone()).collect(),
                    constants,
                    extern_values,
                },
                Some(ItemDefinitionInner::Enum(ed)) => ItemMembers::Enum {
                    variants: ed.variants.iter().map(|v| v.name.clone()).collect(),
                    methods: ed
                        .associated_functions
                        .iter()
                        .map(|f| f.name.clone())
                        .collect(),
                    constants,
                    extern_values,
                },
                Some(ItemDefinitionInner::Bitflags(bd)) => ItemMembers::Bitflags {
                    flags: bd.flags.iter().map(|f| f.name.clone()).collect(),
                    constants,
                    extern_values,
                },
                _ => ItemMembers::Other,
            };
            items.insert(
                path.clone(),
                ItemInfo {
                    visibility: item.visibility,
                    members,
                },
            );
        }

        let module_functions = modules
            .iter()
            .map(|(path, module)| {
                (
                    path.clone(),
                    module.functions().iter().map(|f| f.name.clone()).collect(),
                )
            })
            .collect();

        // Module-level extern values: extern-value items whose parent is a
        // module (i.e. not nested inside another item), keyed by that module.
        let mut module_extern_values: BTreeMap<ItemPath, Vec<String>> = BTreeMap::new();
        for module_path in modules.keys() {
            module_extern_values.entry(module_path.clone()).or_default();
        }
        for path in &extern_value_paths {
            let Some(parent) = path.parent() else {
                continue;
            };
            if modules.contains_key(&parent)
                && let Some(name) = path.last()
            {
                module_extern_values
                    .entry(parent)
                    .or_default()
                    .push(name.as_str().to_string());
            }
        }

        DocLinkResolver {
            items,
            nested_constant_paths,
            extern_value_paths,
            module_functions,
            module_extern_values,
        }
    }

    /// Resolve a written link path (e.g. `Action`, `Type::method`) against a
    /// module scope. Returns `None` if it doesn't resolve to anything.
    ///
    /// `enclosing_type` is the path of the item whose emitted docs will contain
    /// the link — the type/enum/bitflags itself for its own and its members'
    /// docs, or the *parent* type for a nested constant/extern value (their
    /// docs land in the parent's `impl` block). It substitutes a `Self` prefix;
    /// `None` at module scope, where `Self` doesn't resolve.
    pub fn resolve(
        &self,
        scope: &[ItemPath],
        path: &DocLinkPath,
        enclosing_type: Option<&ItemPath>,
    ) -> Option<DocLinkTarget> {
        // Substitute a `Self` prefix with the enclosing type's segments.
        let owned_segments;
        let path: &[ItemPathSegment] = if path.self_prefixed {
            let enclosing = enclosing_type?;
            owned_segments = enclosing
                .iter()
                .chain(path.segments.iter())
                .cloned()
                .collect::<Vec<_>>();
            &owned_segments
        } else {
            &path.segments
        };

        // 1. The whole path as a type. A nested constant is skipped here so it
        //    falls through to the `Type::member` branch and resolves as a
        //    member of its parent — the Rust backend emits it as an associated
        //    const, so it has no importable free-item path of its own.
        if let Some(item_path) = self.find_item(scope, path)
            && !self.nested_constant_paths.contains(&item_path)
            && !self.extern_value_paths.contains(&item_path)
        {
            return Some(DocLinkTarget::Item(item_path));
        }
        // 2. `Type::member`.
        if path.len() >= 2 {
            let (prefix, member) = path.split_at(path.len() - 1);
            let member = member[0].as_str();
            if let Some(item_path) = self.find_item(scope, prefix)
                && let Some(kind) = self.find_member(&item_path, member)
            {
                return Some(DocLinkTarget::Member {
                    item: item_path,
                    name: member.to_string(),
                    kind,
                });
            }
        }
        // 3/4. A module-level freestanding function or extern value — the
        //      current module first, then any module in the crate (the backend
        //      imports it, like types).
        if path.len() == 1 {
            let name = path[0].as_str();
            if let Some(module) = self.find_in_modules(scope, &self.module_functions, name) {
                return Some(DocLinkTarget::Function {
                    module,
                    name: name.to_string(),
                });
            }
            if let Some(module) = self.find_in_modules(scope, &self.module_extern_values, name) {
                return Some(DocLinkTarget::ExternValue {
                    module,
                    name: name.to_string(),
                });
            }
        } else if path.len() >= 2 {
            // Qualified: resolve the module path, then check that module.
            let name = path[path.len() - 1].as_str();
            let module_segments = &path[..path.len() - 1];
            let bases = std::iter::once(ItemPath::empty()).chain(scope.iter().cloned());
            for base in bases {
                let mut full = base;
                for seg in module_segments {
                    full.push(seg.clone());
                }
                if let Some(fns) = self.module_functions.get(&full)
                    && fns.iter().any(|n| n == name)
                {
                    return Some(DocLinkTarget::Function {
                        module: full,
                        name: name.to_string(),
                    });
                }
                if let Some(evs) = self.module_extern_values.get(&full)
                    && evs.iter().any(|n| n == name)
                {
                    return Some(DocLinkTarget::ExternValue {
                        module: full,
                        name: name.to_string(),
                    });
                }
            }
        }
        None
    }

    /// The current module within a resolution scope: the first entry that
    /// actually is a module.
    ///
    /// A scope is not just module paths — resolving a doc block inside a type
    /// prepends the type's own path (so bare references to its nested items
    /// resolve), and the tail carries `use`-imported item paths. Assuming
    /// "first entry = current module" mis-anchors same-module preference at
    /// the type path for those blocks, silently changing which of several
    /// same-named crate-wide candidates wins.
    fn current_module<'s>(&self, scope: &'s [ItemPath]) -> Option<&'s ItemPath> {
        scope
            .iter()
            .find(|ip| self.module_functions.contains_key(ip))
    }

    /// Find the module that declares a member with `name` in `by_module`,
    /// preferring the current module.
    fn find_in_modules(
        &self,
        scope: &[ItemPath],
        by_module: &BTreeMap<ItemPath, Vec<String>>,
        name: &str,
    ) -> Option<ItemPath> {
        self.current_module(scope)
            .filter(|m| {
                by_module
                    .get(m)
                    .is_some_and(|ns| ns.iter().any(|n| n == name))
            })
            .cloned()
            .or_else(|| {
                by_module
                    .iter()
                    .find(|(_, ns)| ns.iter().any(|n| n == name))
                    .map(|(m, _)| m.clone())
            })
    }

    /// Find the absolute path of a named type. A bare name is resolved
    /// crate-wide (any accessible type with that name) since the Rust backend
    /// imports doc-referenced types regardless of the current `use`s; a
    /// `::`-qualified name is resolved root- or scope-relative.
    fn find_item(&self, scope: &[ItemPath], segments: &[ItemPathSegment]) -> Option<ItemPath> {
        let from_module = self.current_module(scope);

        if segments.len() == 1 {
            // 1. A type directly imported into scope wins.
            let name = segments[0].as_str();
            if let Some(p) = scope.iter().rev().find(|ip| {
                self.items.contains_key(ip) && ip.last().map(|s| s.as_str()) == Some(name)
            }) {
                return Some(p.clone());
            }
            // 2. Otherwise any accessible type with that name, preferring the
            //    current module, then a stable alphabetical order.
            let mut candidates: Vec<&ItemPath> = self
                .items
                .keys()
                .filter(|ip| {
                    ip.last().map(|s| s.as_str()) == Some(name) && self.can_access(from_module, ip)
                })
                .collect();
            candidates.sort_by_key(|ip| {
                let same_module = from_module == ip.parent().as_ref();
                (!same_module, ip.to_string())
            });
            return candidates.first().map(|ip| (*ip).clone());
        }

        // Qualified path: try it root-relative or relative to a scope module.
        let bases = std::iter::once(ItemPath::empty()).chain(scope.iter().cloned());
        for base in bases {
            let mut full = base.clone();
            for seg in segments {
                full.push(seg.clone());
            }
            if self.items.contains_key(&full) && self.can_access(from_module, &full) {
                return Some(full);
            }
        }
        None
    }

    fn can_access(&self, from_module: Option<&ItemPath>, item_path: &ItemPath) -> bool {
        let Some(info) = self.items.get(item_path) else {
            return false;
        };
        if info.visibility == Visibility::Public {
            return true;
        }
        let Some(from) = from_module else {
            return true;
        };
        match item_path.parent() {
            Some(item_module) => from == &item_module || from.starts_with(&item_module),
            None => true,
        }
    }

    fn find_member(&self, item_path: &ItemPath, member: &str) -> Option<DocLinkMemberKind> {
        match &self.items.get(item_path)?.members {
            ItemMembers::Type {
                methods,
                vftable_methods,
                fields,
                constants,
                extern_values,
            } => {
                if methods.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::Method)
                } else if vftable_methods.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::VftableMethod)
                } else if fields.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::Field)
                } else if constants.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::Constant)
                } else if extern_values.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::ExternValue)
                } else {
                    None
                }
            }
            ItemMembers::Enum {
                variants,
                methods,
                constants,
                extern_values,
            } => {
                if variants.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::Variant)
                } else if methods.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::Method)
                } else if constants.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::Constant)
                } else if extern_values.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::ExternValue)
                } else {
                    None
                }
            }
            ItemMembers::Bitflags {
                flags,
                constants,
                extern_values,
            } => {
                if flags.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::Flag)
                } else if constants.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::Constant)
                } else if extern_values.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::ExternValue)
                } else {
                    None
                }
            }
            ItemMembers::Other => None,
        }
    }
}

/// Resolve every intra-doc link in every doc comment across the crate, in one
/// pass. Returns the per-module location-keyed link tables that backends
/// consume, or the first link that fails to resolve as an error.
///
/// This is the single point where doc links are resolved — validation is this
/// pass's failure path, and every downstream consumer (backends, LSP hover on
/// build results) reads the returned tables rather than re-resolving with
/// locally-reconstructed context.
pub fn resolve_all(
    resolver: &DocLinkResolver,
    type_registry: &TypeRegistry,
    modules: &BTreeMap<ItemPath, Module>,
) -> Result<DocLinks> {
    let mut links: DocLinks = modules
        .keys()
        .map(|k| (k.clone(), ModuleDocLinks::default()))
        .collect();

    // `key` identifies the doc block in the output table; `location` anchors
    // any resolution error (for the module's own doc block, its proxy
    // location — see [`DocBlockKey`]).
    let mut record = |module_path: &ItemPath,
                      doc: &[String],
                      scope: &[ItemPath],
                      enclosing: Option<&ItemPath>,
                      key: DocBlockKey,
                      location: &ItemLocation|
     -> Result<()> {
        for (text, path) in extract_links(doc) {
            let Some(target) = resolver.resolve(scope, &path, enclosing) else {
                return Err(SemanticError::DocLinkNotFound {
                    path: text,
                    location: *location,
                });
            };
            links
                .entry(module_path.clone())
                .or_default()
                .by_block
                .entry(key)
                .or_default()
                .push(ResolvedDocLink { text, target });
        }
        Ok(())
    };
    let scopes: BTreeMap<&ItemPath, Vec<ItemPath>> = modules
        .iter()
        .map(|(path, module)| (path, module.scope()))
        .collect();

    for (module_path, module) in modules {
        let scope = &scopes[module_path];
        record(
            module_path,
            module.doc(),
            scope,
            None,
            DocBlockKey::Module,
            module.location(),
        )?;
        for f in module.functions() {
            record(
                module_path,
                &f.doc,
                scope,
                None,
                DocBlockKey::Node(f.location),
                &f.location,
            )?;
        }
    }

    // Top-level items — those whose parent is a module. Items nested inside
    // another item are reached by `walk_item_docs`' recursion instead, with
    // the enclosing type's augmented scope.
    for (path, item) in type_registry.iter() {
        let Some(parent) = path.parent() else {
            continue;
        };
        let Some(scope) = scopes.get(&parent) else {
            continue;
        };
        walk_item_docs(
            type_registry,
            &parent,
            path,
            item,
            scope,
            None,
            &mut |module_path, doc, scope, enclosing, location| {
                record(
                    module_path,
                    doc,
                    scope,
                    enclosing,
                    DocBlockKey::Node(*location),
                    location,
                )
            },
        )?;
    }

    Ok(links)
}

/// Walk the doc comments of one item — its own doc, its members' docs, and
/// (recursively) any nested items' — calling `record` with the scope and
/// `Self`-enclosing context each doc block resolves under.
///
/// `enclosing_type` for a doc block is the item whose *emitted* docs will
/// contain it: the type/enum/bitflags itself for its own, its members', and
/// its associated functions' docs, and the parent type for a nested
/// constant/extern value (the Rust backend emits those inside the parent's
/// `impl` block, where rustdoc resolves `Self` as the parent). Type aliases
/// and module-level constants/extern values get `None` — `Self` has nothing
/// to refer to in their emitted docs.
///
/// `parent_type` is the type this item is nested inside, `None` at module
/// level.
fn walk_item_docs<F>(
    type_registry: &TypeRegistry,
    module_path: &ItemPath,
    path: &ItemPath,
    item: &crate::semantic::types::ItemDefinition,
    scope: &[ItemPath],
    parent_type: Option<&ItemPath>,
    record: &mut F,
) -> Result<()>
where
    F: FnMut(&ItemPath, &[String], &[ItemPath], Option<&ItemPath>, &ItemLocation) -> Result<()>,
{
    let Some(resolved) = item.resolved() else {
        return Ok(());
    };
    let enclosing = Some(path);
    match &resolved.inner {
        ItemDefinitionInner::Type(td) => {
            // Augment scope with the type's own path so bare references to
            // nested items (e.g. [InnerEnum]) resolve.
            let type_scope: Vec<ItemPath> = std::iter::once(path.clone())
                .chain(scope.iter().cloned())
                .collect();
            record(module_path, &td.doc, &type_scope, enclosing, &item.location)?;
            for r in &td.regions {
                record(module_path, &r.doc, &type_scope, enclosing, &r.location)?;
            }
            for f in &td.associated_functions {
                record(module_path, &f.doc, &type_scope, enclosing, &f.location)?;
            }
            if let Some(v) = &td.vftable {
                for f in &v.functions {
                    record(module_path, &f.doc, &type_scope, enclosing, &f.location)?;
                }
            }
            for nested_path in &td.nested_item_paths {
                let Ok(nested_item) = type_registry.get(nested_path, &ItemLocation::internal())
                else {
                    continue;
                };
                walk_item_docs(
                    type_registry,
                    module_path,
                    nested_path,
                    nested_item,
                    &type_scope,
                    Some(path),
                    record,
                )?;
            }
        }
        ItemDefinitionInner::Enum(ed) => {
            record(module_path, &ed.doc, scope, enclosing, &item.location)?;
            for v in &ed.variants {
                record(module_path, &v.doc, scope, enclosing, &v.location)?;
            }
            for f in &ed.associated_functions {
                record(module_path, &f.doc, scope, enclosing, &f.location)?;
            }
        }
        ItemDefinitionInner::Bitflags(bd) => {
            record(module_path, &bd.doc, scope, enclosing, &item.location)?;
            for f in &bd.flags {
                record(module_path, &f.doc, scope, enclosing, &f.location)?;
            }
        }
        ItemDefinitionInner::TypeAlias(ta) => {
            record(module_path, &ta.doc, scope, None, &item.location)?;
        }
        ItemDefinitionInner::Constant(cd) => {
            record(module_path, &cd.doc, scope, parent_type, &item.location)?;
        }
        ItemDefinitionInner::ExternValue(ev) => {
            record(module_path, &ev.doc, scope, parent_type, &item.location)?;
        }
    }
    Ok(())
}
