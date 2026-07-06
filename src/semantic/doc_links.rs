//! Resolution of rustdoc-style intra-doc links embedded in doc comments, e.g.
//! `[`Type`]`, `[`Type::method`]`, `[`Enum::VARIANT`]`, and the inline form
//! `` [label](Type::method) ``.
//!
//! Resolution happens at the semantic layer (after every item and function is
//! resolved) so links are validated up-front; the resolver is then reused by
//! the backends to surface the resolved targets.

use std::collections::{BTreeMap, BTreeSet};

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

/// A resolved intra-doc link target.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DocLinkTarget {
    /// A type / enum / bitflags / type alias item.
    Item(ItemPath),
    /// A member of an item: an associated/vftable method, enum variant, or flag.
    Member {
        item: ItemPath,
        name: String,
        kind: DocLinkMemberKind,
    },
    /// A freestanding, module-level function.
    Function { module: ItemPath, name: String },
    /// A module-level extern value (global).
    ExternValue { module: ItemPath, name: String },
}

impl DocLinkTarget {
    /// The absolute path of the base item/function/extern to import so a Rust
    /// consumer (rustdoc) can resolve the link.
    pub fn import_path(&self) -> ItemPath {
        match self {
            DocLinkTarget::Item(path) => path.clone(),
            DocLinkTarget::Member { item, .. } => item.clone(),
            DocLinkTarget::Function { module, name }
            | DocLinkTarget::ExternValue { module, name } => {
                module.join(ItemPathSegment::from(name.clone()))
            }
        }
    }

    /// If this link points at an extern value, its full item path — `module::name`
    /// for a module-level one, `Parent::name` for a nested one. `None` for any
    /// other target.
    ///
    /// Extern values emit as `get_<name>` accessors rather than an item named
    /// `<name>`, so a backend can't just resolve the logical path; it needs the
    /// value's path to compute the accessor's path (see the Rust backend's doc
    /// link rewriting).
    pub fn extern_value_path(&self) -> Option<ItemPath> {
        match self {
            DocLinkTarget::ExternValue { module, name } => {
                Some(module.join(ItemPathSegment::from(name.clone())))
            }
            DocLinkTarget::Member {
                item,
                name,
                kind: DocLinkMemberKind::ExternValue,
            } => Some(item.join(ItemPathSegment::from(name.clone()))),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DocLinkMemberKind {
    Method,
    VftableMethod,
    Field,
    Variant,
    Flag,
    Constant,
    ExternValue,
}

/// The intra-doc links referenced across a module's documentation, gathered by
/// [`DocLinkResolver::module_doc_links`] for backend rewriting.
#[derive(Debug, Clone, Default)]
pub struct ModuleDocLinks {
    /// Absolute paths of every item/function/extern referenced by a link, to be
    /// imported so rustdoc resolves them.
    pub imports: BTreeSet<ItemPath>,
    /// `(written link text, extern-value item path)` for each link pointing at
    /// an extern value. The backend rewrites the link destination to the emitted
    /// `get_<name>` accessor rather than the value's logical name.
    pub extern_value_links: Vec<(String, ItemPath)>,
}

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
        for (module_path, _) in modules.iter() {
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
    pub fn resolve(&self, scope: &[ItemPath], path_str: &str) -> Option<DocLinkTarget> {
        // 1. The whole path as a type. A nested constant is skipped here so it
        //    falls through to the `Type::member` branch and resolves as a
        //    member of its parent — the Rust backend emits it as an associated
        //    const, so it has no importable free-item path of its own.
        if let Some(item_path) = self.find_item(scope, path_str)
            && !self.nested_constant_paths.contains(&item_path)
            && !self.extern_value_paths.contains(&item_path)
        {
            return Some(DocLinkTarget::Item(item_path));
        }
        // 2. `Type::member`.
        if let Some((prefix, member)) = path_str.rsplit_once("::")
            && let Some(item_path) = self.find_item(scope, prefix)
            && let Some(kind) = self.find_member(&item_path, member)
        {
            return Some(DocLinkTarget::Member {
                item: item_path,
                name: member.to_string(),
                kind,
            });
        }
        // 3/4. A module-level freestanding function or extern value — the
        //      current module first, then any module in the crate (the backend
        //      imports it, like types).
        if !path_str.contains("::") {
            if let Some(module) = self.find_in_modules(scope, &self.module_functions, path_str) {
                return Some(DocLinkTarget::Function {
                    module,
                    name: path_str.to_string(),
                });
            }
            if let Some(module) = self.find_in_modules(scope, &self.module_extern_values, path_str)
            {
                return Some(DocLinkTarget::ExternValue {
                    module,
                    name: path_str.to_string(),
                });
            }
        }
        None
    }

    /// Find the module that declares a member with `name` in `by_module`,
    /// preferring the current module (scope's first entry).
    fn find_in_modules(
        &self,
        scope: &[ItemPath],
        by_module: &BTreeMap<ItemPath, Vec<String>>,
        name: &str,
    ) -> Option<ItemPath> {
        scope
            .first()
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
    fn find_item(&self, scope: &[ItemPath], path_str: &str) -> Option<ItemPath> {
        let from_module = scope.first();

        if !path_str.contains("::") {
            // 1. A type directly imported into scope wins.
            if let Some(p) = scope.iter().rev().find(|ip| {
                self.items.contains_key(ip) && ip.last().map(|s| s.as_str()) == Some(path_str)
            }) {
                return Some(p.clone());
            }
            // 2. Otherwise any accessible type with that name, preferring the
            //    current module, then a stable alphabetical order.
            let mut candidates: Vec<&ItemPath> = self
                .items
                .keys()
                .filter(|ip| {
                    ip.last().map(|s| s.as_str()) == Some(path_str)
                        && self.can_access(from_module, ip)
                })
                .collect();
            candidates.sort_by_key(|ip| {
                let same_module = from_module == ip.parent().as_ref();
                (!same_module, ip.to_string())
            });
            return candidates.first().map(|ip| (*ip).clone());
        }

        // Qualified path: try it root-relative or relative to a scope module.
        let segments: Vec<ItemPathSegment> =
            path_str.split("::").map(ItemPathSegment::from).collect();
        let bases = std::iter::once(ItemPath::empty()).chain(scope.iter().cloned());
        for base in bases {
            let mut full = base.clone();
            for seg in &segments {
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

    /// Collect every intra-doc link referenced anywhere in `module_path`'s
    /// documentation (its own doc, its items + their members, its functions) —
    /// the item paths to import so rustdoc resolves them, plus the extern-value
    /// links the Rust backend rewrites to `get_<name>` accessors. See
    /// [`ModuleDocLinks`].
    pub fn module_doc_links(
        &self,
        type_registry: &TypeRegistry,
        modules: &BTreeMap<ItemPath, Module>,
        module_path: &ItemPath,
    ) -> ModuleDocLinks {
        let Some(module) = modules.get(module_path) else {
            return ModuleDocLinks::default();
        };
        let scope = module.scope();
        let mut links = ModuleDocLinks::default();

        self.add_doc_imports(&scope, module.doc(), &mut links);
        for f in module.functions() {
            self.add_doc_imports(&scope, &f.doc, &mut links);
        }
        // Extern values (including module-level ones) are registry items, so
        // their docs are collected by the registry walk below.

        for (path, item) in type_registry.iter() {
            if path.parent().as_ref() != Some(module_path) {
                continue;
            }
            let Some(resolved) = item.resolved() else {
                continue;
            };
            match &resolved.inner {
                ItemDefinitionInner::Type(td) => {
                    // Augment scope with the type's own path so bare
                    // references to nested items (e.g. [InnerEnum]) resolve.
                    let type_scope: Vec<ItemPath> = std::iter::once(path.clone())
                        .chain(scope.iter().cloned())
                        .collect();
                    self.add_doc_imports(&type_scope, &td.doc, &mut links);
                    for r in &td.regions {
                        self.add_doc_imports(&type_scope, &r.doc, &mut links);
                    }
                    for f in &td.associated_functions {
                        self.add_doc_imports(&type_scope, &f.doc, &mut links);
                    }
                    if let Some(v) = &td.vftable {
                        for f in &v.functions {
                            self.add_doc_imports(&type_scope, &f.doc, &mut links);
                        }
                    }
                    // Also scan doc comments on nested items
                    for nested_path in &td.nested_item_paths {
                        if let Some(nested_item) = type_registry
                            .get(nested_path, &ItemLocation::internal())
                            .ok()
                            && let Some(nested_resolved) = nested_item.resolved()
                        {
                            match &nested_resolved.inner {
                                ItemDefinitionInner::Type(ntd) => {
                                    self.add_doc_imports(&type_scope, &ntd.doc, &mut links);
                                }
                                ItemDefinitionInner::Enum(ned) => {
                                    self.add_doc_imports(&type_scope, &ned.doc, &mut links);
                                    for v in &ned.variants {
                                        self.add_doc_imports(&type_scope, &v.doc, &mut links);
                                    }
                                }
                                ItemDefinitionInner::Bitflags(nbd) => {
                                    self.add_doc_imports(&type_scope, &nbd.doc, &mut links);
                                    for f in &nbd.flags {
                                        self.add_doc_imports(&type_scope, &f.doc, &mut links);
                                    }
                                }
                                ItemDefinitionInner::TypeAlias(nta) => {
                                    self.add_doc_imports(&type_scope, &nta.doc, &mut links);
                                }
                                ItemDefinitionInner::Constant(ncd) => {
                                    self.add_doc_imports(&type_scope, &ncd.doc, &mut links);
                                }
                                ItemDefinitionInner::ExternValue(nev) => {
                                    self.add_doc_imports(&type_scope, &nev.doc, &mut links);
                                }
                            }
                        }
                    }
                }
                ItemDefinitionInner::Enum(ed) => {
                    self.add_doc_imports(&scope, &ed.doc, &mut links);
                    for v in &ed.variants {
                        self.add_doc_imports(&scope, &v.doc, &mut links);
                    }
                    for f in &ed.associated_functions {
                        self.add_doc_imports(&scope, &f.doc, &mut links);
                    }
                }
                ItemDefinitionInner::Bitflags(bd) => {
                    self.add_doc_imports(&scope, &bd.doc, &mut links);
                    for f in &bd.flags {
                        self.add_doc_imports(&scope, &f.doc, &mut links);
                    }
                }
                ItemDefinitionInner::TypeAlias(ta) => {
                    self.add_doc_imports(&scope, &ta.doc, &mut links);
                }
                ItemDefinitionInner::Constant(cd) => {
                    self.add_doc_imports(&scope, &cd.doc, &mut links);
                }
                ItemDefinitionInner::ExternValue(ev) => {
                    self.add_doc_imports(&scope, &ev.doc, &mut links);
                }
            }
        }

        links
    }

    fn add_doc_imports(&self, scope: &[ItemPath], doc: &[String], links: &mut ModuleDocLinks) {
        for text in extract_links(doc) {
            if let Some(target) = self.resolve(scope, &text) {
                links.imports.insert(target.import_path());
                if let Some(value_path) = target.extern_value_path() {
                    links.extern_value_links.push((text, value_path));
                }
            }
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

/// Validate every doc comment's intra-doc links, erroring on the first that
/// doesn't resolve.
pub fn validate(
    resolver: &DocLinkResolver,
    type_registry: &TypeRegistry,
    modules: &BTreeMap<ItemPath, Module>,
) -> Result<()> {
    let check = |doc: &[String], scope: &[ItemPath], location: &ItemLocation| -> Result<()> {
        for link in extract_links(doc) {
            if resolver.resolve(scope, &link).is_none() {
                return Err(SemanticError::DocLinkNotFound {
                    path: link,
                    location: *location,
                });
            }
        }
        Ok(())
    };

    for (path, module) in modules {
        let scope = module.scope();
        check(module.doc(), &scope, module.location())?;
        for f in module.functions() {
            check(&f.doc, &scope, &f.location)?;
        }
        // Extern values are registry items; their docs are checked in the
        // registry walk below.
        let _ = path;
    }

    for (path, item) in type_registry.iter() {
        let Some(resolved) = item.resolved() else {
            continue;
        };
        let module_path = path.parent().unwrap_or_else(ItemPath::empty);
        let Some(module) = modules.get(&module_path) else {
            continue;
        };
        let scope = module.scope();
        let loc = &item.location;
        match &resolved.inner {
            ItemDefinitionInner::Type(td) => {
                check(&td.doc, &scope, loc)?;
                for r in &td.regions {
                    check(&r.doc, &scope, &r.location)?;
                }
                for f in &td.associated_functions {
                    check(&f.doc, &scope, &f.location)?;
                }
                if let Some(v) = &td.vftable {
                    for f in &v.functions {
                        check(&f.doc, &scope, &f.location)?;
                    }
                }
            }
            ItemDefinitionInner::Enum(ed) => {
                check(&ed.doc, &scope, loc)?;
                for v in &ed.variants {
                    check(&v.doc, &scope, &v.location)?;
                }
                for f in &ed.associated_functions {
                    check(&f.doc, &scope, &f.location)?;
                }
            }
            ItemDefinitionInner::Bitflags(bd) => {
                check(&bd.doc, &scope, loc)?;
                for f in &bd.flags {
                    check(&f.doc, &scope, &f.location)?;
                }
            }
            ItemDefinitionInner::TypeAlias(ta) => {
                check(&ta.doc, &scope, loc)?;
            }
            ItemDefinitionInner::Constant(cd) => {
                check(&cd.doc, &scope, loc)?;
            }
            ItemDefinitionInner::ExternValue(ev) => {
                check(&ev.doc, &scope, loc)?;
            }
        }
    }

    Ok(())
}

/// Which Markdown syntax produced a scanned intra-doc link.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocLinkSyntax {
    /// An inline link `[label](path::To)`; the destination is the path.
    Inline,
    /// A code-span shortcut `` [`path::To`] ``; the code span is the path.
    CodeShortcut,
    /// A bare bracketed shortcut `[path::To]`; the bracket text is the path.
    /// The compiler ignores these (only backtick/inline forms are validated
    /// and imported), but the LSP surfaces them as navigable links.
    PlainShortcut,
}

/// A rustdoc-style intra-doc link located in a doc comment, with byte offsets
/// into the scanned text. This is the single source of truth for link syntax;
/// the compiler (validation/imports), the Rust backend (link rewriting), and
/// the LSP (navigation/rename) all consume it and filter by [`DocLinkSyntax`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScannedLink {
    pub syntax: DocLinkSyntax,
    /// The resolvable path text (backticks / surrounding whitespace trimmed).
    pub path: String,
    /// Byte range of the whole link (`[Foo]` / `` [`Foo`] `` / `[label](path)`).
    pub link: (usize, usize),
    /// Precise byte range of `path` in the source — the `(...)` destination for
    /// an inline link, or the bracket / code-span content for a shortcut. This
    /// is exactly the span a rename or a backend rewrite substitutes.
    pub path_region: (usize, usize),
    /// Byte range of the bracket label `[...]` interior. Distinguished from
    /// `path_region` so a rename rewrites an inline link's destination but only
    /// an *echoing* label — never arbitrary prose like `[the Foo struct]`.
    pub label_region: (usize, usize),
}

/// Scan every rustdoc-style intra-doc link in `text`, returning each with its
/// syntax and byte offsets.
///
/// Backed by pulldown-cmark so brackets inside code spans (`` `[first, last)` ``)
/// or code blocks can't accidentally consume a real link's closing `]`. Three
/// forms are recognised:
/// - Inline: `[label](target)` — a `Link` tag whose `dest_url` is the target.
/// - Code shortcut: `` [`Path`] `` — emitted as `Text("[")` + `Code` + `Text("]")`.
/// - Plain shortcut: `[Path]` — emitted as `Text("[")` + `Text` + `Text("]")`.
///
/// Only `::`-path targets (per [`is_path`]) are kept; prose and URLs are dropped.
pub fn scan_links(text: &str) -> Vec<ScannedLink> {
    use pulldown_cmark::{Event, Tag, TagEnd};
    let events: Vec<(Event, std::ops::Range<usize>)> = pulldown_cmark::Parser::new(text)
        .into_offset_iter()
        .collect();

    // Locate the precise span of `needle` within `text[range]`, falling back to
    // the whole range's start if (unexpectedly) not found.
    let span_of = |needle: &str, range: &std::ops::Range<usize>| -> (usize, usize) {
        let start = text[range.clone()]
            .find(needle)
            .map(|off| range.start + off)
            .unwrap_or(range.start);
        (start, start + needle.len())
    };

    let mut out = Vec::new();
    let mut i = 0;
    while i < events.len() {
        let range = events[i].1.clone();
        match &events[i].0 {
            // Inline link: [label](target).
            Event::Start(Tag::Link { dest_url, .. }) => {
                let dest = dest_url.as_ref();
                // The label ends at the `]` that precedes `(`.
                let bracket = text[range.clone()].find("](");
                if !dest.is_empty()
                    && is_path(dest)
                    && let Some(rb) = bracket
                {
                    let label_region = (range.start + 1, range.start + rb);
                    let dest_search = (range.start + rb + 2)..range.end;
                    out.push(ScannedLink {
                        syntax: DocLinkSyntax::Inline,
                        path: dest.to_string(),
                        link: (range.start, range.end),
                        path_region: span_of(dest, &dest_search),
                        label_region,
                    });
                } else if dest.is_empty() {
                    // Reference-style link with an empty URL (`[text][]`);
                    // collect its inner content as the path.
                    let mut content = String::new();
                    let mut inner = i + 1;
                    while inner < events.len()
                        && !matches!(events[inner].0, Event::End(TagEnd::Link))
                    {
                        match &events[inner].0 {
                            Event::Text(t) => content.push_str(t),
                            Event::Code(c) => content.push_str(c),
                            _ => {}
                        }
                        inner += 1;
                    }
                    if is_path(&content) {
                        let interior = (range.start + 1, range.end.saturating_sub(1));
                        out.push(ScannedLink {
                            syntax: DocLinkSyntax::CodeShortcut,
                            path: content,
                            link: (range.start, range.end),
                            path_region: interior,
                            label_region: interior,
                        });
                    }
                }
                // Skip the link's inner events so they don't re-trigger below.
                while i < events.len() && !matches!(events[i].0, Event::End(TagEnd::Link)) {
                    i += 1;
                }
            }

            // Code-span shortcut: [`code`] → Text("[") + Code + Text("]").
            Event::Code(content) => {
                let prev_open =
                    i > 0 && matches!(&events[i - 1].0, Event::Text(t) if t.ends_with('['));
                let next_close = i + 1 < events.len()
                    && matches!(&events[i + 1].0, Event::Text(t) if t.starts_with(']'));
                if prev_open && next_close && is_path(content) {
                    // `[` sits immediately before the code span, `]` immediately after.
                    let link = (range.start - 1, range.end + 1);
                    out.push(ScannedLink {
                        syntax: DocLinkSyntax::CodeShortcut,
                        path: content.to_string(),
                        link,
                        path_region: span_of(content, &range),
                        label_region: (link.0 + 1, link.1 - 1),
                    });
                }
            }

            // Plain shortcut: [code] → Text("[") + Text(code) + Text("]").
            Event::Text(t) if t.ends_with('[') => {
                if i + 2 < events.len()
                    && let Event::Text(content) = &events[i + 1].0
                    && let Event::Text(close) = &events[i + 2].0
                    && close.starts_with(']')
                    && is_path(content.trim())
                {
                    let content_range = events[i + 1].1.clone();
                    let path = content.trim();
                    let open_pos = range.end - 1; // position of `[`
                    let close_pos = events[i + 2].1.start; // position of `]`
                    out.push(ScannedLink {
                        syntax: DocLinkSyntax::PlainShortcut,
                        path: path.to_string(),
                        link: (open_pos, close_pos + 1),
                        path_region: span_of(path, &content_range),
                        label_region: (open_pos + 1, close_pos),
                    });
                }
            }

            _ => {}
        }
        i += 1;
    }
    out
}

/// Extract the intra-doc link path strings the compiler validates and imports:
/// the inline (`[label](path)`) and code-shortcut (`` [`path`] ``) forms. Bare
/// `[path]` shortcuts are intentionally excluded.
pub fn extract_links(doc: &[String]) -> Vec<String> {
    let text = doc.join("\n");
    scan_links(&text)
        .into_iter()
        .filter(|l| l.syntax != DocLinkSyntax::PlainShortcut)
        .map(|l| l.path)
        .collect()
}

/// Whether `s` is a valid intra-doc link target: a `::`-separated path of
/// identifier segments (e.g. `Type`, `module::Type::method`), as opposed to
/// arbitrary prose or a URL. Shared with editor tooling so the LSP's link
/// scanner recognises the same targets the compiler resolves.
pub fn is_path(s: &str) -> bool {
    !s.is_empty()
        && s.split("::").all(|seg| {
            let mut chars = seg.chars();
            chars
                .next()
                .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
                && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
        })
}
