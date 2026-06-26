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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DocLinkMemberKind {
    Method,
    VftableMethod,
    Field,
    Variant,
    Flag,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ItemMembers {
    Type {
        methods: Vec<String>,
        vftable_methods: Vec<String>,
        fields: Vec<String>,
    },
    Enum {
        variants: Vec<String>,
        methods: Vec<String>,
    },
    Bitflags {
        flags: Vec<String>,
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
    module_functions: BTreeMap<ItemPath, Vec<String>>,
    module_extern_values: BTreeMap<ItemPath, Vec<String>>,
}

impl DocLinkResolver {
    pub fn build(type_registry: &TypeRegistry, modules: &BTreeMap<ItemPath, Module>) -> Self {
        let mut items = BTreeMap::new();
        for (path, item) in type_registry.iter() {
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
                },
                Some(ItemDefinitionInner::Enum(ed)) => ItemMembers::Enum {
                    variants: ed.variants.iter().map(|v| v.name.clone()).collect(),
                    methods: ed
                        .associated_functions
                        .iter()
                        .map(|f| f.name.clone())
                        .collect(),
                },
                Some(ItemDefinitionInner::Bitflags(bd)) => ItemMembers::Bitflags {
                    flags: bd.flags.iter().map(|f| f.name.clone()).collect(),
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

        let module_extern_values = modules
            .iter()
            .map(|(path, module)| {
                (
                    path.clone(),
                    module
                        .extern_values
                        .iter()
                        .map(|e| e.name.clone())
                        .collect(),
                )
            })
            .collect();

        DocLinkResolver {
            items,
            module_functions,
            module_extern_values,
        }
    }

    /// Resolve a written link path (e.g. `Action`, `Type::method`) against a
    /// module scope. Returns `None` if it doesn't resolve to anything.
    pub fn resolve(&self, scope: &[ItemPath], path_str: &str) -> Option<DocLinkTarget> {
        // 1. The whole path as a type.
        if let Some(item_path) = self.find_item(scope, path_str) {
            return Some(DocLinkTarget::Item(item_path));
        }
        // 2. `Type::member`.
        if let Some((prefix, member)) = path_str.rsplit_once("::") {
            if let Some(item_path) = self.find_item(scope, prefix) {
                if let Some(kind) = self.find_member(&item_path, member) {
                    return Some(DocLinkTarget::Member {
                        item: item_path,
                        name: member.to_string(),
                        kind,
                    });
                }
            }
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
        let bases = std::iter::once(ItemPath::empty()).chain(
            scope
                .iter()
                .filter(|ip| !self.items.contains_key(ip))
                .cloned(),
        );
        for base in bases {
            let mut full = base;
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

    /// Collect the absolute paths of every item/function/extern referenced by
    /// an intra-doc link anywhere in `module_path`'s documentation (its own
    /// doc, its items + their members, its functions, and its extern values).
    /// Used by the Rust backend to import them so rustdoc resolves the links.
    pub fn module_imports(
        &self,
        type_registry: &TypeRegistry,
        modules: &BTreeMap<ItemPath, Module>,
        module_path: &ItemPath,
    ) -> Vec<ItemPath> {
        let Some(module) = modules.get(module_path) else {
            return Vec::new();
        };
        let scope = module.scope();
        let mut imports = BTreeSet::new();

        self.add_doc_imports(&scope, module.doc(), &mut imports);
        for f in module.functions() {
            self.add_doc_imports(&scope, &f.doc, &mut imports);
        }
        for ev in &module.extern_values {
            self.add_doc_imports(&scope, &ev.doc, &mut imports);
        }

        for (path, item) in type_registry.iter() {
            if path.parent().as_ref() != Some(module_path) {
                continue;
            }
            let Some(resolved) = item.resolved() else {
                continue;
            };
            match &resolved.inner {
                ItemDefinitionInner::Type(td) => {
                    self.add_doc_imports(&scope, &td.doc, &mut imports);
                    for r in &td.regions {
                        self.add_doc_imports(&scope, &r.doc, &mut imports);
                    }
                    for f in &td.associated_functions {
                        self.add_doc_imports(&scope, &f.doc, &mut imports);
                    }
                    if let Some(v) = &td.vftable {
                        for f in &v.functions {
                            self.add_doc_imports(&scope, &f.doc, &mut imports);
                        }
                    }
                }
                ItemDefinitionInner::Enum(ed) => {
                    self.add_doc_imports(&scope, &ed.doc, &mut imports);
                    for v in &ed.variants {
                        self.add_doc_imports(&scope, &v.doc, &mut imports);
                    }
                    for f in &ed.associated_functions {
                        self.add_doc_imports(&scope, &f.doc, &mut imports);
                    }
                }
                ItemDefinitionInner::Bitflags(bd) => {
                    self.add_doc_imports(&scope, &bd.doc, &mut imports);
                    for f in &bd.flags {
                        self.add_doc_imports(&scope, &f.doc, &mut imports);
                    }
                }
                ItemDefinitionInner::TypeAlias(ta) => {
                    self.add_doc_imports(&scope, &ta.doc, &mut imports);
                }
            }
        }

        imports.into_iter().collect()
    }

    fn add_doc_imports(
        &self,
        scope: &[ItemPath],
        doc: &[String],
        imports: &mut BTreeSet<ItemPath>,
    ) {
        for text in extract_links(doc) {
            if let Some(target) = self.resolve(scope, &text) {
                imports.insert(target.import_path());
            }
        }
    }

    fn find_member(&self, item_path: &ItemPath, member: &str) -> Option<DocLinkMemberKind> {
        match &self.items.get(item_path)?.members {
            ItemMembers::Type {
                methods,
                vftable_methods,
                fields,
            } => {
                if methods.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::Method)
                } else if vftable_methods.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::VftableMethod)
                } else if fields.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::Field)
                } else {
                    None
                }
            }
            ItemMembers::Enum { variants, methods } => {
                if variants.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::Variant)
                } else if methods.iter().any(|n| n == member) {
                    Some(DocLinkMemberKind::Method)
                } else {
                    None
                }
            }
            ItemMembers::Bitflags { flags } => flags
                .iter()
                .any(|n| n == member)
                .then_some(DocLinkMemberKind::Flag),
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
        for ev in &module.extern_values {
            check(&ev.doc, &scope, &ev.location)?;
        }
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
        }
    }

    Ok(())
}

/// Extract the intra-doc link path strings from doc comment lines. Handles the
/// shortcut form `` [`path`] `` and the inline form `[label](path)` (where the
/// target is a `::`-path, not a URL).
/// Extract rustdoc-style intra-doc link targets from a doc comment.
///
/// Two forms are recognised:
/// - Inline: `[label](target)` — parsed by pulldown-cmark as a `Link` tag
///   whose `dest_url` is the target.
/// - Shortcut: `[`Path`]` — without a reference definition, pulldown-cmark
///   emits `Text("[")` + `Code("Path")` + `Text("]")`. We detect this
///   pattern by looking at the events surrounding each `Code` node.
///
/// Using a real Markdown parser means brackets inside code spans
/// (`` `[first, last)` ``) or code blocks can't accidentally consume a
/// real link's closing `]`.
pub fn extract_links(doc: &[String]) -> Vec<String> {
    let text = doc.join("\n");
    let events: Vec<pulldown_cmark::Event> = pulldown_cmark::Parser::new(&text).collect();

    let mut links = Vec::new();
    let mut i = 0;
    while i < events.len() {
        match &events[i] {
            // Inline link: [label](target)
            pulldown_cmark::Event::Start(pulldown_cmark::Tag::Link { dest_url, .. }) => {
                let url = dest_url.as_ref();
                if !url.is_empty() {
                    if is_path(url) {
                        links.push(url.to_string());
                    }
                    // Skip to the matching End so the Code/text inside
                    // the link doesn't trigger the shortcut handler below.
                    while i < events.len()
                        && !matches!(
                            events[i],
                            pulldown_cmark::Event::End(pulldown_cmark::TagEnd::Link)
                        )
                    {
                        i += 1;
                    }
                } else {
                    // Reference link with empty URL: collect text content.
                    let mut content = String::new();
                    i += 1;
                    while i < events.len()
                        && !matches!(
                            events[i],
                            pulldown_cmark::Event::End(pulldown_cmark::TagEnd::Link)
                        )
                    {
                        match &events[i] {
                            pulldown_cmark::Event::Text(t) => content.push_str(t),
                            pulldown_cmark::Event::Code(c) => content.push_str(c),
                            _ => {}
                        }
                        i += 1;
                    }
                    if is_path(&content) {
                        links.push(content);
                    }
                }
            }

            // Shortcut link: [`code`] → Text("[") + Code + Text("]")
            pulldown_cmark::Event::Code(content) => {
                let prev_is_open = i > 0
                    && matches!(&events[i - 1], pulldown_cmark::Event::Text(t) if t.ends_with('['));
                let next_is_close = i + 1 < events.len()
                    && matches!(&events[i + 1], pulldown_cmark::Event::Text(t) if t.starts_with(']'));
                if prev_is_open && next_is_close && is_path(content) {
                    links.push(content.to_string());
                }
            }

            _ => {}
        }
        i += 1;
    }
    links
}

fn is_path(s: &str) -> bool {
    !s.is_empty()
        && s.split("::").all(|seg| {
            let mut chars = seg.chars();
            chars
                .next()
                .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
                && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
        })
}
