use std::collections::{BTreeMap, BTreeSet};

use crate::{
    grammar::{self, ItemPath},
    semantic::{
        error::{Result, SemanticError, TypeResolutionContext},
        function::{self, Function},
        type_registry::{TypeLookupResult, TypeRegistry},
        types::{Backend, ExternValue, ItemDefinition, Type},
    },
    span::{HasLocation, ItemLocation},
};

/// Backend type bindings declared on an `extern type` — which concrete
/// C++/Rust type the opaque extern actually maps to.
#[derive(Debug, Clone, Copy, Default)]
pub struct ExternBindings<'a> {
    pub cpp_name: Option<&'a str>,
    pub cpp_header: Option<&'a str>,
    pub rust_name: Option<&'a str>,
}

/// Value of a `name = "..."` assign-form attribute, if present.
fn extern_assign<'a>(attributes: &'a grammar::Attributes, key: &str) -> Option<&'a str> {
    attributes.iter().find_map(|attr| {
        let (k, items) = attr.assign()?;
        if k.as_str() != key {
            return None;
        }
        items.exprs().next()?.string_literal()
    })
}

#[derive(Debug, Clone)]
pub struct Module {
    pub(crate) path: ItemPath,
    pub(crate) ast: grammar::Module,
    pub(crate) definition_paths: BTreeSet<ItemPath>,
    pub(crate) extern_values: Vec<ExternValue>,
    pub(crate) functions: Vec<Function>,
    pub(crate) impls: BTreeMap<ItemPath, Vec<grammar::FunctionBlock>>,
    pub(crate) backends: BTreeMap<crate::Backend, Vec<Backend>>,
    pub(crate) doc: Vec<String>,
    /// Where the module is declared, derived from its first item. Internal for
    /// synthesized/folder modules that have no source of their own.
    pub(crate) location: ItemLocation,
}

impl Default for Module {
    fn default() -> Self {
        Self {
            path: ItemPath::empty(),
            ast: Default::default(),
            definition_paths: Default::default(),
            extern_values: Default::default(),
            functions: Default::default(),
            impls: Default::default(),
            backends: Default::default(),
            doc: Default::default(),
            location: ItemLocation::internal(),
        }
    }
}

impl Module {
    /// A module with no items of its own, used for folders that contain
    /// `.pyxis` files but have no `mod.pyxis` to attach glue/items to.
    /// Backends still emit wiring (e.g. child `mod`/`use` declarations)
    /// for these synthesized modules.
    pub(crate) fn empty(path: ItemPath) -> Self {
        Self {
            path,
            ..Default::default()
        }
    }

    pub(crate) fn new(
        path: ItemPath,
        ast: grammar::Module,
        extern_values: Vec<ExternValue>,
        impls: &[grammar::FunctionBlock],
        grammar_backends: &[grammar::Backend],
    ) -> Result<Self> {
        let mut impls_map: BTreeMap<ItemPath, Vec<grammar::FunctionBlock>> = BTreeMap::new();
        for fb in impls {
            impls_map
                .entry(path.join(fb.name.as_str().into()))
                .or_default()
                .push(fb.clone());
        }
        let impls = impls_map;

        let mut backends: BTreeMap<crate::Backend, Vec<Backend>> = BTreeMap::new();
        for backend in grammar_backends {
            // Flatten each `use` tree on the backend block into absolute
            // item paths for the semantic representation. Validation
            // (existence + visibility) happens later during semantic
            // checking, alongside module-level use validation.
            let uses: Vec<ItemPath> = backend
                .uses
                .iter()
                .flat_map(|tree| tree.flatten())
                .collect();
            backends.entry(backend.name).or_default().push(Backend {
                prologue: crate::semantic::types::BackendSplice {
                    header: backend.prologue.header.clone(),
                    definition: backend.prologue.definition.clone(),
                },
                epilogue: crate::semantic::types::BackendSplice {
                    header: backend.epilogue.header.clone(),
                    definition: backend.epilogue.definition.clone(),
                },
                uses,
                location: backend.location,
            });
        }

        let doc = ast.doc_comments.clone();
        // Use the first item's location as a proxy for the module's own source
        // location (the top of its `.pyxis` file).
        let location = ast
            .items
            .first()
            .map(|item| *item.location())
            .unwrap_or_else(ItemLocation::internal);
        Ok(Self {
            path,
            ast,
            definition_paths: BTreeSet::new(),
            extern_values,
            functions: vec![],
            impls,
            backends,
            doc,
            location,
        })
    }

    pub fn uses(&self) -> impl Iterator<Item = &grammar::ModuleItem> {
        self.ast.uses()
    }

    /// Whether this module opted out of being glob-re-exported by its parent
    /// via `#![rust(no_reexport)]`. Honoured by the Rust backend when
    /// `rust_reexport_children` is enabled, so a module can be declared
    /// (`pub mod foo;`) without flattening its items into the parent.
    pub fn rust_no_reexport(&self) -> bool {
        self.ast.inner_attributes().any(|attr| {
            let Some((name, items)) = attr.function() else {
                return false;
            };
            name.as_str() == "rust"
                && items.exprs().any(|expr| {
                    matches!(expr, grammar::Expr::Ident { ident, .. } if ident.as_str() == "no_reexport")
                })
        })
    }

    /// Each `extern type`'s `#[rust_name = "..."]` binding (extern name ->
    /// fully-qualified Rust path), the Rust counterpart to `#[cpp_name]`.
    /// The Rust backend emits a `pub use <path> as <name>;` alias for these,
    /// so an extern type backed by a real Rust type doesn't need a
    /// hand-written `backend rust prologue "use ...;"`.
    pub fn extern_rust_names(&self) -> impl Iterator<Item = (&str, &str)> {
        self.extern_bindings()
            .filter_map(|(name, b)| b.rust_name.map(|r| (name, r)))
    }

    /// Per-`extern type` backend bindings (`#[cpp_name]` / `#[cpp_header]` /
    /// `#[rust_name]`) — which concrete C++/Rust type the opaque extern maps
    /// to. Consumed by the Rust backend (rust_name) and surfaced in the JSON.
    pub fn extern_bindings(&self) -> impl Iterator<Item = (&str, ExternBindings<'_>)> {
        self.ast.extern_types().filter_map(|item| {
            let grammar::ModuleItem::ExternType {
                name, attributes, ..
            } = item
            else {
                return None;
            };
            Some((
                name.as_str(),
                ExternBindings {
                    cpp_name: extern_assign(attributes, "cpp_name"),
                    cpp_header: extern_assign(attributes, "cpp_header"),
                    rust_name: extern_assign(attributes, "rust_name"),
                },
            ))
        })
    }

    pub fn definition_paths(&self) -> &BTreeSet<ItemPath> {
        &self.definition_paths
    }

    pub fn definitions<'a>(
        &'a self,
        type_registry: &'a TypeRegistry,
    ) -> impl Iterator<Item = &'a ItemDefinition> {
        self.definition_paths()
            .iter()
            .filter_map(|p| type_registry.get(p, &ItemLocation::internal()).ok())
    }

    pub fn scope(&self) -> Vec<ItemPath> {
        std::iter::once(self.path.clone())
            .chain(self.uses().flat_map(|u| {
                if let grammar::ModuleItem::Use { tree, .. } = u {
                    tree.flatten()
                } else {
                    vec![]
                }
            }))
            .collect()
    }

    pub(crate) fn resolve_extern_values(&mut self, type_registry: &mut TypeRegistry) -> Result<()> {
        let scope = self.scope();

        for ev in &mut self.extern_values {
            if let Type::Unresolved(type_ref) = &ev.type_ {
                ev.type_ = match type_registry.resolve_grammar_type(&scope, type_ref, &[]) {
                    TypeLookupResult::Found(t) => t,
                    TypeLookupResult::NotYetResolved
                    | TypeLookupResult::NotFound { .. }
                    | TypeLookupResult::PrivateAccess { .. } => {
                        return Err(SemanticError::TypeResolutionFailed {
                            type_: type_ref.clone(),
                            resolution_context: TypeResolutionContext::ExternValue {
                                extern_name: ev.name.clone(),
                            },
                            location: *type_ref.location(),
                        });
                    }
                };
            }
        }

        Ok(())
    }

    pub(crate) fn resolve_functions(&mut self, type_registry: &TypeRegistry) -> Result<()> {
        let scope = self.scope();

        for function in self.ast.functions().collect::<Vec<_>>() {
            let resolved_function = match function::build(
                type_registry,
                &scope,
                false, // is_vfunc
                function,
                &[],
            )? {
                function::FunctionBuildOutcome::Built(f) => *f,
                function::FunctionBuildOutcome::Deferred => {
                    // This shouldn't happen since freestanding functions are resolved
                    // after all types are resolved, but handle it gracefully
                    continue;
                }
            };
            self.functions.push(resolved_function);
        }

        Ok(())
    }

    pub fn doc(&self) -> &[String] {
        &self.doc
    }

    pub fn location(&self) -> &ItemLocation {
        &self.location
    }

    pub fn functions(&self) -> &[Function] {
        &self.functions
    }
}
