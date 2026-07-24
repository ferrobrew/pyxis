//! Item → C++ text rendering for the C++ backend.
//!
//! Owns the conversion of resolved IR items (structs, enums, bitflags, type
//! aliases, vftables, generics, extern bindings) into the textual output for
//! `.hpp` and `.cpp` files.

use std::collections::{BTreeMap, BTreeSet};

use crate::{
    backends::{Result, cpp::extern_bindings::CppExternBinding},
    grammar::ItemPath,
    semantic::{
        TypeRegistry,
        types::{ItemDefinition, ItemDefinitionInner},
    },
};

mod idents;
mod items;
mod structs;
mod types;

pub use idents::{cpp_ident, cpp_namespace_ident};
pub use items::{render_free_function_decl, render_free_function_definition};
pub use types::render_type;

/// Bundle of state every render call needs: the module being rendered (for
/// "same-module → bare name vs cross-module → fully qualified" decisions),
/// the resolved type registry, the project's extern-binding table, and the
/// active backend's cfg context (for filtering items that have `#[cfg(...)]`
/// predicates).
#[derive(Copy, Clone)]
pub struct RenderCtx<'a> {
    pub module_path: &'a ItemPath,
    pub registry: &'a TypeRegistry,
    pub bindings: &'a BTreeMap<ItemPath, CppExternBinding>,
    pub cfg_ctx: crate::parser::cfg::CfgContext,
    /// Resolved intra-doc links of the module being rendered, for rewriting
    /// doc-comment links into doxygen `@ref`s.
    pub doc_links: &'a crate::semantic::doc_links::ModuleDocLinks,
    /// Member names of the class currently being rendered, if any. A
    /// same-module type reference whose leaf is in this set collides with a
    /// member and must be emitted qualified (see [`render_path`]). `None`
    /// outside a class body.
    pub shadowed_members: Option<&'a BTreeSet<String>>,
}

impl<'a> RenderCtx<'a> {
    pub fn new(
        module_path: &'a ItemPath,
        registry: &'a TypeRegistry,
        bindings: &'a BTreeMap<ItemPath, CppExternBinding>,
        cfg_ctx: crate::parser::cfg::CfgContext,
        doc_links: &'a crate::semantic::doc_links::ModuleDocLinks,
    ) -> Self {
        Self {
            module_path,
            registry,
            bindings,
            cfg_ctx,
            doc_links,
            shadowed_members: None,
        }
    }

    /// Derive a context whose type references are aware of the enclosing
    /// class's member names, so leaves that collide with a member get
    /// qualified.
    fn with_shadowed_members(self, members: &'a BTreeSet<String>) -> Self {
        Self {
            shadowed_members: Some(members),
            ..self
        }
    }

    /// Returns true if the function should be emitted under the current
    /// cfg context (or has no cfg).
    pub fn cfg_passes(&self, cfg: &Option<crate::parser::cfg::CfgPredicate>) -> bool {
        match cfg {
            Some(p) => p.evaluate(&self.cfg_ctx),
            None => true,
        }
    }
}

/// Two-phase output for an item: the struct/enum body that goes inside the
/// namespace at first pass, plus any out-of-class inline method definitions
/// that have to come after every peer type is fully declared.
#[derive(Default)]
pub struct RenderedItem {
    /// In-class declarations + the struct/enum/etc. body itself. Lands
    /// in the `.hpp`.
    pub decl: String,
    /// Out-of-class definitions that must stay header-visible (template
    /// methods, accessors on generic templates, etc.). Lands in the
    /// `.hpp` after `decl`.
    pub post_header: String,
    /// Out-of-class definitions for non-template methods/accessors.
    /// Lands in the `.cpp` so the header sticks to declarations.
    pub post_cpp: String,
}

/// Render a single item as a C++ definition. Returns `None` if the item
/// doesn't produce direct output in this phase (predefined, or an extern
/// type — extern bindings are inlined at use sites via `render_path`).
pub fn render_item(item: &ItemDefinition, ctx: RenderCtx) -> Result<Option<RenderedItem>> {
    if item.is_predefined() {
        return Ok(None);
    }
    if matches!(item.category, crate::semantic::types::ItemCategory::Extern) {
        // Externs with a cpp_name binding are substituted at every use site
        // via `render_path`; we don't emit a `using` alias because the
        // pyxis leaf name can contain generic syntax (`Foo<Bar<u32>>`),
        // which is invalid C++ on the LHS of `using`.
        return Ok(None);
    }
    let resolved = match item.resolved() {
        Some(r) => r,
        None => return Ok(None),
    };

    let name = item
        .path
        .last()
        .map(|s| s.as_str().to_string())
        .unwrap_or_else(|| "Unnamed".to_string());

    let rendered = match &resolved.inner {
        ItemDefinitionInner::Type(td) => structs::render_struct(
            &name,
            td,
            resolved.size,
            resolved.alignment,
            ctx,
            item.visibility,
            &item.type_parameters,
            &item.location,
        )?,
        ItemDefinitionInner::Enum(ed) => {
            let (mut decl, mut post_cpp) =
                items::render_enum(&name, ed, resolved.size, ctx, &item.location)?;
            // Enums have no struct body for static members, so nested value items
            // are flattened to module scope: `constexpr` consts in the header,
            // extern-value getters declared in the header and defined in the `.cpp`.
            items::render_nested_values_cpp_flat(&mut decl, &mut post_cpp, ctx, &item.path, &name)?;
            RenderedItem {
                decl,
                post_header: String::new(),
                post_cpp,
            }
        }
        ItemDefinitionInner::Bitflags(bd) => {
            let (mut decl, mut post_cpp) =
                items::render_bitflags(&name, bd, resolved.size, ctx, &item.location)?;
            items::render_nested_values_cpp_flat(&mut decl, &mut post_cpp, ctx, &item.path, &name)?;
            RenderedItem {
                decl,
                post_header: String::new(),
                post_cpp,
            }
        }
        ItemDefinitionInner::TypeAlias(ta) => RenderedItem {
            decl: items::render_type_alias(&name, ta, ctx, &item.type_parameters, &item.location)?,
            post_header: String::new(),
            post_cpp: String::new(),
        },
        ItemDefinitionInner::Constant(cd) => items::render_const(&name, cd, ctx, &item.location)?,
        ItemDefinitionInner::ExternValue(ev) => {
            let mut decl = String::new();
            types::render_doc(&mut decl, &ev.doc, 0, ctx, &item.location)?;
            decl.push_str(&items::render_extern_value_decl(&name, ev, ctx)?);
            let post_cpp = items::render_extern_value_definition(&name, ev, ctx)?;
            RenderedItem {
                decl,
                post_header: String::new(),
                post_cpp,
            }
        }
    };
    Ok(Some(rendered))
}

pub(super) fn template_clause(type_parameters: &[String]) -> String {
    if type_parameters.is_empty() {
        return String::new();
    }
    let params = type_parameters
        .iter()
        .map(|p| format!("class {p}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!("template <{params}>\n")
}
