//! Dependency-graph construction for the C++ backend.
//!
//! Each item in a project references other items via either a **FullDef** edge
//! (by-value field, base, array element, FullDef-typed template arg — needs
//! `#include`) or a **FwdOnly** edge (pointer or function param/return — a
//! forward declaration is enough). The semantic resolver does not guarantee
//! that the FullDef graph is acyclic, so we will eventually run SCC analysis
//! ourselves and emit a clear diagnostic if a true value-cycle is found.
//!
//! Phase 1: classifier + per-module aggregation. SCC cycle detection is a
//! later-phase concern; the codegen-tests corpus has no cycles to trigger it.

use std::collections::{BTreeMap, BTreeSet};

use crate::{
    grammar::ItemPath,
    semantic::{
        Module, TypeRegistry,
        types::{
            BitflagsDefinition, EnumDefinition, ItemCategory, ItemDefinitionInner, Region, Type,
            TypeAliasDefinition, TypeDefinition,
        },
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EdgeKind {
    FullDef,
    FwdOnly,
}

/// All cross-module dependencies a module's items collectively pull in.
#[derive(Debug, Default)]
pub struct ModuleDeps {
    /// Modules whose `.hpp` must be `#include`d.
    pub include_modules: BTreeSet<ItemPath>,
    /// Items that only need a forward declaration, grouped by defining module.
    pub forward_decls: BTreeMap<ItemPath, BTreeSet<ItemPath>>,
}

/// Collect `ModuleDeps` for the given module by walking every item it owns.
pub fn collect_module_deps(
    module_path: &ItemPath,
    module: &Module,
    registry: &TypeRegistry,
) -> ModuleDeps {
    let mut deps = ModuleDeps::default();

    for item in module.definitions(registry) {
        let Some(resolved) = item.resolved() else {
            continue;
        };
        match &resolved.inner {
            ItemDefinitionInner::Type(td) => walk_type_def(td, &mut deps, module_path, registry),
            ItemDefinitionInner::Enum(ed) => walk_enum_def(ed, &mut deps, module_path, registry),
            ItemDefinitionInner::Bitflags(bd) => {
                walk_bitflags_def(bd, &mut deps, module_path, registry)
            }
            ItemDefinitionInner::TypeAlias(ta) => {
                walk_type_alias_def(ta, &mut deps, module_path, registry)
            }
        }
    }

    // Drop self-references so we don't try to include our own header.
    deps.include_modules.remove(module_path);
    deps.forward_decls.remove(module_path);

    deps
}

fn walk_type_def(
    td: &TypeDefinition,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) {
    for region in &td.regions {
        walk_region(region, deps, module_path, registry);
    }
}

fn walk_region(
    region: &Region,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) {
    walk_type(&region.type_ref, EdgeKind::FullDef, deps, module_path, registry);
}

fn walk_enum_def(
    ed: &EnumDefinition,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) {
    walk_type(&ed.type_, EdgeKind::FullDef, deps, module_path, registry);
}

fn walk_bitflags_def(
    bd: &BitflagsDefinition,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) {
    walk_type(&bd.type_, EdgeKind::FullDef, deps, module_path, registry);
}

fn walk_type_alias_def(
    ta: &TypeAliasDefinition,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) {
    // Aliases re-export their target; the target needs to be fully visible to
    // anyone substituting through the alias.
    walk_type(&ta.target, EdgeKind::FullDef, deps, module_path, registry);
}

fn walk_type(
    ty: &Type,
    kind: EdgeKind,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) {
    match ty {
        Type::Unresolved(_) | Type::TypeParameter(_) => {}
        Type::Raw(path) => record_path(path, kind, deps, module_path, registry),
        Type::Generic(base, args) => {
            // The template's body must be fully visible to instantiate it.
            record_path(base, EdgeKind::FullDef, deps, module_path, registry);
            // Phase 1: assume args are FullDef. Phase 3 introduces
            // #[cpp(template_args_pointer_only)] to flip these to FwdOnly.
            for arg in args {
                walk_type(arg, EdgeKind::FullDef, deps, module_path, registry);
            }
        }
        Type::ConstPointer(inner) | Type::MutPointer(inner) => {
            walk_type(inner, EdgeKind::FwdOnly, deps, module_path, registry);
        }
        Type::Array(inner, _) => {
            walk_type(inner, kind, deps, module_path, registry);
        }
        Type::Function(_, args, ret) => {
            for (_, arg_ty) in args {
                walk_type(arg_ty, EdgeKind::FwdOnly, deps, module_path, registry);
            }
            if let Some(ret) = ret {
                walk_type(ret, EdgeKind::FwdOnly, deps, module_path, registry);
            }
        }
    }
}

fn record_path(
    target: &ItemPath,
    kind: EdgeKind,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) {
    // Predefined items (u32, f32, bool, AtomicI32, ...) are handled by
    // `<cstdint>` / hand-rolled aliases in the runtime header; nothing to
    // include or forward-declare per-module.
    let Ok(item) = registry.get(target, &crate::span::ItemLocation::internal()) else {
        return;
    };
    if item.is_predefined() {
        return;
    }
    if matches!(item.category, ItemCategory::Extern) {
        // Extern types are resolved through #[cpp_header]/#[cpp_name] in
        // Phase 3; for now skip them so we don't try to include a module
        // that doesn't exist.
        return;
    }
    let target_module = target.parent().unwrap_or_else(ItemPath::empty);
    if &target_module == module_path {
        return;
    }
    match kind {
        EdgeKind::FullDef => {
            deps.include_modules.insert(target_module);
        }
        EdgeKind::FwdOnly => {
            // If we already need a full include of the module, no point
            // recording a forward decl too — the include subsumes it.
            if !deps.include_modules.contains(&target_module) {
                deps.forward_decls
                    .entry(target_module)
                    .or_default()
                    .insert(target.clone());
            }
        }
    }
}

