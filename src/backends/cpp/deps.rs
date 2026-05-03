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
    backends::cpp::extern_bindings::CppExternBinding,
    grammar::ItemPath,
    semantic::{
        Module, TypeRegistry,
        types::{
            Argument, BitflagsDefinition, EnumDefinition, ExternValue, Function, ItemCategory,
            ItemDefinitionInner, Region, Type, TypeAliasDefinition, TypeDefinition,
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
    /// External `#include` directives (e.g. `<atomic>`, `\"windows.h\"`)
    /// pulled in by referenced extern types via `#[cpp_header]`.
    pub include_headers: BTreeSet<String>,
}

/// Collect `ModuleDeps` for the given module by walking every item it owns.
pub fn collect_module_deps(
    module_path: &ItemPath,
    module: &Module,
    registry: &TypeRegistry,
    bindings: &BTreeMap<ItemPath, CppExternBinding>,
) -> ModuleDeps {
    let mut deps = ModuleDeps::default();

    for item in module.definitions(registry) {
        let Some(resolved) = item.resolved() else {
            continue;
        };
        match &resolved.inner {
            ItemDefinitionInner::Type(td) => {
                walk_type_def(td, &mut deps, module_path, registry, bindings)
            }
            ItemDefinitionInner::Enum(ed) => {
                walk_enum_def(ed, &mut deps, module_path, registry, bindings)
            }
            ItemDefinitionInner::Bitflags(bd) => {
                walk_bitflags_def(bd, &mut deps, module_path, registry, bindings)
            }
            ItemDefinitionInner::TypeAlias(ta) => {
                walk_type_alias_def(ta, &mut deps, module_path, registry, bindings)
            }
        }
    }

    for ev in &module.extern_values {
        walk_extern_value(ev, &mut deps, module_path, registry, bindings);
    }
    for func in module.functions() {
        walk_function(func, &mut deps, module_path, registry, bindings);
    }

    // Drop self-references so we don't try to include our own header.
    deps.include_modules.remove(module_path);
    deps.forward_decls.remove(module_path);

    deps
}

fn walk_extern_value(
    ev: &ExternValue,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
    bindings: &BTreeMap<ItemPath, CppExternBinding>,
) {
    // Pointer-typed externs only need a forward decl; by-value externs
    // (rare) need a full include. Use the conservative FullDef rule for
    // anything that isn't already pointer-shaped.
    let kind = match &ev.type_ {
        Type::ConstPointer(_) | Type::MutPointer(_) | Type::Function(..) => EdgeKind::FwdOnly,
        _ => EdgeKind::FullDef,
    };
    walk_type(&ev.type_, kind, deps, module_path, registry, bindings);
}

fn walk_function(
    func: &Function,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
    bindings: &BTreeMap<ItemPath, CppExternBinding>,
) {
    for arg in &func.arguments {
        if let Argument::Field { type_, .. } = arg {
            walk_type(
                type_,
                EdgeKind::FwdOnly,
                deps,
                module_path,
                registry,
                bindings,
            );
        }
    }
    if let Some(ret) = &func.return_type {
        walk_type(
            ret,
            EdgeKind::FwdOnly,
            deps,
            module_path,
            registry,
            bindings,
        );
    }
}

fn walk_type_def(
    td: &TypeDefinition,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
    bindings: &BTreeMap<ItemPath, CppExternBinding>,
) {
    for region in &td.regions {
        walk_region(region, deps, module_path, registry, bindings);
    }
    if let Some(vftable) = &td.vftable {
        for func in &vftable.functions {
            walk_function(func, deps, module_path, registry, bindings);
        }
    }
    for func in &td.associated_functions {
        walk_function(func, deps, module_path, registry, bindings);
    }
}

fn walk_region(
    region: &Region,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
    bindings: &BTreeMap<ItemPath, CppExternBinding>,
) {
    walk_type(
        &region.type_ref,
        EdgeKind::FullDef,
        deps,
        module_path,
        registry,
        bindings,
    );
}

fn walk_enum_def(
    ed: &EnumDefinition,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
    bindings: &BTreeMap<ItemPath, CppExternBinding>,
) {
    walk_type(
        &ed.type_,
        EdgeKind::FullDef,
        deps,
        module_path,
        registry,
        bindings,
    );
}

fn walk_bitflags_def(
    bd: &BitflagsDefinition,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
    bindings: &BTreeMap<ItemPath, CppExternBinding>,
) {
    walk_type(
        &bd.type_,
        EdgeKind::FullDef,
        deps,
        module_path,
        registry,
        bindings,
    );
}

fn walk_type_alias_def(
    ta: &TypeAliasDefinition,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
    bindings: &BTreeMap<ItemPath, CppExternBinding>,
) {
    walk_type(
        &ta.target,
        EdgeKind::FullDef,
        deps,
        module_path,
        registry,
        bindings,
    );
}

fn walk_type(
    ty: &Type,
    kind: EdgeKind,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
    bindings: &BTreeMap<ItemPath, CppExternBinding>,
) {
    match ty {
        Type::Unresolved(_) | Type::TypeParameter(_) => {}
        Type::Raw(path) => record_path(path, kind, deps, module_path, registry, bindings),
        Type::Generic(base, args) => {
            record_path(base, EdgeKind::FullDef, deps, module_path, registry, bindings);
            let arg_kind = if generic_is_pointer_only(base, registry) {
                EdgeKind::FwdOnly
            } else {
                EdgeKind::FullDef
            };
            for arg in args {
                walk_type(arg, arg_kind, deps, module_path, registry, bindings);
            }
        }
        Type::ConstPointer(inner) | Type::MutPointer(inner) => {
            walk_type(inner, EdgeKind::FwdOnly, deps, module_path, registry, bindings);
        }
        Type::Array(inner, _) => {
            walk_type(inner, kind, deps, module_path, registry, bindings);
        }
        Type::Function(_, args, ret) => {
            for (_, arg_ty) in args {
                walk_type(
                    arg_ty,
                    EdgeKind::FwdOnly,
                    deps,
                    module_path,
                    registry,
                    bindings,
                );
            }
            if let Some(ret) = ret {
                walk_type(
                    ret,
                    EdgeKind::FwdOnly,
                    deps,
                    module_path,
                    registry,
                    bindings,
                );
            }
        }
    }
}

/// Walk a generic definition's body and determine whether every reference
/// to a `TypeParameter` is reached only through a pointer/reference/function
/// boundary. If so, callers can pass forward-declared types as args.
pub fn generic_is_pointer_only(base: &ItemPath, registry: &TypeRegistry) -> bool {
    let Ok(item) = registry.get(base, &crate::span::ItemLocation::internal()) else {
        return false;
    };
    if !item.is_generic() {
        return false;
    }
    let Some(resolved) = item.resolved() else {
        return false;
    };
    match &resolved.inner {
        ItemDefinitionInner::Type(td) => {
            for region in &td.regions {
                if !type_param_only_reached_through_indirection(&region.type_ref) {
                    return false;
                }
            }
            true
        }
        ItemDefinitionInner::TypeAlias(ta) => {
            type_param_only_reached_through_indirection(&ta.target)
        }
        _ => false,
    }
}

fn type_param_only_reached_through_indirection(ty: &Type) -> bool {
    match ty {
        // Hitting a raw TypeParameter directly means it's used by-value —
        // not pointer-only.
        Type::TypeParameter(_) => false,
        // Pointers and functions are an "exit" — anything inside them is
        // safe regardless of what it contains.
        Type::ConstPointer(_) | Type::MutPointer(_) | Type::Function(..) => true,
        Type::Array(inner, _) => type_param_only_reached_through_indirection(inner),
        // A nested generic instantiation is only pointer-only-safe if its
        // own args are. For Phase 3 we conservatively require all immediate
        // type-arg slots to be indirected.
        Type::Generic(_, args) => args
            .iter()
            .all(type_param_only_reached_through_indirection),
        Type::Raw(_) | Type::Unresolved(_) => true,
    }
}

fn record_path(
    target: &ItemPath,
    kind: EdgeKind,
    deps: &mut ModuleDeps,
    module_path: &ItemPath,
    registry: &TypeRegistry,
    bindings: &BTreeMap<ItemPath, CppExternBinding>,
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
        // Extern types: pull in the binding's `#[cpp_header]` (if any) and
        // emit the corresponding `using` alias from this module's header.
        // The alias itself lives in the defining module's `.hpp`, so we
        // also include that module unless this is the defining module.
        if let Some(binding) = bindings.get(target) {
            if let Some(header) = &binding.header {
                deps.include_headers.insert(header.clone());
            }
        }
        let target_module = target.parent().unwrap_or_else(ItemPath::empty);
        if &target_module != module_path {
            deps.include_modules.insert(target_module);
        }
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

