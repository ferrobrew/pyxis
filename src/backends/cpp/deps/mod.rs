//! Dependency-graph construction for the C++ backend.
//!
//! Each item in a project references other items via either a **FullDef** edge
//! (by-value field, base, array element, FullDef-typed template arg — needs
//! `#include`) or a **FwdOnly** edge (pointer or function param/return — a
//! forward declaration is enough). The semantic resolver does not guarantee
//! that the FullDef graph is acyclic, so this module runs Tarjan's SCC
//! algorithm over both the intra-module item graph and the cross-module
//! aggregate graph and emits a `BackendError::CppLayoutCycle` if any real
//! value-cycle is found (one that no forward declaration can break).

use std::collections::BTreeMap;

use crate::{
    backends::cpp::extern_bindings::CppExternBinding,
    grammar::ItemPath,
    semantic::{
        Module, TypeRegistry,
        types::{
            Argument, BitflagsDefinition, ConstValue, EnumDefinition,
            ExternValueDefinition as SemanticExternValueDefinition, Function, ItemCategory,
            ItemDefinitionInner, Region, Type, TypeAliasDefinition, TypeDefinition,
        },
    },
};

mod graph;

pub use graph::{EdgeKind, ModuleDeps, first_scc_cycle, topo_sort_module_items};

pub(super) fn collect_intra_module_full_deps(
    inner: &ItemDefinitionInner,
    module_path: &ItemPath,
    item_paths: &std::collections::BTreeSet<ItemPath>,
    registry: &TypeRegistry,
    bindings: &BTreeMap<ItemPath, CppExternBinding>,
    out: &mut std::collections::BTreeSet<ItemPath>,
) {
    match inner {
        ItemDefinitionInner::Type(td) => {
            for region in &td.regions {
                walk_intra(
                    &region.type_ref,
                    EdgeKind::FullDef,
                    module_path,
                    item_paths,
                    registry,
                    bindings,
                    out,
                );
            }
        }
        // A union's members are laid out inside it, so they are full-definition
        // dependencies exactly as a struct's fields are.
        ItemDefinitionInner::Union(ud) => {
            for region in &ud.regions {
                walk_intra(
                    &region.type_ref,
                    EdgeKind::FullDef,
                    module_path,
                    item_paths,
                    registry,
                    bindings,
                    out,
                );
            }
        }
        ItemDefinitionInner::Enum(ed) => {
            walk_intra(
                &ed.type_,
                EdgeKind::FullDef,
                module_path,
                item_paths,
                registry,
                bindings,
                out,
            );
        }
        ItemDefinitionInner::Bitflags(bd) => {
            walk_intra(
                &bd.type_,
                EdgeKind::FullDef,
                module_path,
                item_paths,
                registry,
                bindings,
                out,
            );
        }
        ItemDefinitionInner::TypeAlias(ta) => {
            walk_intra(
                &ta.target,
                EdgeKind::FullDef,
                module_path,
                item_paths,
                registry,
                bindings,
                out,
            );
        }
        ItemDefinitionInner::Constant(cd) => {
            // Walk ConstValue::ConstRef paths so that const aliases are
            // ordered correctly in the C++ output (referenced const before
            // the alias).
            walk_const_value(&cd.value, module_path, item_paths, out);
        }
        ItemDefinitionInner::ExternValue(_) => {}
    }
}

/// Walk a `ConstValue` for intra-module dependencies. This ensures that
/// `ConstValue::ConstRef` references are tracked so the C++ backend emits
/// the referenced constant before the alias.
fn walk_const_value(
    value: &ConstValue,
    module_path: &ItemPath,
    item_paths: &std::collections::BTreeSet<ItemPath>,
    out: &mut std::collections::BTreeSet<ItemPath>,
) {
    match value {
        ConstValue::Struct { fields, .. } => {
            for (_, v) in fields {
                walk_const_value(v, module_path, item_paths, out);
            }
        }
        ConstValue::Array(elements) => {
            for e in elements {
                walk_const_value(e, module_path, item_paths, out);
            }
        }
        ConstValue::ConstRef(path) => {
            // Only track if the referenced const is in the same module.
            if let Some(parent) = path.parent() {
                if parent == *module_path || item_paths.contains(path) {
                    out.insert(path.clone());
                }
            }
        }
        // Literal values have no intra-module dependencies.
        ConstValue::Int(_)
        | ConstValue::Float(_)
        | ConstValue::String(_)
        | ConstValue::CString(_)
        | ConstValue::EnumValue(_) => {}
    }
}

#[allow(clippy::only_used_in_recursion)]
fn walk_intra(
    ty: &Type,
    kind: EdgeKind,
    module_path: &ItemPath,
    item_paths: &std::collections::BTreeSet<ItemPath>,
    registry: &TypeRegistry,
    bindings: &BTreeMap<ItemPath, CppExternBinding>,
    out: &mut std::collections::BTreeSet<ItemPath>,
) {
    match ty {
        Type::Unresolved(_) | Type::TypeParameter(_) => {}
        Type::Raw(path) => {
            if matches!(kind, EdgeKind::FullDef) && item_paths.contains(path) {
                out.insert(path.clone());
            }
        }
        Type::Generic(base, args) => {
            if item_paths.contains(base) {
                out.insert(base.clone());
            }
            let arg_kind = if generic_is_pointer_only(base, registry) {
                EdgeKind::FwdOnly
            } else {
                EdgeKind::FullDef
            };
            for arg in args {
                walk_intra(
                    arg,
                    arg_kind,
                    module_path,
                    item_paths,
                    registry,
                    bindings,
                    out,
                );
            }
        }
        Type::ConstPointer(inner) | Type::MutPointer(inner) => {
            walk_intra(
                inner,
                EdgeKind::FwdOnly,
                module_path,
                item_paths,
                registry,
                bindings,
                out,
            );
        }
        Type::Array(inner, _) => {
            walk_intra(
                inner,
                kind,
                module_path,
                item_paths,
                registry,
                bindings,
                out,
            );
        }
        Type::Function(_, args, ret) => {
            for arg in args {
                walk_intra(
                    &arg.type_,
                    EdgeKind::FwdOnly,
                    module_path,
                    item_paths,
                    registry,
                    bindings,
                    out,
                );
            }
            if let Some(t) = ret {
                walk_intra(
                    t,
                    EdgeKind::FwdOnly,
                    module_path,
                    item_paths,
                    registry,
                    bindings,
                    out,
                );
            }
        }
    }
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
            ItemDefinitionInner::Union(ud) => {
                for region in &ud.regions {
                    walk_region(region, &mut deps, module_path, registry, bindings);
                }
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
            ItemDefinitionInner::Constant(_) => {}
            ItemDefinitionInner::ExternValue(ev) => {
                walk_extern_value(ev, &mut deps, module_path, registry, bindings)
            }
        }
    }

    for func in module.functions() {
        walk_function(func, &mut deps, module_path, registry, bindings);
    }

    // Re-exports (`pub use foo::Bar`) emit a `using` alias in this module's
    // header. The alias names the target's fully-qualified type, so the
    // defining module's header must be visible. A `using` to an incomplete
    // type is legal, but we mirror a normal cross-module full reference and
    // pull in the header via `record_path` (which also handles
    // predefined/extern targets and same-module self-references).
    for (_local, target) in module.reexports() {
        let canonical = registry.canonicalize(&target);
        if !registry.contains(&canonical) {
            continue;
        }
        record_path(
            &canonical,
            EdgeKind::FullDef,
            &mut deps,
            module_path,
            registry,
            bindings,
        );
    }

    // Promote cpp-gated `#[cfg(backend = "cpp")] use ...;` paths to
    // `#include` edges. A cfg-gated use is the explicit signal of which
    // other modules the cpp prologue/epilogue depends on — the splice text
    // is opaque to the dep walker, so we trust the declaration. Ungated
    // uses are resolution-only and deliberately excluded.
    for use_path in module.gated_uses_for(crate::Backend::Cpp) {
        if let Ok(item) = registry.get(&use_path, &crate::span::ItemLocation::internal())
            && let Some(parent) = item.path.parent()
            && parent != *module_path
        {
            deps.include_modules.insert(parent);
        }
    }

    // Drop self-references so we don't try to include our own header.
    deps.include_modules.remove(module_path);
    deps.forward_decls.remove(module_path);

    // Anything we decided to fully include shouldn't double up as a
    // forward decl in the dep walk - prune the forward_decls set.
    deps.forward_decls
        .retain(|m, _| !deps.include_modules.contains(m));

    deps
}

fn walk_extern_value(
    ev: &SemanticExternValueDefinition,
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
            record_path(
                base,
                EdgeKind::FullDef,
                deps,
                module_path,
                registry,
                bindings,
            );
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
            walk_type(
                inner,
                EdgeKind::FwdOnly,
                deps,
                module_path,
                registry,
                bindings,
            );
        }
        Type::Array(inner, _) => {
            walk_type(inner, kind, deps, module_path, registry, bindings);
        }
        Type::Function(_, args, ret) => {
            for arg in args {
                walk_type(
                    &arg.type_,
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
        Type::Generic(_, args) => args.iter().all(type_param_only_reached_through_indirection),
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
        if let Some(binding) = bindings.get(target)
            && let Some(header) = &binding.header
        {
            deps.include_headers.insert(header.clone());
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
