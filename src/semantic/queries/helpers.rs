//! Resolution helpers for the query graph: per-item building and the
//! TypeRegistry/ItemDefinition construction used by `analyze` and `resolve_item`.

use std::{collections::BTreeMap, sync::Arc};

use crate::{
    grammar::{self, Argument, Ident, ItemDefinitionInner, ItemPath, TypeField, TypeParameter},
    semantic::{
        Module, SemanticError, TypeRegistry, bitflags_definition,
        db::Db,
        declaration_registry::ExternTypeInfo,
        doc_links, enum_definition,
        error::{BuildOutcome, Result},
        ir::SemanticAnalysis,
        name_index::{ExternSig, NameIndex, NameResolution},
        resolution_context::{ResolutionContext, ResolutionContextRef},
        type_alias_definition, type_definition,
        types::{
            self, ItemCategory, ItemDefinition, ItemState, ItemStateResolved, PredefinedItem,
            TypeDefinition, Visibility,
        },
    },
    span::ItemLocation,
};

use super::compute_associated_functions;

/// Compute associated functions (own impl methods + inherited from base types)
/// and merge them into `registry` in place, returning any errors. Shared by
/// `analyze`'s success path (so backends and the doc-link resolver see methods)
/// and its error paths (so `Type::method` doc links still resolve when analysis
/// bails out early).
pub(super) fn merge_associated_functions(
    db: &dyn Db,
    pointer_size: usize,
    registry: &mut TypeRegistry,
    modules: &BTreeMap<ItemPath, Module>,
) -> Vec<SemanticError> {
    let initial = SemanticAnalysis::new(
        db,
        Arc::new(registry.clone()),
        Arc::new(modules.clone()),
        // Placeholder resolver — callers build the real one from the merged registry.
        Arc::new(doc_links::DocLinkResolver::build(
            &TypeRegistry::new(pointer_size),
            &BTreeMap::new(),
        )),
        Arc::new(vec![]),
        Arc::new(vec![]),
    );
    let af = compute_associated_functions(db, initial);
    for (path, fns) in af.functions.iter() {
        if let Ok(item) = registry.get_mut(path, &ItemLocation::internal())
            && let ItemState::Resolved(state) = &mut item.state
            && let types::ItemDefinitionInner::Type(td) = &mut state.inner
        {
            td.associated_functions = fns.clone();
        }
    }
    af.errors.clone()
}

/// Build a single item using the existing type_definition/enum/bitflags/type_alias
/// build functions, with the given registry and modules.
#[allow(clippy::too_many_arguments)]
pub(super) fn build_item(
    definition: &grammar::ItemDefinition,
    item_path: &ItemPath,
    visibility: Visibility,
    def_location: &ItemLocation,
    doc_comments: &[String],
    type_param_names: &[String],
    type_registry: &mut TypeRegistry,
    modules: &mut BTreeMap<ItemPath, Module>,
) -> Result<BuildOutcome> {
    match &definition.inner {
        ItemDefinitionInner::Type(ty) => {
            let mut ctx = ResolutionContext::new(type_registry, modules);
            type_definition::build(
                &mut ctx,
                item_path,
                visibility,
                ty,
                def_location,
                doc_comments,
                type_param_names,
            )
        }
        ItemDefinitionInner::Enum(e) => {
            let ctx_ref = ResolutionContextRef::new(type_registry, modules);
            enum_definition::build(&ctx_ref, item_path, e, def_location, doc_comments)
        }
        ItemDefinitionInner::Bitflags(b) => {
            let ctx_ref = ResolutionContextRef::new(type_registry, modules);
            bitflags_definition::build(&ctx_ref, item_path, b, def_location, doc_comments)
        }
        ItemDefinitionInner::TypeAlias(ta) => {
            let ctx_ref = ResolutionContextRef::new(type_registry, modules);
            type_alias_definition::build(
                &ctx_ref,
                item_path,
                ta,
                def_location,
                doc_comments,
                type_param_names,
            )
        }
    }
}

pub(super) fn register_predefined(type_registry: &mut TypeRegistry) {
    for predefined in PredefinedItem::ALL {
        let size = predefined.size();
        let alignment = size.max(1);
        type_registry.add(ItemDefinition {
            path: ItemPath::from(predefined.name()),
            state: ItemState::Resolved(ItemStateResolved {
                size,
                alignment,
                inner: TypeDefinition {
                    copyable: true,
                    cloneable: true,
                    defaultable: true,
                    ..Default::default()
                }
                .into(),
            }),
            category: ItemCategory::Predefined,
            predefined: Some(*predefined),
            ..Default::default()
        });
    }
}

pub(super) fn register_unresolved(
    type_registry: &mut TypeRegistry,
    path: &ItemPath,
    definition: &grammar::ItemDefinition,
) {
    let type_parameters: Vec<String> = definition
        .type_parameters
        .iter()
        .map(|tp| tp.name.clone())
        .collect();
    let cfg = match &definition.inner {
        ItemDefinitionInner::Type(td) => td.attributes.cfg(),
        ItemDefinitionInner::Enum(e) => e.attributes.cfg(),
        ItemDefinitionInner::Bitflags(b) => b.attributes.cfg(),
        ItemDefinitionInner::TypeAlias(ta) => ta.attributes.cfg(),
    };
    type_registry.add(ItemDefinition {
        visibility: definition.visibility.into(),
        path: path.clone(),
        type_parameters,
        state: ItemState::Unresolved(definition.clone()),
        cfg,
        location: definition.location,
        declaration_location: definition.declaration_location,
        ..Default::default()
    });
}

pub(super) fn make_unresolved_definition(path: &ItemPath) -> ItemDefinition {
    ItemDefinition {
        path: path.clone(),
        ..Default::default()
    }
}

pub(super) fn make_predefined_definition(
    path: &ItemPath,
    size: usize,
    alignment: usize,
) -> ItemDefinition {
    ItemDefinition {
        path: path.clone(),
        state: ItemState::Resolved(ItemStateResolved {
            size,
            alignment,
            inner: TypeDefinition {
                copyable: true,
                cloneable: true,
                defaultable: true,
                ..Default::default()
            }
            .into(),
        }),
        category: ItemCategory::Predefined,
        ..Default::default()
    }
}

pub(super) fn make_extern_definition(path: &ItemPath, info: &ExternTypeInfo) -> ItemDefinition {
    ItemDefinition {
        path: path.clone(),
        state: ItemState::Resolved(ItemStateResolved {
            size: info.size,
            alignment: info.alignment,
            inner: TypeDefinition {
                doc: info.doc_comments.clone(),
                ..Default::default()
            }
            .into(),
        }),
        category: ItemCategory::Extern,
        cfg: info.cfg.clone(),
        location: info.location,
        declaration_location: info.declaration_location,
        ..Default::default()
    }
}

/// An `Unresolved` placeholder entry built from the stable signature alone
/// (kind-agnostic, no body/location). Sufficient for *references* to the item
/// while building another: a pointer needs only the path to exist, and a
/// generic needs the arity. Value references inject the fully-resolved form
/// instead, so the placeholder's emptiness is never observed for them.
pub(super) fn make_placeholder(path: &ItemPath, arity: usize) -> ItemDefinition {
    let type_param_names: Vec<String> = (0..arity).map(|i| format!("T{i}")).collect();
    let grammar_def = grammar::ItemDefinition {
        visibility: grammar::Visibility::Public,
        name: Ident::from(path.last().map(|s| s.as_str()).unwrap_or("")),
        type_parameters: type_param_names
            .iter()
            .map(|name| TypeParameter {
                name: name.clone(),
                location: ItemLocation::internal(),
            })
            .collect(),
        ..Default::default()
    };

    ItemDefinition {
        path: path.clone(),
        type_parameters: type_param_names,
        state: ItemState::Unresolved(grammar_def),
        ..Default::default()
    }
}

/// A resolved extern-type entry from its stable size/alignment signature.
pub(super) fn make_extern_from_sig(path: &ItemPath, sig: &ExternSig) -> ItemDefinition {
    let info = ExternTypeInfo {
        size: sig.size,
        alignment: sig.alignment,
        location: ItemLocation::internal(),
        declaration_location: ItemLocation::internal(),
        doc_comments: vec![],
        cfg: None,
    };
    make_extern_definition(path, &info)
}

/// The declared types a definition references *by value or array element* (not
/// through a pointer) — the types whose layout must be resolved before it can be
/// built. Pointers are excluded: a pointee only needs to exist (the Unresolved
/// placeholders provide that), so excluding them keeps resolution acyclic for
/// the common mutually-pointing types and bounds each item's dependency set.
pub(super) fn value_referenced_types(
    definition: &grammar::ItemDefinition,
    scope: &[ItemPath],
    index: &NameIndex,
) -> Vec<ItemPath> {
    let mut refs = Vec::new();
    match &definition.inner {
        ItemDefinitionInner::Type(td) => {
            for statement in td.statements() {
                match &statement.field {
                    TypeField::Field(_, _, type_) => {
                        collect_value_refs(type_, scope, index, &mut refs)
                    }
                    // vftable function signatures resolve their by-value arg /
                    // return types too — a generic like `WeakPtr<GameObject>` in
                    // a method signature must be resolved to build the vftable.
                    TypeField::Vftable(functions) => {
                        for function in functions {
                            for argument in &function.arguments {
                                if let Argument::Named { type_, .. } = argument {
                                    collect_value_refs(type_, scope, index, &mut refs);
                                }
                            }
                            if let Some(return_type) = &function.return_type {
                                collect_value_refs(return_type, scope, index, &mut refs);
                            }
                        }
                    }
                }
            }
        }
        ItemDefinitionInner::Enum(e) => collect_value_refs(&e.type_, scope, index, &mut refs),
        ItemDefinitionInner::Bitflags(b) => collect_value_refs(&b.type_, scope, index, &mut refs),
        ItemDefinitionInner::TypeAlias(ta) => {
            collect_value_refs(&ta.target, scope, index, &mut refs)
        }
    }
    refs
}

fn collect_value_refs(
    type_: &grammar::Type,
    scope: &[ItemPath],
    index: &NameIndex,
    refs: &mut Vec<ItemPath>,
) {
    use grammar::Type;
    match type_ {
        Type::Ident {
            path, generic_args, ..
        } => {
            let name = path.last().map(|s| s.as_str()).unwrap_or("");
            if let NameResolution::Found(p) = index.resolve_name(scope, name) {
                refs.push(p);
            }
            // Generic arguments of a value type are treated as value deps too
            // (conservative — resolving an extra one is harmless; missing one
            // would wrongly stall the build).
            for arg in generic_args {
                collect_value_refs(arg, scope, index, refs);
            }
        }
        Type::Array { element, .. } => collect_value_refs(element, scope, index, refs),
        // Pointers: the pointee need only exist, so don't recurse into it.
        Type::ConstPointer { .. } | Type::MutPointer { .. } => {}
        Type::Unknown { .. } => {}
    }
}
