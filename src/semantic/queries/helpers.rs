//! Resolution helpers for the query graph: per-item building and the
//! TypeRegistry/ItemDefinition construction used by `analyze` and `resolve_item`.

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::grammar::{ItemDefinitionInner, ItemPath};
use crate::semantic::TypeRegistry;
use crate::semantic::name_index::{ExternSig, NameIndex, NameResolution};

use crate::semantic::db::Db;
use crate::semantic::ir::SemanticAnalysis;

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
    modules: &BTreeMap<ItemPath, crate::semantic::Module>,
) -> Vec<crate::semantic::SemanticError> {
    use crate::semantic::doc_links;
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
        if let Ok(item) = registry.get_mut(path, &crate::span::ItemLocation::internal())
            && let crate::semantic::types::ItemState::Resolved(state) = &mut item.state
            && let crate::semantic::types::ItemDefinitionInner::Type(td) = &mut state.inner
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
    definition: &crate::grammar::ItemDefinition,
    item_path: &ItemPath,
    visibility: crate::semantic::types::Visibility,
    def_location: &crate::span::ItemLocation,
    doc_comments: &[String],
    type_param_names: &[String],
    type_registry: &mut TypeRegistry,
    modules: &mut BTreeMap<ItemPath, crate::semantic::Module>,
) -> crate::semantic::error::Result<crate::semantic::error::BuildOutcome> {
    match &definition.inner {
        ItemDefinitionInner::Type(ty) => {
            let mut ctx =
                crate::semantic::resolution_context::ResolutionContext::new(type_registry, modules);
            crate::semantic::type_definition::build(
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
            let ctx_ref = crate::semantic::resolution_context::ResolutionContextRef::new(
                type_registry,
                modules,
            );
            crate::semantic::enum_definition::build(
                &ctx_ref,
                item_path,
                e,
                def_location,
                doc_comments,
            )
        }
        ItemDefinitionInner::Bitflags(b) => {
            let ctx_ref = crate::semantic::resolution_context::ResolutionContextRef::new(
                type_registry,
                modules,
            );
            crate::semantic::bitflags_definition::build(
                &ctx_ref,
                item_path,
                b,
                def_location,
                doc_comments,
            )
        }
        ItemDefinitionInner::TypeAlias(ta) => {
            let ctx_ref = crate::semantic::resolution_context::ResolutionContextRef::new(
                type_registry,
                modules,
            );
            crate::semantic::type_alias_definition::build(
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
    for predefined in crate::semantic::types::PredefinedItem::ALL {
        let path = ItemPath::from(predefined.name());
        let size = predefined.size();
        let alignment = size.max(1);
        let location = crate::span::ItemLocation::internal();
        type_registry.add(crate::semantic::types::ItemDefinition {
            visibility: crate::semantic::types::Visibility::Public,
            path,
            type_parameters: vec![],
            state: crate::semantic::types::ItemState::Resolved(
                crate::semantic::types::ItemStateResolved {
                    size,
                    alignment,
                    inner: crate::semantic::types::TypeDefinition {
                        copyable: true,
                        cloneable: true,
                        defaultable: true,
                        ..Default::default()
                    }
                    .into(),
                },
            ),
            category: crate::semantic::types::ItemCategory::Predefined,
            predefined: Some(*predefined),
            cfg: None,
            location,
            declaration_location: location,
        });
    }
}

pub(super) fn register_unresolved(
    type_registry: &mut TypeRegistry,
    path: &ItemPath,
    definition: &crate::grammar::ItemDefinition,
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
    type_registry.add(crate::semantic::types::ItemDefinition {
        visibility: definition.visibility.into(),
        path: path.clone(),
        type_parameters,
        state: crate::semantic::types::ItemState::Unresolved(definition.clone()),
        category: crate::semantic::types::ItemCategory::Defined,
        predefined: None,
        cfg,
        location: definition.location,
        declaration_location: definition.declaration_location,
    });
}

pub(super) fn make_unresolved_definition(
    path: &ItemPath,
) -> crate::semantic::types::ItemDefinition {
    use crate::grammar::{
        Ident, ItemDefinition as GrammarDef, ItemDefinitionInner, TypeDefinition,
    };
    use crate::span::ItemLocation;

    let grammar_def = GrammarDef {
        visibility: crate::grammar::Visibility::Private,
        name: Ident::from(""),
        type_parameters: vec![],
        inner: ItemDefinitionInner::Type(TypeDefinition {
            items: vec![],
            attributes: Default::default(),
            inline_trailing_comments: vec![],
            following_comments: vec![],
        }),
        doc_comments: vec![],
        location: ItemLocation::internal(),
        declaration_location: ItemLocation::internal(),
    };

    crate::semantic::types::ItemDefinition {
        visibility: crate::semantic::types::Visibility::Public,
        path: path.clone(),
        type_parameters: vec![],
        state: crate::semantic::types::ItemState::Unresolved(grammar_def),
        category: crate::semantic::types::ItemCategory::Defined,
        predefined: None,
        cfg: None,
        location: ItemLocation::internal(),
        declaration_location: ItemLocation::internal(),
    }
}

pub(super) fn make_predefined_definition(
    path: &ItemPath,
    size: usize,
    alignment: usize,
) -> crate::semantic::types::ItemDefinition {
    crate::semantic::types::ItemDefinition {
        visibility: crate::semantic::types::Visibility::Public,
        path: path.clone(),
        type_parameters: vec![],
        state: crate::semantic::types::ItemState::Resolved(
            crate::semantic::types::ItemStateResolved {
                size,
                alignment,
                inner: crate::semantic::types::TypeDefinition {
                    copyable: true,
                    cloneable: true,
                    defaultable: true,
                    ..Default::default()
                }
                .into(),
            },
        ),
        category: crate::semantic::types::ItemCategory::Predefined,
        predefined: None,
        cfg: None,
        location: crate::span::ItemLocation::internal(),
        declaration_location: crate::span::ItemLocation::internal(),
    }
}

pub(super) fn make_extern_definition(
    path: &ItemPath,
    info: &crate::semantic::declaration_registry::ExternTypeInfo,
) -> crate::semantic::types::ItemDefinition {
    crate::semantic::types::ItemDefinition {
        visibility: crate::semantic::types::Visibility::Public,
        path: path.clone(),
        type_parameters: vec![],
        state: crate::semantic::types::ItemState::Resolved(
            crate::semantic::types::ItemStateResolved {
                size: info.size,
                alignment: info.alignment,
                inner: crate::semantic::types::TypeDefinition {
                    doc: info.doc_comments.clone(),
                    ..Default::default()
                }
                .into(),
            },
        ),
        category: crate::semantic::types::ItemCategory::Extern,
        predefined: None,
        cfg: info.cfg.clone(),
        location: info.location,
        declaration_location: info.declaration_location,
    }
}

/// An `Unresolved` placeholder entry built from the stable signature alone
/// (kind-agnostic, no body/location). Sufficient for *references* to the item
/// while building another: a pointer needs only the path to exist, and a
/// generic needs the arity. Value references inject the fully-resolved form
/// instead, so the placeholder's emptiness is never observed for them.
pub(super) fn make_placeholder(
    path: &ItemPath,
    arity: usize,
) -> crate::semantic::types::ItemDefinition {
    use crate::grammar::{
        Ident, ItemDefinition as GrammarDef, ItemDefinitionInner, TypeDefinition, TypeParameter,
    };
    use crate::span::ItemLocation;

    let type_param_names: Vec<String> = (0..arity).map(|i| format!("T{i}")).collect();
    let grammar_def = GrammarDef {
        visibility: crate::grammar::Visibility::Public,
        name: Ident::from(path.last().map(|s| s.as_str()).unwrap_or("")),
        type_parameters: type_param_names
            .iter()
            .map(|name| TypeParameter {
                name: name.clone(),
                location: ItemLocation::internal(),
            })
            .collect(),
        inner: ItemDefinitionInner::Type(TypeDefinition {
            items: vec![],
            attributes: Default::default(),
            inline_trailing_comments: vec![],
            following_comments: vec![],
        }),
        doc_comments: vec![],
        location: ItemLocation::internal(),
        declaration_location: ItemLocation::internal(),
    };

    crate::semantic::types::ItemDefinition {
        visibility: crate::semantic::types::Visibility::Public,
        path: path.clone(),
        type_parameters: type_param_names,
        state: crate::semantic::types::ItemState::Unresolved(grammar_def),
        category: crate::semantic::types::ItemCategory::Defined,
        predefined: None,
        cfg: None,
        location: ItemLocation::internal(),
        declaration_location: ItemLocation::internal(),
    }
}

/// A resolved extern-type entry from its stable size/alignment signature.
pub(super) fn make_extern_from_sig(
    path: &ItemPath,
    sig: &ExternSig,
) -> crate::semantic::types::ItemDefinition {
    let info = crate::semantic::declaration_registry::ExternTypeInfo {
        size: sig.size,
        alignment: sig.alignment,
        location: crate::span::ItemLocation::internal(),
        declaration_location: crate::span::ItemLocation::internal(),
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
    definition: &crate::grammar::ItemDefinition,
    scope: &[ItemPath],
    index: &NameIndex,
) -> Vec<ItemPath> {
    use crate::grammar::TypeField;
    let mut refs = Vec::new();
    match &definition.inner {
        ItemDefinitionInner::Type(td) => {
            for statement in td.statements() {
                if let TypeField::Field(_, _, type_) = &statement.field {
                    collect_value_refs(type_, scope, index, &mut refs);
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
    type_: &crate::grammar::Type,
    scope: &[ItemPath],
    index: &NameIndex,
    refs: &mut Vec<ItemPath>,
) {
    use crate::grammar::Type;
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

/// Collect `(span, resolved path)` for every named type reference in a
/// definition — fields, vftable signatures, bases/aliases, recursing through
/// pointers, arrays, and generic arguments. Powers the per-file source map.
pub(super) fn collect_type_ref_spans(
    definition: &crate::grammar::ItemDefinition,
    scope: &[ItemPath],
    index: &NameIndex,
    out: &mut Vec<(crate::span::Span, ItemPath)>,
) {
    use crate::grammar::TypeField;
    match &definition.inner {
        ItemDefinitionInner::Type(td) => {
            for statement in td.statements() {
                match &statement.field {
                    TypeField::Field(_, _, type_) => type_ref_spans(type_, scope, index, out),
                    TypeField::Vftable(functions) => {
                        for function in functions {
                            for argument in &function.arguments {
                                if let crate::grammar::Argument::Named { type_, .. } = argument {
                                    type_ref_spans(type_, scope, index, out);
                                }
                            }
                            if let Some(return_type) = &function.return_type {
                                type_ref_spans(return_type, scope, index, out);
                            }
                        }
                    }
                }
            }
        }
        ItemDefinitionInner::Enum(e) => type_ref_spans(&e.type_, scope, index, out),
        ItemDefinitionInner::Bitflags(b) => type_ref_spans(&b.type_, scope, index, out),
        ItemDefinitionInner::TypeAlias(ta) => type_ref_spans(&ta.target, scope, index, out),
    }
}

fn type_ref_spans(
    type_: &crate::grammar::Type,
    scope: &[ItemPath],
    index: &NameIndex,
    out: &mut Vec<(crate::span::Span, ItemPath)>,
) {
    use crate::grammar::Type;
    use crate::span::HasLocation;
    match type_ {
        Type::Ident {
            path, generic_args, ..
        } => {
            let name = path.last().map(|s| s.as_str()).unwrap_or("");
            if let NameResolution::Found(p) | NameResolution::FoundExtern(p) =
                index.resolve_name(scope, name)
            {
                out.push((type_.location().span, p));
            }
            for arg in generic_args {
                type_ref_spans(arg, scope, index, out);
            }
        }
        Type::ConstPointer { pointee, .. } | Type::MutPointer { pointee, .. } => {
            type_ref_spans(pointee, scope, index, out)
        }
        Type::Array { element, .. } => type_ref_spans(element, scope, index, out),
        Type::Unknown { .. } => {}
    }
}
