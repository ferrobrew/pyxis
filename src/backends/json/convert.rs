use std::{collections::BTreeMap, path::Path};

use crate::{
    backends::{BackendError, Result},
    grammar::ItemPath,
    semantic::{
        ExternBindings, SemanticOutput, TypeRegistry,
        types::{
            Argument, BitflagField, BitflagsDefinition, ConstDefinition as SemanticConstDefinition,
            ConstValue, EnumDefinition, EnumVariant,
            ExternValueDefinition as SemanticExternValueDefinition, Function, FunctionBody,
            ItemCategory, ItemDefinition, ItemDefinitionInner, Region, Type, TypeAliasDefinition,
            TypeDefinition, TypeVftable, UnionDefinition,
        },
    },
    source_store::FileStore,
    span::FileId,
};

use super::schema::*;

impl JsonDocLink {
    fn from_target(text: String, target: crate::semantic::doc_links::DocLinkTarget) -> JsonDocLink {
        use crate::semantic::doc_links::{DocLinkMemberKind as K, DocLinkTarget as T};
        match target {
            T::Item(path) => JsonDocLink {
                text,
                target_kind: JsonDocLinkTargetKind::Item,
                path: path.to_string(),
                anchor: None,
            },
            // A nested constant or extern value is emitted as its own item page
            // in the viewer (unlike Rust, where they're associated members), so
            // point the link at the value's own path rather than an anchor on
            // its parent.
            T::Member {
                item,
                name,
                kind: K::Constant | K::ExternValue,
            } => JsonDocLink {
                text,
                target_kind: JsonDocLinkTargetKind::Item,
                path: item
                    .join(crate::grammar::ItemPathSegment::from(name.as_str()))
                    .to_string(),
                anchor: None,
            },
            T::Member { item, name, kind } => {
                let anchor = match kind {
                    K::Method => format!("func-{name}"),
                    K::VftableMethod => format!("vfunc-{name}"),
                    K::Field => format!("field-{name}"),
                    K::Variant => format!("variant-{name}"),
                    K::Flag => format!("flag-{name}"),
                    K::Constant | K::ExternValue => unreachable!("handled above"),
                };
                JsonDocLink {
                    text,
                    target_kind: JsonDocLinkTargetKind::Item,
                    path: item.to_string(),
                    anchor: Some(anchor),
                }
            }
            T::Function { module, name } => JsonDocLink {
                text,
                target_kind: JsonDocLinkTargetKind::Module,
                path: module.to_string(),
                anchor: Some(format!("func-{name}")),
            },
            // A module-level extern value is its own item page at `module::name`,
            // so link to the item rather than a module anchor.
            T::ExternValue { module, name } => JsonDocLink {
                text,
                target_kind: JsonDocLinkTargetKind::Item,
                path: module
                    .join(crate::grammar::ItemPathSegment::from(name.as_str()))
                    .to_string(),
                anchor: None,
            },
        }
    }
}

/// Context for surfacing doc-comment links during conversion: the module's
/// resolved link table, produced once during semantic analysis. Links are
/// looked up by the doc-bearing node's location — conversion never re-scans
/// or re-resolves doc text, so it cannot diverge from what the compiler
/// validated.
pub(super) struct DocCx<'a> {
    pub(super) links: &'a crate::semantic::doc_links::ModuleDocLinks,
}

impl DocCx<'_> {
    /// Convert a doc comment into its markdown text and the resolved links of
    /// the doc block owned by the node at `location`.
    fn convert(
        &self,
        doc: &[String],
        location: &crate::span::ItemLocation,
    ) -> (Option<String>, Vec<JsonDocLink>) {
        Self::convert_resolved(doc, self.links.at(location))
    }

    /// Convert the module's own doc block (keyed separately from node docs —
    /// see [`crate::semantic::doc_links::DocBlockKey`]).
    pub(super) fn convert_module_doc(&self, doc: &[String]) -> (Option<String>, Vec<JsonDocLink>) {
        Self::convert_resolved(doc, self.links.module_doc())
    }

    fn convert_resolved(
        doc: &[String],
        resolved: &[crate::semantic::doc_links::ResolvedDocLink],
    ) -> (Option<String>, Vec<JsonDocLink>) {
        let mut links: Vec<JsonDocLink> = Vec::new();
        for link in resolved {
            if links.iter().any(|l| l.text == link.text) {
                continue;
            }
            links.push(JsonDocLink::from_target(
                link.text.clone(),
                link.target.clone(),
            ));
        }
        (doc_to_option(doc), links)
    }
}

/// Generate the JSON documentation for the entire project
pub fn build(
    out_dir: &Path,
    semantic_state: &SemanticOutput,
    project_name: &str,
    file_store: &FileStore,
) -> Result<()> {
    let type_registry = semantic_state.type_registry();

    // Build source_paths from file store
    // We collect all unique file IDs from items, then build the paths list
    let mut max_file_id = 0usize;
    for module in semantic_state.modules().values() {
        for definition in module.definitions(type_registry) {
            let file_index = definition.location.file_id.index();
            if file_index > max_file_id {
                max_file_id = file_index;
            }
        }
    }

    // Build source paths array (indices 0 and 1 are reserved for internal/test)
    let source_paths: Vec<String> = (0..=max_file_id)
        .map(|i| file_store.filename(FileId::new(i as u32)).to_string())
        .collect();

    // Build items map. The JSON output is documentation, not a build
    // target, so we deliberately do NOT filter by `cfg(backend = ...)` -
    // every item (and every method/function) is emitted with its `cfg`
    // predicate attached as structured data so downstream tooling can
    // render or filter per their own rules.
    let mut items = BTreeMap::new();
    for (module_path, module) in semantic_state.modules() {
        let bindings: BTreeMap<&str, ExternBindings> = module.extern_bindings().collect();
        let cx = DocCx {
            links: semantic_state.module_doc_links(module_path),
        };
        for definition in module.definitions(type_registry) {
            let binding = definition
                .path
                .last()
                .and_then(|leaf| bindings.get(leaf.as_str()).copied())
                .unwrap_or_default();
            if let Some(json_item) = convert_item(definition, type_registry, binding, &cx) {
                items.insert(json_item.path.clone(), json_item);
            }
        }
    }

    // Predefined types (`f32`, `u32`, `bool`, `void`, `str`, the atomics, ...)
    // live in the type registry for resolution but aren't attached to any
    // module, so the loop above never emits them. Add them here so the viewer
    // can render the builtin types user code references. They carry no source
    // location (see `ItemDefinition::default`), and are public.
    let predefined_links = Default::default();
    let predefined_cx = DocCx {
        links: &predefined_links,
    };
    for (path, item) in type_registry.iter() {
        if item.category != ItemCategory::Predefined {
            continue;
        }
        if items.contains_key(&path.to_string()) {
            continue;
        }
        if let Some(json_item) = convert_item(
            item,
            type_registry,
            ExternBindings::default(),
            &predefined_cx,
        ) {
            items.insert(json_item.path.clone(), json_item);
        }
    }

    // Build module hierarchy
    let modules = super::hierarchy::build_module_hierarchy(semantic_state);

    // Create the top-level documentation structure
    let documentation = JsonDocumentation {
        schema_version: CURRENT_SCHEMA_VERSION,
        pyxis_version: env!("CARGO_PKG_VERSION").to_string(),
        pointer_size: type_registry.pointer_size(),
        project_name: project_name.to_string(),
        items,
        modules,
        source_paths,
    };

    // Write to file
    let output_path = out_dir.join("output.json");
    let json_string = serde_json::to_string_pretty(&documentation).map_err(|e| {
        BackendError::Formatting(format!("Failed to serialize JSON documentation: {e}"))
    })?;
    std::fs::write(&output_path, &json_string).map_err(|e| BackendError::Io {
        error: e,
        context: format!("Failed to write JSON output to {}", output_path.display()),
    })?;

    Ok(())
}

fn convert_type(type_ref: &Type) -> JsonType {
    match type_ref {
        Type::Unresolved(_) => {
            // This shouldn't happen in resolved state, but handle it gracefully
            JsonType::Raw {
                path: "unresolved".to_string(),
            }
        }
        Type::Raw(path) => JsonType::Raw {
            path: path.to_string(),
        },
        Type::Generic(base_path, args) => JsonType::Generic {
            base: base_path.to_string(),
            args: args.iter().map(convert_type).collect(),
        },
        Type::TypeParameter(name) => JsonType::TypeParameter { name: name.clone() },
        Type::ConstPointer(inner) => JsonType::ConstPointer {
            inner: Box::new(convert_type(inner)),
        },
        Type::MutPointer(inner) => JsonType::MutPointer {
            inner: Box::new(convert_type(inner)),
        },
        Type::Array(inner, size) => JsonType::Array {
            inner: Box::new(convert_type(inner)),
            size: *size,
        },
        Type::Function(cc, args, return_type) => JsonType::Function {
            calling_convention: (*cc).into(),
            arguments: args
                .iter()
                .map(|(name, type_ref)| JsonFunctionArgument {
                    name: name.clone(),
                    type_ref: convert_type(type_ref),
                })
                .collect(),
            return_type: return_type.as_ref().map(|t| Box::new(convert_type(t))),
        },
    }
}

fn convert_argument(arg: &Argument) -> JsonArgument {
    match arg {
        Argument::ConstSelf { .. } => JsonArgument::ConstSelf,
        Argument::MutSelf { .. } => JsonArgument::MutSelf,
        Argument::Field { name, type_, .. } => JsonArgument::Field {
            name: name.clone(),
            type_ref: convert_type(type_),
        },
    }
}

fn convert_function_body(body: &FunctionBody) -> JsonFunctionBody {
    match body {
        FunctionBody::Address { address } => JsonFunctionBody::Address { address: *address },
        FunctionBody::Field {
            field,
            function_name,
        } => JsonFunctionBody::Field {
            field: field.clone(),
            function_name: function_name.clone(),
        },
        FunctionBody::Vftable { function_name } => JsonFunctionBody::Vftable {
            function_name: function_name.clone(),
        },
        FunctionBody::External => JsonFunctionBody::External,
    }
}

pub(super) fn convert_function(func: &Function, cx: &DocCx) -> JsonFunction {
    let (doc, doc_links) = cx.convert(&func.doc, &func.location);
    JsonFunction {
        visibility: func.visibility.into(),
        name: func.name.clone(),
        doc,
        doc_links,
        body: convert_function_body(&func.body),
        arguments: func.arguments.iter().map(convert_argument).collect(),
        return_type: func.return_type.as_ref().map(convert_type),
        calling_convention: func.calling_convention.into(),
        method_type_parameters: func.method_type_parameters.clone(),
        cfg: func.cfg.as_ref().map(convert_cfg),
        source: convert_location(&func.location),
    }
}

pub(super) fn convert_cfg(pred: &crate::parser::cfg::CfgPredicate) -> JsonCfg {
    use crate::parser::cfg::{CfgAtom, CfgPredicate};
    match pred {
        CfgPredicate::Atom { atom, .. } => match atom {
            CfgAtom::Ident { name, .. } => JsonCfg::Ident { name: name.clone() },
            CfgAtom::KeyValue { key, value, .. } => JsonCfg::KeyValue {
                key: key.clone(),
                value: value.clone(),
            },
        },
        CfgPredicate::Any { predicates, .. } => JsonCfg::Any {
            predicates: predicates.iter().map(convert_cfg).collect(),
        },
        CfgPredicate::All { predicates, .. } => JsonCfg::All {
            predicates: predicates.iter().map(convert_cfg).collect(),
        },
        CfgPredicate::Not { predicate, .. } => JsonCfg::Not {
            predicate: Box::new(convert_cfg(predicate)),
        },
    }
}

fn convert_region(
    region: &Region,
    type_registry: &TypeRegistry,
    offset: usize,
    cx: &DocCx,
) -> JsonRegion {
    let size = region.type_ref.size(type_registry).unwrap_or(0);
    let alignment = region.type_ref.alignment(type_registry).unwrap_or(1);
    let (doc, doc_links) = cx.convert(&region.doc, &region.location);

    JsonRegion {
        visibility: region.visibility.into(),
        name: region.name.clone(),
        doc,
        doc_links,
        type_ref: convert_type(&region.type_ref),
        offset,
        size,
        alignment,
        is_base: region.is_base,
        source: convert_location(&region.location),
    }
}

fn convert_vftable(vftable: &TypeVftable, cx: &DocCx) -> JsonTypeVftable {
    JsonTypeVftable {
        functions: vftable
            .functions
            .iter()
            .map(|f| convert_function(f, cx))
            .collect(),
    }
}

fn convert_type_definition(
    td: &TypeDefinition,
    type_registry: &TypeRegistry,
    cx: &DocCx,
    item_location: &crate::span::ItemLocation,
) -> JsonTypeDefinition {
    // Calculate field offsets
    let mut current_offset = 0;
    let fields = td
        .regions
        .iter()
        .map(|region| {
            let json_region = convert_region(region, type_registry, current_offset, cx);
            current_offset += json_region.size;
            json_region
        })
        .collect();

    let (doc, doc_links) = cx.convert(&td.doc, item_location);
    JsonTypeDefinition {
        doc,
        doc_links,
        fields,
        associated_functions: td
            .associated_functions
            .iter()
            .map(|f| convert_function(f, cx))
            .collect(),
        vftable: td.vftable.as_ref().map(|v| convert_vftable(v, cx)),
        singleton: td.singleton,
        copyable: td.copyable,
        cloneable: td.cloneable,
        defaultable: td.defaultable,
        packed: td.packed,
        pinned: td.pinned,
        nested_items: td.nested_item_paths.iter().map(|p| p.to_string()).collect(),
    }
}

/// Convert a union. Unlike a type, there is no running offset to accumulate:
/// every member starts at offset 0, which is the whole point of a union.
fn convert_union_definition(
    ud: &UnionDefinition,
    type_registry: &TypeRegistry,
    size: usize,
    alignment: usize,
    cx: &DocCx,
    item_location: &crate::span::ItemLocation,
) -> JsonUnionDefinition {
    let fields = ud
        .regions
        .iter()
        .map(|region| convert_region(region, type_registry, 0, cx))
        .collect();

    let (doc, doc_links) = cx.convert(&ud.doc, item_location);
    JsonUnionDefinition {
        doc,
        doc_links,
        fields,
        size,
        alignment,
        copyable: ud.copyable,
        cloneable: ud.cloneable,
        defaultable: ud.defaultable,
        packed: ud.packed,
        pinned: ud.pinned,
        nested_items: ud.nested_item_paths.iter().map(|p| p.to_string()).collect(),
    }
}

fn convert_enum_variant(variant: &EnumVariant, cx: &DocCx) -> JsonEnumVariant {
    let (doc, doc_links) = cx.convert(&variant.doc, &variant.location);
    JsonEnumVariant {
        name: variant.name.clone(),
        value: variant.value,
        doc,
        doc_links,
        source: convert_location(&variant.location),
    }
}

fn convert_enum_definition(
    ed: &EnumDefinition,
    type_registry: &TypeRegistry,
    parent_path: &ItemPath,
    cx: &DocCx,
    item_location: &crate::span::ItemLocation,
) -> JsonEnumDefinition {
    let (doc, doc_links) = cx.convert(&ed.doc, item_location);
    let nested_items: Vec<String> = type_registry
        .iter()
        .filter(|(p, _)| p.parent().as_ref() == Some(parent_path))
        .map(|(p, _)| p.to_string())
        .collect();
    JsonEnumDefinition {
        doc,
        doc_links,
        underlying_type: convert_type(&ed.type_),
        variants: ed
            .variants
            .iter()
            .map(|v| convert_enum_variant(v, cx))
            .collect(),
        associated_functions: ed
            .associated_functions
            .iter()
            .map(|f| convert_function(f, cx))
            .collect(),
        singleton: ed.singleton,
        copyable: ed.copyable,
        cloneable: ed.cloneable,
        default: ed.default,
        pinned: ed.pinned,
        nested_items,
    }
}

fn convert_bitflag_field(flag: &BitflagField, cx: &DocCx) -> JsonBitflag {
    let (doc, doc_links) = cx.convert(&flag.doc, &flag.location);
    JsonBitflag {
        name: flag.name.clone(),
        value: flag.value,
        doc,
        doc_links,
        source: convert_location(&flag.location),
    }
}

fn convert_bitflags_definition(
    bd: &BitflagsDefinition,
    type_registry: &TypeRegistry,
    parent_path: &ItemPath,
    cx: &DocCx,
    item_location: &crate::span::ItemLocation,
) -> JsonBitflagsDefinition {
    let (doc, doc_links) = cx.convert(&bd.doc, item_location);
    let nested_items: Vec<String> = type_registry
        .iter()
        .filter(|(p, _)| p.parent().as_ref() == Some(parent_path))
        .map(|(p, _)| p.to_string())
        .collect();
    JsonBitflagsDefinition {
        doc,
        doc_links,
        underlying_type: convert_type(&bd.type_),
        flags: bd
            .flags
            .iter()
            .map(|f| convert_bitflag_field(f, cx))
            .collect(),
        singleton: bd.singleton,
        copyable: bd.copyable,
        cloneable: bd.cloneable,
        default: bd.default,
        pinned: bd.pinned,
        nested_items,
    }
}

fn convert_type_alias_definition(
    ta: &TypeAliasDefinition,
    cx: &DocCx,
    item_location: &crate::span::ItemLocation,
) -> JsonTypeAliasDefinition {
    let (doc, doc_links) = cx.convert(&ta.doc, item_location);
    JsonTypeAliasDefinition {
        doc,
        doc_links,
        target: convert_type(&ta.target),
    }
}

fn convert_const_definition(
    cd: &SemanticConstDefinition,
    cx: &DocCx,
    item_location: &crate::span::ItemLocation,
) -> JsonConstantDefinition {
    let (doc, doc_links) = cx.convert(&cd.doc, item_location);
    let value = convert_const_value(&cd.value, cx);
    JsonConstantDefinition {
        doc,
        doc_links,
        value_type: convert_type(&cd.type_),
        value,
    }
}

/// Convert a semantic `ConstValue` to its JSON representation. Extracted as a
/// standalone function so struct fields and array elements can recurse.
fn convert_const_value(value: &ConstValue, _cx: &DocCx) -> JsonConstValue {
    match value {
        ConstValue::Int(v) => JsonConstValue::Int { value: *v },
        ConstValue::Float(bits) => JsonConstValue::Float {
            value: f64::from_bits(*bits),
        },
        ConstValue::String(s) => JsonConstValue::String { value: s.clone() },
        ConstValue::CString(s) => JsonConstValue::CString { value: s.clone() },
        ConstValue::EnumValue(path) => JsonConstValue::EnumValue {
            path: path.to_string(),
        },
        ConstValue::Struct { fields, .. } => JsonConstValue::Struct {
            fields: fields
                .iter()
                .map(|(name, val)| JsonConstField {
                    name: name.clone(),
                    value: convert_const_value(val, _cx),
                })
                .collect(),
        },
        ConstValue::Array(elements) => JsonConstValue::Array {
            elements: elements
                .iter()
                .map(|e| convert_const_value(e, _cx))
                .collect(),
        },
        ConstValue::ConstRef(path) => JsonConstValue::ConstRef {
            path: path.to_string(),
        },
    }
}

fn convert_item(
    item: &ItemDefinition,
    type_registry: &TypeRegistry,
    binding: ExternBindings,
    cx: &DocCx,
) -> Option<JsonItem> {
    let resolved = item.resolved()?;

    let kind =
        match &resolved.inner {
            ItemDefinitionInner::Type(td) => JsonItemKind::Type(convert_type_definition(
                td,
                type_registry,
                cx,
                &item.location,
            )),
            ItemDefinitionInner::Union(ud) => JsonItemKind::Union(convert_union_definition(
                ud,
                type_registry,
                resolved.size,
                resolved.alignment,
                cx,
                &item.location,
            )),
            ItemDefinitionInner::Enum(ed) => JsonItemKind::Enum(convert_enum_definition(
                ed,
                type_registry,
                &item.path,
                cx,
                &item.location,
            )),
            ItemDefinitionInner::Bitflags(bd) => JsonItemKind::Bitflags(
                convert_bitflags_definition(bd, type_registry, &item.path, cx, &item.location),
            ),
            ItemDefinitionInner::TypeAlias(ta) => {
                JsonItemKind::TypeAlias(convert_type_alias_definition(ta, cx, &item.location))
            }
            ItemDefinitionInner::Constant(cd) => {
                JsonItemKind::Constant(convert_const_definition(cd, cx, &item.location))
            }
            ItemDefinitionInner::ExternValue(ev) => {
                JsonItemKind::ExternValue(convert_extern_value_definition(ev, cx, &item.location))
            }
        };

    // Build source location for defined items (not predefined/internal)
    let source = convert_location(&item.declaration_location);

    Some(JsonItem {
        path: item.path.to_string(),
        visibility: item.visibility.into(),
        type_parameters: item.type_parameters.clone(),
        size: resolved.size,
        alignment: resolved.alignment,
        category: item.category.into(),
        cpp_name: binding.cpp_name.map(str::to_string),
        cpp_header: binding.cpp_header.map(str::to_string),
        rust_name: binding.rust_name.map(str::to_string),
        kind,
        cfg: item.cfg.as_ref().map(convert_cfg),
        source,
    })
}

fn convert_extern_value_definition(
    ev: &SemanticExternValueDefinition,
    cx: &DocCx,
    item_location: &crate::span::ItemLocation,
) -> JsonExternValueDefinition {
    let (doc, doc_links) = cx.convert(&ev.doc, item_location);
    JsonExternValueDefinition {
        doc,
        doc_links,
        value_type: convert_type(&ev.type_),
        address: ev.address,
    }
}
