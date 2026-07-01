use std::{collections::BTreeMap, path::Path};

use crate::{
    backends::{BackendError, Result},
    grammar::ItemPath,
    semantic::types::{Backend, Type},
    source_store::FileStore,
    span::FileId,
};
use serde::{Deserialize, Serialize};

use crate::semantic::{
    ExternBindings, SemanticOutput, TypeRegistry,
    types::{
        Argument, BitflagField, BitflagsDefinition, CallingConvention, EnumDefinition, EnumVariant,
        ExternValue, Function, FunctionBody, ItemCategory, ItemDefinition, ItemDefinitionInner,
        Region, TypeAliasDefinition, TypeDefinition, TypeVftable, Visibility,
    },
};

// If changing the structure, ensure you rerun `cargo run -- gen-types` to
// update the TypeScript definitions. When making a breaking change to the
// shape, bump `CURRENT_SCHEMA_VERSION` so downstream consumers can detect
// the new format.

/// Current JSON schema version. Bump on any breaking shape change.
///
/// History:
/// - v1: original flat splice shape (`backend.prologue: string | null`).
/// - v2: structured splice (`backend.prologue: { header, definition } | null`);
///   added `schema_version` field so consumers can detect the format.
/// - v3: added optional `cpp_name` / `cpp_header` / `rust_name` to items,
///   surfacing the backend type bindings of `extern type`s.
/// - v4: added `source` locations to modules and extern values; added `doc` to
///   extern values, enum variants, and bitflag flags; surfaced extern-type
///   doc comments.
/// - v5: added resolved `doc_links` (rustdoc-style intra-doc links) alongside
///   each `doc`.
/// - v6: added top-level `pyxis_version`, recording which pyxis produced the
///   document (from `CARGO_PKG_VERSION`) so downstream consumers can tell
///   which toolchain generated a given doc set.
/// - v7: added `for_type` to `JsonBackendSplice` — the resolved absolute item
///   path for `prologue/epilogue for <Type>` attribution, so the viewer can
///   render the splice on the owning type's page instead of the module page.
pub const CURRENT_SCHEMA_VERSION: u32 = 7;

/// Top-level JSON documentation structure
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonDocumentation {
    /// Schema version. See [`CURRENT_SCHEMA_VERSION`]. Older documents
    /// (pre-v2) omit this field; consumers should treat a missing value
    /// as v1.
    #[serde(default = "default_schema_version_v1")]
    pub schema_version: u32,
    /// Version of pyxis that generated this document (from
    /// `CARGO_PKG_VERSION`), so downstream consumers can tell which
    /// toolchain produced a given doc set. Surfaced by the pyxis-defs
    /// `build.py` index, for example.
    #[serde(default = "default_pyxis_version_unknown")]
    pub pyxis_version: String,
    /// Pointer size for the target platform
    pub pointer_size: usize,
    /// Project name
    pub project_name: String,
    /// Map of absolute paths to items
    pub items: BTreeMap<String, JsonItem>,
    /// Nested module hierarchy
    pub modules: BTreeMap<String, JsonModule>,
    /// Source file paths indexed by file ID (index 0 and 1 are reserved for internal/test)
    pub source_paths: Vec<String>,
}

fn default_schema_version_v1() -> u32 {
    1
}

fn default_pyxis_version_unknown() -> String {
    "unknown".to_string()
}

/// A module containing items and potentially submodules
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonModule {
    /// Module documentation
    pub doc: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_links: Vec<JsonDocLink>,
    /// Items defined directly in this module
    pub items: Vec<String>, // Paths to items
    /// Child modules
    #[specta(inline)]
    pub submodules: BTreeMap<String, JsonModule>,
    /// Extern values (global variables)
    pub extern_values: Vec<JsonExternValue>,
    /// Freestanding functions
    pub functions: Vec<JsonFunction>,
    /// Backend configurations (prologue/epilogue for code generation)
    pub backends: BTreeMap<String, Vec<JsonBackend>>,
    /// Source location (file and line) - None for synthesized/folder modules
    #[serde(default)]
    pub source: Option<JsonSourceLocation>,
}

/// Backend configuration with prologue and epilogue
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonBackend {
    /// Prologue code inserted at the beginning of generated output
    pub prologue: Option<JsonBackendSplice>,
    /// Epilogue code inserted at the end of generated output
    pub epilogue: Option<JsonBackendSplice>,
}

/// A backend splice payload. `header` lands in the language's primary
/// declaration surface (Rust module, C++ header). `definition` lands in
/// the C++ source file and is always `None` for non-cpp backends.
/// `for_type`, when set, is the resolved absolute item path this splice is
/// attributed to (`prologue/epilogue for <Type>`); the viewer renders such
/// splices on the owning type's page rather than the module page.
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonBackendSplice {
    /// Code spliced into the header / declaration surface
    pub header: Option<String>,
    /// Code spliced into the C++ source file (cpp backend only)
    pub definition: Option<String>,
    /// Resolved absolute item path this splice is attributed to, when tagged
    /// with `for <Type>`. `None`/absent means module-level rendering.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub for_type: Option<String>,
}

/// Source location of an item (file index and line number)
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonSourceLocation {
    /// Index into the source_paths array in JsonDocumentation
    pub file_index: usize,
    /// Line number (1-indexed)
    pub line: usize,
}

/// A resolved rustdoc-style intra-doc link found in a doc comment. Consumers
/// rewrite the matching `[`text`]` / `[label](text)` in the markdown into a
/// link to `(target_kind, path, anchor)`.
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonDocLink {
    /// The link path as written in the doc (e.g. `Type::method`, `Action`).
    pub text: String,
    /// Whether `path` names an item or a module.
    pub target_kind: JsonDocLinkTargetKind,
    /// Absolute path to the item or module the link resolves to.
    pub path: String,
    /// Anchor within the target page (e.g. `field-m_Foo`, `variant-Bar`), if the
    /// link points at a member rather than the page itself.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub anchor: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
#[serde(rename_all = "snake_case")]
pub enum JsonDocLinkTargetKind {
    Item,
    Module,
}

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
            T::Member { item, name, kind } => {
                let anchor = match kind {
                    K::Method => format!("func-{name}"),
                    K::VftableMethod => format!("vfunc-{name}"),
                    K::Field => format!("field-{name}"),
                    K::Variant => format!("variant-{name}"),
                    K::Flag => format!("flag-{name}"),
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
            T::ExternValue { module, name } => JsonDocLink {
                text,
                target_kind: JsonDocLinkTargetKind::Module,
                path: module.to_string(),
                anchor: Some(format!("extval-{name}")),
            },
        }
    }
}

/// Context for resolving doc-comment links during conversion: the shared
/// resolver plus the scope of the module currently being converted.
struct DocCx<'a> {
    resolver: &'a crate::semantic::doc_links::DocLinkResolver,
    scope: Vec<ItemPath>,
}

impl DocCx<'_> {
    /// Convert a doc comment into its markdown text and resolved links.
    fn convert(&self, doc: &[String]) -> (Option<String>, Vec<JsonDocLink>) {
        let mut links: Vec<JsonDocLink> = Vec::new();
        for text in crate::semantic::doc_links::extract_links(doc) {
            if links.iter().any(|l| l.text == text) {
                continue;
            }
            if let Some(target) = self.resolver.resolve(&self.scope, &text) {
                links.push(JsonDocLink::from_target(text, target));
            }
        }
        (doc_to_option(doc), links)
    }
}

/// An item (type, enum, or bitflags) in the documentation
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonItem {
    /// Item path
    pub path: String,
    /// Visibility
    pub visibility: JsonVisibility,
    /// Type parameters for generic types (e.g., ["T", "U"] for `type Map<T, U>`)
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub type_parameters: Vec<String>,
    /// Size in bytes
    pub size: usize,
    /// Alignment in bytes
    pub alignment: usize,
    /// Item category (Defined, Predefined, Extern)
    pub category: JsonItemCategory,
    /// For `extern type`s: the backend type bindings (`#[cpp_name]`,
    /// `#[cpp_header]`, `#[rust_name]`) that say which concrete C++/Rust type
    /// the opaque extern maps to. Empty/None for non-extern items.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cpp_name: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cpp_header: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub rust_name: Option<String>,
    /// Item kind and details
    pub kind: JsonItemKind,
    /// `#[cfg(...)]` predicate the item is gated by, if any. Always
    /// emitted (the JSON output is documentation, not a build target);
    /// downstream tooling decides how to render and/or filter.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cfg: Option<JsonCfg>,
    /// Source location (file and line) - None for predefined/internal items
    pub source: Option<JsonSourceLocation>,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum JsonItemKind {
    Type(JsonTypeDefinition),
    Enum(JsonEnumDefinition),
    Bitflags(JsonBitflagsDefinition),
    TypeAlias(JsonTypeAliasDefinition),
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonTypeAliasDefinition {
    /// Documentation
    pub doc: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_links: Vec<JsonDocLink>,
    /// The resolved target type that this alias refers to
    pub target: JsonType,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonTypeDefinition {
    /// Documentation
    pub doc: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_links: Vec<JsonDocLink>,
    /// Fields/regions
    pub fields: Vec<JsonRegion>,
    /// Associated functions
    pub associated_functions: Vec<JsonFunction>,
    /// Virtual function table
    pub vftable: Option<JsonTypeVftable>,
    /// Singleton address
    pub singleton: Option<usize>,
    /// Whether the type is copyable
    pub copyable: bool,
    /// Whether the type is cloneable
    pub cloneable: bool,
    /// Whether the type is defaultable
    pub defaultable: bool,
    /// Whether the type is packed
    pub packed: bool,
    /// Whether the type is pinned (non-relocatable)
    pub pinned: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonRegion {
    /// Visibility
    pub visibility: JsonVisibility,
    /// Field name
    pub name: Option<String>,
    /// Documentation
    pub doc: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_links: Vec<JsonDocLink>,
    /// Type reference
    pub type_ref: JsonType,
    /// Offset in bytes from start of structure
    pub offset: usize,
    /// Size in bytes
    pub size: usize,
    /// Alignment in bytes
    pub alignment: usize,
    /// Whether this is a base class field
    pub is_base: bool,
    /// Source location (file and line) - None for generated/padding fields
    pub source: Option<JsonSourceLocation>,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonTypeVftable {
    /// Virtual functions
    pub functions: Vec<JsonFunction>,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonEnumDefinition {
    /// Documentation
    pub doc: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_links: Vec<JsonDocLink>,
    /// Underlying type
    pub underlying_type: JsonType,
    /// Enum variants
    pub variants: Vec<JsonEnumVariant>,
    /// Associated functions
    pub associated_functions: Vec<JsonFunction>,
    /// Singleton address
    pub singleton: Option<usize>,
    /// Whether the enum is copyable
    pub copyable: bool,
    /// Whether the enum is cloneable
    pub cloneable: bool,
    /// Default variant index
    pub default: Option<usize>,
    /// Whether the enum is pinned (non-relocatable)
    pub pinned: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonEnumVariant {
    /// Variant name
    pub name: String,
    /// Variant value
    pub value: isize,
    /// Documentation
    #[serde(default)]
    pub doc: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_links: Vec<JsonDocLink>,
    /// Source location (file and line)
    pub source: Option<JsonSourceLocation>,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonBitflagsDefinition {
    /// Documentation
    pub doc: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_links: Vec<JsonDocLink>,
    /// Underlying type
    pub underlying_type: JsonType,
    /// Bitflag fields
    pub flags: Vec<JsonBitflag>,
    /// Singleton address
    pub singleton: Option<usize>,
    /// Whether the bitflags is copyable
    pub copyable: bool,
    /// Whether the bitflags is cloneable
    pub cloneable: bool,
    /// Default flag index
    pub default: Option<usize>,
    /// Whether the bitflags is pinned (non-relocatable)
    pub pinned: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonBitflag {
    /// Flag name
    pub name: String,
    /// Flag value
    pub value: usize,
    /// Documentation
    #[serde(default)]
    pub doc: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_links: Vec<JsonDocLink>,
    /// Source location (file and line)
    pub source: Option<JsonSourceLocation>,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonFunction {
    /// Visibility
    pub visibility: JsonVisibility,
    /// Function name
    pub name: String,
    /// Documentation
    pub doc: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_links: Vec<JsonDocLink>,
    /// Function body (how it's implemented)
    pub body: JsonFunctionBody,
    /// Arguments
    pub arguments: Vec<JsonArgument>,
    /// Return type
    pub return_type: Option<JsonType>,
    /// Calling convention
    pub calling_convention: JsonCallingConvention,
    /// Method-level type parameters declared at the impl block beyond the
    /// parent struct's own type parameters (`Y` in `impl<T, Y> Foo<T> {...}`).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub method_type_parameters: Vec<String>,
    /// `#[cfg(...)]` predicate the function is gated by, if any. Methods
    /// inherit the conjunction of their impl block's cfg and their own.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cfg: Option<JsonCfg>,
    /// Source location (file and line)
    pub source: Option<JsonSourceLocation>,
}

/// `#[cfg(...)]` predicate AST, mirroring the parser's
/// [`crate::parser::cfg::CfgPredicate`] shape with locations stripped.
/// Emitted on items/functions so documentation consumers can decide
/// per-backend rendering without re-parsing.
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum JsonCfg {
    /// A bare ident atom: `#[cfg(test)]`.
    Ident { name: String },
    /// A key/value atom: `#[cfg(backend = "cpp")]`.
    KeyValue { key: String, value: String },
    /// `any(...)` combinator.
    Any { predicates: Vec<JsonCfg> },
    /// `all(...)` combinator.
    All { predicates: Vec<JsonCfg> },
    /// `not(...)` combinator.
    Not { predicate: Box<JsonCfg> },
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum JsonFunctionBody {
    Address {
        address: usize,
    },
    Field {
        field: String,
        function_name: String,
    },
    Vftable {
        function_name: String,
    },
    /// Body supplied by the target backend's prologue/epilogue (the pyxis
    /// `#[external_body]` attribute).
    External,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum JsonArgument {
    ConstSelf,
    MutSelf,
    Field { name: String, type_ref: JsonType },
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonExternValue {
    /// Visibility
    pub visibility: JsonVisibility,
    /// Variable name
    pub name: String,
    /// Type
    pub type_ref: JsonType,
    /// Memory address
    pub address: usize,
    /// Documentation
    #[serde(default)]
    pub doc: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_links: Vec<JsonDocLink>,
    /// Source location (file and line)
    #[serde(default)]
    pub source: Option<JsonSourceLocation>,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum JsonType {
    Raw {
        path: String,
    },
    /// A generic type instantiation, e.g., `SharedPtr<GameObject>`
    Generic {
        base: String,
        args: Vec<JsonType>,
    },
    /// A type parameter reference, e.g., `T` inside a generic type definition
    TypeParameter {
        name: String,
    },
    ConstPointer {
        inner: Box<JsonType>,
    },
    MutPointer {
        inner: Box<JsonType>,
    },
    Array {
        inner: Box<JsonType>,
        size: usize,
    },
    Function {
        calling_convention: JsonCallingConvention,
        arguments: Vec<JsonFunctionArgument>,
        return_type: Option<Box<JsonType>>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonFunctionArgument {
    pub name: String,
    pub type_ref: JsonType,
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize, specta::Type)]
#[serde(rename_all = "snake_case")]
pub enum JsonVisibility {
    Public,
    Private,
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize, specta::Type)]
#[serde(rename_all = "snake_case")]
pub enum JsonItemCategory {
    Defined,
    Predefined,
    Extern,
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize, specta::Type)]
#[serde(rename_all = "snake_case")]
pub enum JsonCallingConvention {
    C,
    Cdecl,
    Stdcall,
    Fastcall,
    Thiscall,
    Vectorcall,
    System,
}

/// Convert a doc comment slice to an optional string.
/// Returns Some(joined_string) if the slice is non-empty, None otherwise.
fn doc_to_option(doc: &[String]) -> Option<String> {
    if doc.is_empty() {
        None
    } else {
        Some(doc.join("\n"))
    }
}

// Conversion functions from semantic types to JSON types
impl From<Visibility> for JsonVisibility {
    fn from(v: Visibility) -> Self {
        match v {
            Visibility::Public => JsonVisibility::Public,
            Visibility::Private => JsonVisibility::Private,
        }
    }
}

impl From<ItemCategory> for JsonItemCategory {
    fn from(c: ItemCategory) -> Self {
        match c {
            ItemCategory::Defined => JsonItemCategory::Defined,
            ItemCategory::Predefined => JsonItemCategory::Predefined,
            ItemCategory::Extern => JsonItemCategory::Extern,
        }
    }
}

impl From<CallingConvention> for JsonCallingConvention {
    fn from(cc: CallingConvention) -> Self {
        match cc {
            CallingConvention::C => JsonCallingConvention::C,
            CallingConvention::Cdecl => JsonCallingConvention::Cdecl,
            CallingConvention::Stdcall => JsonCallingConvention::Stdcall,
            CallingConvention::Fastcall => JsonCallingConvention::Fastcall,
            CallingConvention::Thiscall => JsonCallingConvention::Thiscall,
            CallingConvention::Vectorcall => JsonCallingConvention::Vectorcall,
            CallingConvention::System => JsonCallingConvention::System,
        }
    }
}

/// Convert an ItemLocation to JsonSourceLocation, returning None for internal/synthetic locations
fn convert_location(location: &crate::span::ItemLocation) -> Option<JsonSourceLocation> {
    if location.file_id != FileId::INTERNAL && location.span.start.line > 0 {
        Some(JsonSourceLocation {
            file_index: location.file_id.index(),
            line: location.span.start.line,
        })
    } else {
        None
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
    for module in semantic_state.modules().values() {
        let bindings: BTreeMap<&str, ExternBindings> = module.extern_bindings().collect();
        let cx = DocCx {
            resolver: semantic_state.doc_link_resolver(),
            scope: module.scope(),
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

    // Build module hierarchy
    let modules = build_module_hierarchy(semantic_state);

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

/// Return the [`specta::TypeCollection`] for the JSON documentation.
pub fn export_types() -> specta::TypeCollection {
    specta::export()
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

fn convert_function(func: &Function, cx: &DocCx) -> JsonFunction {
    let (doc, doc_links) = cx.convert(&func.doc);
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

fn convert_cfg(pred: &crate::parser::cfg::CfgPredicate) -> JsonCfg {
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
    let (doc, doc_links) = cx.convert(&region.doc);

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

    let (doc, doc_links) = cx.convert(&td.doc);
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
    }
}

fn convert_enum_variant(variant: &EnumVariant, cx: &DocCx) -> JsonEnumVariant {
    let (doc, doc_links) = cx.convert(&variant.doc);
    JsonEnumVariant {
        name: variant.name.clone(),
        value: variant.value,
        doc,
        doc_links,
        source: convert_location(&variant.location),
    }
}

fn convert_enum_definition(ed: &EnumDefinition, cx: &DocCx) -> JsonEnumDefinition {
    let (doc, doc_links) = cx.convert(&ed.doc);
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
    }
}

fn convert_bitflag_field(flag: &BitflagField, cx: &DocCx) -> JsonBitflag {
    let (doc, doc_links) = cx.convert(&flag.doc);
    JsonBitflag {
        name: flag.name.clone(),
        value: flag.value,
        doc,
        doc_links,
        source: convert_location(&flag.location),
    }
}

fn convert_bitflags_definition(bd: &BitflagsDefinition, cx: &DocCx) -> JsonBitflagsDefinition {
    let (doc, doc_links) = cx.convert(&bd.doc);
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
    }
}

fn convert_type_alias_definition(ta: &TypeAliasDefinition, cx: &DocCx) -> JsonTypeAliasDefinition {
    let (doc, doc_links) = cx.convert(&ta.doc);
    JsonTypeAliasDefinition {
        doc,
        doc_links,
        target: convert_type(&ta.target),
    }
}

fn convert_item(
    item: &ItemDefinition,
    type_registry: &TypeRegistry,
    binding: ExternBindings,
    cx: &DocCx,
) -> Option<JsonItem> {
    let resolved = item.resolved()?;

    let kind = match &resolved.inner {
        ItemDefinitionInner::Type(td) => {
            JsonItemKind::Type(convert_type_definition(td, type_registry, cx))
        }
        ItemDefinitionInner::Enum(ed) => JsonItemKind::Enum(convert_enum_definition(ed, cx)),
        ItemDefinitionInner::Bitflags(bd) => {
            JsonItemKind::Bitflags(convert_bitflags_definition(bd, cx))
        }
        ItemDefinitionInner::TypeAlias(ta) => {
            JsonItemKind::TypeAlias(convert_type_alias_definition(ta, cx))
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

fn convert_extern_value(ev: &ExternValue, cx: &DocCx) -> JsonExternValue {
    let (doc, doc_links) = cx.convert(&ev.doc);
    JsonExternValue {
        visibility: ev.visibility.into(),
        name: ev.name.clone(),
        type_ref: convert_type(&ev.type_),
        address: ev.address,
        doc,
        doc_links,
        source: convert_location(&ev.location),
    }
}

fn convert_backend(backend: &Backend) -> JsonBackend {
    // Mirror the IR's `BackendSplice { header, definition }` shape.
    // `definition` is cpp-only (semantic validation rejects it for any
    // other backend) and ends up `None` for rust/json. A splice with
    // both fields empty round-trips as `None` so consumers can skip
    // rendering it entirely.
    let convert_splice = |splice: &crate::semantic::types::BackendSplice| {
        if splice.is_empty() {
            None
        } else {
            Some(JsonBackendSplice {
                header: splice.header.clone(),
                definition: splice.definition.clone(),
                for_type: splice.for_type.as_ref().map(|p| p.to_string()),
            })
        }
    };
    JsonBackend {
        prologue: convert_splice(&backend.prologue),
        epilogue: convert_splice(&backend.epilogue),
    }
}

/// Build the module hierarchy from a flat list of modules
fn build_module_hierarchy(semantic_state: &SemanticOutput) -> BTreeMap<String, JsonModule> {
    let mut root_modules: BTreeMap<String, JsonModule> = BTreeMap::new();

    for (module_path, module) in semantic_state.modules() {
        let segments: Vec<String> = module_path
            .to_string()
            .split("::")
            .map(|s| s.to_string())
            .collect();

        let cx = DocCx {
            resolver: semantic_state.doc_link_resolver(),
            scope: module.scope(),
        };

        // Items, externs, and functions are all emitted regardless of
        // any `#[cfg(...)]` predicate: consumers read the predicate off
        // each item/function and decide for themselves how to render.
        let items: Vec<String> = module
            .definitions(semantic_state.type_registry())
            .map(|item| item.path.to_string())
            .collect();
        let extern_values: Vec<JsonExternValue> = module
            .extern_values
            .iter()
            .map(|e| convert_extern_value(e, &cx))
            .collect();
        let functions: Vec<JsonFunction> = module
            .functions()
            .iter()
            .map(|f| convert_function(f, &cx))
            .collect();

        // Convert backends. Map the typed `crate::Backend` key back to its
        // canonical lower-case string for JSON output.
        let backends: BTreeMap<String, Vec<JsonBackend>> = module
            .backends
            .iter()
            .map(|(name, backend_list)| {
                (
                    name.name().to_string(),
                    backend_list.iter().map(convert_backend).collect(),
                )
            })
            .collect();

        let (doc, doc_links) = cx.convert(module.doc());
        let json_module = JsonModule {
            doc,
            doc_links,
            items,
            submodules: BTreeMap::new(),
            extern_values,
            functions,
            backends,
            source: convert_location(module.location()),
        };

        // Insert into hierarchy
        if segments.is_empty() || (segments.len() == 1 && segments[0].is_empty()) {
            // Root module
            root_modules.insert("root".to_string(), json_module);
        } else if segments.len() == 1 {
            // Top-level module (e.g., "clock", "game")
            root_modules.insert(segments[0].clone(), json_module);
        } else {
            // Nested module - navigate to the right place in the hierarchy
            let root_name = segments[0].clone();
            let mut current = root_modules
                .entry(root_name.clone())
                .or_insert_with(|| JsonModule {
                    doc: None,
                    doc_links: vec![],
                    items: vec![],
                    submodules: BTreeMap::new(),
                    extern_values: vec![],
                    functions: vec![],
                    backends: BTreeMap::new(),
                    source: None,
                });

            for (i, segment) in segments.iter().enumerate().skip(1) {
                if i == segments.len() - 1 {
                    // Last segment, insert the module here
                    current
                        .submodules
                        .insert(segment.clone(), json_module.clone());
                    break;
                } else {
                    // Navigate deeper
                    current = current
                        .submodules
                        .entry(segment.clone())
                        .or_insert_with(|| JsonModule {
                            doc: None,
                            doc_links: vec![],
                            items: vec![],
                            submodules: BTreeMap::new(),
                            extern_values: vec![],
                            functions: vec![],
                            backends: BTreeMap::new(),
                            source: None,
                        });
                }
            }
        }
    }

    root_modules
}
