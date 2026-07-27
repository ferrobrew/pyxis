use std::collections::BTreeMap;

use crate::span::FileId;
use serde::{Deserialize, Serialize};

use crate::semantic::types::{CallingConvention, ItemCategory, Visibility};

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
/// - v8: added `Constant` item kind with `JsonConstValue` for int/float/
///   string/enum-value constants.
/// - v9: extern values are item pages (`ExternValue` item kind) instead of a
///   per-module `extern_values` array; nested extern values appear under their
///   parent type's `nested_items`.
/// - v10: replaced the per-module `backends` map (keyed by backend name, with
///   `prologue`/`epilogue` splice objects) with a flat `splices` array. Each
///   `JsonSplice` carries its own `kind`, `cfg` predicate (null = every
///   backend), `definition` flag, `for_type`, and `text`, mirroring the
///   retirement of the `backend { ... }` wrapper in favour of cfg-gated
///   standalone `prologue`/`epilogue` statements.
/// - v11: added `c_string`, `struct`, `array`, and `const_ref` variants to
///   `JsonConstValue` for C-string literals, structured initializers, and
///   constant aliases.
/// - v12: added a `Union` item kind (`JsonUnionDefinition`). Its `fields` are
///   `JsonRegion`s like a type's, but every one has `offset: 0` — a union's
///   members are competing readings of the same bytes, not a sequence.
/// - v13: `JsonFunctionArgument.name` became nullable. Function-pointer types
///   are now writable in any type position (`fn(*mut Engine, f32)`), and a
///   parameter written without a name stays unnamed rather than being given
///   a synthesized one. Vftable-derived signatures still carry their names.
pub const CURRENT_SCHEMA_VERSION: u32 = 13;

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
    /// Explicit re-exports (`pub use`): items imported and re-exported from this
    /// module. Each points at the canonical path of the item being re-exported.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub reexports: Vec<JsonReexport>,
    /// Child modules
    #[specta(inline)]
    pub submodules: BTreeMap<String, JsonModule>,
    /// Freestanding functions
    pub functions: Vec<JsonFunction>,
    /// Standalone `prologue`/`epilogue` splices, in source order, each with
    /// its own optional `cfg` gate.
    pub splices: Vec<JsonSplice>,
    /// Source location (file and line) - None for synthesized/folder modules
    #[serde(default)]
    pub source: Option<JsonSourceLocation>,
}

/// An explicit re-export (`pub use path::Item;`). `name` is the local name the
/// item is re-exported as; `path` is the canonical absolute path of the target
/// item (following any re-export chain), which the viewer links to.
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonReexport {
    /// The local name the item is re-exported as.
    pub name: String,
    /// Canonical absolute path of the re-exported item.
    pub path: String,
}

/// Which end of the module's generated output a splice attaches to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, specta::Type)]
#[serde(rename_all = "snake_case")]
pub enum JsonSpliceKind {
    Prologue,
    Epilogue,
}

/// A standalone `prologue`/`epilogue` splice: raw backend code spliced into
/// the module's generated output.
///
/// `cfg`, when present, gates which backends emit it (`null`/absent = every
/// backend). `definition` routes the splice into the C++ `.cpp` source
/// rather than the header (only meaningful for cpp-gated splices).
/// `for_type`, when set, is the resolved absolute item path this splice is
/// attributed to (`prologue/epilogue for <Type>`); the viewer renders such
/// splices on the owning type's page rather than the module page.
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonSplice {
    /// Whether the splice is a prologue or an epilogue.
    pub kind: JsonSpliceKind,
    /// `#[cfg(...)]` gate; `null`/absent means emitted for every backend.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cfg: Option<JsonCfg>,
    /// Whether this is a `definition` splice (C++ `.cpp` source).
    pub definition: bool,
    /// Resolved absolute item path this splice is attributed to, when tagged
    /// with `for <Type>`. `None`/absent means module-level rendering.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub for_type: Option<String>,
    /// The spliced code text.
    pub text: String,
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
    Union(JsonUnionDefinition),
    TypeAlias(JsonTypeAliasDefinition),
    Constant(JsonConstantDefinition),
    ExternValue(JsonExternValueDefinition),
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonConstantDefinition {
    /// Documentation
    pub doc: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_links: Vec<JsonDocLink>,
    /// The type annotation of the constant
    pub value_type: JsonType,
    /// The compile-time value
    pub value: JsonConstValue,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum JsonConstValue {
    Int { value: isize },
    Float { value: f64 },
    String { value: String },
    CString { value: String },
    EnumValue { path: String },
    Struct { fields: Vec<JsonConstField> },
    Array { elements: Vec<JsonConstValue> },
    ConstRef { path: String },
}

/// A named field in a struct constant initializer.
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonConstField {
    pub name: String,
    pub value: JsonConstValue,
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
    /// Item paths of nested items declared inside this type body
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub nested_items: Vec<String>,
}

/// A union: several readings of the same bytes, only one of which applies at a
/// time. Which one is a property of the surrounding data, not of the union.
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonUnionDefinition {
    /// Documentation
    pub doc: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_links: Vec<JsonDocLink>,
    /// Members. Every one has `offset: 0`; `size` is the member's own size,
    /// which may be smaller than the union's.
    pub fields: Vec<JsonRegion>,
    /// Total size in bytes: the largest member, rounded up to the alignment
    pub size: usize,
    /// Alignment in bytes: the strictest of the members'
    pub alignment: usize,
    /// Whether the union is copyable
    pub copyable: bool,
    /// Whether the union is cloneable
    pub cloneable: bool,
    /// Whether the union is defaultable
    pub defaultable: bool,
    /// Whether the union is packed
    pub packed: bool,
    /// Whether the union is pinned (non-relocatable)
    pub pinned: bool,
    /// Item paths of nested items declared inside this union body
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub nested_items: Vec<String>,
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
    /// Item paths of nested items declared inside this enum body
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub nested_items: Vec<String>,
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
    /// Item paths of nested items declared inside this bitflags body
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub nested_items: Vec<String>,
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

/// An `extern` value item (a global at a fixed address). Emitted as an item
/// page like any other definition; `visibility`, `path`, and `source` live on
/// the enclosing [`JsonItem`].
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonExternValueDefinition {
    /// Documentation
    pub doc: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_links: Vec<JsonDocLink>,
    /// The type annotation of the extern value
    pub value_type: JsonType,
    /// Memory address
    pub address: usize,
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
    /// `None` for a parameter written without a name, as in `fn(u32)`.
    pub name: Option<String>,
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
pub(super) fn doc_to_option(doc: &[String]) -> Option<String> {
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
pub(super) fn convert_location(location: &crate::span::ItemLocation) -> Option<JsonSourceLocation> {
    if location.file_id != FileId::INTERNAL && location.span.start.line > 0 {
        Some(JsonSourceLocation {
            file_index: location.file_id.index(),
            line: location.span.start.line,
        })
    } else {
        None
    }
}

/// Return the [`specta::TypeCollection`] for the JSON documentation.
pub fn export_types() -> specta::TypeCollection {
    specta::export()
}
