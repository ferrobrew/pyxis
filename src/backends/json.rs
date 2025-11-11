use std::{collections::BTreeMap, path::Path};

use anyhow::Context;
use serde::{Deserialize, Serialize};

use crate::semantic::{
    ResolvedSemanticState, TypeRegistry,
    types::{
        Argument, BitflagsDefinition, CallingConvention, EnumDefinition, ExternValue, Function,
        FunctionBody, ItemCategory, ItemDefinition, ItemDefinitionInner, Region, TypeDefinition,
        TypeVftable, Visibility,
    },
};

// If changing the structure, ensure you rerun `cargo run -- gen-types` to
// update the TypeScript definitions.

/// Top-level JSON documentation structure
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonDocumentation {
    /// Pointer size for the target platform
    pub pointer_size: usize,
    /// Project name
    pub project_name: String,
    /// Map of absolute paths to items
    pub items: BTreeMap<String, JsonItem>,
    /// Nested module hierarchy
    pub modules: BTreeMap<String, JsonModule>,
}

/// A module containing items and potentially submodules
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonModule {
    /// Module documentation
    pub doc: Option<String>,
    /// Items defined directly in this module
    pub items: Vec<String>, // Paths to items
    /// Child modules
    #[specta(inline)]
    pub submodules: BTreeMap<String, JsonModule>,
    /// Extern values (global variables)
    pub extern_values: Vec<JsonExternValue>,
    /// Freestanding functions
    pub functions: Vec<JsonFunction>,
}

/// An item (type, enum, or bitflags) in the documentation
#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonItem {
    /// Item path
    pub path: String,
    /// Visibility
    pub visibility: JsonVisibility,
    /// Size in bytes
    pub size: usize,
    /// Alignment in bytes
    pub alignment: usize,
    /// Item category (Defined, Predefined, Extern)
    pub category: JsonItemCategory,
    /// Item kind and details
    pub kind: JsonItemKind,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum JsonItemKind {
    Type(JsonTypeDefinition),
    Enum(JsonEnumDefinition),
    Bitflags(JsonBitflagsDefinition),
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonTypeDefinition {
    /// Documentation
    pub doc: Option<String>,
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
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonRegion {
    /// Visibility
    pub visibility: JsonVisibility,
    /// Field name
    pub name: Option<String>,
    /// Documentation
    pub doc: Option<String>,
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
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonEnumVariant {
    /// Variant name
    pub name: String,
    /// Variant value
    pub value: isize,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonBitflagsDefinition {
    /// Documentation
    pub doc: Option<String>,
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
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonBitflag {
    /// Flag name
    pub name: String,
    /// Flag value
    pub value: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
pub struct JsonFunction {
    /// Visibility
    pub visibility: JsonVisibility,
    /// Function name
    pub name: String,
    /// Documentation
    pub doc: Option<String>,
    /// Function body (how it's implemented)
    pub body: JsonFunctionBody,
    /// Arguments
    pub arguments: Vec<JsonArgument>,
    /// Return type
    pub return_type: Option<JsonType>,
    /// Calling convention
    pub calling_convention: JsonCallingConvention,
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
}

#[derive(Debug, Clone, Serialize, Deserialize, specta::Type)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum JsonType {
    Raw {
        path: String,
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

/// Generate the JSON documentation for the entire project
pub fn build(
    out_dir: &Path,
    semantic_state: &ResolvedSemanticState,
    project_name: &str,
) -> anyhow::Result<()> {
    let type_registry = semantic_state.type_registry();

    // Build items map
    let mut items = BTreeMap::new();
    for module in semantic_state.modules().values() {
        for definition in module.definitions(type_registry) {
            if let Some(json_item) = convert_item(definition, type_registry) {
                items.insert(json_item.path.clone(), json_item);
            }
        }
    }

    // Build module hierarchy
    let modules = build_module_hierarchy(semantic_state);

    // Create the top-level documentation structure
    let documentation = JsonDocumentation {
        pointer_size: type_registry.pointer_size(),
        project_name: project_name.to_string(),
        items,
        modules,
    };

    // Write to file
    let output_path = out_dir.join("output.json");
    let json_string = serde_json::to_string_pretty(&documentation)
        .context("Failed to serialize JSON documentation")?;
    std::fs::write(&output_path, json_string)
        .context(format!("Failed to write JSON to {:?}", output_path))?;

    Ok(())
}

/// Return the [`specta::TypeCollection`] for the JSON documentation.
pub fn export_types() -> specta::TypeCollection {
    specta::export()
}

fn convert_type(type_ref: &crate::semantic::types::Type) -> JsonType {
    match type_ref {
        crate::semantic::types::Type::Unresolved(_) => {
            // This shouldn't happen in resolved state, but handle it gracefully
            JsonType::Raw {
                path: "unresolved".to_string(),
            }
        }
        crate::semantic::types::Type::Raw(path) => JsonType::Raw {
            path: path.to_string(),
        },
        crate::semantic::types::Type::ConstPointer(inner) => JsonType::ConstPointer {
            inner: Box::new(convert_type(inner)),
        },
        crate::semantic::types::Type::MutPointer(inner) => JsonType::MutPointer {
            inner: Box::new(convert_type(inner)),
        },
        crate::semantic::types::Type::Array(inner, size) => JsonType::Array {
            inner: Box::new(convert_type(inner)),
            size: *size,
        },
        crate::semantic::types::Type::Function(cc, args, return_type) => JsonType::Function {
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
        Argument::ConstSelf => JsonArgument::ConstSelf,
        Argument::MutSelf => JsonArgument::MutSelf,
        Argument::Field(name, type_ref) => JsonArgument::Field {
            name: name.clone(),
            type_ref: convert_type(type_ref),
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
    }
}

fn convert_function(func: &Function) -> JsonFunction {
    JsonFunction {
        visibility: func.visibility.into(),
        name: func.name.clone(),
        doc: func.doc.clone(),
        body: convert_function_body(&func.body),
        arguments: func.arguments.iter().map(convert_argument).collect(),
        return_type: func.return_type.as_ref().map(convert_type),
        calling_convention: func.calling_convention.into(),
    }
}

fn convert_region(region: &Region, type_registry: &TypeRegistry, offset: usize) -> JsonRegion {
    let size = region.type_ref.size(type_registry).unwrap_or(0);
    let alignment = region.type_ref.alignment(type_registry).unwrap_or(1);

    JsonRegion {
        visibility: region.visibility.into(),
        name: region.name.clone(),
        doc: region.doc.clone(),
        type_ref: convert_type(&region.type_ref),
        offset,
        size,
        alignment,
        is_base: region.is_base,
    }
}

fn convert_vftable(vftable: &TypeVftable) -> JsonTypeVftable {
    JsonTypeVftable {
        functions: vftable.functions.iter().map(convert_function).collect(),
    }
}

fn convert_type_definition(
    td: &TypeDefinition,
    type_registry: &TypeRegistry,
) -> JsonTypeDefinition {
    // Calculate field offsets
    let mut current_offset = 0;
    let fields = td
        .regions
        .iter()
        .map(|region| {
            let json_region = convert_region(region, type_registry, current_offset);
            current_offset += json_region.size;
            json_region
        })
        .collect();

    JsonTypeDefinition {
        doc: td.doc.clone(),
        fields,
        associated_functions: td
            .associated_functions
            .iter()
            .map(convert_function)
            .collect(),
        vftable: td.vftable.as_ref().map(convert_vftable),
        singleton: td.singleton,
        copyable: td.copyable,
        cloneable: td.cloneable,
        defaultable: td.defaultable,
        packed: td.packed,
    }
}

fn convert_enum_definition(ed: &EnumDefinition) -> JsonEnumDefinition {
    JsonEnumDefinition {
        doc: ed.doc.clone(),
        underlying_type: convert_type(&ed.type_),
        variants: ed
            .fields
            .iter()
            .map(|(name, value)| JsonEnumVariant {
                name: name.clone(),
                value: *value,
            })
            .collect(),
        associated_functions: ed
            .associated_functions
            .iter()
            .map(convert_function)
            .collect(),
        singleton: ed.singleton,
        copyable: ed.copyable,
        cloneable: ed.cloneable,
        default: ed.default,
    }
}

fn convert_bitflags_definition(bd: &BitflagsDefinition) -> JsonBitflagsDefinition {
    JsonBitflagsDefinition {
        doc: bd.doc.clone(),
        underlying_type: convert_type(&bd.type_),
        flags: bd
            .fields
            .iter()
            .map(|(name, value)| JsonBitflag {
                name: name.clone(),
                value: *value,
            })
            .collect(),
        singleton: bd.singleton,
        copyable: bd.copyable,
        cloneable: bd.cloneable,
        default: bd.default,
    }
}

fn convert_item(item: &ItemDefinition, type_registry: &TypeRegistry) -> Option<JsonItem> {
    let resolved = item.resolved()?;

    let kind = match &resolved.inner {
        ItemDefinitionInner::Type(td) => {
            JsonItemKind::Type(convert_type_definition(td, type_registry))
        }
        ItemDefinitionInner::Enum(ed) => JsonItemKind::Enum(convert_enum_definition(ed)),
        ItemDefinitionInner::Bitflags(bd) => {
            JsonItemKind::Bitflags(convert_bitflags_definition(bd))
        }
    };

    Some(JsonItem {
        path: item.path.to_string(),
        visibility: item.visibility.into(),
        size: resolved.size,
        alignment: resolved.alignment,
        category: item.category.into(),
        kind,
    })
}

fn convert_extern_value(ev: &ExternValue) -> JsonExternValue {
    JsonExternValue {
        visibility: ev.visibility.into(),
        name: ev.name.clone(),
        type_ref: convert_type(&ev.type_),
        address: ev.address,
    }
}

/// Build the module hierarchy from a flat list of modules
fn build_module_hierarchy(semantic_state: &ResolvedSemanticState) -> BTreeMap<String, JsonModule> {
    let mut root_modules: BTreeMap<String, JsonModule> = BTreeMap::new();

    for (module_path, module) in semantic_state.modules() {
        let segments: Vec<String> = module_path
            .to_string()
            .split("::")
            .map(|s| s.to_string())
            .collect();

        // Get items for this module
        let items: Vec<String> = module
            .definitions(semantic_state.type_registry())
            .map(|item| item.path.to_string())
            .collect();

        // Convert extern values and functions
        let extern_values: Vec<JsonExternValue> = module
            .extern_values
            .iter()
            .map(convert_extern_value)
            .collect();
        let functions: Vec<JsonFunction> =
            module.functions().iter().map(convert_function).collect();

        let json_module = JsonModule {
            doc: module.doc().map(|s| s.to_string()),
            items,
            submodules: BTreeMap::new(),
            extern_values,
            functions,
        };

        // Insert into hierarchy
        if segments.is_empty() || (segments.len() == 1 && segments[0].is_empty()) {
            // Root module
            root_modules.insert("root".to_string(), json_module);
        } else {
            // Navigate to the right place in the hierarchy
            let root_name = segments[0].clone();
            let mut current = root_modules
                .entry(root_name.clone())
                .or_insert_with(|| JsonModule {
                    doc: None,
                    items: vec![],
                    submodules: BTreeMap::new(),
                    extern_values: vec![],
                    functions: vec![],
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
                            items: vec![],
                            submodules: BTreeMap::new(),
                            extern_values: vec![],
                            functions: vec![],
                        });
                }
            }
        }
    }

    root_modules
}
