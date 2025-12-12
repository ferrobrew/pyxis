#![allow(clippy::result_large_err)]

use crate::{
    grammar::{self, ItemPath},
    semantic::types::{CallingConvention, Type as SemanticType},
    source_store::FileStore,
    span::{self, ItemLocation},
};
use ariadne::{Label, Report, ReportKind, Source};
use std::fmt;

/// Expected type kind for bitflags type validation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BitflagsExpectedType {
    /// Expected a raw type (not a pointer, array, etc.)
    RawType,
    /// Expected a predefined type
    PredefinedType,
    /// Expected an unsigned integer
    UnsignedInteger,
}

impl fmt::Display for BitflagsExpectedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BitflagsExpectedType::RawType => write!(f, "a raw type"),
            BitflagsExpectedType::PredefinedType => write!(f, "a predefined type"),
            BitflagsExpectedType::UnsignedInteger => write!(f, "an unsigned integer"),
        }
    }
}

/// Context for attribute not supported errors
#[derive(Debug, Clone)]
pub enum AttributeNotSupportedContext {
    /// Attribute not supported for a virtual function
    VirtualFunction { function_name: String },
    /// Attribute not supported for a non-virtual function
    NonVirtualFunction { function_name: String },
}

impl fmt::Display for AttributeNotSupportedContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AttributeNotSupportedContext::VirtualFunction { function_name } => {
                write!(f, "virtual function `{function_name}`")
            }
            AttributeNotSupportedContext::NonVirtualFunction { function_name } => {
                write!(f, "non-virtual function `{function_name}`")
            }
        }
    }
}

/// Context for type resolution failures
#[derive(Debug, Clone)]
pub enum TypeResolutionContext {
    /// Resolving alignment for base type of an enum
    EnumBaseTypeAlignment { enum_path: ItemPath },
    /// Resolving alignment for base type of a bitflags
    BitflagsBaseTypeAlignment { bitflags_path: ItemPath },
    /// Resolving type for an extern value
    ExternValue { extern_name: String },
    /// Resolving type for a function argument
    FunctionArgument {
        argument_name: String,
        function_name: String,
    },
}

impl fmt::Display for TypeResolutionContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeResolutionContext::EnumBaseTypeAlignment { enum_path } => {
                write!(f, "alignment for base type of enum `{enum_path}`")
            }
            TypeResolutionContext::BitflagsBaseTypeAlignment { bitflags_path } => {
                write!(f, "alignment for base type of bitflags `{bitflags_path}`")
            }
            TypeResolutionContext::ExternValue { extern_name } => {
                write!(f, "extern value `{extern_name}`")
            }
            TypeResolutionContext::FunctionArgument {
                argument_name,
                function_name,
            } => {
                write!(
                    f,
                    "argument `{argument_name}` in function `{function_name}`"
                )
            }
        }
    }
}

/// Semantic analysis errors
#[derive(Debug, Clone)]
pub enum SemanticError {
    /// Failed to find a module for a given path
    ModuleNotFound {
        path: ItemPath,
        location: ItemLocation,
    },
    /// Failed to find a type in the registry
    TypeNotFound {
        path: ItemPath,
        location: ItemLocation,
    },
    /// Failed to find an item referenced in a use statement
    UseItemNotFound {
        path: ItemPath,
        location: ItemLocation,
    },
    /// Missing required attribute for extern type
    MissingExternAttribute {
        attribute_name: String,
        extern_kind: String,
        type_name: String,
        module_name: String,
        location: ItemLocation,
    },
    /// Missing required attribute (generic)
    MissingAttribute {
        attribute_name: String,
        item_kind: String,
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// This function-attribute has the wrong number of arguments
    InvalidAttributeFunctionArgumentCount {
        attribute_name: String,
        expected_count: usize,
        actual_count: usize,
        location: ItemLocation,
    },
    /// Invalid attribute value
    InvalidAttributeValue {
        attribute_name: String,
        expected_type: String,
        location: ItemLocation,
    },
    /// Conflicting attributes
    ConflictingAttributes {
        attr1: String,
        attr2: String,
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Type resolution failed
    TypeResolutionFailed {
        type_: grammar::Type,
        resolution_context: TypeResolutionContext,
        location: ItemLocation,
    },
    /// Type resolution stalled (circular dependency or missing type)
    TypeResolutionStalled {
        unresolved_types: Vec<String>,
        resolved_types: Vec<String>,
    },
    /// Invalid type for bitflags definition
    BitflagsInvalidType {
        expected: BitflagsExpectedType,
        found: SemanticType,
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Invalid type for context (generic)
    InvalidType {
        expected: String,
        found: String,
        item_path: ItemPath,
        context_description: String,
        location: ItemLocation,
    },
    /// Vftable is missing functions from base class
    VftableMissingFunctions {
        item_path: ItemPath,
        base_name: String,
        expected_count: usize,
        actual_count: usize,
        location: ItemLocation,
    },
    /// Vftable function mismatch with base class
    VftableFunctionMismatch {
        item_path: ItemPath,
        base_name: String,
        index: usize,
        derived_function: String,
        base_function: String,
        location: ItemLocation,
    },
    /// Field or region error (generic)
    FieldError {
        field_name: String,
        item_path: ItemPath,
        message: String,
        location: ItemLocation,
    },
    /// Calculated size is below minimum required size
    SizeBelowMinimum {
        minimum_size: usize,
        actual_size: usize,
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Calculated size doesn't match target size
    SizeMismatch {
        expected: usize,
        actual: usize,
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Type alignment is below minimum required alignment
    AlignmentBelowMinimum {
        alignment: usize,
        required_alignment: usize,
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Field is not properly aligned
    FieldNotAligned {
        field_name: String,
        item_path: ItemPath,
        address: usize,
        required_alignment: usize,
        location: ItemLocation,
    },
    /// Type size is not a multiple of its alignment
    SizeNotAlignmentMultiple {
        size: usize,
        alignment: usize,
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Vftable must be first field
    VftableMustBeFirst {
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Duplicate definition
    DuplicateDefinition {
        name: String,
        item_path: ItemPath,
        message: String,
        location: ItemLocation,
    },
    /// Function missing implementation
    FunctionMissingImplementation {
        function_name: String,
        location: ItemLocation,
    },
    /// Invalid calling convention
    InvalidCallingConvention {
        convention: String,
        function_name: String,
        location: ItemLocation,
    },
    /// Attribute not supported in context
    AttributeNotSupported {
        attribute_name: String,
        attribute_context: AttributeNotSupportedContext,
        location: ItemLocation,
    },
    /// Unsupported enum value for a case
    EnumUnsupportedValue {
        item_path: ItemPath,
        case_name: String,
        location: ItemLocation,
    },
    /// Enum has multiple default variants
    EnumMultipleDefaults {
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Enum has a default variant set but is not marked as defaultable
    EnumDefaultWithoutDefaultable {
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Enum is marked as defaultable but has no default variant set
    EnumDefaultableMissingDefault {
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Unsupported bitflags value for a case
    BitflagsUnsupportedValue {
        item_path: ItemPath,
        case_name: String,
        location: ItemLocation,
    },
    /// Bitflags has multiple default values
    BitflagsMultipleDefaults {
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Bitflags has a default value set but is not marked as defaultable
    BitflagsDefaultWithoutDefaultable {
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Bitflags is marked as defaultable but has no default value set
    BitflagsDefaultableMissingDefault {
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Defaultable type error
    DefaultableError {
        field_name: String,
        item_path: ItemPath,
        message: String,
        location: ItemLocation,
    },
    /// Integer conversion error
    IntegerConversion {
        value: String,
        target_type: String,
        location: ItemLocation,
    },
    /// Overlapping regions
    OverlappingRegions {
        item_path: ItemPath,
        region_name: String,
        address: usize,
        existing_end: usize,
        location: ItemLocation,
    },
}

impl SemanticError {
    /// Returns the core error message without location prefix
    pub fn error_message(&self) -> String {
        match self {
            SemanticError::ModuleNotFound { path, .. } => {
                format!("Module not found: `{path}`")
            }
            SemanticError::TypeNotFound { path, .. } => {
                format!("Type not found: `{path}`")
            }
            SemanticError::UseItemNotFound { path, .. } => {
                format!("Item in use statement not found: `{path}`")
            }
            SemanticError::MissingExternAttribute {
                attribute_name,
                extern_kind,
                type_name,
                module_name,
                ..
            } => {
                format!(
                    "failed to find `{attribute_name}` attribute for {extern_kind} `{type_name}` in module `{module_name}`"
                )
            }
            SemanticError::MissingAttribute {
                attribute_name,
                item_kind,
                item_path,
                ..
            } => {
                format!(
                    "Missing required attribute `{attribute_name}` for {item_kind} `{item_path}`"
                )
            }
            SemanticError::InvalidAttributeFunctionArgumentCount {
                attribute_name,
                expected_count,
                actual_count,
                ..
            } => {
                format!(
                    "Invalid number of arguments for attribute `{attribute_name}`: expected {expected_count}, found {actual_count}"
                )
            }
            SemanticError::InvalidAttributeValue {
                attribute_name,
                expected_type,
                ..
            } => {
                format!("Invalid value for attribute `{attribute_name}` (expected {expected_type})")
            }
            SemanticError::ConflictingAttributes {
                attr1,
                attr2,
                item_path,
                ..
            } => {
                format!(
                    "cannot specify both `{attr1}` and `{attr2}` attributes for type `{item_path}`"
                )
            }
            SemanticError::TypeResolutionFailed {
                type_,
                resolution_context,
                ..
            } => {
                format!("Failed to resolve type `{type_}` for {resolution_context}")
            }
            SemanticError::TypeResolutionStalled {
                unresolved_types,
                resolved_types,
            } => {
                let unresolved_quoted: Vec<String> = unresolved_types
                    .iter()
                    .map(|s| format!("\"{s}\""))
                    .collect();
                let resolved_quoted: Vec<String> =
                    resolved_types.iter().map(|s| format!("\"{s}\"")).collect();
                format!(
                    "type resolution will not terminate, failed on types: [{}] (resolved types: [{}])",
                    unresolved_quoted.join(", "),
                    resolved_quoted.join(", ")
                )
            }
            SemanticError::BitflagsInvalidType {
                expected,
                found,
                item_path,
                ..
            } => {
                format!(
                    "bitflags definition `{item_path}` has a type that is not {expected}: {found}"
                )
            }
            SemanticError::InvalidType {
                expected,
                found,
                item_path,
                context_description,
                ..
            } => {
                format!(
                    "Invalid type for `{context_description}` in `{item_path}`: expected {expected}, found {found}"
                )
            }
            SemanticError::VftableMissingFunctions {
                item_path,
                base_name,
                expected_count,
                actual_count,
                ..
            } => {
                format!(
                    "vftable for `{item_path}` has {actual_count} functions but base class `{base_name}` requires at least {expected_count}"
                )
            }
            SemanticError::VftableFunctionMismatch {
                item_path,
                base_name,
                index,
                derived_function,
                base_function,
                ..
            } => {
                format!(
                    "vftable for `{item_path}` has function `{derived_function}` at index {index} but base class `{base_name}` has function `{base_function}`"
                )
            }
            SemanticError::FieldError {
                field_name,
                item_path,
                message,
                ..
            } => {
                format!("Error in field `{field_name}` of `{item_path}`: {message}")
            }
            SemanticError::SizeBelowMinimum {
                minimum_size,
                actual_size,
                item_path,
                ..
            } => {
                format!(
                    "Size {actual_size} for `{item_path}` is less than minimum size {minimum_size}"
                )
            }
            SemanticError::SizeMismatch {
                expected,
                actual,
                item_path,
                ..
            } => {
                format!(
                    "while processing `{item_path}`\ncalculated size {actual} for type `{item_path}` does not match target size {expected}; is your target size correct?"
                )
            }
            SemanticError::AlignmentBelowMinimum {
                alignment,
                required_alignment,
                item_path,
                ..
            } => {
                format!(
                    "alignment {alignment} is less than minimum required alignment {required_alignment} for type `{item_path}`"
                )
            }
            SemanticError::FieldNotAligned {
                field_name,
                item_path,
                address,
                required_alignment,
                ..
            } => {
                format!(
                    "field `{field_name}` of type `{item_path}` is located at {address:#x}, which is not divisible by {required_alignment} (the alignment of the type of the field)"
                )
            }
            SemanticError::SizeNotAlignmentMultiple {
                size,
                alignment,
                item_path,
                ..
            } => {
                format!(
                    "the type `{item_path}` has a size of {size}, which is not a multiple of its alignment {alignment}"
                )
            }
            SemanticError::VftableMustBeFirst { item_path, .. } => {
                format!("Vftable field must be the first field in type `{item_path}`")
            }
            SemanticError::DuplicateDefinition {
                name,
                item_path,
                message,
                ..
            } => {
                format!("Duplicate definition of `{name}` in `{item_path}` ({message})")
            }
            SemanticError::FunctionMissingImplementation { function_name, .. } => {
                format!(
                    "Function `{function_name}` has no implementation (missing address attribute?)"
                )
            }
            SemanticError::InvalidCallingConvention {
                convention,
                function_name,
                ..
            } => {
                format!("Invalid calling convention `{convention}` for function `{function_name}`")
            }
            SemanticError::AttributeNotSupported {
                attribute_name,
                attribute_context,
                ..
            } => {
                format!("Attribute `{attribute_name}` is not supported for {attribute_context}")
            }
            SemanticError::EnumUnsupportedValue {
                item_path,
                case_name,
                ..
            } => {
                format!("enum `{item_path}` has an unsupported value for case `{case_name}`")
            }
            SemanticError::EnumMultipleDefaults { item_path, .. } => {
                format!("enum `{item_path}` has multiple default variants")
            }
            SemanticError::EnumDefaultWithoutDefaultable { item_path, .. } => {
                format!(
                    "enum `{item_path}` has a default variant set but is not marked as defaultable"
                )
            }
            SemanticError::EnumDefaultableMissingDefault { item_path, .. } => {
                format!(
                    "enum `{item_path}` is marked as defaultable but has no default variant set"
                )
            }
            SemanticError::BitflagsUnsupportedValue {
                item_path,
                case_name,
                ..
            } => {
                format!("bitflags `{item_path}` has an unsupported value for case `{case_name}`")
            }
            SemanticError::BitflagsMultipleDefaults { item_path, .. } => {
                format!("bitflags `{item_path}` has multiple default values")
            }
            SemanticError::BitflagsDefaultWithoutDefaultable { item_path, .. } => {
                format!(
                    "bitflags `{item_path}` has a default value set but is not marked as defaultable"
                )
            }
            SemanticError::BitflagsDefaultableMissingDefault { item_path, .. } => {
                format!(
                    "bitflags `{item_path}` is marked as defaultable but has no default value set"
                )
            }
            SemanticError::DefaultableError {
                field_name,
                item_path,
                message,
                ..
            } => {
                format!("field `{field_name}` of type `{item_path}` {message}")
            }
            SemanticError::IntegerConversion {
                value, target_type, ..
            } => {
                format!("Failed to convert `{value}` to {target_type}")
            }
            SemanticError::OverlappingRegions {
                item_path,
                region_name,
                address,
                existing_end,
                ..
            } => {
                format!(
                    "Overlapping regions in `{item_path}`: attempted to insert padding at {address:#x}, but overlapped with existing region `{region_name}` that ends at {existing_end:#x}"
                )
            }
        }
    }

    fn location(&self) -> Option<&ItemLocation> {
        match self {
            SemanticError::ModuleNotFound { location, .. } => Some(location),
            SemanticError::TypeNotFound { location, .. } => Some(location),
            SemanticError::UseItemNotFound { location, .. } => Some(location),
            SemanticError::MissingExternAttribute { location, .. } => Some(location),
            SemanticError::MissingAttribute { location, .. } => Some(location),
            SemanticError::InvalidAttributeFunctionArgumentCount { location, .. } => Some(location),
            SemanticError::InvalidAttributeValue { location, .. } => Some(location),
            SemanticError::ConflictingAttributes { location, .. } => Some(location),
            SemanticError::TypeResolutionFailed { location, .. } => Some(location),
            SemanticError::TypeResolutionStalled { .. } => None,
            SemanticError::BitflagsInvalidType { location, .. } => Some(location),
            SemanticError::InvalidType { location, .. } => Some(location),
            SemanticError::VftableMissingFunctions { location, .. } => Some(location),
            SemanticError::VftableFunctionMismatch { location, .. } => Some(location),
            SemanticError::FieldError { location, .. } => Some(location),
            SemanticError::SizeBelowMinimum { location, .. } => Some(location),
            SemanticError::SizeMismatch { location, .. } => Some(location),
            SemanticError::AlignmentBelowMinimum { location, .. } => Some(location),
            SemanticError::FieldNotAligned { location, .. } => Some(location),
            SemanticError::SizeNotAlignmentMultiple { location, .. } => Some(location),
            SemanticError::VftableMustBeFirst { location, .. } => Some(location),
            SemanticError::DuplicateDefinition { location, .. } => Some(location),
            SemanticError::FunctionMissingImplementation { location, .. } => Some(location),
            SemanticError::InvalidCallingConvention { location, .. } => Some(location),
            SemanticError::AttributeNotSupported { location, .. } => Some(location),
            SemanticError::EnumUnsupportedValue { location, .. } => Some(location),
            SemanticError::EnumMultipleDefaults { location, .. } => Some(location),
            SemanticError::EnumDefaultWithoutDefaultable { location, .. } => Some(location),
            SemanticError::EnumDefaultableMissingDefault { location, .. } => Some(location),
            SemanticError::BitflagsUnsupportedValue { location, .. } => Some(location),
            SemanticError::BitflagsMultipleDefaults { location, .. } => Some(location),
            SemanticError::BitflagsDefaultWithoutDefaultable { location, .. } => Some(location),
            SemanticError::BitflagsDefaultableMissingDefault { location, .. } => Some(location),
            SemanticError::DefaultableError { location, .. } => Some(location),
            SemanticError::IntegerConversion { location, .. } => Some(location),
            SemanticError::OverlappingRegions { location, .. } => Some(location),
        }
    }

    fn augment_builder<'a, S: ariadne::Span>(
        &self,
        report_builder: ariadne::ReportBuilder<'a, S>,
    ) -> ariadne::ReportBuilder<'a, S> {
        match self {
            Self::EnumDefaultWithoutDefaultable { .. } => report_builder
                .with_help("Add the #[defaultable] attribute to the enum declaration")
                .with_note("Only enums marked as defaultable can have default variants"),
            Self::EnumDefaultableMissingDefault { .. } => report_builder
                .with_help("Add the #[default] attribute to one of the enum variants")
                .with_note(
                    "Defaultable enums must have exactly one variant marked with #[default]",
                ),
            Self::BitflagsDefaultWithoutDefaultable { .. } => report_builder
                .with_help("Add the #[defaultable] attribute to the bitflags declaration")
                .with_note("Only bitflags marked as defaultable can have default values"),
            Self::BitflagsDefaultableMissingDefault { .. } => report_builder
                .with_help("Add the #[default] attribute to one of the bitflags values")
                .with_note(
                    "Defaultable bitflags must have exactly one value marked with #[default]",
                ),
            Self::InvalidCallingConvention { .. } => {
                let valid_list = CallingConvention::ALL
                    .iter()
                    .map(|cc| cc.as_str())
                    .collect::<Vec<_>>()
                    .join(", ");
                report_builder.with_help(format!("Valid calling conventions are: {valid_list}"))
            }
            Self::BitflagsInvalidType { found, .. } => {
                // Generate list of unsigned integer types dynamically
                let unsigned_types = crate::semantic::types::PredefinedItem::ALL
                    .iter()
                    .filter(|item| item.is_unsigned_integer())
                    .map(|item| item.name())
                    .collect::<Vec<_>>()
                    .join(", ");

                report_builder
                    .with_help(format!(
                        "Bitflags must be based on an unsigned integer type: {unsigned_types}",
                    ))
                    .with_note(format!("The type `{found}` is not an unsigned integer"))
            }
            _ => report_builder,
        }
    }

    /// Format the error using ariadne with the provided file store.
    /// Always produces an ariadne-formatted error, even without source code.
    pub fn format_with_ariadne(&self, file_store: &FileStore) -> String {
        let message = self.error_message();
        let location = self.location();

        let (offset, length, filename, source) = if let Some(location) = location {
            let filename = file_store.filename(location.file_id);
            if let Some(source) = file_store.source(location.file_id) {
                (
                    span::span_to_offset(&source, &location.span),
                    span::span_length(&source, &location.span),
                    filename,
                    source,
                )
            } else {
                (0, 0, filename, String::new())
            }
        } else {
            (0, 0, "<unknown>", String::new())
        };

        // Build the report with the primary label
        let mut report_builder =
            Report::build(ReportKind::Error, (filename, offset..offset + length))
                .with_message(&message)
                .with_label(
                    Label::new((filename, offset..offset + length))
                        .with_message("error occurred here")
                        .with_color(ariadne::Color::Red),
                );

        report_builder = self.augment_builder(report_builder);

        let report = report_builder.finish();

        let mut buffer = Vec::new();
        report
            .write((filename, Source::from(source)), &mut buffer)
            .expect("writing to Vec should not fail");

        String::from_utf8_lossy(&buffer).to_string()
    }
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Get the location prefix if available
        if let Some(location) = self.location() {
            write!(f, "{location}: ")?;
        }
        // Write the core message
        write!(f, "{}", self.error_message())
    }
}

impl std::error::Error for SemanticError {}

/// Result type for semantic analysis
#[allow(clippy::result_large_err)]
pub type Result<T> = std::result::Result<T, SemanticError>;
