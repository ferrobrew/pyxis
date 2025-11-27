#![allow(clippy::result_large_err)]

use crate::{
    grammar::{self, ItemPath},
    semantic::types::{CallingConvention, Type as SemanticType},
    source_store::SourceStore,
    span::{self, ErrorContext, ItemLocation, Span},
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
                write!(f, "virtual function `{}`", function_name)
            }
            AttributeNotSupportedContext::NonVirtualFunction { function_name } => {
                write!(f, "non-virtual function `{}`", function_name)
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
                write!(f, "alignment for base type of enum `{}`", enum_path)
            }
            TypeResolutionContext::BitflagsBaseTypeAlignment { bitflags_path } => {
                write!(f, "alignment for base type of bitflags `{}`", bitflags_path)
            }
            TypeResolutionContext::ExternValue { extern_name } => {
                write!(f, "extern value `{}`", extern_name)
            }
            TypeResolutionContext::FunctionArgument {
                argument_name,
                function_name,
            } => {
                write!(
                    f,
                    "argument `{}` in function `{}`",
                    argument_name, function_name
                )
            }
        }
    }
}

/// Context for integer conversion errors
#[derive(Debug, Clone)]
pub enum IntegerConversionContext {
    /// Converting address attribute for a function
    AddressAttribute { function_name: String },
    /// Converting size attribute for an extern type
    ExternSizeAttribute {
        type_name: String,
        module_name: String,
    },
    /// Converting align attribute for an extern type
    ExternAlignAttribute {
        type_name: String,
        module_name: String,
    },
    /// Converting size attribute for a type
    SizeAttribute { type_path: ItemPath },
    /// Converting min_size attribute for a type
    MinSizeAttribute { type_path: ItemPath },
    /// Converting singleton attribute for a type
    SingletonAttribute { type_path: ItemPath },
    /// Converting align attribute for a type
    AlignAttribute { type_path: ItemPath },
    /// Converting address attribute for a field
    FieldAddressAttribute {
        field_name: String,
        type_path: ItemPath,
    },
    /// Converting bitflags value
    BitflagsValue {
        field_name: String,
        type_path: ItemPath,
    },
}

impl fmt::Display for IntegerConversionContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntegerConversionContext::AddressAttribute { function_name } => {
                write!(f, "address attribute for function `{}`", function_name)
            }
            IntegerConversionContext::ExternSizeAttribute {
                type_name,
                module_name,
            } => {
                write!(
                    f,
                    "size attribute for extern type `{}` in module `{}`",
                    type_name, module_name
                )
            }
            IntegerConversionContext::ExternAlignAttribute {
                type_name,
                module_name,
            } => {
                write!(
                    f,
                    "align attribute for extern type `{}` in module `{}`",
                    type_name, module_name
                )
            }
            IntegerConversionContext::SizeAttribute { type_path } => {
                write!(f, "size attribute for type `{}`", type_path)
            }
            IntegerConversionContext::MinSizeAttribute { type_path } => {
                write!(f, "min_size attribute for type `{}`", type_path)
            }
            IntegerConversionContext::SingletonAttribute { type_path } => {
                write!(f, "singleton attribute for type `{}`", type_path)
            }
            IntegerConversionContext::AlignAttribute { type_path } => {
                write!(f, "align attribute for type `{}`", type_path)
            }
            IntegerConversionContext::FieldAddressAttribute {
                field_name,
                type_path,
            } => {
                write!(
                    f,
                    "address attribute for field `{}` of type `{}`",
                    field_name, type_path
                )
            }
            IntegerConversionContext::BitflagsValue {
                field_name,
                type_path,
            } => {
                write!(f, "bitflags value for `{}` of `{}`", field_name, type_path)
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
        context: ErrorContext,
    },
    /// Failed to find a type in the registry
    TypeNotFound {
        path: ItemPath,
        context: ErrorContext,
    },
    /// Missing required attribute for extern type
    MissingExternAttribute {
        attribute_name: String,
        extern_kind: String,
        type_name: String,
        module_name: String,
        context: ErrorContext,
    },
    /// Missing required attribute (generic)
    MissingAttribute {
        attribute_name: String,
        item_kind: String,
        item_path: ItemPath,
        context: ErrorContext,
    },
    /// Invalid attribute value
    InvalidAttributeValue {
        attribute_name: String,
        expected_type: String,
        item_path: ItemPath,
        context: ErrorContext,
    },
    /// Conflicting attributes
    ConflictingAttributes {
        attr1: String,
        attr2: String,
        item_path: ItemPath,
        context: ErrorContext,
    },
    /// Type resolution failed
    TypeResolutionFailed {
        type_: grammar::Type,
        resolution_context: TypeResolutionContext,
        context: ErrorContext,
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
        context: ErrorContext,
    },
    /// Invalid type for context (generic)
    InvalidType {
        expected: String,
        found: String,
        item_path: ItemPath,
        context_description: String,
        context: ErrorContext,
    },
    /// Vftable is missing functions from base class
    VftableMissingFunctions {
        item_path: ItemPath,
        base_name: String,
        expected_count: usize,
        actual_count: usize,
        context: ErrorContext,
    },
    /// Vftable function mismatch with base class
    VftableFunctionMismatch {
        item_path: ItemPath,
        base_name: String,
        index: usize,
        derived_function: String,
        base_function: String,
        context: ErrorContext,
    },
    /// Field or region error (generic)
    FieldError {
        field_name: String,
        item_path: ItemPath,
        message: String,
        context: ErrorContext,
    },
    /// Calculated size is below minimum required size
    SizeBelowMinimum {
        minimum_size: usize,
        actual_size: usize,
        item_path: ItemPath,
        context: ErrorContext,
    },
    /// Calculated size doesn't match target size
    SizeMismatch {
        expected: usize,
        actual: usize,
        item_path: ItemPath,
        context: ErrorContext,
    },
    /// Type alignment is below minimum required alignment
    AlignmentBelowMinimum {
        alignment: usize,
        required_alignment: usize,
        item_path: ItemPath,
        context: ErrorContext,
    },
    /// Field is not properly aligned
    FieldNotAligned {
        field_name: String,
        item_path: ItemPath,
        address: usize,
        required_alignment: usize,
        context: ErrorContext,
    },
    /// Type size is not a multiple of its alignment
    SizeNotAlignmentMultiple {
        size: usize,
        alignment: usize,
        item_path: ItemPath,
        context: ErrorContext,
    },
    /// Vftable must be first field
    VftableMustBeFirst {
        item_path: ItemPath,
        context: ErrorContext,
    },
    /// Duplicate definition
    DuplicateDefinition {
        name: String,
        item_path: ItemPath,
        message: String,
        context: ErrorContext,
    },
    /// Function missing implementation
    FunctionMissingImplementation {
        function_name: String,
        context: ErrorContext,
    },
    /// Invalid calling convention
    InvalidCallingConvention {
        convention: String,
        function_name: String,
        context: ErrorContext,
    },
    /// Attribute not supported in context
    AttributeNotSupported {
        attribute_name: String,
        attribute_context: AttributeNotSupportedContext,
        context: ErrorContext,
    },
    /// Unsupported enum value for a case
    EnumUnsupportedValue {
        item_path: ItemPath,
        case_name: String,
        context: ErrorContext,
    },
    /// Enum has multiple default variants
    EnumMultipleDefaults {
        item_path: ItemPath,
        context: ErrorContext,
    },
    /// Enum has a default variant set but is not marked as defaultable
    EnumDefaultWithoutDefaultable {
        item_path: ItemPath,
        context: ErrorContext,
    },
    /// Enum is marked as defaultable but has no default variant set
    EnumDefaultableMissingDefault {
        item_path: ItemPath,
        context: ErrorContext,
    },
    /// Unsupported bitflags value for a case
    BitflagsUnsupportedValue {
        item_path: ItemPath,
        case_name: String,
        context: ErrorContext,
    },
    /// Bitflags has multiple default values
    BitflagsMultipleDefaults {
        item_path: ItemPath,
        context: ErrorContext,
    },
    /// Bitflags has a default value set but is not marked as defaultable
    BitflagsDefaultWithoutDefaultable {
        item_path: ItemPath,
        context: ErrorContext,
    },
    /// Bitflags is marked as defaultable but has no default value set
    BitflagsDefaultableMissingDefault {
        item_path: ItemPath,
        context: ErrorContext,
    },
    /// Defaultable type error
    DefaultableError {
        field_name: String,
        item_path: ItemPath,
        message: String,
        context: ErrorContext,
    },
    /// Integer conversion error
    IntegerConversion {
        value: String,
        target_type: String,
        conversion_context: IntegerConversionContext,
        context: ErrorContext,
    },
    /// Overlapping regions
    OverlappingRegions {
        item_path: ItemPath,
        region_name: String,
        address: usize,
        existing_end: usize,
        context: ErrorContext,
    },
}

impl SemanticError {
    pub fn module_not_found(path: ItemPath, location: ItemLocation) -> Self {
        Self::ModuleNotFound {
            path,
            context: location.into(),
        }
    }

    pub fn type_not_found(path: ItemPath, location: ItemLocation) -> Self {
        Self::TypeNotFound {
            path,
            context: location.into(),
        }
    }

    pub fn missing_extern_attribute(
        attribute_name: impl Into<String>,
        extern_kind: impl Into<String>,
        type_name: impl Into<String>,
        module_name: impl Into<String>,
        location: ItemLocation,
    ) -> Self {
        Self::MissingExternAttribute {
            attribute_name: attribute_name.into(),
            extern_kind: extern_kind.into(),
            type_name: type_name.into(),
            module_name: module_name.into(),
            context: location.into(),
        }
    }

    pub fn missing_attribute(
        attribute_name: impl Into<String>,
        item_kind: impl Into<String>,
        item_path: ItemPath,
        location: ItemLocation,
    ) -> Self {
        Self::MissingAttribute {
            attribute_name: attribute_name.into(),
            item_kind: item_kind.into(),
            item_path,
            context: location.into(),
        }
    }

    pub fn invalid_attribute_value(
        attribute_name: impl Into<String>,
        expected_type: impl Into<String>,
        item_path: ItemPath,
        location: ItemLocation,
    ) -> Self {
        Self::InvalidAttributeValue {
            attribute_name: attribute_name.into(),
            expected_type: expected_type.into(),
            item_path,
            context: location.into(),
        }
    }

    pub fn conflicting_attributes(
        attr1: impl Into<String>,
        attr2: impl Into<String>,
        item_path: ItemPath,
        location: ItemLocation,
    ) -> Self {
        Self::ConflictingAttributes {
            attr1: attr1.into(),
            attr2: attr2.into(),
            item_path,
            context: location.into(),
        }
    }

    pub fn type_resolution_failed(
        type_: grammar::Type,
        resolution_context: TypeResolutionContext,
        location: ItemLocation,
    ) -> Self {
        Self::TypeResolutionFailed {
            type_,
            resolution_context,
            context: location.into(),
        }
    }

    pub fn bitflags_invalid_type(
        expected: BitflagsExpectedType,
        found: SemanticType,
        item_path: ItemPath,
        location: ItemLocation,
    ) -> Self {
        Self::BitflagsInvalidType {
            expected,
            found,
            item_path,
            context: location.into(),
        }
    }

    pub fn invalid_type(
        expected: impl Into<String>,
        found: impl Into<String>,
        item_path: ItemPath,
        context_description: impl Into<String>,
        location: ItemLocation,
    ) -> Self {
        Self::InvalidType {
            expected: expected.into(),
            found: found.into(),
            item_path,
            context_description: context_description.into(),
            context: location.into(),
        }
    }

    pub fn vftable_missing_functions(
        item_path: ItemPath,
        base_name: impl Into<String>,
        expected_count: usize,
        actual_count: usize,
        location: ItemLocation,
    ) -> Self {
        Self::VftableMissingFunctions {
            item_path,
            base_name: base_name.into(),
            expected_count,
            actual_count,
            context: location.into(),
        }
    }

    pub fn vftable_function_mismatch(
        item_path: ItemPath,
        base_name: impl Into<String>,
        index: usize,
        derived_function: impl Into<String>,
        base_function: impl Into<String>,
        location: ItemLocation,
    ) -> Self {
        Self::VftableFunctionMismatch {
            item_path,
            base_name: base_name.into(),
            index,
            derived_function: derived_function.into(),
            base_function: base_function.into(),
            context: location.into(),
        }
    }

    pub fn field_error(
        field_name: impl Into<String>,
        item_path: ItemPath,
        message: impl Into<String>,
        location: ItemLocation,
    ) -> Self {
        Self::FieldError {
            field_name: field_name.into(),
            item_path,
            message: message.into(),
            context: location.into(),
        }
    }

    pub fn size_below_minimum(
        minimum_size: usize,
        actual_size: usize,
        item_path: ItemPath,
        location: ItemLocation,
    ) -> Self {
        Self::SizeBelowMinimum {
            minimum_size,
            actual_size,
            item_path,
            context: location.into(),
        }
    }

    pub fn size_mismatch(
        expected: usize,
        actual: usize,
        item_path: ItemPath,
        location: ItemLocation,
    ) -> Self {
        Self::SizeMismatch {
            expected,
            actual,
            item_path,
            context: location.into(),
        }
    }

    pub fn alignment_below_minimum(
        alignment: usize,
        required_alignment: usize,
        item_path: ItemPath,
        location: ItemLocation,
    ) -> Self {
        Self::AlignmentBelowMinimum {
            alignment,
            required_alignment,
            item_path,
            context: location.into(),
        }
    }

    pub fn field_not_aligned(
        field_name: impl Into<String>,
        item_path: ItemPath,
        address: usize,
        required_alignment: usize,
        location: ItemLocation,
    ) -> Self {
        Self::FieldNotAligned {
            field_name: field_name.into(),
            item_path,
            address,
            required_alignment,
            context: location.into(),
        }
    }

    pub fn size_not_alignment_multiple(
        size: usize,
        alignment: usize,
        item_path: ItemPath,
        location: ItemLocation,
    ) -> Self {
        Self::SizeNotAlignmentMultiple {
            size,
            alignment,
            item_path,
            context: location.into(),
        }
    }

    pub fn duplicate_definition(
        name: impl Into<String>,
        item_path: ItemPath,
        message: impl Into<String>,
        location: ItemLocation,
    ) -> Self {
        Self::DuplicateDefinition {
            name: name.into(),
            item_path,
            message: message.into(),
            context: location.into(),
        }
    }

    pub fn enum_unsupported_value(
        item_path: ItemPath,
        case_name: impl Into<String>,
        location: ItemLocation,
    ) -> Self {
        Self::EnumUnsupportedValue {
            item_path,
            case_name: case_name.into(),
            context: location.into(),
        }
    }

    pub fn enum_multiple_defaults(item_path: ItemPath, location: ItemLocation) -> Self {
        Self::EnumMultipleDefaults {
            item_path,
            context: location.into(),
        }
    }

    pub fn enum_default_without_defaultable(item_path: ItemPath, location: ItemLocation) -> Self {
        Self::EnumDefaultWithoutDefaultable {
            item_path,
            context: ErrorContext::from(location),
        }
    }

    pub fn enum_defaultable_missing_default(item_path: ItemPath, location: ItemLocation) -> Self {
        Self::EnumDefaultableMissingDefault {
            item_path,
            context: ErrorContext::from(location),
        }
    }

    pub fn bitflags_unsupported_value(
        item_path: ItemPath,
        case_name: impl Into<String>,
        location: ItemLocation,
    ) -> Self {
        Self::BitflagsUnsupportedValue {
            item_path,
            case_name: case_name.into(),
            context: location.into(),
        }
    }

    pub fn bitflags_multiple_defaults(item_path: ItemPath, location: ItemLocation) -> Self {
        Self::BitflagsMultipleDefaults {
            item_path,
            context: location.into(),
        }
    }

    pub fn bitflags_default_without_defaultable(
        item_path: ItemPath,
        location: ItemLocation,
    ) -> Self {
        Self::BitflagsDefaultWithoutDefaultable {
            item_path,
            context: ErrorContext::from(location),
        }
    }

    pub fn bitflags_defaultable_missing_default(
        item_path: ItemPath,
        location: ItemLocation,
    ) -> Self {
        Self::BitflagsDefaultableMissingDefault {
            item_path,
            context: ErrorContext::from(location),
        }
    }

    pub fn integer_conversion(
        value: impl Into<String>,
        target_type: impl Into<String>,
        conversion_context: IntegerConversionContext,
        location: ItemLocation,
    ) -> Self {
        Self::IntegerConversion {
            value: value.into(),
            target_type: target_type.into(),
            conversion_context,
            context: location.into(),
        }
    }

    pub fn attribute_not_supported(
        attribute_name: impl Into<String>,
        attribute_context: AttributeNotSupportedContext,
        location: ItemLocation,
    ) -> Self {
        Self::AttributeNotSupported {
            attribute_name: attribute_name.into(),
            attribute_context,
            context: location.into(),
        }
    }

    pub fn defaultable_error(
        field_name: impl Into<String>,
        item_path: ItemPath,
        message: impl Into<String>,
        location: ItemLocation,
    ) -> Self {
        Self::DefaultableError {
            field_name: field_name.into(),
            item_path,
            message: message.into(),
            context: location.into(),
        }
    }

    pub fn function_missing_implementation(
        function_name: impl Into<String>,
        location: ItemLocation,
    ) -> Self {
        Self::FunctionMissingImplementation {
            function_name: function_name.into(),
            context: location.into(),
        }
    }

    pub fn invalid_calling_convention(
        convention: impl Into<String>,
        function_name: impl Into<String>,
        location: ItemLocation,
    ) -> Self {
        Self::InvalidCallingConvention {
            convention: convention.into(),
            function_name: function_name.into(),
            context: ErrorContext::from(location),
        }
    }

    pub fn vftable_must_be_first(item_path: ItemPath, location: ItemLocation) -> Self {
        Self::VftableMustBeFirst {
            item_path,
            context: location.into(),
        }
    }

    pub fn overlapping_regions(
        item_path: ItemPath,
        region_name: impl Into<String>,
        address: usize,
        existing_end: usize,
        location: ItemLocation,
    ) -> Self {
        Self::OverlappingRegions {
            item_path,
            region_name: region_name.into(),
            address,
            existing_end,
            context: location.into(),
        }
    }

    /// Returns the core error message without location prefix
    pub fn error_message(&self) -> String {
        match self {
            SemanticError::ModuleNotFound { path, .. } => {
                std::format!("Module not found: `{}`", path)
            }
            SemanticError::TypeNotFound { path, .. } => {
                std::format!("Type not found: `{}`", path)
            }
            SemanticError::MissingExternAttribute {
                attribute_name,
                extern_kind,
                type_name,
                module_name,
                ..
            } => {
                std::format!(
                    "failed to find `{}` attribute for {} `{}` in module `{}`",
                    attribute_name,
                    extern_kind,
                    type_name,
                    module_name
                )
            }
            SemanticError::MissingAttribute {
                attribute_name,
                item_kind,
                item_path,
                ..
            } => {
                std::format!(
                    "Missing required attribute `{}` for {} `{}`",
                    attribute_name,
                    item_kind,
                    item_path
                )
            }
            SemanticError::InvalidAttributeValue {
                attribute_name,
                expected_type,
                item_path,
                ..
            } => {
                std::format!(
                    "Invalid value for attribute `{}` (expected {}) in `{}`",
                    attribute_name,
                    expected_type,
                    item_path
                )
            }
            SemanticError::ConflictingAttributes {
                attr1,
                attr2,
                item_path,
                ..
            } => {
                std::format!(
                    "cannot specify both `{}` and `{}` attributes for type `{}`",
                    attr1,
                    attr2,
                    item_path
                )
            }
            SemanticError::TypeResolutionFailed {
                type_,
                resolution_context,
                ..
            } => {
                std::format!(
                    "Failed to resolve type `{}` for {}",
                    type_,
                    resolution_context
                )
            }
            SemanticError::TypeResolutionStalled {
                unresolved_types,
                resolved_types,
            } => {
                let unresolved_quoted: Vec<String> = unresolved_types
                    .iter()
                    .map(|s| std::format!("\"{}\"", s))
                    .collect();
                let resolved_quoted: Vec<String> = resolved_types
                    .iter()
                    .map(|s| std::format!("\"{}\"", s))
                    .collect();
                std::format!(
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
                std::format!(
                    "bitflags definition `{}` has a type that is not {}: {}",
                    item_path,
                    expected,
                    found
                )
            }
            SemanticError::InvalidType {
                expected,
                found,
                item_path,
                context_description,
                ..
            } => {
                std::format!(
                    "Invalid type for `{}` in `{}`: expected {}, found {}",
                    context_description,
                    item_path,
                    expected,
                    found
                )
            }
            SemanticError::VftableMissingFunctions {
                item_path,
                base_name,
                expected_count,
                actual_count,
                ..
            } => {
                std::format!(
                    "vftable for `{}` has {} functions but base class `{}` requires at least {}",
                    item_path,
                    actual_count,
                    base_name,
                    expected_count
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
                std::format!(
                    "vftable for `{}` has function `{}` at index {} but base class `{}` has function `{}`",
                    item_path,
                    derived_function,
                    index,
                    base_name,
                    base_function
                )
            }
            SemanticError::FieldError {
                field_name,
                item_path,
                message,
                ..
            } => {
                std::format!(
                    "Error in field `{}` of `{}`: {}",
                    field_name,
                    item_path,
                    message
                )
            }
            SemanticError::SizeBelowMinimum {
                minimum_size,
                actual_size,
                item_path,
                ..
            } => {
                std::format!(
                    "Size {} for `{}` is less than minimum size {}",
                    actual_size,
                    item_path,
                    minimum_size
                )
            }
            SemanticError::SizeMismatch {
                expected,
                actual,
                item_path,
                ..
            } => {
                std::format!(
                    "while processing `{}`\ncalculated size {} for type `{}` does not match target size {}; is your target size correct?",
                    item_path,
                    actual,
                    item_path,
                    expected
                )
            }
            SemanticError::AlignmentBelowMinimum {
                alignment,
                required_alignment,
                item_path,
                ..
            } => {
                std::format!(
                    "alignment {} is less than minimum required alignment {} for type `{}`",
                    alignment,
                    required_alignment,
                    item_path
                )
            }
            SemanticError::FieldNotAligned {
                field_name,
                item_path,
                address,
                required_alignment,
                ..
            } => {
                std::format!(
                    "field `{}` of type `{}` is located at {:#x}, which is not divisible by {} (the alignment of the type of the field)",
                    field_name,
                    item_path,
                    address,
                    required_alignment
                )
            }
            SemanticError::SizeNotAlignmentMultiple {
                size,
                alignment,
                item_path,
                ..
            } => {
                std::format!(
                    "the type `{}` has a size of {}, which is not a multiple of its alignment {}",
                    item_path,
                    size,
                    alignment
                )
            }
            SemanticError::VftableMustBeFirst { item_path, .. } => {
                std::format!(
                    "Vftable field must be the first field in type `{}`",
                    item_path
                )
            }
            SemanticError::DuplicateDefinition {
                name,
                item_path,
                message,
                ..
            } => {
                std::format!(
                    "Duplicate definition of `{}` in `{}` ({})",
                    name,
                    item_path,
                    message
                )
            }
            SemanticError::FunctionMissingImplementation { function_name, .. } => {
                std::format!(
                    "Function `{}` has no implementation (missing address attribute?)",
                    function_name
                )
            }
            SemanticError::InvalidCallingConvention {
                convention,
                function_name,
                ..
            } => {
                std::format!(
                    "Invalid calling convention `{}` for function `{}`",
                    convention,
                    function_name
                )
            }
            SemanticError::AttributeNotSupported {
                attribute_name,
                attribute_context,
                ..
            } => {
                std::format!(
                    "Attribute `{}` is not supported for {}",
                    attribute_name,
                    attribute_context
                )
            }
            SemanticError::EnumUnsupportedValue {
                item_path,
                case_name,
                ..
            } => {
                std::format!(
                    "enum `{}` has an unsupported value for case `{}`",
                    item_path,
                    case_name
                )
            }
            SemanticError::EnumMultipleDefaults { item_path, .. } => {
                std::format!("enum `{}` has multiple default variants", item_path)
            }
            SemanticError::EnumDefaultWithoutDefaultable { item_path, .. } => {
                std::format!(
                    "enum `{}` has a default variant set but is not marked as defaultable",
                    item_path
                )
            }
            SemanticError::EnumDefaultableMissingDefault { item_path, .. } => {
                std::format!(
                    "enum `{}` is marked as defaultable but has no default variant set",
                    item_path
                )
            }
            SemanticError::BitflagsUnsupportedValue {
                item_path,
                case_name,
                ..
            } => {
                std::format!(
                    "bitflags `{}` has an unsupported value for case `{}`",
                    item_path,
                    case_name
                )
            }
            SemanticError::BitflagsMultipleDefaults { item_path, .. } => {
                std::format!("bitflags `{}` has multiple default values", item_path)
            }
            SemanticError::BitflagsDefaultWithoutDefaultable { item_path, .. } => {
                std::format!(
                    "bitflags `{}` has a default value set but is not marked as defaultable",
                    item_path
                )
            }
            SemanticError::BitflagsDefaultableMissingDefault { item_path, .. } => {
                std::format!(
                    "bitflags `{}` is marked as defaultable but has no default value set",
                    item_path
                )
            }
            SemanticError::DefaultableError {
                field_name,
                item_path,
                message,
                ..
            } => {
                std::format!("field `{}` of type `{}` {}", field_name, item_path, message)
            }
            SemanticError::IntegerConversion {
                value,
                target_type,
                conversion_context,
                ..
            } => {
                std::format!(
                    "Failed to convert `{}` to {} in {}",
                    value,
                    target_type,
                    conversion_context
                )
            }
            SemanticError::OverlappingRegions {
                item_path,
                region_name,
                address,
                existing_end,
                ..
            } => {
                std::format!(
                    "Overlapping regions in `{}`: attempted to insert padding at {:#x}, but overlapped with existing region `{}` that ends at {:#x}",
                    item_path,
                    address,
                    region_name,
                    existing_end
                )
            }
        }
    }

    /// Extract the error context if available
    fn error_context(&self) -> Option<&ErrorContext> {
        match self {
            SemanticError::ModuleNotFound { context, .. } => Some(context),
            SemanticError::TypeNotFound { context, .. } => Some(context),
            SemanticError::MissingExternAttribute { context, .. } => Some(context),
            SemanticError::MissingAttribute { context, .. } => Some(context),
            SemanticError::InvalidAttributeValue { context, .. } => Some(context),
            SemanticError::ConflictingAttributes { context, .. } => Some(context),
            SemanticError::TypeResolutionFailed { context, .. } => Some(context),
            SemanticError::TypeResolutionStalled { .. } => None,
            SemanticError::BitflagsInvalidType { context, .. } => Some(context),
            SemanticError::InvalidType { context, .. } => Some(context),
            SemanticError::VftableMissingFunctions { context, .. } => Some(context),
            SemanticError::VftableFunctionMismatch { context, .. } => Some(context),
            SemanticError::FieldError { context, .. } => Some(context),
            SemanticError::SizeBelowMinimum { context, .. } => Some(context),
            SemanticError::SizeMismatch { context, .. } => Some(context),
            SemanticError::AlignmentBelowMinimum { context, .. } => Some(context),
            SemanticError::FieldNotAligned { context, .. } => Some(context),
            SemanticError::SizeNotAlignmentMultiple { context, .. } => Some(context),
            SemanticError::VftableMustBeFirst { context, .. } => Some(context),
            SemanticError::DuplicateDefinition { context, .. } => Some(context),
            SemanticError::FunctionMissingImplementation { context, .. } => Some(context),
            SemanticError::InvalidCallingConvention { context, .. } => Some(context),
            SemanticError::AttributeNotSupported { context, .. } => Some(context),
            SemanticError::EnumUnsupportedValue { context, .. } => Some(context),
            SemanticError::EnumMultipleDefaults { context, .. } => Some(context),
            SemanticError::EnumDefaultWithoutDefaultable { context, .. } => Some(context),
            SemanticError::EnumDefaultableMissingDefault { context, .. } => Some(context),
            SemanticError::BitflagsUnsupportedValue { context, .. } => Some(context),
            SemanticError::BitflagsMultipleDefaults { context, .. } => Some(context),
            SemanticError::BitflagsDefaultWithoutDefaultable { context, .. } => Some(context),
            SemanticError::BitflagsDefaultableMissingDefault { context, .. } => Some(context),
            SemanticError::DefaultableError { context, .. } => Some(context),
            SemanticError::IntegerConversion { context, .. } => Some(context),
            SemanticError::OverlappingRegions { context, .. } => Some(context),
        }
    }

    /// Format the error using ariadne with the provided source store.
    /// Always produces an ariadne-formatted error, even without source code.
    pub fn format_with_ariadne(&self, source_store: &mut dyn SourceStore) -> String {
        let message = self.error_message();
        let context = self.error_context();

        let (offset, length, location, source) = if let Some(ctx) = context
            && let Some(source) = source_store.get(ctx.location.filename.as_ref())
        {
            let location = ctx.location.clone();
            (
                span::span_to_offset(source, &location.span),
                span::span_length(source, &location.span),
                location,
                source,
            )
        } else {
            (0, 0, ItemLocation::new("<unknown>", Span::synthetic()), "")
        };

        // Build the report with the primary label
        let mut report_builder =
            Report::build(ReportKind::Error, location.filename.as_ref(), offset)
                .with_message(&message)
                .with_label(
                    Label::new((location.filename.as_ref(), offset..offset + length))
                        .with_message("error occurred here")
                        .with_color(ariadne::Color::Red),
                );

        // Add helpful context
        match self {
            Self::EnumDefaultWithoutDefaultable { .. } => {
                report_builder = report_builder
                    .with_help("Add the #[defaultable] attribute to the enum declaration")
                    .with_note("Only enums marked as defaultable can have default variants")
            }
            Self::EnumDefaultableMissingDefault { .. } => {
                report_builder = report_builder
                    .with_help("Add the #[default] attribute to one of the enum variants")
                    .with_note(
                        "Defaultable enums must have exactly one variant marked with #[default]",
                    )
            }
            Self::BitflagsDefaultWithoutDefaultable { .. } => {
                report_builder = report_builder
                    .with_help("Add the #[defaultable] attribute to the bitflags declaration")
                    .with_note("Only bitflags marked as defaultable can have default values")
            }
            Self::BitflagsDefaultableMissingDefault { .. } => {
                report_builder = report_builder
                    .with_help("Add the #[default] attribute to one of the bitflags values")
                    .with_note(
                        "Defaultable bitflags must have exactly one value marked with #[default]",
                    )
            }
            Self::InvalidCallingConvention { .. } => {
                let valid_list = CallingConvention::ALL
                    .iter()
                    .map(|cc| cc.as_str())
                    .collect::<Vec<_>>()
                    .join(", ");
                report_builder =
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

                report_builder = report_builder
                    .with_help(format!(
                        "Bitflags must be based on an unsigned integer type: {unsigned_types}",
                    ))
                    .with_note(format!("The type `{found}` is not an unsigned integer"));
            }
            _ => {}
        }

        let report = report_builder.finish();

        let mut buffer = Vec::new();
        if report
            .write(
                (location.filename.as_ref(), Source::from(source)),
                &mut buffer,
            )
            .is_ok()
        {
            return String::from_utf8_lossy(&buffer).to_string();
        }

        // This should never happen, but if ariadne fails completely, return the message
        message
    }
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Get the location prefix if available
        if let Some(ctx) = self.error_context() {
            write!(f, "{}", span::format_error_location(ctx))?;
        }
        // Write the core message
        write!(f, "{}", self.error_message())
    }
}

impl std::error::Error for SemanticError {}

/// Result type for semantic analysis
#[allow(clippy::result_large_err)]
pub type Result<T> = std::result::Result<T, SemanticError>;
