use crate::grammar::ItemPath;
use std::fmt;

/// Semantic analysis errors
#[derive(Debug, Clone)]
pub enum SemanticError {
    /// Failed to find a module for a given path
    ModuleNotFound { path: ItemPath },
    /// Failed to find a type in the registry
    TypeNotFound { path: ItemPath },
    /// Missing required attribute
    MissingAttribute {
        attribute_name: String,
        item_kind: String,
        item_path: ItemPath,
    },
    /// Invalid attribute value
    InvalidAttributeValue {
        attribute_name: String,
        expected_type: String,
        item_path: ItemPath,
    },
    /// Conflicting attributes
    ConflictingAttributes {
        attr1: String,
        attr2: String,
        item_path: ItemPath,
    },
    /// Type resolution failed
    TypeResolutionFailed { type_name: String, context: String },
    /// Type resolution stalled (circular dependency or missing type)
    TypeResolutionStalled {
        unresolved_types: Vec<String>,
        resolved_types: Vec<String>,
    },
    /// Invalid type for context
    InvalidType {
        expected: String,
        found: String,
        item_path: ItemPath,
        context: String,
    },
    /// Field or region error
    FieldError {
        field_name: String,
        item_path: ItemPath,
        message: String,
    },
    /// Size mismatch
    SizeMismatch {
        expected: usize,
        actual: usize,
        item_path: ItemPath,
        is_min_size: bool,
    },
    /// Alignment error
    AlignmentError {
        item_path: ItemPath,
        message: String,
    },
    /// Vftable must be first field
    VftableMustBeFirst { item_path: ItemPath },
    /// Duplicate definition
    DuplicateDefinition {
        name: String,
        item_path: ItemPath,
        context: String,
    },
    /// Function missing implementation
    FunctionMissingImplementation { function_name: String },
    /// Invalid calling convention
    InvalidCallingConvention {
        convention: String,
        function_name: String,
    },
    /// Attribute not supported in context
    AttributeNotSupported {
        attribute_name: String,
        context: String,
    },
    /// Enum or bitflags error
    EnumError {
        item_path: ItemPath,
        message: String,
    },
    /// Defaultable type error
    DefaultableError {
        field_name: String,
        item_path: ItemPath,
        message: String,
    },
    /// Integer conversion error
    IntegerConversion {
        value: String,
        target_type: String,
        context: String,
    },
    /// Overlapping regions
    OverlappingRegions {
        item_path: ItemPath,
        region_name: String,
        address: usize,
        existing_end: usize,
    },
    /// IO error
    Io {
        message: String,
        path: Option<String>,
    },
    /// Parser error (wrapped)
    Parser(crate::parser::ParseError),
}

impl SemanticError {
    pub fn module_not_found(path: ItemPath) -> Self {
        Self::ModuleNotFound { path }
    }

    pub fn type_not_found(path: ItemPath) -> Self {
        Self::TypeNotFound { path }
    }

    pub fn missing_attribute(
        attribute_name: impl Into<String>,
        item_kind: impl Into<String>,
        item_path: ItemPath,
    ) -> Self {
        Self::MissingAttribute {
            attribute_name: attribute_name.into(),
            item_kind: item_kind.into(),
            item_path,
        }
    }

    pub fn invalid_attribute_value(
        attribute_name: impl Into<String>,
        expected_type: impl Into<String>,
        item_path: ItemPath,
    ) -> Self {
        Self::InvalidAttributeValue {
            attribute_name: attribute_name.into(),
            expected_type: expected_type.into(),
            item_path,
        }
    }

    pub fn conflicting_attributes(
        attr1: impl Into<String>,
        attr2: impl Into<String>,
        item_path: ItemPath,
    ) -> Self {
        Self::ConflictingAttributes {
            attr1: attr1.into(),
            attr2: attr2.into(),
            item_path,
        }
    }

    pub fn type_resolution_failed(
        type_name: impl Into<String>,
        context: impl Into<String>,
    ) -> Self {
        Self::TypeResolutionFailed {
            type_name: type_name.into(),
            context: context.into(),
        }
    }

    pub fn invalid_type(
        expected: impl Into<String>,
        found: impl Into<String>,
        item_path: ItemPath,
        context: impl Into<String>,
    ) -> Self {
        Self::InvalidType {
            expected: expected.into(),
            found: found.into(),
            item_path,
            context: context.into(),
        }
    }

    pub fn field_error(
        field_name: impl Into<String>,
        item_path: ItemPath,
        message: impl Into<String>,
    ) -> Self {
        Self::FieldError {
            field_name: field_name.into(),
            item_path,
            message: message.into(),
        }
    }

    pub fn size_mismatch(
        expected: usize,
        actual: usize,
        item_path: ItemPath,
        is_min_size: bool,
    ) -> Self {
        Self::SizeMismatch {
            expected,
            actual,
            item_path,
            is_min_size,
        }
    }

    pub fn alignment_error(item_path: ItemPath, message: impl Into<String>) -> Self {
        Self::AlignmentError {
            item_path,
            message: message.into(),
        }
    }

    pub fn duplicate_definition(
        name: impl Into<String>,
        item_path: ItemPath,
        context: impl Into<String>,
    ) -> Self {
        Self::DuplicateDefinition {
            name: name.into(),
            item_path,
            context: context.into(),
        }
    }

    pub fn enum_error(item_path: ItemPath, message: impl Into<String>) -> Self {
        Self::EnumError {
            item_path,
            message: message.into(),
        }
    }

    pub fn integer_conversion(
        value: impl Into<String>,
        target_type: impl Into<String>,
        context: impl Into<String>,
    ) -> Self {
        Self::IntegerConversion {
            value: value.into(),
            target_type: target_type.into(),
            context: context.into(),
        }
    }

    pub fn attribute_not_supported(
        attribute_name: impl Into<String>,
        context: impl Into<String>,
    ) -> Self {
        Self::AttributeNotSupported {
            attribute_name: attribute_name.into(),
            context: context.into(),
        }
    }

    pub fn defaultable_error(
        field_name: impl Into<String>,
        item_path: ItemPath,
        message: impl Into<String>,
    ) -> Self {
        Self::DefaultableError {
            field_name: field_name.into(),
            item_path,
            message: message.into(),
        }
    }
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemanticError::ModuleNotFound { path } => {
                write!(f, "Module not found: `{}`", path)
            }
            SemanticError::TypeNotFound { path } => {
                write!(f, "Type not found: `{}`", path)
            }
            SemanticError::MissingAttribute {
                attribute_name,
                item_kind,
                item_path,
            } => {
                // Extract module name from item_path for better error message
                let path_str = item_path.to_string();
                if let Some(module_name) = path_str.split("::").next() {
                    // For extern types, show module name
                    if item_kind.contains("extern") {
                        let type_name = path_str.split("::").last().unwrap_or(&path_str);
                        write!(
                            f,
                            "failed to find `{}` attribute for {} `{}` in module `{}`",
                            attribute_name, item_kind, type_name, module_name
                        )
                    } else {
                        write!(
                            f,
                            "Missing required attribute `{}` for {} `{}`",
                            attribute_name, item_kind, item_path
                        )
                    }
                } else {
                    write!(
                        f,
                        "Missing required attribute `{}` for {} `{}`",
                        attribute_name, item_kind, item_path
                    )
                }
            }
            SemanticError::InvalidAttributeValue {
                attribute_name,
                expected_type,
                item_path,
            } => {
                write!(
                    f,
                    "Invalid value for attribute `{}` (expected {}) in `{}`",
                    attribute_name, expected_type, item_path
                )
            }
            SemanticError::ConflictingAttributes {
                attr1,
                attr2,
                item_path,
            } => {
                write!(
                    f,
                    "cannot specify both `{}` and `{}` attributes for type `{}`",
                    attr1, attr2, item_path
                )
            }
            SemanticError::TypeResolutionFailed { type_name, context } => {
                write!(f, "Failed to resolve type `{}`: {}", type_name, context)
            }
            SemanticError::TypeResolutionStalled {
                unresolved_types,
                resolved_types,
            } => {
                let unresolved_quoted: Vec<String> = unresolved_types
                    .iter()
                    .map(|s| format!("\"{}\"", s))
                    .collect();
                let resolved_quoted: Vec<String> = resolved_types
                    .iter()
                    .map(|s| format!("\"{}\"", s))
                    .collect();
                write!(
                    f,
                    "type resolution will not terminate, failed on types: [{}] (resolved types: [{}])",
                    unresolved_quoted.join(", "),
                    resolved_quoted.join(", ")
                )
            }
            SemanticError::InvalidType {
                expected,
                found,
                item_path,
                context,
            } => {
                // Special formatting for bitflags definition
                if context == "bitflags definition" {
                    // Add "a" or "an" before the expected type
                    let article = if expected.starts_with(|c: char| "aeiouAEIOU".contains(c)) {
                        "an"
                    } else {
                        "a"
                    };
                    write!(
                        f,
                        "bitflags definition `{}` has a type that is not {} {}: {}",
                        item_path, article, expected, found
                    )
                } else {
                    write!(
                        f,
                        "Invalid type for `{}` in `{}`: expected {}, found {}",
                        context, item_path, expected, found
                    )
                }
            }
            SemanticError::FieldError {
                field_name,
                item_path,
                message,
            } => {
                // Special formatting for vftable errors
                if field_name == "vftable" && message.contains("vftable") {
                    write!(f, "while processing `{}`\n{}", item_path, message)
                } else {
                    write!(
                        f,
                        "Error in field `{}` of `{}`: {}",
                        field_name, item_path, message
                    )
                }
            }
            SemanticError::SizeMismatch {
                expected,
                actual,
                item_path,
                is_min_size,
            } => {
                if *is_min_size {
                    write!(
                        f,
                        "Size {} for `{}` is less than minimum size {}",
                        actual, item_path, expected
                    )
                } else {
                    write!(
                        f,
                        "while processing `{}`\ncalculated size {} for type `{}` does not match target size {}; is your target size correct?",
                        item_path, actual, item_path, expected
                    )
                }
            }
            SemanticError::AlignmentError { item_path, message } => {
                // Check for specific alignment error patterns and format accordingly
                if message.contains("not a multiple") {
                    write!(f, "{}", message.replace("{path}", &item_path.to_string()))
                } else {
                    write!(f, "{}", message)
                }
            }
            SemanticError::VftableMustBeFirst { item_path } => {
                write!(
                    f,
                    "Vftable field must be the first field in type `{}`",
                    item_path
                )
            }
            SemanticError::DuplicateDefinition {
                name,
                item_path,
                context,
            } => {
                write!(
                    f,
                    "Duplicate definition of `{}` in `{}` ({})",
                    name, item_path, context
                )
            }
            SemanticError::FunctionMissingImplementation { function_name } => {
                write!(
                    f,
                    "Function `{}` has no implementation (missing address attribute?)",
                    function_name
                )
            }
            SemanticError::InvalidCallingConvention {
                convention,
                function_name,
            } => {
                write!(
                    f,
                    "Invalid calling convention `{}` for function `{}`",
                    convention, function_name
                )
            }
            SemanticError::AttributeNotSupported {
                attribute_name,
                context,
            } => {
                write!(
                    f,
                    "Attribute `{}` is not supported for {}",
                    attribute_name, context
                )
            }
            SemanticError::EnumError { item_path, message } => {
                // Check if this is for an enum or bitflags based on message content
                // "variant" indicates enum, "value" indicates bitflags
                if message.contains("variant") || message.contains("enum") {
                    write!(f, "enum `{}` {}", item_path, message)
                } else if message.contains("value") || message.contains("bitflags") {
                    write!(f, "bitflags `{}` {}", item_path, message)
                } else {
                    write!(f, "Error in enum `{}`: {}", item_path, message)
                }
            }
            SemanticError::DefaultableError {
                field_name,
                item_path,
                message,
            } => {
                // Format as "field `{field}` of type `{path}` {message}"
                write!(
                    f,
                    "field `{}` of type `{}` {}",
                    field_name, item_path, message
                )
            }
            SemanticError::IntegerConversion {
                value,
                target_type,
                context,
            } => {
                write!(
                    f,
                    "Failed to convert `{}` to {} in {}",
                    value, target_type, context
                )
            }
            SemanticError::OverlappingRegions {
                item_path,
                region_name,
                address,
                existing_end,
            } => {
                write!(
                    f,
                    "Overlapping regions in `{}`: attempted to insert padding at {:#x}, but overlapped with existing region `{}` that ends at {:#x}",
                    item_path, address, region_name, existing_end
                )
            }
            SemanticError::Io { message, path } => {
                if let Some(path) = path {
                    write!(f, "IO error for `{}`: {}", path, message)
                } else {
                    write!(f, "IO error: {}", message)
                }
            }
            SemanticError::Parser(err) => write!(f, "{}", err),
        }
    }
}

impl std::error::Error for SemanticError {}

impl From<crate::parser::ParseError> for SemanticError {
    fn from(err: crate::parser::ParseError) -> Self {
        SemanticError::Parser(err)
    }
}

impl From<std::io::Error> for SemanticError {
    fn from(err: std::io::Error) -> Self {
        SemanticError::Io {
            message: err.to_string(),
            path: None,
        }
    }
}

/// Result type for semantic analysis
pub type Result<T> = std::result::Result<T, SemanticError>;
