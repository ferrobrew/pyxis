#![allow(clippy::result_large_err)]

use crate::grammar::ItemPath;
use crate::source_store::SourceStore;
use crate::span::{ErrorContext, ErrorLabelColor, Span};
use ariadne::{Color, Label, Report, ReportKind, Source};
use std::fmt;

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
    /// Missing required attribute
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
        type_name: String,
        message: String,
        context: ErrorContext,
    },
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
        message: String,
        context: ErrorContext,
    },
    /// Field or region error
    FieldError {
        field_name: String,
        item_path: ItemPath,
        message: String,
        context: ErrorContext,
    },
    /// Size mismatch
    SizeMismatch {
        expected: usize,
        actual: usize,
        item_path: ItemPath,
        is_min_size: bool,
        context: ErrorContext,
    },
    /// Alignment error
    AlignmentError {
        item_path: ItemPath,
        message: String,
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
        message: String,
        context: ErrorContext,
    },
    /// Enum or bitflags error
    EnumError {
        item_path: ItemPath,
        message: String,
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
        message: String,
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
    /// IO error
    Io {
        message: String,
        path: Option<String>,
    },
    /// Parser error (wrapped)
    Parser(crate::parser::ParseError),
}

impl SemanticError {
    /// Convert ErrorLabelColor to ariadne Color
    fn label_color_to_ariadne(color: ErrorLabelColor) -> Color {
        match color {
            ErrorLabelColor::Red => Color::Red,
            ErrorLabelColor::Yellow => Color::Yellow,
            ErrorLabelColor::Blue => Color::Blue,
            ErrorLabelColor::Cyan => Color::Cyan,
        }
    }

    /// Format location prefix for error messages
    fn format_location(context: &ErrorContext) -> String {
        match (&context.filename, &context.span) {
            (Some(f), Some(s)) => format!("Error at {}:{}:{}: ", f, s.start.line, s.start.column),
            (Some(f), None) => format!("Error in {}: ", f),
            _ => String::new(),
        }
    }

    pub fn module_not_found(path: ItemPath) -> Self {
        Self::ModuleNotFound {
            path,
            context: ErrorContext::new(),
        }
    }

    pub fn type_not_found(path: ItemPath) -> Self {
        Self::TypeNotFound {
            path,
            context: ErrorContext::new(),
        }
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
            context: ErrorContext::new(),
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
            context: ErrorContext::new(),
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
            context: ErrorContext::new(),
        }
    }

    pub fn type_resolution_failed(
        type_name: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self::TypeResolutionFailed {
            type_name: type_name.into(),
            message: message.into(),
            context: ErrorContext::new(),
        }
    }

    pub fn invalid_type(
        expected: impl Into<String>,
        found: impl Into<String>,
        item_path: ItemPath,
        message: impl Into<String>,
    ) -> Self {
        Self::InvalidType {
            expected: expected.into(),
            found: found.into(),
            item_path,
            message: message.into(),
            context: ErrorContext::new(),
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
            context: ErrorContext::new(),
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
            context: ErrorContext::new(),
        }
    }

    pub fn alignment_error(item_path: ItemPath, message: impl Into<String>) -> Self {
        Self::AlignmentError {
            item_path,
            message: message.into(),
            context: ErrorContext::new(),
        }
    }

    pub fn duplicate_definition(
        name: impl Into<String>,
        item_path: ItemPath,
        message: impl Into<String>,
    ) -> Self {
        Self::DuplicateDefinition {
            name: name.into(),
            item_path,
            message: message.into(),
            context: ErrorContext::new(),
        }
    }

    pub fn enum_error(item_path: ItemPath, message: impl Into<String>) -> Self {
        Self::EnumError {
            item_path,
            message: message.into(),
            context: ErrorContext::new(),
        }
    }

    pub fn integer_conversion(
        value: impl Into<String>,
        target_type: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self::IntegerConversion {
            value: value.into(),
            target_type: target_type.into(),
            message: message.into(),
            context: ErrorContext::new(),
        }
    }

    pub fn attribute_not_supported(
        attribute_name: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self::AttributeNotSupported {
            attribute_name: attribute_name.into(),
            message: message.into(),
            context: ErrorContext::new(),
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
            context: ErrorContext::new(),
        }
    }

    /// Convert a span's location to a byte offset in the source
    fn span_to_offset(source: &str, span: &Span) -> usize {
        let mut offset = 0;
        let mut current_line = 1;
        let mut current_col = 1;

        for ch in source.chars() {
            if current_line == span.start.line && current_col == span.start.column {
                return offset;
            }

            if ch == '\n' {
                current_line += 1;
                current_col = 1;
            } else {
                current_col += 1;
            }

            offset += ch.len_utf8();
        }

        offset
    }

    /// Get the length of a span in bytes
    fn span_length(source: &str, span: &Span) -> usize {
        let start_offset = Self::span_to_offset(source, span);
        let mut offset = start_offset;
        let mut current_line = span.start.line;
        let mut current_col = span.start.column;

        for ch in source[start_offset..].chars() {
            if current_line == span.end.line && current_col == span.end.column {
                return offset - start_offset;
            }

            if ch == '\n' {
                current_line += 1;
                current_col = 1;
            } else {
                current_col += 1;
            }

            offset += ch.len_utf8();
        }

        offset - start_offset
    }

    /// Format the error using ariadne with the provided source store
    pub fn format_with_ariadne(&self, source_store: &mut dyn SourceStore) -> String {
        // Extract context from the error if available
        let context = match self {
            SemanticError::ModuleNotFound { context, .. } => Some(context),
            SemanticError::TypeNotFound { context, .. } => Some(context),
            SemanticError::MissingAttribute { context, .. } => Some(context),
            SemanticError::InvalidAttributeValue { context, .. } => Some(context),
            SemanticError::ConflictingAttributes { context, .. } => Some(context),
            SemanticError::TypeResolutionFailed { context, .. } => Some(context),
            SemanticError::TypeResolutionStalled { .. } => None,
            SemanticError::InvalidType { context, .. } => Some(context),
            SemanticError::FieldError { context, .. } => Some(context),
            SemanticError::SizeMismatch { context, .. } => Some(context),
            SemanticError::AlignmentError { context, .. } => Some(context),
            SemanticError::VftableMustBeFirst { context, .. } => Some(context),
            SemanticError::DuplicateDefinition { context, .. } => Some(context),
            SemanticError::FunctionMissingImplementation { context, .. } => Some(context),
            SemanticError::InvalidCallingConvention { context, .. } => Some(context),
            SemanticError::AttributeNotSupported { context, .. } => Some(context),
            SemanticError::EnumError { context, .. } => Some(context),
            SemanticError::DefaultableError { context, .. } => Some(context),
            SemanticError::IntegerConversion { context, .. } => Some(context),
            SemanticError::OverlappingRegions { context, .. } => Some(context),
            SemanticError::Io { .. } => None,
            SemanticError::Parser(_) => None,
        };

        // If we have both filename and span, try to get the source and create an ariadne report
        if let Some(ctx) = context {
            if let (Some(filename), Some(span)) = (&ctx.filename, &ctx.span) {
                if let Some(source) = source_store.get(filename.as_ref()) {
                    let offset = Self::span_to_offset(source, span);
                    let length = Self::span_length(source, span);

                    // Build the report with the primary label
                    let mut report_builder = Report::build(ReportKind::Error, filename.as_ref(), offset)
                        .with_message(self.to_string())
                        .with_label(
                            Label::new((filename.as_ref(), offset..offset + length))
                                .with_message("error occurred here")
                                .with_color(Color::Red),
                        );

                    // Add additional labels if present
                    for label in &ctx.labels {
                        let label_offset = Self::span_to_offset(source, &label.span);
                        let label_length = Self::span_length(source, &label.span);
                        report_builder = report_builder.with_label(
                            Label::new((filename.as_ref(), label_offset..label_offset + label_length))
                                .with_message(&label.message)
                                .with_color(Self::label_color_to_ariadne(label.color)),
                        );
                    }

                    // Add notes if present
                    for note in &ctx.notes {
                        report_builder = report_builder.with_note(note);
                    }

                    // Add help text if present
                    if let Some(help) = &ctx.help {
                        report_builder = report_builder.with_help(help);
                    }

                    let report = report_builder.finish();

                    let mut buffer = Vec::new();
                    if report.write((filename.as_ref(), Source::from(source)), &mut buffer).is_ok() {
                        return String::from_utf8_lossy(&buffer).to_string();
                    }
                }
            }
        }

        // Fall back to the Display implementation
        self.to_string()
    }

}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemanticError::ModuleNotFound { path, context } => {
                write!(f, "{}Module not found: `{}`", Self::format_location(context), path)
            }
            SemanticError::TypeNotFound { path, context } => {
                write!(f, "{}Type not found: `{}`", Self::format_location(context), path)
            }
            SemanticError::MissingAttribute {
                attribute_name,
                item_kind,
                item_path,
                context,
            } => {
                let loc = Self::format_location(context);
                // Extract module name from item_path for better error message
                let path_str = item_path.to_string();
                if let Some(module_name) = path_str.split("::").next() {
                    // For extern types, show module name
                    if item_kind.contains("extern") {
                        let type_name = path_str.split("::").last().unwrap_or(&path_str);
                        write!(
                            f,
                            "{}failed to find `{}` attribute for {} `{}` in module `{}`",
                            loc, attribute_name, item_kind, type_name, module_name
                        )
                    } else {
                        write!(
                            f,
                            "{}Missing required attribute `{}` for {} `{}`",
                            loc, attribute_name, item_kind, item_path
                        )
                    }
                } else {
                    write!(
                        f,
                        "{}Missing required attribute `{}` for {} `{}`",
                        loc, attribute_name, item_kind, item_path
                    )
                }
            }
            SemanticError::InvalidAttributeValue {
                attribute_name,
                expected_type,
                item_path,
                context,
            } => {
                write!(
                    f,
                    "{}Invalid value for attribute `{}` (expected {}) in `{}`",
                    Self::format_location(context),
                    attribute_name, expected_type, item_path
                )
            }
            SemanticError::ConflictingAttributes {
                attr1,
                attr2,
                item_path,
                context,
            } => {
                write!(
                    f,
                    "{}cannot specify both `{}` and `{}` attributes for type `{}`",
                    Self::format_location(context),
                    attr1, attr2, item_path
                )
            }
            SemanticError::TypeResolutionFailed {
                type_name,
                message,
                context,
            } => {
                write!(f, "{}Failed to resolve type `{}`: {}", Self::format_location(context), type_name, message)
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
                message,
                context,
            } => {
                let loc = Self::format_location(context);
                // Special formatting for bitflags definition
                if message == "bitflags definition" {
                    // Add "a" or "an" before the expected type
                    let article = if expected.starts_with(|c: char| "aeiouAEIOU".contains(c)) {
                        "an"
                    } else {
                        "a"
                    };
                    write!(
                        f,
                        "{}bitflags definition `{}` has a type that is not {} {}: {}",
                        loc, item_path, article, expected, found
                    )
                } else {
                    write!(
                        f,
                        "{}Invalid type for `{}` in `{}`: expected {}, found {}",
                        loc, message, item_path, expected, found
                    )
                }
            }
            SemanticError::FieldError {
                field_name,
                item_path,
                message,
                context,
            } => {
                let loc = Self::format_location(context);
                // Special formatting for vftable errors
                if field_name == "vftable" && message.contains("vftable") {
                    write!(f, "{}while processing `{}`\n{}", loc, item_path, message)
                } else {
                    write!(
                        f,
                        "{}Error in field `{}` of `{}`: {}",
                        loc, field_name, item_path, message
                    )
                }
            }
            SemanticError::SizeMismatch {
                expected,
                actual,
                item_path,
                is_min_size,
                context,
            } => {
                let loc = Self::format_location(context);
                if *is_min_size {
                    write!(
                        f,
                        "{}Size {} for `{}` is less than minimum size {}",
                        loc, actual, item_path, expected
                    )
                } else {
                    write!(
                        f,
                        "{}while processing `{}`\ncalculated size {} for type `{}` does not match target size {}; is your target size correct?",
                        loc, item_path, actual, item_path, expected
                    )
                }
            }
            SemanticError::AlignmentError {
                item_path, message, context
            } => {
                let loc = Self::format_location(context);
                // Check for specific alignment error patterns and format accordingly
                if message.contains("not a multiple") {
                    write!(f, "{}{}", loc, message.replace("{path}", &item_path.to_string()))
                } else {
                    write!(f, "{}{}", loc, message)
                }
            }
            SemanticError::VftableMustBeFirst { item_path, context } => {
                write!(
                    f,
                    "{}Vftable field must be the first field in type `{}`",
                    Self::format_location(context),
                    item_path
                )
            }
            SemanticError::DuplicateDefinition {
                name,
                item_path,
                message,
                context,
            } => {
                write!(
                    f,
                    "{}Duplicate definition of `{}` in `{}` ({})",
                    Self::format_location(context),
                    name, item_path, message
                )
            }
            SemanticError::FunctionMissingImplementation { function_name, context } => {
                write!(
                    f,
                    "{}Function `{}` has no implementation (missing address attribute?)",
                    Self::format_location(context),
                    function_name
                )
            }
            SemanticError::InvalidCallingConvention {
                convention,
                function_name,
                context,
            } => {
                write!(
                    f,
                    "{}Invalid calling convention `{}` for function `{}`",
                    Self::format_location(context),
                    convention, function_name
                )
            }
            SemanticError::AttributeNotSupported {
                attribute_name,
                message,
                context,
            } => {
                write!(
                    f,
                    "{}Attribute `{}` is not supported for {}",
                    Self::format_location(context),
                    attribute_name, message
                )
            }
            SemanticError::EnumError {
                item_path, message, context
            } => {
                let loc = Self::format_location(context);
                // Check if this is for an enum or bitflags based on message content
                // "variant" indicates enum, "value" indicates bitflags
                if message.contains("variant") || message.contains("enum") {
                    write!(f, "{}enum `{}` {}", loc, item_path, message)
                } else if message.contains("value") || message.contains("bitflags") {
                    write!(f, "{}bitflags `{}` {}", loc, item_path, message)
                } else {
                    write!(f, "{}Error in enum `{}`: {}", loc, item_path, message)
                }
            }
            SemanticError::DefaultableError {
                field_name,
                item_path,
                message,
                context,
            } => {
                // Format as "field `{field}` of type `{path}` {message}"
                write!(
                    f,
                    "{}field `{}` of type `{}` {}",
                    Self::format_location(context),
                    field_name, item_path, message
                )
            }
            SemanticError::IntegerConversion {
                value,
                target_type,
                message,
                context,
            } => {
                write!(
                    f,
                    "{}Failed to convert `{}` to {} in {}",
                    Self::format_location(context),
                    value, target_type, message
                )
            }
            SemanticError::OverlappingRegions {
                item_path,
                region_name,
                address,
                existing_end,
                context,
            } => {
                write!(
                    f,
                    "{}Overlapping regions in `{}`: attempted to insert padding at {:#x}, but overlapped with existing region `{}` that ends at {:#x}",
                    Self::format_location(context),
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
#[allow(clippy::result_large_err)]
pub type Result<T> = std::result::Result<T, SemanticError>;
