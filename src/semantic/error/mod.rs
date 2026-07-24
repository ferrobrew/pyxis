#![allow(clippy::result_large_err)]

mod context;
mod messages;

pub use context::*;

use crate::{
    grammar::{self, ItemPath},
    semantic::types::{CallingConvention, ItemStateResolved, PredefinedItem, Type as SemanticType},
    source_store::FileStore,
    span::{self, ItemLocation},
};
use ariadne::{Label, Report, ReportKind, Source};
#[cfg(test)]
use pyxis_macros::StripLocations;
use std::fmt;

/// Semantic analysis errors
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub enum SemanticError {
    /// Failed to find a module for a given path
    ModuleNotFound {
        path: ItemPath,
        location: ItemLocation,
    },
    /// Two source files mapped to the same module path. The usual cause is
    /// a folder module (`world/mod.pyxis`) coexisting with a sibling file
    /// of the same name (`world.pyxis`); `mod.pyxis` is the supported way
    /// to attach items/glue to a folder.
    DuplicateModule {
        path: ItemPath,
        location: ItemLocation,
    },
    /// Failed to find a type in the registry
    TypeNotFound {
        path: ItemPath,
        location: ItemLocation,
    },
    /// An intra-doc link in a doc comment (e.g. `[`Type::method`]`) didn't
    /// resolve to a known item or member.
    DocLinkNotFound {
        path: String,
        location: ItemLocation,
    },
    /// Failed to find an item referenced in a use statement
    UseItemNotFound {
        path: ItemPath,
        location: ItemLocation,
    },
    /// A `prologue definition` / `epilogue definition` splice's cfg didn't
    /// resolve cpp-only (it was ungated, or gated so it's active for a
    /// non-cpp backend). Only cpp distinguishes a header from a source file,
    /// so the `definition` modifier is meaningless elsewhere.
    SpliceDefinitionNotCppOnly { location: ItemLocation },
    /// `prologue for <Type>` / `epilogue for <Type>` referenced a type that
    /// doesn't resolve to any known item. The target must name a type
    /// visible from the backend block's module.
    BackendForTargetNotFound {
        target: ItemPath,
        module: ItemPath,
        location: ItemLocation,
    },
    /// `prologue for <Type>` / `epilogue for <Type>` resolved to a type, but
    /// that type is defined in a different module. Attribution is
    /// module-scoped: a splice can only target a type defined in the same
    /// module as the `backend` block.
    BackendForTargetCrossModule {
        target: ItemPath,
        module: ItemPath,
        defined_in: ItemPath,
        location: ItemLocation,
    },
    /// Missing required attribute for extern type
    MissingExternAttribute {
        attribute_name: AttributeName,
        extern_kind: ExternKind,
        type_name: String,
        module_name: String,
        location: ItemLocation,
    },
    /// Missing required attribute (generic)
    MissingAttribute {
        attribute_name: AttributeName,
        extern_kind: ExternKind,
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// A value item (a `const` or an `extern` value) was nested inside a
    /// generic type. The backends can't emit a correctly-qualified
    /// `impl<T> Parent<T>` accessor for it, and a fixed-address / compile-time
    /// value scoped under a per-instantiation generic is semantically murky.
    /// Nesting non-value items (types/enums/bitflags) in a generic is fine.
    ValueItemInGenericParent {
        item_path: ItemPath,
        parent_path: ItemPath,
        location: ItemLocation,
    },
    /// This function-attribute has the wrong number of arguments
    InvalidAttributeFunctionArgumentCount {
        attribute_name: AttributeName,
        expected_count: usize,
        actual_count: usize,
        location: ItemLocation,
    },
    /// Invalid attribute value
    InvalidAttributeValue {
        attribute_name: AttributeName,
        expected_type: String,
        location: ItemLocation,
    },
    /// Conflicting attributes
    ConflictingAttributes {
        attr1: AttributeName,
        attr2: AttributeName,
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
        /// Specific type references that couldn't be resolved (with locations and context)
        unresolved_references: Vec<UnresolvedTypeReference>,
    },
    /// Invalid type for bitflags definition
    BitflagsInvalidType {
        expected: BitflagsExpectedType,
        found: SemanticType,
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Region field type reference is not a raw type
    RegionFieldNotRawType {
        field_name: String,
        item_path: ItemPath,
        found: TypeRefKind,
        location: ItemLocation,
    },
    /// Region field resolved type is not a struct type
    RegionFieldNotStructType {
        field_name: String,
        item_path: ItemPath,
        found: ItemKind,
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
    /// Vftable function index is not strictly ascending
    VftableNonAscendingIndex {
        item_path: ItemPath,
        function_name: String,
        declared_index: usize,
        min_index: usize,
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
        kind: DuplicateDefinitionKind,
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
        attribute_name: AttributeName,
        attribute_context: AttributeNotSupportedContext,
        location: ItemLocation,
    },
    /// An attribute was written with a syntactic form it doesn't accept
    /// (e.g. `#[external_body(...)]` instead of the bare ident
    /// `#[external_body]`). `expected` is a short description of the
    /// supported form for the diagnostic.
    AttributeWrongForm {
        attribute_name: AttributeName,
        expected: String,
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
        kind: DefaultableErrorKind,
        location: ItemLocation,
    },
    /// Copyable type error (field is not copyable)
    CopyableError {
        field_name: String,
        item_path: ItemPath,
        location: ItemLocation,
    },
    /// Cloneable type error (field is not cloneable)
    CloneableError {
        field_name: String,
        item_path: ItemPath,
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
    /// Attempted to access a private item from another module
    PrivateItemAccess {
        /// The path of the private item being accessed
        item_path: ItemPath,
        /// The module from which the access was attempted
        from_module: ItemPath,
        location: ItemLocation,
    },
    /// A const declaration's value type doesn't match its type annotation
    ConstValueTypeMismatch {
        item_path: ItemPath,
        expected: String,
        found: String,
        location: ItemLocation,
    },
    /// `str` type used in a non-const context (fields, function args, etc.)
    StrTypeNotConst { location: ItemLocation },
    /// A field embeds a zero-size type as a concrete member. Backends that
    /// lack zero-size objects would give the member a nonzero footprint,
    /// silently disagreeing with the semantic layout assumption and
    /// shifting every subsequent field. Use `#[min_size(1)]` or an
    /// explicit `#[size(...)]` on the field's type to give it a nonzero
    /// footprint.
    ZeroSizeFieldEmbedding {
        field_name: String,
        item_path: ItemPath,
        field_type: SemanticType,
        location: ItemLocation,
    },
}

impl SemanticError {
    pub fn location(&self) -> Option<&ItemLocation> {
        match self {
            SemanticError::ModuleNotFound { location, .. } => Some(location),
            SemanticError::DuplicateModule { location, .. } => Some(location),
            SemanticError::TypeNotFound { location, .. } => Some(location),
            SemanticError::DocLinkNotFound { location, .. } => Some(location),
            SemanticError::UseItemNotFound { location, .. } => Some(location),
            SemanticError::SpliceDefinitionNotCppOnly { location, .. } => Some(location),
            SemanticError::BackendForTargetNotFound { location, .. } => Some(location),
            SemanticError::BackendForTargetCrossModule { location, .. } => Some(location),
            SemanticError::MissingExternAttribute { location, .. } => Some(location),
            SemanticError::MissingAttribute { location, .. } => Some(location),
            SemanticError::ValueItemInGenericParent { location, .. } => Some(location),
            SemanticError::InvalidAttributeFunctionArgumentCount { location, .. } => Some(location),
            SemanticError::InvalidAttributeValue { location, .. } => Some(location),
            SemanticError::ConflictingAttributes { location, .. } => Some(location),
            SemanticError::TypeResolutionFailed { location, .. } => Some(location),
            SemanticError::TypeResolutionStalled {
                unresolved_references,
                ..
            } => unresolved_references.first().map(|r| &r.location),
            SemanticError::BitflagsInvalidType { location, .. } => Some(location),
            SemanticError::RegionFieldNotRawType { location, .. } => Some(location),
            SemanticError::RegionFieldNotStructType { location, .. } => Some(location),
            SemanticError::VftableMissingFunctions { location, .. } => Some(location),
            SemanticError::VftableFunctionMismatch { location, .. } => Some(location),
            SemanticError::VftableNonAscendingIndex { location, .. } => Some(location),
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
            SemanticError::AttributeWrongForm { location, .. } => Some(location),
            SemanticError::EnumUnsupportedValue { location, .. } => Some(location),
            SemanticError::EnumMultipleDefaults { location, .. } => Some(location),
            SemanticError::EnumDefaultWithoutDefaultable { location, .. } => Some(location),
            SemanticError::EnumDefaultableMissingDefault { location, .. } => Some(location),
            SemanticError::BitflagsUnsupportedValue { location, .. } => Some(location),
            SemanticError::BitflagsMultipleDefaults { location, .. } => Some(location),
            SemanticError::BitflagsDefaultWithoutDefaultable { location, .. } => Some(location),
            SemanticError::BitflagsDefaultableMissingDefault { location, .. } => Some(location),
            SemanticError::DefaultableError { location, .. } => Some(location),
            SemanticError::CopyableError { location, .. } => Some(location),
            SemanticError::CloneableError { location, .. } => Some(location),
            SemanticError::IntegerConversion { location, .. } => Some(location),
            SemanticError::OverlappingRegions { location, .. } => Some(location),
            SemanticError::PrivateItemAccess { location, .. } => Some(location),
            SemanticError::ConstValueTypeMismatch { location, .. } => Some(location),
            SemanticError::StrTypeNotConst { location, .. } => Some(location),
            SemanticError::ZeroSizeFieldEmbedding { location, .. } => Some(location),
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
                let unsigned_types = PredefinedItem::ALL
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
            Self::ZeroSizeFieldEmbedding { field_type, .. } => report_builder
                .with_help(format!(
                    "add `#[min_size(1)]` or an explicit `#[size(...)]` to the definition of \
                     `{field_type}` so it has a nonzero footprint"
                ))
                .with_note(
                    "zero-size types are valid as pointer targets (*const T) but not as embedded fields",
                ),
            _ => report_builder,
        }
    }

    /// Format the error using ariadne with the provided file store.
    /// Always produces an ariadne-formatted error, even without source code.
    pub fn format_with_ariadne(&self, file_store: &FileStore) -> String {
        // Handle TypeResolutionStalled specially to show all unresolved references
        if let SemanticError::TypeResolutionStalled {
            unresolved_references,
            ..
        } = self
        {
            if !unresolved_references.is_empty() {
                return self.format_unresolved_references(file_store, unresolved_references);
            }
        }

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

    /// Format unresolved type references with their source locations
    fn format_unresolved_references(
        &self,
        file_store: &FileStore,
        refs: &[UnresolvedTypeReference],
    ) -> String {
        let mut output = String::new();

        for (i, unresolved_ref) in refs.iter().enumerate() {
            let location = &unresolved_ref.location;
            let filename = file_store.filename(location.file_id);
            let source = file_store.source(location.file_id).unwrap_or_default();

            let offset = span::span_to_offset(&source, &location.span);
            let length = span::span_length(&source, &location.span);

            let message = format!(
                "cannot find type `{}` in this scope",
                unresolved_ref.type_name
            );

            let report = Report::build(ReportKind::Error, (filename, offset..offset + length))
                .with_message(&message)
                .with_label(
                    Label::new((filename, offset..offset + length))
                        .with_message("not found in this scope")
                        .with_color(ariadne::Color::Red),
                )
                .with_note(format!("in {}", unresolved_ref.context))
                .finish();

            let mut buffer = Vec::new();
            report
                .write((filename, Source::from(source)), &mut buffer)
                .expect("writing to Vec should not fail");

            if i > 0 {
                output.push('\n');
            }
            output.push_str(&String::from_utf8_lossy(&buffer));
        }

        output
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

/// Outcome of attempting to build/resolve an item definition.
/// This is more informative than `Option<ItemStateResolved>` as it distinguishes
/// between different reasons for deferral.
#[derive(Debug)]
pub enum BuildOutcome {
    /// Item was successfully resolved
    Resolved(ItemStateResolved),
    /// Item resolution should be deferred (dependency exists but not yet resolved)
    Deferred,
    /// Item resolution failed because a referenced type doesn't exist
    NotFoundType(UnresolvedTypeReference),
}

impl BuildOutcome {
    /// Convert to Option, collapsing Deferred and NotFoundType to None
    pub fn into_option(self) -> Option<ItemStateResolved> {
        match self {
            BuildOutcome::Resolved(t) => Some(t),
            _ => None,
        }
    }

    /// Returns true if this is a NotFoundType outcome
    pub fn is_not_found(&self) -> bool {
        matches!(self, BuildOutcome::NotFoundType(_))
    }

    /// Extract the unresolved reference if this is a NotFoundType outcome
    pub fn unresolved_reference(self) -> Option<UnresolvedTypeReference> {
        match self {
            BuildOutcome::NotFoundType(r) => Some(r),
            _ => None,
        }
    }
}
