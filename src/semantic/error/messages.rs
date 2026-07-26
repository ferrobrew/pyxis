use super::SemanticError;

impl SemanticError {
    /// Returns the core error message without location prefix
    pub fn error_message(&self) -> String {
        match self {
            SemanticError::ModuleNotFound { .. }
            | SemanticError::DuplicateModule { .. }
            | SemanticError::TypeNotFound { .. }
            | SemanticError::DocLinkNotFound { .. }
            | SemanticError::UseItemNotFound { .. } => self.resolution_error_message(),

            SemanticError::SpliceDefinitionNotCppOnly { .. }
            | SemanticError::BackendForTargetNotFound { .. }
            | SemanticError::BackendForTargetCrossModule { .. } => self.splice_error_message(),

            SemanticError::MissingExternAttribute { .. }
            | SemanticError::MissingAttribute { .. }
            | SemanticError::ValueItemInGenericParent { .. }
            | SemanticError::InvalidAttributeFunctionArgumentCount { .. }
            | SemanticError::InvalidAttributeValue { .. }
            | SemanticError::ConflictingAttributes { .. }
            | SemanticError::AttributeNotSupported { .. }
            | SemanticError::AttributeWrongForm { .. } => self.attribute_error_message(),

            SemanticError::TypeResolutionFailed { .. }
            | SemanticError::TypeResolutionStalled { .. } => self.type_resolution_error_message(),

            SemanticError::SizeBelowMinimum { .. }
            | SemanticError::SizeMismatch { .. }
            | SemanticError::AlignmentBelowMinimum { .. }
            | SemanticError::FieldNotAligned { .. }
            | SemanticError::SizeNotAlignmentMultiple { .. }
            | SemanticError::OverlappingRegions { .. }
            | SemanticError::ZeroSizeFieldEmbedding { .. } => self.layout_error_message(),

            SemanticError::UnionBaseNotAllowed { .. }
            | SemanticError::UnionVftableNotAllowed { .. }
            | SemanticError::UnionMemberAddress { .. }
            | SemanticError::EmptyUnion { .. }
            | SemanticError::UnionMemberExceedsSize { .. }
            | SemanticError::UnionAnonymousMember { .. }
            | SemanticError::InlineUnionNestedItem { .. }
            | SemanticError::InlineUnionNameCollision { .. } => self.union_error_message(),

            SemanticError::VftableMissingFunctions { .. }
            | SemanticError::VftableFunctionMismatch { .. }
            | SemanticError::VftableNonAscendingIndex { .. }
            | SemanticError::VftableMustBeFirst { .. } => self.vftable_error_message(),

            SemanticError::BitflagsInvalidType { .. }
            | SemanticError::EnumUnsupportedValue { .. }
            | SemanticError::EnumMultipleDefaults { .. }
            | SemanticError::EnumDefaultWithoutDefaultable { .. }
            | SemanticError::EnumDefaultableMissingDefault { .. }
            | SemanticError::BitflagsUnsupportedValue { .. }
            | SemanticError::BitflagsMultipleDefaults { .. }
            | SemanticError::BitflagsDefaultWithoutDefaultable { .. }
            | SemanticError::BitflagsDefaultableMissingDefault { .. } => {
                self.enum_bitflags_error_message()
            }

            SemanticError::RegionFieldNotRawType { .. }
            | SemanticError::RegionFieldNotStructType { .. }
            | SemanticError::DefaultableError { .. }
            | SemanticError::CopyableError { .. }
            | SemanticError::CloneableError { .. } => self.member_error_message(),

            SemanticError::DuplicateDefinition { .. }
            | SemanticError::FunctionMissingImplementation { .. }
            | SemanticError::InvalidCallingConvention { .. }
            | SemanticError::IntegerConversion { .. }
            | SemanticError::PrivateItemAccess { .. }
            | SemanticError::ConstValueTypeMismatch { .. }
            | SemanticError::StrTypeNotConst { .. } => self.item_error_message(),
        }
    }

    /// Module, type, doc-link, and use-item lookup failures.
    fn resolution_error_message(&self) -> String {
        match self {
            SemanticError::ModuleNotFound { path, .. } => {
                format!("Module not found: `{path}`")
            }
            SemanticError::DuplicateModule { path, .. } => {
                format!(
                    "Module `{path}` is defined more than once. A folder module (`mod.pyxis`) cannot coexist with a sibling file of the same name."
                )
            }
            SemanticError::TypeNotFound { path, .. } => {
                format!("Type not found: `{path}`")
            }
            SemanticError::DocLinkNotFound { path, .. } => {
                format!("Doc-comment link could not be resolved: `{path}`")
            }
            SemanticError::UseItemNotFound { path, .. } => {
                format!("Item in use statement not found: `{path}`")
            }
            _ => unreachable!(),
        }
    }

    /// Splice / `backend` block attribution failures.
    fn splice_error_message(&self) -> String {
        match self {
            SemanticError::SpliceDefinitionNotCppOnly { .. } => {
                "`prologue definition` / `epilogue definition` is only valid for cpp; gate the \
                 splice with `#[cfg(backend = \"cpp\")]`"
                    .to_string()
            }
            SemanticError::BackendForTargetNotFound { target, module, .. } => {
                format!(
                    "`for {target}` on a splice in module `{module}` does not resolve to a known type"
                )
            }
            SemanticError::BackendForTargetCrossModule {
                target,
                module,
                defined_in,
                ..
            } => {
                format!(
                    "`for {target}` on a `backend` block in module `{module}` resolves to a type defined in module `{defined_in}`; attribution must target a type defined in the same module"
                )
            }
            _ => unreachable!(),
        }
    }

    /// Attribute presence, form, value, and conflict failures.
    fn attribute_error_message(&self) -> String {
        match self {
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
                extern_kind,
                item_path,
                ..
            } => {
                format!(
                    "Missing required attribute `{attribute_name}` for {extern_kind} `{item_path}`"
                )
            }
            SemanticError::ValueItemInGenericParent {
                item_path,
                parent_path,
                ..
            } => {
                format!(
                    "`{item_path}` cannot be nested inside generic type `{parent_path}`: \
                     `const` and `extern` value items are not supported inside a generic type"
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
            SemanticError::AttributeNotSupported {
                attribute_name,
                attribute_context,
                ..
            } => {
                format!("Attribute `{attribute_name}` is not supported for {attribute_context}")
            }
            SemanticError::AttributeWrongForm {
                attribute_name,
                expected,
                ..
            } => {
                format!("Attribute `{attribute_name}` must be written as {expected}")
            }
            _ => unreachable!(),
        }
    }

    /// Type-resolution failures (single type and whole-graph stalls).
    fn type_resolution_error_message(&self) -> String {
        match self {
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
                unresolved_references,
            } => {
                // If we have specific unresolved references, show those prominently
                if !unresolved_references.is_empty() {
                    let refs_formatted: Vec<String> = unresolved_references
                        .iter()
                        .map(|r| format!("  - {r}"))
                        .collect();
                    format!(
                        "type resolution failed due to unresolved type references:\n{}",
                        refs_formatted.join("\n")
                    )
                } else {
                    // Fallback to the old message if no specific references
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
            }
            _ => unreachable!(),
        }
    }

    /// Size, alignment, region overlap, and layout embedding failures.
    fn layout_error_message(&self) -> String {
        match self {
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
            SemanticError::ZeroSizeFieldEmbedding {
                field_name,
                item_path,
                field_type,
                ..
            } => {
                format!(
                    "field `{field_name}` in `{item_path}` embeds zero-size type `{field_type}`, \
                     which has no representable layout in backends that lack zero-size objects"
                )
            }
            _ => unreachable!(),
        }
    }

    /// Constructs that a union body cannot express.
    fn union_error_message(&self) -> String {
        match self {
            SemanticError::UnionBaseNotAllowed { item_path, .. } => {
                format!(
                    "union `{item_path}` declares a `#[base]` member; a base class must sit at a \
                     known offset, but every union member starts at offset 0"
                )
            }
            SemanticError::UnionVftableNotAllowed { item_path, .. } => {
                format!(
                    "union `{item_path}` declares a `vftable` block; a vftable pointer must occupy \
                     offset 0 exclusively, which a union cannot guarantee"
                )
            }
            SemanticError::UnionMemberAddress {
                member_name,
                item_path,
                ..
            } => {
                format!(
                    "member `{member_name}` of union `{item_path}` has an `#[address]`; every \
                     union member starts at offset 0 by definition"
                )
            }
            SemanticError::EmptyUnion { item_path, .. } => {
                format!("union `{item_path}` has no members, so it has no size")
            }
            SemanticError::UnionMemberExceedsSize {
                member_name,
                member_size,
                declared_size,
                item_path,
                ..
            } => {
                format!(
                    "member `{member_name}` of union `{item_path}` is {member_size} bytes, which \
                     exceeds the union's declared size of {declared_size}"
                )
            }
            SemanticError::UnionAnonymousMember { item_path, .. } => {
                format!(
                    "`{item_path}` has a union member named `_`; `_` marks padding, but every \
                     union member already covers the same bytes as its siblings"
                )
            }
            SemanticError::InlineUnionNestedItem {
                item_name,
                item_path,
                ..
            } => {
                format!(
                    "`{item_name}` is declared inside the inline union field `{item_path}`; inline \
                     unions become module-scope siblings, so a nested item under one would never \
                     be reachable. Declare it in the enclosing type, or give the union a name."
                )
            }
            SemanticError::InlineUnionNameCollision {
                generated_path,
                item_path,
                ..
            } => {
                format!(
                    "building `{item_path}` generates `{generated_path}`, but that name is already \
                     taken. Rename the inline union's field (or, for a vftable, the type itself), \
                     or declare the generated item separately."
                )
            }
            _ => unreachable!(),
        }
    }

    /// Vftable ordering and base-class consistency failures.
    fn vftable_error_message(&self) -> String {
        match self {
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
            SemanticError::VftableNonAscendingIndex {
                item_path,
                function_name,
                declared_index,
                min_index,
                ..
            } => {
                format!(
                    "vftable for `{item_path}`: function `{function_name}` is declared at index {declared_index}, but must be at least {min_index} to avoid overwriting earlier slots (indices must be strictly ascending)"
                )
            }
            SemanticError::VftableMustBeFirst { item_path, .. } => {
                format!("Vftable field must precede all fields in type `{item_path}`")
            }
            _ => unreachable!(),
        }
    }

    /// Enum and bitflags value / default-variant failures.
    fn enum_bitflags_error_message(&self) -> String {
        match self {
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
            _ => unreachable!(),
        }
    }

    /// Region field and derive (defaultable/copyable/cloneable) member failures.
    fn member_error_message(&self) -> String {
        match self {
            SemanticError::RegionFieldNotRawType {
                field_name,
                item_path,
                found,
                ..
            } => {
                format!(
                    "region field `{field_name}` in `{item_path}` must be a raw type, found {found}"
                )
            }
            SemanticError::RegionFieldNotStructType {
                field_name,
                item_path,
                found,
                ..
            } => {
                format!(
                    "region field `{field_name}` in `{item_path}` must reference a struct type, found {found}"
                )
            }
            SemanticError::DefaultableError {
                field_name,
                item_path,
                kind,
                ..
            } => {
                format!("field `{field_name}` of type `{item_path}` {kind}")
            }
            SemanticError::CopyableError {
                field_name,
                item_path,
                ..
            } => {
                format!("field `{field_name}` of type `{item_path}` is not a copyable type")
            }
            SemanticError::CloneableError {
                field_name,
                item_path,
                ..
            } => {
                format!("field `{field_name}` of type `{item_path}` is not a cloneable type")
            }
            _ => unreachable!(),
        }
    }

    /// Item-level failures: definitions, functions, conversions, visibility, consts.
    fn item_error_message(&self) -> String {
        match self {
            SemanticError::DuplicateDefinition {
                name,
                item_path,
                kind,
                ..
            } => {
                format!("Duplicate definition of `{name}` in `{item_path}` ({kind})")
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
            SemanticError::IntegerConversion {
                value, target_type, ..
            } => {
                format!("Failed to convert `{value}` to {target_type}")
            }
            SemanticError::PrivateItemAccess {
                item_path,
                from_module,
                ..
            } => {
                format!("cannot access private item `{item_path}` from module `{from_module}`")
            }
            SemanticError::ConstValueTypeMismatch {
                item_path,
                expected,
                found,
                ..
            } => {
                format!("const `{item_path}` expected {expected}, but the value is {found}")
            }
            SemanticError::StrTypeNotConst { .. } => {
                "`str` type is only allowed on `const` declarations".to_string()
            }
            _ => unreachable!(),
        }
    }
}
