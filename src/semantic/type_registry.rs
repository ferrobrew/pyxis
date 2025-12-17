use std::collections::BTreeMap;

use crate::{
    SemanticError,
    grammar::{self, ItemPath},
    semantic::{
        error::Result,
        types::{ItemDefinition, Type},
    },
    span::ItemLocation,
};

/// Result of attempting to look up a type in the registry.
/// This distinguishes between different failure modes for better error reporting.
#[derive(Debug, Clone)]
pub enum TypeLookupResult {
    /// Type was found and resolved successfully
    Found(Type),
    /// Type exists in the registry but is not yet resolved (should defer)
    NotYetResolved,
    /// Type doesn't exist in the registry at all
    NotFound {
        /// The type name that wasn't found (as written in source)
        type_name: String,
    },
}

#[derive(Debug)]
pub struct TypeRegistry {
    types: BTreeMap<ItemPath, ItemDefinition>,
    pointer_size: usize,
}

impl TypeRegistry {
    pub(crate) fn new(pointer_size: usize) -> TypeRegistry {
        TypeRegistry {
            types: BTreeMap::new(),
            pointer_size,
        }
    }

    pub fn pointer_size(&self) -> usize {
        self.pointer_size
    }

    /// Check if a type exists in the registry
    pub fn contains(&self, item_path: &ItemPath) -> bool {
        self.types.contains_key(item_path)
    }

    pub fn get(
        &self,
        item_path: &ItemPath,
        from_location: &ItemLocation,
    ) -> Result<&ItemDefinition> {
        self.types
            .get(item_path)
            .ok_or_else(|| SemanticError::TypeNotFound {
                path: item_path.clone(),
                location: *from_location,
            })
    }

    pub fn get_mut(
        &mut self,
        item_path: &ItemPath,
        from_location: &ItemLocation,
    ) -> Result<&mut ItemDefinition> {
        self.types
            .get_mut(item_path)
            .ok_or_else(|| SemanticError::TypeNotFound {
                path: item_path.clone(),
                location: *from_location,
            })
    }

    pub(crate) fn resolved(&self) -> Vec<ItemPath> {
        self.types
            .iter()
            .filter(|(_, t)| !t.is_predefined() && t.is_resolved())
            .map(|(k, _)| k.clone())
            .collect()
    }

    pub(crate) fn unresolved(&self) -> Vec<ItemPath> {
        self.types
            .iter()
            .filter(|(_, t)| !t.is_predefined() && !t.is_resolved())
            .map(|(k, _)| k.clone())
            .collect()
    }

    pub(crate) fn add(&mut self, type_: ItemDefinition) {
        self.types.insert(type_.path.clone(), type_);
    }

    /// Follows non-generic type aliases recursively to get the final resolved type.
    /// Generic type aliases (those with type_parameters) are NOT followed here,
    /// as they require parameter substitution which is handled separately.
    fn resolve_type_alias(&self, type_: Type) -> Type {
        match &type_ {
            Type::Raw(path) => {
                // Check if this path refers to a non-generic type alias
                if let Some(item_def) = self.types.get(path) {
                    // Only follow non-generic type aliases
                    if item_def.type_parameters.is_empty() {
                        if let Some(resolved) = item_def.resolved() {
                            if let Some(type_alias) = resolved.inner.as_type_alias() {
                                // Return the target type (which is already resolved)
                                return type_alias.target.clone();
                            }
                        }
                    }
                }
                type_
            }
            // For compound types, we don't need to resolve here since the inner types
            // would have been resolved when they were constructed
            _ => type_,
        }
    }

    /// Checks if a path refers to a generic type alias and returns its type parameters
    /// and target type if so.
    fn get_generic_type_alias(
        &self,
        path: &ItemPath,
    ) -> Option<(
        Vec<String>,
        crate::semantic::type_alias_definition::TypeAliasDefinition,
    )> {
        if let Some(item_def) = self.types.get(path) {
            if !item_def.type_parameters.is_empty() {
                if let Some(resolved) = item_def.resolved() {
                    if let Some(type_alias) = resolved.inner.as_type_alias() {
                        return Some((item_def.type_parameters.clone(), type_alias.clone()));
                    }
                }
            }
        }
        None
    }

    /// Substitutes type parameters in a type with concrete types.
    /// `param_names` are the type parameter names, `args` are the concrete types.
    fn substitute_type_params(type_: &Type, param_names: &[String], args: &[Type]) -> Type {
        match type_ {
            Type::TypeParameter(name) => {
                // Find the parameter index and substitute
                if let Some(idx) = param_names.iter().position(|p| p == name) {
                    if idx < args.len() {
                        return args[idx].clone();
                    }
                }
                type_.clone()
            }
            Type::ConstPointer(inner) => Type::ConstPointer(Box::new(
                Self::substitute_type_params(inner, param_names, args),
            )),
            Type::MutPointer(inner) => Type::MutPointer(Box::new(Self::substitute_type_params(
                inner,
                param_names,
                args,
            ))),
            Type::Array(inner, size) => Type::Array(
                Box::new(Self::substitute_type_params(inner, param_names, args)),
                *size,
            ),
            Type::Generic(path, inner_args) => {
                // Substitute in the generic arguments too
                let substituted_args: Vec<Type> = inner_args
                    .iter()
                    .map(|a| Self::substitute_type_params(a, param_names, args))
                    .collect();
                Type::Generic(path.clone(), substituted_args)
            }
            // Raw types don't need substitution
            Type::Raw(_) => type_.clone(),
            // These shouldn't appear in type alias targets, but handle them for completeness
            Type::Unresolved(_) | Type::Function(_, _, _) => type_.clone(),
        }
    }

    pub(crate) fn padding_type(&self, bytes: usize) -> Type {
        match self.resolve_string(&[], "u8") {
            TypeLookupResult::Found(t) => Type::Array(Box::new(t), bytes),
            _ => panic!("u8 type not found in type registry"),
        }
    }

    /// Computes the size of a generic type instantiation.
    /// Returns the size by substituting type parameters with the provided arguments
    /// and computing the size of each field.
    pub(crate) fn compute_generic_size(&self, base: &ItemPath, args: &[Type]) -> Option<usize> {
        let item_def = self.types.get(base)?;
        let resolved = item_def.resolved()?;

        // Get the type definition (struct with fields)
        let type_def = resolved.inner.as_type()?;

        // Get the type parameters for substitution
        let param_names = &item_def.type_parameters;

        // Compute total size from all regions (fields)
        let mut size = 0usize;
        for region in &type_def.regions {
            let substituted_type =
                Self::substitute_type_params(&region.type_ref, param_names, args);
            let field_size = substituted_type.size(self)?;
            size += field_size;
        }

        Some(size)
    }

    /// Computes the alignment of a generic type instantiation.
    /// Returns the alignment by substituting type parameters with the provided arguments
    /// and finding the maximum alignment of all fields.
    pub(crate) fn compute_generic_alignment(
        &self,
        base: &ItemPath,
        args: &[Type],
    ) -> Option<usize> {
        let item_def = self.types.get(base)?;
        let resolved = item_def.resolved()?;

        // Get the type definition (struct with fields)
        let type_def = resolved.inner.as_type()?;

        // Get the type parameters for substitution
        let param_names = &item_def.type_parameters;

        // Compute max alignment from all regions (fields)
        let mut max_alignment = 1usize;
        for region in &type_def.regions {
            let substituted_type =
                Self::substitute_type_params(&region.type_ref, param_names, args);
            if let Some(field_alignment) = substituted_type.alignment(self) {
                max_alignment = max_alignment.max(field_alignment);
            }
        }

        Some(max_alignment)
    }

    /// Applies generic arguments to a compound type.
    /// This handles cases where a non-generic type alias resolves to a type containing
    /// type parameters (e.g., `type Ptr<T> = *mut T` used as `Ptr<u32>`).
    fn apply_generic_args_to_type(
        type_: Type,
        args: Vec<Type>,
        param_names: &[String],
    ) -> TypeLookupResult {
        match type_ {
            Type::MutPointer(inner) => {
                let substituted = Self::apply_generic_args_to_inner(*inner, &args, param_names);
                TypeLookupResult::Found(Type::MutPointer(Box::new(substituted)))
            }
            Type::ConstPointer(inner) => {
                let substituted = Self::apply_generic_args_to_inner(*inner, &args, param_names);
                TypeLookupResult::Found(Type::ConstPointer(Box::new(substituted)))
            }
            Type::Array(inner, size) => {
                let substituted = Self::apply_generic_args_to_inner(*inner, &args, param_names);
                TypeLookupResult::Found(Type::Array(Box::new(substituted), size))
            }
            Type::Generic(path, existing_args) => {
                // Apply generic args to each existing argument
                let substituted_args: Vec<Type> = existing_args
                    .into_iter()
                    .map(|arg| Self::apply_generic_args_to_inner(arg, &args, param_names))
                    .collect();
                TypeLookupResult::Found(Type::Generic(path, substituted_args))
            }
            // TypeParameter, Raw, etc. - these shouldn't have type arguments applied
            // directly at the top level, but we handle them for completeness
            other => TypeLookupResult::Found(Self::apply_generic_args_to_inner(
                other,
                &args,
                param_names,
            )),
        }
    }

    /// Helper to apply generic arguments to the inner parts of a type.
    /// Type parameters are replaced by their corresponding arguments based on
    /// their position in `param_names`.
    fn apply_generic_args_to_inner(type_: Type, args: &[Type], param_names: &[String]) -> Type {
        match type_ {
            Type::TypeParameter(ref name) => {
                // Find the parameter index by looking up the name in param_names
                if let Some(idx) = param_names.iter().position(|p| p == name) {
                    if idx < args.len() {
                        return args[idx].clone();
                    }
                }
                type_
            }
            Type::MutPointer(inner) => Type::MutPointer(Box::new(
                Self::apply_generic_args_to_inner(*inner, args, param_names),
            )),
            Type::ConstPointer(inner) => Type::ConstPointer(Box::new(
                Self::apply_generic_args_to_inner(*inner, args, param_names),
            )),
            Type::Array(inner, size) => Type::Array(
                Box::new(Self::apply_generic_args_to_inner(*inner, args, param_names)),
                size,
            ),
            Type::Generic(path, existing_args) => {
                let substituted_args: Vec<Type> = existing_args
                    .into_iter()
                    .map(|arg| Self::apply_generic_args_to_inner(arg, args, param_names))
                    .collect();
                Type::Generic(path, substituted_args)
            }
            // Raw, Unresolved, Function - no substitution needed
            other => other,
        }
    }

    /// Resolves a type name in the given scope, returning detailed information
    /// about why resolution failed if it does.
    pub(crate) fn resolve_string(&self, scope: &[ItemPath], name: &str) -> TypeLookupResult {
        let (scope_types, scope_modules): (Vec<&ItemPath>, Vec<&ItemPath>) =
            scope.iter().partition(|ip| self.types.contains_key(ip));

        // If we find the relevant type within our scope, take the last one
        let found_path = scope_types
            .into_iter()
            .rev()
            .find(|st| st.last().map(|i| i.as_str()) == Some(name))
            .cloned()
            .or_else(|| {
                // Otherwise, search our scopes
                std::iter::once(&ItemPath::empty())
                    .chain(scope_modules.iter().copied())
                    .map(|ip| ip.join(name.into()))
                    .find(|ip| self.types.contains_key(ip))
            });

        match found_path {
            Some(path) => {
                // Check if the type is resolved
                if let Some(item_def) = self.types.get(&path) {
                    if item_def.is_resolved() {
                        TypeLookupResult::Found(self.resolve_type_alias(Type::Raw(path)))
                    } else {
                        TypeLookupResult::NotYetResolved
                    }
                } else {
                    TypeLookupResult::NotFound {
                        type_name: name.to_string(),
                    }
                }
            }
            None => TypeLookupResult::NotFound {
                type_name: name.to_string(),
            },
        }
    }

    /// Resolves a path, checking if it's a qualified path or needs scope lookup.
    /// Returns detailed information about why resolution failed if it does.
    pub(crate) fn resolve_path(&self, scope: &[ItemPath], path: &ItemPath) -> TypeLookupResult {
        // If path has multiple segments, try to resolve it directly first
        if path.len() > 1 {
            if let Some(item_def) = self.types.get(path) {
                if item_def.is_resolved() {
                    return TypeLookupResult::Found(
                        self.resolve_type_alias(Type::Raw(path.clone())),
                    );
                } else {
                    return TypeLookupResult::NotYetResolved;
                }
            }
        }

        // For single-segment paths or unresolved multi-segment paths,
        // try scope-based resolution using the last segment
        if let Some(last_segment) = path.last() {
            self.resolve_string(scope, last_segment.as_str())
        } else {
            TypeLookupResult::NotFound {
                type_name: path.to_string(),
            }
        }
    }

    /// Resolves a grammar type to a semantic type.
    /// Returns detailed information about why resolution failed if it does.
    /// The `type_params` parameter contains the names of type parameters in scope
    /// (for resolving `T` to `Type::TypeParameter("T")` inside generic types).
    pub(crate) fn resolve_grammar_type(
        &self,
        scope: &[ItemPath],
        type_: &grammar::Type,
        type_params: &[String],
    ) -> TypeLookupResult {
        match type_ {
            grammar::Type::ConstPointer { pointee, .. } => {
                match self.resolve_grammar_type(scope, pointee, type_params) {
                    TypeLookupResult::Found(t) => {
                        TypeLookupResult::Found(Type::ConstPointer(Box::new(t)))
                    }
                    other => other,
                }
            }
            grammar::Type::MutPointer { pointee, .. } => {
                match self.resolve_grammar_type(scope, pointee, type_params) {
                    TypeLookupResult::Found(t) => {
                        TypeLookupResult::Found(Type::MutPointer(Box::new(t)))
                    }
                    other => other,
                }
            }
            grammar::Type::Array { element, size, .. } => {
                match self.resolve_grammar_type(scope, element, type_params) {
                    TypeLookupResult::Found(t) => {
                        TypeLookupResult::Found(Type::Array(Box::new(t), *size))
                    }
                    other => other,
                }
            }
            grammar::Type::Ident {
                path, generic_args, ..
            } => {
                // Check if this is a type parameter reference
                if path.len() == 1 && generic_args.is_empty() {
                    let name = path.iter().next().unwrap().as_str();
                    if type_params.contains(&name.to_string()) {
                        return TypeLookupResult::Found(Type::TypeParameter(name.to_string()));
                    }
                }

                // Resolve generic arguments recursively
                if !generic_args.is_empty() {
                    // First, check if there's an exact-match extern type with the full name
                    // (e.g., "SharedPtr<u32>" as a literal type name rather than a generic instantiation)
                    let full_type_name = format!("{type_}");
                    let exact_match_path = if path.len() == 1 {
                        // Single-segment path: try scope-based resolution with the full name
                        ItemPath::from(full_type_name.as_str())
                    } else {
                        // Multi-segment path: replace the last segment with the full type name
                        let mut segments: Vec<_> = path.iter().take(path.len() - 1).cloned().collect();
                        segments.push(full_type_name.clone().into());
                        segments.into_iter().collect()
                    };

                    // Try to find an exact match first (check single-segment path with scope resolution)
                    if path.len() == 1 {
                        if let TypeLookupResult::Found(t) = self.resolve_string(scope, &full_type_name) {
                            return TypeLookupResult::Found(t);
                        }
                    } else if let Some(item_def) = self.types.get(&exact_match_path) {
                        if item_def.is_resolved() {
                            return TypeLookupResult::Found(self.resolve_type_alias(Type::Raw(exact_match_path)));
                        } else {
                            return TypeLookupResult::NotYetResolved;
                        }
                    }

                    // No exact match found, proceed with generic type resolution
                    let mut resolved_args = Vec::new();
                    for arg in generic_args {
                        match self.resolve_grammar_type(scope, arg, type_params) {
                            TypeLookupResult::Found(t) => resolved_args.push(t),
                            other => return other,
                        }
                    }

                    // Resolve the base type path. This follows non-generic type aliases,
                    // so we might get a compound type if the alias target isn't a raw type.
                    let resolved_path = self.resolve_path(scope, path);
                    match resolved_path {
                        TypeLookupResult::Found(Type::Raw(base_path)) => {
                            // Check if this is a generic type alias
                            if let Some((param_names, type_alias)) =
                                self.get_generic_type_alias(&base_path)
                            {
                                // Substitute type parameters in the alias target
                                let substituted = Self::substitute_type_params(
                                    &type_alias.target,
                                    &param_names,
                                    &resolved_args,
                                );
                                TypeLookupResult::Found(substituted)
                            } else {
                                // Regular generic type instantiation
                                TypeLookupResult::Found(Type::Generic(base_path, resolved_args))
                            }
                        }
                        TypeLookupResult::Found(other_type) => {
                            // The base type resolved to a compound type (e.g., non-generic
                            // alias to a pointer). Apply the generic arguments to it.
                            // Get parameter names from the original path before alias resolution.
                            let original_param_names = self
                                .types
                                .get(path)
                                .map(|def| def.type_parameters.as_slice())
                                .unwrap_or(&[]);
                            Self::apply_generic_args_to_type(
                                other_type,
                                resolved_args,
                                original_param_names,
                            )
                        }
                        other => other,
                    }
                } else {
                    self.resolve_path(scope, path)
                }
            }
            grammar::Type::Unknown { size, .. } => {
                TypeLookupResult::Found(self.padding_type(*size))
            }
        }
    }
}
