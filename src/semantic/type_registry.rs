use std::collections::BTreeMap;

use crate::{
    SemanticError,
    grammar::{self, ItemPath},
    semantic::{
        error::Result,
        types::{ItemDefinition, Type, Visibility},
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
    /// Type exists but is private and not accessible from the requesting module
    PrivateAccess {
        /// The path of the private type
        item_path: ItemPath,
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

    /// Check if a module can access an item based on visibility rules.
    /// Private items are only visible to:
    /// - The same module
    /// - Child modules (descendants)
    fn can_access(&self, from_module: &ItemPath, item_path: &ItemPath) -> bool {
        if let Some(item_def) = self.types.get(item_path) {
            // Public items are always accessible
            if item_def.visibility == Visibility::Public {
                return true;
            }

            // Private items: check if from_module is the same as or a child of the item's module
            if let Some(item_module) = item_path.parent() {
                // Same module can always access
                if from_module == &item_module {
                    return true;
                }
                // Child modules can access parent's private items
                return from_module.starts_with(&item_module);
            }
        }
        // If item doesn't exist, let the caller handle it
        true
    }

    /// Extract the current module path from the scope.
    /// The first element of the scope is always the current module path.
    fn get_from_module(scope: &[ItemPath]) -> Option<&ItemPath> {
        scope.first()
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
    ///
    /// This handles all compound types (pointers, arrays, generics) by recursively
    /// substituting in their inner types.
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

    /// Helper to compute a property over a generic type's fields.
    /// Handles the common logic of resolving the type definition and substituting
    /// type parameters, then applies a fold operation over the resulting field types.
    fn fold_generic_fields<T, F>(
        &self,
        base: &ItemPath,
        args: &[Type],
        initial: T,
        mut fold_fn: F,
    ) -> Option<T>
    where
        F: FnMut(T, &Type, &TypeRegistry) -> Option<T>,
    {
        let item_def = self.types.get(base)?;
        let resolved = item_def.resolved()?;
        let type_def = resolved.inner.as_type()?;
        let param_names = &item_def.type_parameters;

        let mut accumulator = initial;
        for region in &type_def.regions {
            let substituted_type =
                Self::substitute_type_params(&region.type_ref, param_names, args);
            accumulator = fold_fn(accumulator, &substituted_type, self)?;
        }

        Some(accumulator)
    }

    /// Computes the size of a generic type instantiation.
    /// Returns the size by substituting type parameters with the provided arguments
    /// and computing the size of each field.
    pub(crate) fn compute_generic_size(&self, base: &ItemPath, args: &[Type]) -> Option<usize> {
        self.fold_generic_fields(base, args, 0usize, |acc, field_type, registry| {
            Some(acc + field_type.size(registry)?)
        })
    }

    /// Computes the alignment of a generic type instantiation.
    /// Returns the alignment by substituting type parameters with the provided arguments
    /// and finding the maximum alignment of all fields.
    pub(crate) fn compute_generic_alignment(
        &self,
        base: &ItemPath,
        args: &[Type],
    ) -> Option<usize> {
        self.fold_generic_fields(base, args, 1usize, |acc, field_type, registry| {
            Some(acc.max(field_type.alignment(registry)?))
        })
    }

    /// Resolves a type name in the given scope, returning detailed information
    /// about why resolution failed if it does.
    pub(crate) fn resolve_string(&self, scope: &[ItemPath], name: &str) -> TypeLookupResult {
        let from_module = Self::get_from_module(scope);
        let (scope_types, scope_modules): (Vec<&ItemPath>, Vec<&ItemPath>) =
            scope.iter().partition(|ip| self.types.contains_key(ip));

        // If we find the relevant type within our scope, take the last one
        // Types in scope_types were explicitly imported via `use`, so they're already visibility-checked
        let found_path = scope_types
            .into_iter()
            .rev()
            .find(|st| st.last().map(|i| i.as_str()) == Some(name))
            .cloned()
            .or_else(|| {
                // Otherwise, search our scopes
                // Note: we need to check visibility for types found through module search
                std::iter::once(&ItemPath::empty())
                    .chain(scope_modules.iter().copied())
                    .map(|ip| ip.join(name.into()))
                    .find(|ip| {
                        if self.types.contains_key(ip) {
                            // Check visibility - skip private types from other modules
                            if let Some(from) = from_module {
                                self.can_access(from, ip)
                            } else {
                                true
                            }
                        } else {
                            false
                        }
                    })
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

    /// Finds the full ItemPath for a type name in the given scope, without requiring
    /// the type to be resolved. This is useful for pointer types which only need
    /// to know the path, not the full type definition.
    /// Returns Some(path) if the type exists (resolved or not), None if not found.
    fn find_type_path(&self, scope: &[ItemPath], name: &str) -> Option<ItemPath> {
        let (scope_types, scope_modules): (Vec<&ItemPath>, Vec<&ItemPath>) =
            scope.iter().partition(|ip| self.types.contains_key(ip));

        // If we find the relevant type within our scope, take the last one
        scope_types
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
            })
    }

    /// Attempts to partially resolve a generic type even when some of its arguments
    /// aren't fully resolved yet. This is crucial for self-referential generic types.
    /// For example, `SharedPtr<GameObject>` where `GameObject` contains `SharedPtr<GameObject>`.
    /// Returns a Type::Generic with the base path and whatever arguments could be resolved.
    fn try_resolve_generic_partially(
        &self,
        scope: &[ItemPath],
        path: &ItemPath,
        generic_args: &[grammar::Type],
        type_params: &[String],
    ) -> Option<Type> {
        // First, find the base type path
        let base_path = if path.len() == 1 {
            let name = path.last()?.as_str();
            self.find_type_path(scope, name)?
        } else {
            // Multi-segment path - check if it exists
            if self.types.contains_key(path) {
                path.clone()
            } else {
                return None;
            }
        };

        // Try to resolve each generic argument. If any can't be resolved,
        // we'll try to at least find their path
        let mut resolved_args = Vec::new();
        for arg in generic_args {
            match self.resolve_grammar_type(scope, arg, type_params) {
                TypeLookupResult::Found(t) => resolved_args.push(t),
                TypeLookupResult::NotYetResolved => {
                    // Try to get at least the path for this unresolved argument
                    if let grammar::Type::Ident {
                        path: arg_path,
                        generic_args: nested_args,
                        ..
                    } = arg
                    {
                        if nested_args.is_empty() {
                            // Simple unresolved type - just use its path
                            if let Some(arg_name) = arg_path.last() {
                                if let Some(full_path) =
                                    self.find_type_path(scope, arg_name.as_str())
                                {
                                    resolved_args.push(Type::Raw(full_path));
                                    continue;
                                }
                            }
                        } else {
                            // Nested generic - try partial resolution recursively
                            if let Some(partial) = self.try_resolve_generic_partially(
                                scope,
                                arg_path,
                                nested_args,
                                type_params,
                            ) {
                                resolved_args.push(partial);
                                continue;
                            }
                        }
                    }
                    // Couldn't resolve this argument at all - give up
                    return None;
                }
                TypeLookupResult::NotFound { .. } | TypeLookupResult::PrivateAccess { .. } => {
                    // Type doesn't exist or is private - give up
                    return None;
                }
            }
        }

        // If no generic args, return Raw type instead of Generic with empty args
        if resolved_args.is_empty() {
            Some(Type::Raw(base_path))
        } else {
            Some(Type::Generic(base_path, resolved_args))
        }
    }

    /// Resolves a path, checking if it's a qualified path or needs scope lookup.
    /// Returns detailed information about why resolution failed if it does.
    pub(crate) fn resolve_path(&self, scope: &[ItemPath], path: &ItemPath) -> TypeLookupResult {
        let from_module = Self::get_from_module(scope);

        // If path has multiple segments, try to resolve it directly first
        if path.len() > 1 {
            if let Some(item_def) = self.types.get(path) {
                // Check visibility for directly resolved paths
                if let Some(from) = from_module {
                    if !self.can_access(from, path) {
                        return TypeLookupResult::PrivateAccess {
                            item_path: path.clone(),
                        };
                    }
                }
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

    /// Helper for resolving pointer types (both const and mut).
    /// Handles the common logic for partial resolution of unresolved pointees.
    fn resolve_pointer_type<F>(
        &self,
        scope: &[ItemPath],
        pointee: &grammar::Type,
        type_params: &[String],
        wrap_pointer: F,
    ) -> TypeLookupResult
    where
        F: Fn(Box<Type>) -> Type,
    {
        match self.resolve_grammar_type(scope, pointee, type_params) {
            TypeLookupResult::Found(t) => TypeLookupResult::Found(wrap_pointer(Box::new(t))),
            TypeLookupResult::NotYetResolved => {
                // For pointers, we can resolve even if the pointee isn't fully resolved yet.
                // This allows mutually recursive types to resolve.
                // We just need to find the path to the pointee type.
                if let grammar::Type::Ident {
                    path, generic_args, ..
                } = pointee
                {
                    if generic_args.is_empty() {
                        // Non-generic type reference - just find its path
                        if let Some(last) = path.last() {
                            if let Some(full_path) = self.find_type_path(scope, last.as_str()) {
                                return TypeLookupResult::Found(wrap_pointer(Box::new(Type::Raw(
                                    full_path,
                                ))));
                            }
                        }
                    } else {
                        // Generic type with potentially unresolved arguments.
                        // Try partial resolution for self-referential generics.
                        if let Some(partial) = self.try_resolve_generic_partially(
                            scope,
                            path,
                            generic_args,
                            type_params,
                        ) {
                            return TypeLookupResult::Found(wrap_pointer(Box::new(partial)));
                        }
                    }
                }
                TypeLookupResult::NotYetResolved
            }
            other => other,
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
                self.resolve_pointer_type(scope, pointee, type_params, Type::ConstPointer)
            }
            grammar::Type::MutPointer { pointee, .. } => {
                self.resolve_pointer_type(scope, pointee, type_params, Type::MutPointer)
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
                        let mut segments: Vec<_> =
                            path.iter().take(path.len() - 1).cloned().collect();
                        segments.push(full_type_name.clone().into());
                        segments.into_iter().collect()
                    };

                    // Try to find an exact match first (check single-segment path with scope resolution)
                    if path.len() == 1 {
                        match self.resolve_string(scope, &full_type_name) {
                            TypeLookupResult::Found(t) => return TypeLookupResult::Found(t),
                            TypeLookupResult::NotYetResolved => {
                                return TypeLookupResult::NotYetResolved;
                            }
                            TypeLookupResult::PrivateAccess { item_path } => {
                                return TypeLookupResult::PrivateAccess { item_path };
                            }
                            TypeLookupResult::NotFound { .. } => {
                                // No exact match, proceed with generic resolution below
                            }
                        }
                    } else if let Some(item_def) = self.types.get(&exact_match_path) {
                        if item_def.is_resolved() {
                            return TypeLookupResult::Found(
                                self.resolve_type_alias(Type::Raw(exact_match_path)),
                            );
                        } else {
                            return TypeLookupResult::NotYetResolved;
                        }
                    }

                    // No exact match found, proceed with generic type resolution
                    let mut resolved_args = Vec::new();
                    let mut has_unresolved_args = false;
                    for arg in generic_args {
                        match self.resolve_grammar_type(scope, arg, type_params) {
                            TypeLookupResult::Found(t) => resolved_args.push(t),
                            TypeLookupResult::NotYetResolved => {
                                // Try to at least get the path for the unresolved argument
                                if let Some(partial) = self.try_resolve_generic_partially(
                                    scope,
                                    if let grammar::Type::Ident { path: arg_path, .. } = arg {
                                        arg_path
                                    } else {
                                        // Non-ident type that's unresolved - can't proceed
                                        return TypeLookupResult::NotYetResolved;
                                    },
                                    if let grammar::Type::Ident {
                                        generic_args: nested_args,
                                        ..
                                    } = arg
                                    {
                                        nested_args
                                    } else {
                                        &[]
                                    },
                                    type_params,
                                ) {
                                    resolved_args.push(partial);
                                    has_unresolved_args = true;
                                } else {
                                    return TypeLookupResult::NotYetResolved;
                                }
                            }
                            other => return other,
                        }
                    }

                    // Resolve the base type path. This follows non-generic type aliases,
                    // so we might get a compound type if the alias target isn't a raw type.
                    let resolved_path = self.resolve_path(scope, path);
                    match resolved_path {
                        TypeLookupResult::Found(Type::Raw(base_path)) => {
                            // If we have unresolved arguments, check if the base type has a fixed size.
                            // Types with #[size(...)] don't depend on their type parameters for size/alignment.
                            if has_unresolved_args {
                                // Check if the base type is resolved and has a fixed size
                                if let Some(item_def) = self.types.get(&base_path) {
                                    if !item_def.is_resolved() {
                                        // Base type isn't resolved yet, can't proceed
                                        return TypeLookupResult::NotYetResolved;
                                    }
                                    // Base is resolved, we can create the generic type even with unresolved args
                                    // because the size is determined by the base type's #[size(...)] attribute
                                } else {
                                    // Base type doesn't exist
                                    return TypeLookupResult::NotFound {
                                        type_name: base_path.to_string(),
                                    };
                                }
                            }

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
                            TypeLookupResult::Found(Self::substitute_type_params(
                                &other_type,
                                original_param_names,
                                &resolved_args,
                            ))
                        }
                        TypeLookupResult::NotYetResolved => TypeLookupResult::NotYetResolved,
                        TypeLookupResult::NotFound { .. } => {
                            // Report the full type name including generic args, not just the base
                            TypeLookupResult::NotFound {
                                type_name: full_type_name,
                            }
                        }
                        TypeLookupResult::PrivateAccess { item_path } => {
                            TypeLookupResult::PrivateAccess { item_path }
                        }
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
