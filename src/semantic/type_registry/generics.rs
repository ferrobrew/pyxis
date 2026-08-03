use crate::{
    grammar::{self, ItemPath},
    semantic::{
        error::SemanticError,
        function::CallingConvention,
        types::{FunctionArg, Type},
    },
    span::HasLocation,
};

use super::{TypeLookupResult, TypeRegistry};

impl TypeRegistry {
    /// Finds the full ItemPath for a type name in the given scope, without requiring
    /// the type to be resolved. This is useful for pointer types which only need
    /// to know the path, not the full type definition.
    /// Returns Some(path) if the type exists (resolved or not), None if not found.
    fn find_type_path(&self, scope: &[ItemPath], name: &str) -> Option<ItemPath> {
        let (scope_types, scope_modules): (Vec<&ItemPath>, Vec<&ItemPath>) =
            scope.iter().partition(|ip| self.has(ip));

        // If we find the relevant type within our scope, take the last one.
        // Canonicalize through any `pub use` re-export so callers (e.g. pointer
        // pointees) reference the defining path, not the re-exporting alias.
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
                    .find(|ip| self.has(ip))
            })
            .map(|p| self.canonicalize(&p))
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
            if self.has(path) {
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
                // A bad attribute inside a generic argument is reported when the
                // argument is resolved for real; this partial path just bails.
                TypeLookupResult::InvalidAttribute { .. } => return None,
                TypeLookupResult::NotYetResolved => {
                    // Try to get at least the path for this unresolved argument
                    if let grammar::TypeKind::Ident {
                        path: arg_path,
                        generic_args: nested_args,
                        ..
                    } = &arg.kind
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

        // If path has multiple segments, try to resolve it directly first.
        // Canonicalize through any `pub use` re-export so `<module>::<alias>`
        // resolves to (and reports as) the item's defining path.
        if path.len() > 1 {
            let canonical = self.canonicalize(path);
            if let Some(item_def) = self.lookup(&canonical) {
                // Check visibility for directly resolved paths
                if let Some(from) = from_module {
                    if !self.can_access(from, &canonical) {
                        return TypeLookupResult::PrivateAccess {
                            item_path: canonical.clone(),
                        };
                    }
                }
                if item_def.is_resolved() {
                    return TypeLookupResult::Found(self.resolve_type_alias(Type::Raw(canonical)));
                } else {
                    return TypeLookupResult::NotYetResolved;
                }
            }
        }

        // Relative multi-segment: mirror doc_links/resolver.rs — try `base + path`
        // for each base in empty() ∪ scope, so `Outer::Header` resolves to
        // `main::Outer::Header` when scope contains `main`.
        if path.len() > 1 {
            for base in std::iter::once(&ItemPath::empty()).chain(scope) {
                let candidate = base.join_path(path);
                let canonical = self.canonicalize(&candidate);
                if let Some(item_def) = self.lookup(&canonical) {
                    // Check visibility for paths resolved through the scope.
                    if let Some(from) = from_module {
                        if !self.can_access(from, &canonical) {
                            return TypeLookupResult::PrivateAccess {
                                item_path: canonical,
                            };
                        }
                    }
                    return if item_def.is_resolved() {
                        TypeLookupResult::Found(self.resolve_type_alias(Type::Raw(canonical)))
                    } else {
                        TypeLookupResult::NotYetResolved
                    };
                }
            }
        }

        // A multi-segment path that matched nothing absolute or relative is a
        // genuine miss: report the full written path, not just the leaf.
        if path.len() > 1 {
            return TypeLookupResult::NotFound {
                type_name: path.to_string(),
            };
        }

        // For single-segment paths, try scope-based resolution using the leaf.
        if let Some(last_segment) = path.last() {
            self.resolve_string(scope, last_segment.as_str())
        } else {
            TypeLookupResult::NotFound {
                type_name: path.to_string(),
            }
        }
    }

    /// Resolve a type that is only reachable through an indirection - a
    /// pointee, or a function-pointer's parameter/return type.
    ///
    /// Such a type only has to *exist*: the enclosing pointer is
    /// pointer-sized whatever it points at, so a target that isn't resolved
    /// yet resolves to its canonical path instead of deferring. That is what
    /// lets recursive and mutually recursive types close - without it
    /// `type A { f: fn(A) }` would stall the build.
    fn resolve_indirect_type(
        &self,
        scope: &[ItemPath],
        type_: &grammar::Type,
        type_params: &[String],
    ) -> TypeLookupResult {
        match self.resolve_grammar_type(scope, type_, type_params) {
            TypeLookupResult::NotYetResolved => {
                if let grammar::TypeKind::Ident {
                    path, generic_args, ..
                } = &type_.kind
                {
                    if generic_args.is_empty() {
                        // Non-generic type reference — find its canonical path
                        // even if the target isn't fully resolved. A
                        // multi-segment path is taken directly (canonicalized
                        // through any `pub use` re-export); a bare name
                        // resolves through the scope.
                        let full_path = if path.len() > 1 && self.has(path) {
                            Some(self.canonicalize(path))
                        } else {
                            path.last()
                                .and_then(|last| self.find_type_path(scope, last.as_str()))
                        };
                        if let Some(full_path) = full_path {
                            return TypeLookupResult::Found(Type::Raw(full_path));
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
                            return TypeLookupResult::Found(partial);
                        }
                    }
                }
                TypeLookupResult::NotYetResolved
            }
            other => other,
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
        match self.resolve_indirect_type(scope, pointee, type_params) {
            TypeLookupResult::Found(t) => TypeLookupResult::Found(wrap_pointer(Box::new(t))),
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
        let calling_convention = match validate_type_attributes(type_) {
            Ok(calling_convention) => calling_convention,
            Err(error) => {
                return TypeLookupResult::InvalidAttribute {
                    error: Box::new(error),
                };
            }
        };

        match &type_.kind {
            grammar::TypeKind::ConstPointer { pointee, .. } => {
                self.resolve_pointer_type(scope, pointee, type_params, Type::ConstPointer)
            }
            grammar::TypeKind::MutPointer { pointee, .. } => {
                self.resolve_pointer_type(scope, pointee, type_params, Type::MutPointer)
            }
            grammar::TypeKind::Function {
                arguments,
                return_type,
            } => {
                // Parameters and the return type sit behind the function
                // pointer, so they resolve like a pointee: they need to exist,
                // not to be laid out. `type A { f: fn(A) }` closes because of
                // this.
                let mut resolved_arguments = Vec::new();
                for argument in arguments {
                    match self.resolve_indirect_type(scope, &argument.type_, type_params) {
                        TypeLookupResult::Found(t) => resolved_arguments.push(FunctionArg {
                            name: argument.name.as_ref().map(|n| n.0.clone()),
                            type_: Box::new(t),
                        }),
                        other => return other,
                    }
                }
                let resolved_return_type = match return_type {
                    Some(return_type) => {
                        match self.resolve_indirect_type(scope, return_type, type_params) {
                            TypeLookupResult::Found(t) => Some(Box::new(t)),
                            other => return other,
                        }
                    }
                    None => None,
                };
                TypeLookupResult::Found(Type::Function(
                    calling_convention.unwrap_or(CallingConvention::System),
                    resolved_arguments,
                    resolved_return_type,
                ))
            }
            grammar::TypeKind::Array { element, size, .. } => {
                match self.resolve_grammar_type(scope, element, type_params) {
                    TypeLookupResult::Found(t) => {
                        TypeLookupResult::Found(Type::Array(Box::new(t), *size))
                    }
                    other => other,
                }
            }
            grammar::TypeKind::Ident {
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
                            TypeLookupResult::InvalidAttribute { error } => {
                                return TypeLookupResult::InvalidAttribute { error };
                            }
                            TypeLookupResult::NotFound { .. } => {
                                // No exact match, proceed with generic resolution below
                            }
                        }
                    } else if let Some(item_def) = self.lookup(&exact_match_path) {
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
                                    if let grammar::TypeKind::Ident { path: arg_path, .. } =
                                        &arg.kind
                                    {
                                        arg_path
                                    } else {
                                        // Non-ident type that's unresolved - can't proceed
                                        return TypeLookupResult::NotYetResolved;
                                    },
                                    if let grammar::TypeKind::Ident {
                                        generic_args: nested_args,
                                        ..
                                    } = &arg.kind
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
                                if let Some(item_def) = self.lookup(&base_path) {
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
                        TypeLookupResult::InvalidAttribute { error } => {
                            TypeLookupResult::InvalidAttribute { error }
                        }
                    }
                } else {
                    self.resolve_path(scope, path)
                }
            }
            grammar::TypeKind::Unknown { size, .. } => {
                TypeLookupResult::Found(self.padding_type(*size))
            }
        }
    }
}

/// Check the attributes written in type position and, for function-pointer
/// types, extract the calling convention they select. Types other than `fn`
/// have nothing to do with an attribute, so any attribute on them is an error
/// rather than silently ignored.
fn validate_type_attributes(
    type_: &grammar::Type,
) -> Result<Option<CallingConvention>, SemanticError> {
    use crate::grammar::{Attribute, Expr};

    if type_.attributes.0.is_empty() {
        return Ok(None);
    }

    let is_function = matches!(type_.kind, grammar::TypeKind::Function { .. });
    let type_description = match &type_.kind {
        grammar::TypeKind::ConstPointer { .. } | grammar::TypeKind::MutPointer { .. } => {
            "a pointer type"
        }
        grammar::TypeKind::Array { .. } => "an array type",
        grammar::TypeKind::Ident { .. } => "a named type",
        grammar::TypeKind::Unknown { .. } => "an unknown-size type",
        grammar::TypeKind::Function { .. } => "a function pointer type",
    };

    let mut calling_convention = None;
    for attribute in &type_.attributes {
        let unsupported = |name: &str| SemanticError::UnsupportedTypeAttribute {
            attribute_name: name.to_string(),
            type_description: type_description.to_string(),
            location: *attribute.location(),
        };

        match attribute {
            Attribute::Function { name, items, .. }
                if is_function && name.as_str() == "calling_convention" =>
            {
                let exprs = items.exprs_vec();
                let [Expr::Ident { ident, .. }] = exprs.as_slice() else {
                    return Err(SemanticError::InvalidTypeCallingConvention {
                        convention: exprs
                            .iter()
                            .map(|e| format!("{e:?}"))
                            .collect::<Vec<_>>()
                            .join(", "),
                        location: *attribute.location(),
                    });
                };
                calling_convention = Some(ident.as_str().parse().map_err(|_| {
                    SemanticError::InvalidTypeCallingConvention {
                        convention: ident.as_str().to_string(),
                        location: *attribute.location(),
                    }
                })?);
            }
            Attribute::Ident { ident, .. } => return Err(unsupported(ident.as_str())),
            Attribute::Function { name, .. } | Attribute::Assign { name, .. } => {
                return Err(unsupported(name.as_str()));
            }
            Attribute::Cfg { .. } => return Err(unsupported("cfg")),
        }
    }

    Ok(calling_convention)
}
