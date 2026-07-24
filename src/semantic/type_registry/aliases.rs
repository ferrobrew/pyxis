use crate::{
    grammar::ItemPath,
    semantic::{type_alias_definition::TypeAliasDefinition, types::Type},
};

use super::{TypeLookupResult, TypeRegistry};

impl TypeRegistry {
    /// Follows non-generic type aliases recursively to get the final resolved type.
    /// Generic type aliases (those with type_parameters) are NOT followed here,
    /// as they require parameter substitution which is handled separately.
    pub(super) fn resolve_type_alias(&self, type_: Type) -> Type {
        match &type_ {
            Type::Raw(path) => {
                // Only follow non-generic type aliases; return their (already
                // resolved) target type.
                if let Some(item_def) = self.lookup(path)
                    && item_def.type_parameters.is_empty()
                    && let Some(resolved) = item_def.resolved()
                    && let Some(type_alias) = resolved.inner.as_type_alias()
                {
                    return type_alias.target.clone();
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
    pub(super) fn get_generic_type_alias(
        &self,
        path: &ItemPath,
    ) -> Option<(Vec<String>, TypeAliasDefinition)> {
        if let Some(item_def) = self.lookup(path)
            && !item_def.type_parameters.is_empty()
            && let Some(resolved) = item_def.resolved()
            && let Some(type_alias) = resolved.inner.as_type_alias()
        {
            return Some((item_def.type_parameters.clone(), type_alias.clone()));
        }
        None
    }

    /// Substitutes type parameters in a type with concrete types.
    /// `param_names` are the type parameter names, `args` are the concrete types.
    ///
    /// This handles all compound types (pointers, arrays, generics) by recursively
    /// substituting in their inner types.
    pub(super) fn substitute_type_params(
        type_: &Type,
        param_names: &[String],
        args: &[Type],
    ) -> Type {
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
        let item_def = self.lookup(base)?;
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
            scope.iter().partition(|ip| self.has(ip));

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
                        if self.has(ip) {
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
            // Canonicalize through any `pub use` re-export so the resolved type
            // carries its defining path, not the re-exporting module's alias.
            Some(path) => {
                let path = self.canonicalize(&path);
                // Check if the type is resolved
                if let Some(item_def) = self.lookup(&path) {
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
}
