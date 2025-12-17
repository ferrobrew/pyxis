//! Semantic representation of type aliases

use crate::{
    grammar::{self, ItemPath},
    semantic::{
        SemanticState,
        error::{BuildOutcome, Result, UnresolvedTypeReference},
        type_registry::TypeLookupResult,
        types::{ItemStateResolved, Type},
    },
    span::{HasLocation, ItemLocation},
};

#[cfg(test)]
use crate::span::StripLocations;

/// Semantic type alias definition - stores the resolved target type
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
#[cfg_attr(test, derive(StripLocations))]
pub struct TypeAliasDefinition {
    /// The resolved target type that this alias refers to
    pub target: Type,
    /// Documentation comments
    pub doc: Vec<String>,
}

impl TypeAliasDefinition {
    pub fn new(target: Type, doc: Vec<String>) -> Self {
        Self { target, doc }
    }

    pub fn doc(&self) -> &[String] {
        &self.doc
    }
}

/// Build a semantic TypeAliasDefinition from the grammar TypeAliasDefinition.
///
/// Type aliases resolve the target type at definition time. When the alias is used
/// elsewhere, the pre-resolved type is returned directly, enabling re-export semantics.
pub fn build(
    semantic: &SemanticState,
    resolvee_path: &ItemPath,
    definition: &grammar::TypeAliasDefinition,
    _location: &ItemLocation,
    doc_comments: &[String],
    type_parameters: &[String],
) -> Result<BuildOutcome> {
    let module = semantic.get_module_for_path(resolvee_path, &definition.location)?;

    // Resolve the target type in the module's scope with type parameters in scope.
    // This allows generic type aliases like `type SharedPtr<T> = Shared<T>;`
    // The cross-module re-export semantics come from storing the fully resolved type,
    // so when used elsewhere, no additional scope/visibility checks are needed.
    let resolved_target = match semantic.type_registry.resolve_grammar_type(
        &module.scope(),
        &definition.target,
        type_parameters,
    ) {
        TypeLookupResult::Found(t) => t,
        TypeLookupResult::NotYetResolved => return Ok(BuildOutcome::Deferred),
        TypeLookupResult::NotFound { type_name } => {
            return Ok(BuildOutcome::NotFoundType(UnresolvedTypeReference {
                type_name,
                location: *definition.target.location(),
                context: format!("target of type alias `{resolvee_path}`"),
            }));
        }
    };

    // Type aliases don't have their own size/alignment - they're just references
    // We use size 0 and alignment 1 as sentinels
    Ok(BuildOutcome::Resolved(ItemStateResolved {
        size: 0,
        alignment: 1,
        inner: TypeAliasDefinition {
            target: resolved_target,
            doc: doc_comments.to_vec(),
        }
        .into(),
    }))
}
