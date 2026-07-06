use crate::{
    grammar::{self, ItemPath},
    semantic::{
        attribute,
        error::{
            AttributeName, BuildOutcome, ExternKind, Result, SemanticError, UnresolvedTypeContext,
            UnresolvedTypeReference,
        },
        resolution_context::ResolutionContextRef,
        type_registry::TypeLookupResult,
        types::{ExternValueDefinition as SemanticExternValueDefinition, ItemStateResolved},
    },
    span::{HasLocation, ItemLocation},
};

/// Build a semantic `ExternValueDefinition` from the grammar
/// `ExternValueDefinition`.
///
/// Resolves the value's type annotation against the enclosing module's scope
/// and extracts the required `#[address(...)]` attribute. Extern values, like
/// constants, have no runtime size/alignment — size 0 and alignment 1 are
/// sentinels; the emitted accessor reads the fixed address.
pub fn build(
    semantic: &ResolutionContextRef<'_>,
    resolvee_path: &ItemPath,
    definition: &grammar::ExternValueDefinition,
    _location: &ItemLocation,
    doc_comments: &[String],
) -> Result<BuildOutcome> {
    let module = semantic.get_module_for_path(resolvee_path, &definition.location)?;
    let scope: Vec<ItemPath> = module.scope();

    // Resolve the value's type annotation.
    let resolved_type = match semantic.type_registry.resolve_grammar_type(
        &scope,
        &definition.type_,
        &[], // Extern values don't have type parameters
    ) {
        TypeLookupResult::Found(t) => t,
        TypeLookupResult::NotYetResolved => return Ok(BuildOutcome::Deferred),
        TypeLookupResult::NotFound { type_name } => {
            return Ok(BuildOutcome::NotFoundType(UnresolvedTypeReference {
                type_name,
                location: *definition.type_.location(),
                context: UnresolvedTypeContext::ExternValueType {
                    extern_path: resolvee_path.clone(),
                },
            }));
        }
        TypeLookupResult::PrivateAccess { item_path } => {
            return Ok(BuildOutcome::NotFoundType(UnresolvedTypeReference {
                type_name: item_path.to_string(),
                location: *definition.type_.location(),
                context: UnresolvedTypeContext::ExternValueType {
                    extern_path: resolvee_path.clone(),
                },
            }));
        }
    };

    // Extract the required `#[address(...)]`.
    let mut address = None;
    for attribute in &definition.attributes {
        let Some((ident, items)) = attribute.function() else {
            continue;
        };
        if let Some(attr_address) = attribute::parse_address(ident, items, attribute.location())? {
            address = Some(attr_address);
        }
    }
    let address = address.ok_or_else(|| SemanticError::MissingAttribute {
        attribute_name: AttributeName::Address,
        extern_kind: ExternKind::Value,
        item_path: resolvee_path.clone(),
        location: definition.location,
    })?;

    Ok(BuildOutcome::Resolved(ItemStateResolved {
        size: 0,
        alignment: 1,
        inner: SemanticExternValueDefinition {
            type_: resolved_type,
            address,
            doc: doc_comments.to_vec(),
        }
        .into(),
    }))
}
