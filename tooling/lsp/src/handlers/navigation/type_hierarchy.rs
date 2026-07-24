use super::*;

impl ServerState {
    /// textDocument/prepareTypeHierarchy — anchor the hierarchy on the type
    /// under the cursor.
    pub fn handle_prepare_type_hierarchy(&self, req: Request) -> Response {
        let items = (|| {
            let params: TypeHierarchyPrepareParams =
                serde_json::from_value(req.params.clone()).ok()?;
            let uri = params.text_document_position_params.text_document.uri;
            let content = self.get_content(&uri)?;
            let loc = lsp_position_to_pyxis_location(
                content,
                params.text_document_position_params.position,
            );
            let symbol = self.symbol_at(&uri, &loc)?;
            let (type_registry, decl_registry) = self.registries_for(&uri);
            let item =
                self.type_hierarchy_item(symbol.type_path()?, &uri, type_registry, decl_registry)?;
            Some(vec![item])
        })();
        Response {
            id: req.id,
            result: Some(serde_json::to_value(items).unwrap()),
            error: None,
        }
    }

    /// typeHierarchy/supertypes — a type's base classes (its `#[base]` fields).
    pub fn handle_type_hierarchy_supertypes(&self, req: Request) -> Response {
        let items = serde_json::from_value::<TypeHierarchySupertypesParams>(req.params.clone())
            .ok()
            .map(|p| self.related_types(&p.item, true))
            .unwrap_or_default();
        Response {
            id: req.id,
            result: Some(serde_json::to_value(items).unwrap()),
            error: None,
        }
    }

    /// typeHierarchy/subtypes — types that declare this one as a `#[base]`.
    pub fn handle_type_hierarchy_subtypes(&self, req: Request) -> Response {
        let items = serde_json::from_value::<TypeHierarchySubtypesParams>(req.params.clone())
            .ok()
            .map(|p| self.related_types(&p.item, false))
            .unwrap_or_default();
        Response {
            id: req.id,
            result: Some(serde_json::to_value(items).unwrap()),
            error: None,
        }
    }

    /// Build a TypeHierarchyItem for a resolved type path; the path round-trips
    /// through `data` so supertypes/subtypes can resume from it.
    fn type_hierarchy_item(
        &self,
        path: &ItemPath,
        from_uri: &Uri,
        type_registry: &TypeRegistry,
        decl_registry: &DeclarationRegistry,
    ) -> Option<TypeHierarchyItem> {
        let rd = self.resolved_definition(path, type_registry, from_uri)?;
        let content = self.get_content(&rd.uri)?;
        let kind = match decl_registry.get_definition(path).map(|d| &d.inner) {
            Some(ItemDefinitionInner::Enum(_) | ItemDefinitionInner::Bitflags(_)) => {
                SymbolKind::ENUM
            }
            _ => SymbolKind::STRUCT,
        };
        Some(TypeHierarchyItem {
            name: path.last()?.as_str().to_string(),
            kind,
            tags: None,
            detail: Some(path.to_string()),
            uri: rd.uri.clone(),
            range: pyxis_span_to_lsp_range(content, &rd.def.location.span),
            selection_range: pyxis_span_to_lsp_range(content, &rd.name_span),
            data: Some(serde_json::Value::String(path.to_string())),
        })
    }

    /// Supertypes (bases) or subtypes (derivers) of the type named by `item`,
    /// via `#[base]` fields.
    fn related_types(&self, item: &TypeHierarchyItem, supertypes: bool) -> Vec<TypeHierarchyItem> {
        let Some(path) = item
            .data
            .as_ref()
            .and_then(|d| d.as_str())
            .map(ItemPath::from)
        else {
            return vec![];
        };
        let (type_registry, decl_registry) = self.registries_for(&item.uri);

        let mut out = Vec::new();
        if supertypes {
            // `path`'s own `#[base]` fields.
            if let Some(bases) = base_field_targets(&path, decl_registry) {
                for base in bases {
                    if let Some(it) =
                        self.type_hierarchy_item(&base, &item.uri, type_registry, decl_registry)
                    {
                        out.push(it);
                    }
                }
            }
        } else {
            // Every project type that lists `path` among its `#[base]` fields.
            for cand in decl_registry.item_paths() {
                if base_field_targets(cand, decl_registry)
                    .is_some_and(|bases| bases.contains(&path))
                    && let Some(it) =
                        self.type_hierarchy_item(cand, &item.uri, type_registry, decl_registry)
                {
                    out.push(it);
                }
            }
        }
        out
    }
}

/// Whether an attribute list contains `#[base]`.
fn has_base_attribute(attrs: &Attributes) -> bool {
    attrs
        .iter()
        .any(|a| matches!(a, Attribute::Ident { ident, .. } if ident.as_str() == "base"))
}

/// The resolved type paths of a type's `#[base]` fields (its base classes).
/// `None` if `path` isn't a declared type; `Some(vec![])` for a type with no
/// bases.
fn base_field_targets(
    path: &ItemPath,
    decl_registry: &DeclarationRegistry,
) -> Option<Vec<ItemPath>> {
    let def = decl_registry.get_definition(path)?;
    let ItemDefinitionInner::Type(td) = &def.inner else {
        return Some(vec![]);
    };
    let scope = decl_registry.get_scope(path).unwrap_or(&[]);
    let mut out = Vec::new();
    for st in td.statements() {
        if let TypeField::Field(_, _, type_) = &st.field
            && has_base_attribute(&st.attributes)
            && let Some(p) = type_
                .as_path()
                .and_then(|tp| resolve_type_path(tp, scope, decl_registry))
        {
            out.push(p);
        }
    }
    Some(out)
}
