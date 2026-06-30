use super::*;

impl ServerState {
    /// textDocument/completion
    pub fn handle_completion(&self, req: Request) -> Response {
        let uri = serde_json::from_value::<CompletionParams>(req.params.clone())
            .ok()
            .map(|p| p.text_document_position.text_document.uri);

        // Keyword spellings come from the tokenizer's canonical table (no
        // duplicated literals); `as` is a contextual keyword with no token, so
        // it stays inline.
        use TokenKind::{
            Backend, Bitflags, Const, Enum, Epilogue, Extern, Fn, Impl, Mut, Prologue, Pub,
            SelfType, SelfValue, Type, Use, Vftable,
        };
        let kw = |k: TokenKind| k.keyword_str().expect("keyword token");
        let mut items: Vec<CompletionItem> = [
            kw(Pub),
            kw(Type),
            kw(Enum),
            kw(Bitflags),
            kw(Impl),
            kw(Fn),
            kw(Extern),
            kw(Use),
            kw(Backend),
            kw(Vftable),
            kw(Const),
            kw(Mut),
            "as",
            kw(Prologue),
            kw(Epilogue),
            kw(SelfValue),
            kw(SelfType),
        ]
        .iter()
        .map(|kw| CompletionItem {
            label: kw.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            ..Default::default()
        })
        .collect();

        if let Some(uri) = uri.as_ref() {
            items.extend(self.type_completions(uri));
        }

        Response {
            id: req.id,
            result: Some(serde_json::to_value(items).unwrap()),
            error: None,
        }
    }

    /// Type-name completions for a document: builtins, in-scope user types
    /// (no edit needed), and out-of-scope project types (each carrying a
    /// `use`-import edit applied on accept). The client filters by typed prefix.
    fn type_completions(&self, uri: &Uri) -> Vec<CompletionItem> {
        use pyxis::semantic::types::PredefinedItem;

        let Some(ctx) = self.analysis_ctx(uri) else {
            return vec![];
        };
        let AnalysisCtx {
            content,
            module,
            scope,
            decl_registry,
            ..
        } = ctx;
        let own_module = self.module_path_for(uri);

        let mut items = Vec::new();
        let mut in_scope_names: std::collections::HashSet<String> =
            std::collections::HashSet::new();

        // Builtins — always usable.
        for predefined in PredefinedItem::ALL {
            in_scope_names.insert(predefined.name().to_string());
            items.push(CompletionItem {
                label: predefined.name().to_string(),
                kind: Some(CompletionItemKind::STRUCT),
                detail: Some("builtin".to_string()),
                ..Default::default()
            });
        }

        // `use`-imported types — usable as their leaf name.
        for path in &scope {
            if Some(path) == own_module.as_ref() {
                continue;
            }
            if let Some(leaf) = path.last()
                && in_scope_names.insert(leaf.as_str().to_string())
            {
                items.push(self.type_item(leaf.as_str(), &path.to_string(), decl_registry, path));
            }
        }

        // Same-module types — usable without an import.
        for path in decl_registry.item_paths() {
            if path.parent().as_ref() == own_module.as_ref()
                && let Some(leaf) = path.last()
                && in_scope_names.insert(leaf.as_str().to_string())
            {
                items.push(self.type_item(leaf.as_str(), "this module", decl_registry, path));
            }
        }

        // Out-of-scope project types — offer with an import edit, unless a
        // same-named type is already usable.
        for path in decl_registry.item_paths() {
            let Some(leaf) = path.last() else { continue };
            let name = leaf.as_str();
            if path.parent().as_ref() == own_module.as_ref()
                || scope.iter().any(|s| s == path)
                || in_scope_names.contains(name)
            {
                continue;
            }
            let mut item = self.type_item(name, &path.to_string(), decl_registry, path);
            if let Some((range, new_text)) = self.import_edit(&module, content, path) {
                item.additional_text_edits = Some(vec![TextEdit { range, new_text }]);
            }
            items.push(item);
        }

        items
    }

    /// A completion item for a declared type, with a kind reflecting whether
    /// it's a type/enum/bitflags.
    fn type_item(
        &self,
        label: &str,
        detail: &str,
        decl_registry: &DeclarationRegistry,
        path: &ItemPath,
    ) -> CompletionItem {
        use pyxis::grammar::ItemDefinitionInner;
        let kind = match decl_registry.get_definition(path).map(|d| &d.inner) {
            Some(ItemDefinitionInner::Enum(_) | ItemDefinitionInner::Bitflags(_)) => {
                CompletionItemKind::ENUM
            }
            _ => CompletionItemKind::STRUCT,
        };
        CompletionItem {
            label: label.to_string(),
            kind: Some(kind),
            detail: Some(detail.to_string()),
            ..Default::default()
        }
    }
}
