use super::*;

use pyxis::{
    grammar::{
        ExternValueDefinition, FunctionBlock, Ident, ImplItem, Splice, TypeDefinition,
        TypeStatement,
    },
    semantic::types::ItemDefinitionInner as ResolvedInner,
};

/// Cheaply-copied bundle of the per-file analysis state the hover-target
/// helpers all need. Lets [`ServerState::hover_at`] dispatch to focused
/// per-target methods without threading a long parameter list through each.
#[derive(Clone, Copy)]
struct HoverCtx<'a> {
    uri: &'a Uri,
    content: &'a str,
    scope: &'a [ItemPath],
    type_registry: &'a TypeRegistry,
    decl_registry: &'a DeclarationRegistry,
    pointer_size: usize,
    source_set: semantic::SourceSet<'a>,
    tokens: &'a [Token],
    loc: Location,
}

impl ServerState {
    /// textDocument/hover
    pub fn handle_hover(&self, req: Request) -> Response {
        let params: TextDocumentPositionParams = match serde_json::from_value(req.params.clone()) {
            Ok(p) => p,
            Err(e) => return error_response(req.id, e),
        };

        let uri = &params.text_document.uri;
        let position = params.position;

        let Some(ctx) = self.analysis_ctx(uri) else {
            return Response {
                id: req.id,
                result: Some(serde_json::Value::Null),
                error: None,
            };
        };
        // The compiler's cached token stream — span helpers locate identifiers
        // by matching real tokens rather than scanning source text, so names in
        // comments / splices (which lex as comment/string tokens) are ignored.
        let AnalysisCtx {
            content,
            module,
            scope,
            type_registry,
            decl_registry,
            pointer_size,
            source_set,
            tokens_arc,
            ..
        } = ctx;
        let tokens: &[Token] = tokens_arc.as_deref().map(Vec::as_slice).unwrap_or(&[]);

        let loc = lsp_position_to_pyxis_location(content, position);

        let hctx = HoverCtx {
            uri,
            content,
            scope: &scope,
            type_registry,
            decl_registry,
            pointer_size,
            source_set,
            tokens,
            loc,
        };
        match self.hover_at(&hctx, &module) {
            Some((value, span)) => hover_response(req.id, value, content, &span),
            None => Response {
                id: req.id,
                result: Some(serde_json::Value::Null),
                error: None,
            },
        }
    }

    /// The hover markdown and highlight span for whatever sits under
    /// `ctx.loc`, trying each target kind in priority order. `None` if nothing
    /// hoverable is there.
    fn hover_at(&self, ctx: &HoverCtx, module: &Module) -> Option<(String, Span)> {
        // 0. A doc-comment cross-reference link → describe the referenced member
        //    (or type), over the whole link.
        if let Some((span, _location, hover)) = self.doc_link_at(ctx.uri, &ctx.loc) {
            return Some((hover, span));
        }

        // 1. Cursor on a type or import reference (e.g. a field's type, or a
        //    segment of a `use`/FQN path) → hover the *referenced* item, not the
        //    enclosing definition. For an intermediate FQN segment, show the
        //    module it names. A pointer/array/unknown *shell* (rather than the
        //    pointee/element) describes the shape — at every type position.
        if let Some(hit) = self.hover_reference(ctx, module) {
            return Some(hit);
        }

        // 2. An attribute → describe the attribute itself, not the item it's
        //    attached to. Must precede the structural checks below, since an
        //    attribute's span sits inside its field/type's span.
        if let Some((attribute, span)) = attribute_at(module, &ctx.loc) {
            return Some((format_attribute_hover(attribute, &span, ctx.content), span));
        }

        // 2b. A numeric/character literal token → describe the literal value
        //     (its representations), not the enclosing variant/field/const.
        //     Precedes the structural checks so an enum variant's value hovers
        //     as the literal rather than resolving to the variant.
        if let Some(token) = ctx
            .tokens
            .iter()
            .find(|t| t.location.span.contains(&ctx.loc))
            && let Some(md) = literal_hover(&token.kind)
        {
            return Some((md, token.location.span));
        }

        // 3. Structural elements — scoped tightly to what's under the cursor
        //    rather than the whole enclosing definition:
        //    - a definition's own name → the type (size/align/fields);
        //    - a field name → the field (type + attributes + size);
        //    - a vftable entry or an impl method → its signature.
        for item in &module.items {
            let hit = match item {
                ModuleItem::Definition { definition } => self.hover_definition(ctx, definition),
                ModuleItem::Impl { impl_block } => self.hover_impl(ctx, impl_block),
                ModuleItem::Function { function } if function.location.span.contains(&ctx.loc) => {
                    self.hover_function(ctx, function)
                }
                ModuleItem::ExternType { name, location, .. } => {
                    self.hover_extern_type(ctx, name, location)
                }
                ModuleItem::Splice { splice } => self.hover_splice(ctx, splice),
                _ => None,
            };
            if hit.is_some() {
                return hit;
            }
        }
        None
    }

    /// Branch 1: a type / import / FQN-segment reference, or a pointer/array
    /// shell. A `Ref::Item` that resolves to neither a type nor a module yields
    /// `None`, so the caller falls through to the structural checks.
    fn hover_reference(&self, ctx: &HoverCtx, module: &Module) -> Option<(String, Span)> {
        match find_reference_at(
            module,
            &ctx.loc,
            ctx.scope,
            ctx.decl_registry,
            ctx.tokens,
            ctx.type_registry,
            ctx.pointer_size,
        )? {
            Ref::Item {
                item,
                module_path,
                span,
            } => {
                let hover = item
                    .as_ref()
                    .and_then(|item_path| {
                        self.type_hover_text(
                            item_path,
                            ctx.type_registry,
                            ctx.decl_registry,
                            ctx.uri,
                        )
                    })
                    .or_else(|| {
                        self.module_uri(&module_path, ctx.uri)
                            .map(|_| format!("**module** `{module_path}`"))
                    })?;
                Some((hover, span))
            }
            Ref::Shell { md, span } => Some((md, span)),
        }
    }

    /// Branch 3, a type/enum/bitflags definition: its own name, then any nested
    /// item declarations, then its body (fields / vftable for a type, variants
    /// for an enum/bitflags).
    fn hover_definition(
        &self,
        ctx: &HoverCtx,
        definition: &ItemDefinition,
    ) -> Option<(String, Span)> {
        // An extern value is a value item (not a type), so its name hover shows
        // the pointed-to type's size rather than the value item's own zero size —
        // handled before hover_definition_name, which resolves names as types.
        if let ItemDefinitionInner::ExternValue(ev) = &definition.inner {
            return self.hover_extern_value(ctx, definition, ev);
        }
        if let Some(hit) = self.hover_definition_name(ctx, definition) {
            return Some(hit);
        }
        // Nested item declarations (`const`/`type`/`enum`/… inside a body) hover
        // like top-level items — recurse so a nested const's name, or a nested
        // type's fields, resolve. These live in each body's item list, not its
        // `statements()` (which yields only fields/variants).
        for nested in nested_items(definition) {
            if let Some(hit) = self.hover_definition(ctx, nested) {
                return Some(hit);
            }
        }
        match &definition.inner {
            ItemDefinitionInner::Type(td) => self.hover_type_body(ctx, definition, td),
            ItemDefinitionInner::Enum(e) => self.hover_variant(
                ctx,
                definition,
                "variant",
                e.statements().map(|s| {
                    (
                        &s.location,
                        s.name.as_str(),
                        &s.attributes,
                        s.doc_comments.as_slice(),
                    )
                }),
            ),
            ItemDefinitionInner::Bitflags(b) => self.hover_variant(
                ctx,
                definition,
                "flag",
                b.statements().map(|s| {
                    (
                        &s.location,
                        s.name.as_str(),
                        &s.attributes,
                        s.doc_comments.as_slice(),
                    )
                }),
            ),
            ItemDefinitionInner::TypeAlias(_) => None,
            ItemDefinitionInner::Constant(_) => None,
            // Handled above, before hover_definition_name.
            ItemDefinitionInner::ExternValue(_) => None,
        }
    }

    /// The definition's own name → the type's hover (size/align/fields), or its
    /// declaration form when it doesn't resolve cleanly.
    fn hover_definition_name(
        &self,
        ctx: &HoverCtx,
        definition: &ItemDefinition,
    ) -> Option<(String, Span)> {
        let span = name_token_span(
            ctx.tokens,
            &definition.declaration_location.span.start,
            definition.name.as_str(),
        )?;
        if !span.contains(&ctx.loc) {
            return None;
        }
        let item_path = self.definition_path(ctx.uri, definition.name.as_str());
        let resolved = resolve_item(&self.db, ctx.source_set, ctx.pointer_size, item_path);
        let value = match resolved.item(&self.db).resolved() {
            Some(rs) => format_type_hover_with_size(definition, rs.size, rs.alignment),
            None => format_type_hover(definition),
        };
        Some((value, span))
    }

    /// A type definition's body: a field (type + attributes + size + offset) or
    /// a vftable entry. Deliberately no "anywhere else in the body" fallback: a
    /// hover must never highlight a token the cursor isn't on, so blank space /
    /// braces / keywords resolve to nothing rather than the (distant) type name.
    fn hover_type_body(
        &self,
        ctx: &HoverCtx,
        definition: &ItemDefinition,
        td: &TypeDefinition,
    ) -> Option<(String, Span)> {
        for statement in td.statements() {
            if !statement.location.span.contains(&ctx.loc) {
                continue;
            }
            match &statement.field {
                TypeField::Field(vis, name, type_) => {
                    // The pointer/array shell and the pointee/element are both
                    // handled as references in branch 1; here we only describe
                    // the field.
                    let span =
                        name_token_span(ctx.tokens, &statement.location.span.start, name.as_str())
                            .unwrap_or(statement.location.span);
                    let size = type_size_of(
                        type_,
                        ctx.type_registry,
                        ctx.scope,
                        ctx.decl_registry,
                        ctx.pointer_size,
                    );
                    // Offset within the parent type's resolved layout. The parent
                    // is resolved via resolve_item (analyze()'s registry leaves
                    // composite types unresolved).
                    let parent_path = self.definition_path(ctx.uri, definition.name.as_str());
                    let parent =
                        resolve_item(&self.db, ctx.source_set, ctx.pointer_size, parent_path);
                    let offset = parent
                        .item(&self.db)
                        .resolved()
                        .and_then(|rs| field_offset(rs, name.as_str(), ctx.type_registry));
                    let value =
                        format_field_hover(vis, name, type_, &statement.attributes, size, offset);
                    return Some((value, span));
                }
                TypeField::Vftable(fns) => {
                    if let Some(hit) = self.hover_vftable(ctx, definition, statement, fns) {
                        return Some(hit);
                    }
                }
                TypeField::Item(_) => {
                    // Nested items are hovered by hover_definition's recursion
                    // (via nested_items), which runs before this body walk.
                }
            }
        }
        None
    }

    /// A vftable statement under the cursor: an entry's argument name, its
    /// `self` receiver (→ the owning type), the entry itself (with slot index
    /// and byte offset), or the `vftable` keyword (→ the generated vtable
    /// struct, whose resolved count includes inherited entries).
    fn hover_vftable(
        &self,
        ctx: &HoverCtx,
        definition: &ItemDefinition,
        statement: &TypeStatement,
        fns: &[Function],
    ) -> Option<(String, Span)> {
        for f in fns {
            if !f.location.span.contains(&ctx.loc) {
                continue;
            }
            // Arg/return *types* (including pointer/array shells) are handled in
            // branch 1. An argument name…
            if let Some(hit) = named_arg_hover(
                f,
                &ctx.loc,
                ctx.tokens,
                ctx.type_registry,
                ctx.scope,
                ctx.decl_registry,
                ctx.pointer_size,
            ) {
                return Some(hit);
            }
            // …`self`, resolving to the owning type…
            if let Some(span) = self_arg_span(f, &ctx.loc) {
                let owner = self.definition_path(ctx.uri, definition.name.as_str());
                if let Some(value) =
                    self.type_hover_text(&owner, ctx.type_registry, ctx.decl_registry, ctx.uri)
                {
                    return Some((value, span));
                }
            }
            // …or the function itself, annotated with its vftable slot index and
            // byte offset.
            let span = name_token_span(ctx.tokens, &f.location.span.start, f.name.as_str())
                .unwrap_or(f.location.span);
            let index = vftable_index_of(fns, f);
            return Some((format_vftable_fn_hover(f, index, ctx.pointer_size), span));
        }
        // Cursor on the `vftable` keyword → describe the generated vtable struct.
        let span = name_token_span(ctx.tokens, &statement.location.span.start, "vftable")?;
        if !span.contains(&ctx.loc) {
            return None;
        }
        let owner = self.definition_path(ctx.uri, definition.name.as_str());
        let resolved = resolve_item(&self.db, ctx.source_set, ctx.pointer_size, owner);
        let count = match resolved.item(&self.db).resolved() {
            Some(rs) => match &rs.inner {
                ResolvedInner::Type(td) => td
                    .vftable
                    .as_ref()
                    .map(|v| v.functions.len())
                    .unwrap_or(fns.len()),
                _ => fns.len(),
            },
            None => fns.len(),
        };
        let md = format!(
            "**vftable** of `{}`\n\nGenerates a vtable struct with `{count}` virtual function(s).",
            definition.name.as_str(),
        );
        Some((md, span))
    }

    /// An enum variant or bitflags flag under the cursor → the member and its
    /// resolved value. `statements` yields `(location, name, attributes, doc)`
    /// so enum and bitflags share one body despite their distinct node types.
    fn hover_variant<'a>(
        &self,
        ctx: &HoverCtx,
        definition: &ItemDefinition,
        kind_label: &str,
        statements: impl Iterator<Item = (&'a ItemLocation, &'a str, &'a Attributes, &'a [String])>,
    ) -> Option<(String, Span)> {
        for (location, name, attributes, doc_comments) in statements {
            if !location.span.contains(&ctx.loc) {
                continue;
            }
            let span =
                name_token_span(ctx.tokens, &location.span.start, name).unwrap_or(location.span);
            let value =
                self.variant_value(ctx.uri, definition, name, ctx.source_set, ctx.pointer_size);
            let md = format_variant_hover(kind_label, name, value, attributes, doc_comments);
            return Some((md, span));
        }
        None
    }

    /// Branch 3, an `impl` block (including `#[cfg(...)]`-gated ones): a method's
    /// argument name, its `self` receiver (→ the impl target type), or the
    /// method itself.
    fn hover_impl(&self, ctx: &HoverCtx, impl_block: &FunctionBlock) -> Option<(String, Span)> {
        for impl_item in &impl_block.items {
            let ImplItem::Function(f) = impl_item else {
                continue;
            };
            if !f.location.span.contains(&ctx.loc) {
                continue;
            }
            // Arg/return *types* (including pointer/array shells) are handled in
            // branch 1. An argument name…
            if let Some(hit) = named_arg_hover(
                f,
                &ctx.loc,
                ctx.tokens,
                ctx.type_registry,
                ctx.scope,
                ctx.decl_registry,
                ctx.pointer_size,
            ) {
                return Some(hit);
            }
            // …`self`, resolving to the impl target type…
            if let Some(span) = self_arg_span(f, &ctx.loc) {
                let owner = ItemPath::from(impl_block.name.as_str());
                if let Some(resolved) = resolve_type_path(&owner, ctx.scope, ctx.decl_registry)
                    && let Some(value) = self.type_hover_text(
                        &resolved,
                        ctx.type_registry,
                        ctx.decl_registry,
                        ctx.uri,
                    )
                {
                    return Some((value, span));
                }
            }
            // …or the function itself.
            let span = name_token_span(ctx.tokens, &f.location.span.start, f.name.as_str())
                .unwrap_or(f.location.span);
            return Some((format_function_hover(f), span));
        }
        None
    }

    /// Branch 3, a free function under the cursor: an argument name, else the
    /// function itself.
    fn hover_function(&self, ctx: &HoverCtx, function: &Function) -> Option<(String, Span)> {
        // Arg/return types (including shells) → branch 1.
        if let Some(hit) = named_arg_hover(
            function,
            &ctx.loc,
            ctx.tokens,
            ctx.type_registry,
            ctx.scope,
            ctx.decl_registry,
            ctx.pointer_size,
        ) {
            return Some(hit);
        }
        let span = name_token_span(
            ctx.tokens,
            &function.location.span.start,
            function.name.as_str(),
        )
        .unwrap_or(function.location.span);
        Some((format_function_hover(function), span))
    }

    /// Branch 3, `extern name: Type;` → the value's own name.
    fn hover_extern_value(
        &self,
        ctx: &HoverCtx,
        definition: &ItemDefinition,
        ev: &ExternValueDefinition,
    ) -> Option<(String, Span)> {
        let span = name_token_span(
            ctx.tokens,
            &definition.declaration_location.span.start,
            definition.name.as_str(),
        )?;
        if !span.contains(&ctx.loc) {
            return None;
        }
        let mut md = format_extern_value_hover(definition.name.as_str(), &ev.type_, &ev.attributes);
        if let Some(size) = type_size_of(
            &ev.type_,
            ctx.type_registry,
            ctx.scope,
            ctx.decl_registry,
            ctx.pointer_size,
        ) {
            push_facts(&mut md, &[("type size", fmt_bytes(size))]);
        }
        Some((md, span))
    }

    /// Branch 3, `extern type Name;` → the declared name.
    fn hover_extern_type(
        &self,
        ctx: &HoverCtx,
        name: &Ident,
        location: &ItemLocation,
    ) -> Option<(String, Span)> {
        let span = name_token_span(ctx.tokens, &location.span.start, name.as_str())?;
        if !span.contains(&ctx.loc) {
            return None;
        }
        let path = match self.module_path_for(ctx.uri) {
            Some(mp) => mp.join(name.as_str().into()),
            None => ItemPath::from(name.as_str()),
        };
        let value = self
            .type_hover_text(&path, ctx.type_registry, ctx.decl_registry, ctx.uri)
            .unwrap_or_else(|| format!("**extern type** `{}`", name.as_str()));
        Some((value, span))
    }

    /// Branch 3, a splice statement: its keywords
    /// (prologue/epilogue/definition/for).
    fn hover_splice(&self, ctx: &HoverCtx, splice: &Splice) -> Option<(String, Span)> {
        if !splice.location.span.contains(&ctx.loc) {
            return None;
        }
        splice_term_at(ctx.tokens, splice, &ctx.loc)
    }
}

/// Build a hover Response with markdown content and a highlight range.
pub(crate) fn hover_response(
    id: lsp_server::RequestId,
    value: String,
    content: &str,
    span: &Span,
) -> Response {
    Response {
        id,
        result: Some(
            serde_json::to_value(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value,
                }),
                range: Some(pyxis_span_to_lsp_range(content, span)),
            })
            .unwrap(),
        ),
        error: None,
    }
}
