use super::*;

use pyxis::grammar::Argument;

pub(crate) enum Ref {
    Item {
        /// Resolved concrete item path, if this segment names a type/extern/predefined.
        item: Option<ItemPath>,
        /// Absolute path of this segment — used as a module target when `item`
        /// is `None` (e.g. the `game::event_handler` part of an FQN reference).
        module_path: ItemPath,
        /// Source span of the *segment* under the cursor (hover-range highlight).
        span: Span,
    },
    Shell {
        /// Pre-rendered hover markdown for the shell.
        md: String,
        /// Span of the shell type (hover-range highlight). Shells have no
        /// definition, so go-to-def ignores this variant.
        span: Span,
    },
}

/// The result of locating the cursor within a grammar `Type`: either on a named
/// type identifier (the leaf to resolve) or on a pointer/array/unknown shell.
pub(crate) enum TypeHit<'a> {
    Ident(&'a ItemPath, Span),
    Shell(&'a Type),
}

/// Find a type or import reference at the given location in a parsed module.
///
/// Walks `use` statements and type positions (field types, enum/bitflags base
/// types, type-alias targets, vftable signatures) looking for the AST node
/// whose span contains the cursor. For multi-segment paths (`a::b::C`), the
/// *individual* segment under the cursor is resolved independently: the leaf
/// resolves to a type, earlier segments to their module namespaces.
///
/// Resolution itself comes from the Salsa-computed `DeclarationRegistry` —
/// this function only does the position→node matching, which is an LSP
/// concern (the compiler never works with cursor positions).
pub(crate) fn find_reference_at(
    module: &Module,
    loc: &Location,
    scope: &[ItemPath],
    decl_registry: &DeclarationRegistry,
    tokens: &[Token],
    type_registry: &TypeRegistry,
    pointer_size: usize,
) -> Option<Ref> {
    let resolve = |raw: &ItemPath, full_span: Span| -> Ref {
        let (sub_path, seg_span) =
            segment_at(raw, &full_span, loc, tokens).unwrap_or((raw.clone(), full_span));
        let item = resolve_type_path(&sub_path, scope, decl_registry);
        Ref::Item {
            item,
            module_path: sub_path,
            span: seg_span,
        }
    };
    // For `use` trees the segment is already resolved by use_tree_reference, so
    // build the reference directly without re-running segment_at.
    let make_ref = |seg_path: ItemPath, span: Span| -> Ref {
        let item = resolve_type_path(&seg_path, scope, decl_registry);
        Ref::Item {
            item,
            module_path: seg_path,
            span,
        }
    };
    // Turn a located type-position hit into a reference: a named ident resolves
    // like any other path; a pointer/array/unknown shell renders its shape.
    let from_hit = |hit: TypeHit| -> Ref {
        match hit {
            TypeHit::Ident(path, span) => resolve(path, span),
            TypeHit::Shell(type_) => Ref::Shell {
                md: shell_hover_md(type_, type_registry, scope, decl_registry, pointer_size),
                span: type_.location().span,
            },
        }
    };

    use pyxis::grammar::ImplItem;

    for item in &module.items {
        match item {
            // `use` statements: cursor on an imported path.
            ModuleItem::Use { tree, .. } => {
                if let Some((path, span)) = use_tree_reference(tree, loc, tokens) {
                    return Some(make_ref(path, span));
                }
            }
            // `backend` blocks carry their own `use`-style dependency list, and
            // prologue/epilogue splices can be attributed `for <Type>`.
            ModuleItem::Backend { backend } => {
                for tree in &backend.uses {
                    if let Some((path, span)) = use_tree_reference(tree, loc, tokens) {
                        return Some(make_ref(path, span));
                    }
                }
                for for_type in [&backend.prologue.for_type, &backend.epilogue.for_type]
                    .into_iter()
                    .flatten()
                {
                    if let Some(span) =
                        find_for_path_span(tokens, &backend.location.span, for_type, loc)
                    {
                        return Some(resolve(for_type, span));
                    }
                }
            }
            // `impl` blocks (including `#[cfg(...)]`-gated ones): the target
            // type name and every function signature's types.
            ModuleItem::Impl { impl_block } => {
                if let Some(span) = name_token_span(
                    tokens,
                    &impl_block.location.span.start,
                    impl_block.name.as_str(),
                ) && span.contains(loc)
                {
                    let raw = ItemPath::from(impl_block.name.as_str());
                    return Some(resolve(&raw, span));
                }
                for impl_item in &impl_block.items {
                    if let ImplItem::Function(f) = impl_item
                        && let Some(hit) = fn_signature_type_ref(f, loc)
                    {
                        return Some(from_hit(hit));
                    }
                }
            }
            // Free functions: argument and return types.
            ModuleItem::Function { function } => {
                if let Some(hit) = fn_signature_type_ref(function, loc) {
                    return Some(from_hit(hit));
                }
            }
            // Extern values: `extern name: Type;` — the referenced type.
            ModuleItem::ExternValue { extern_value } => {
                if let Some(hit) = type_hit_at(&extern_value.type_, loc) {
                    return Some(from_hit(hit));
                }
            }
            _ => {}
        }
    }

    // Type positions inside definitions.
    for definition in module.definitions() {
        if let Some(hit) = find_type_ref_in_definition(definition, loc) {
            return Some(from_hit(hit));
        }
    }

    None
}

/// Given a `::`-separated path as written in `source`, its full span, and a
/// cursor location, return the path truncated to the segment under the cursor
/// (an absolute prefix for FQN paths) plus that segment's own span. Paths are
/// always written on a single line.
pub(crate) fn segment_at(
    path: &ItemPath,
    span: &Span,
    loc: &Location,
    tokens: &[Token],
) -> Option<(ItemPath, Span)> {
    // The path's identifier tokens, in order, within the path's span (the `::`
    // separators lex as their own `ColonColon` tokens and are skipped).
    let idents = tokens
        .iter()
        .filter(|t| matches!(t.kind, TokenKind::Ident(_)) && span.contains(&t.location.span.start));
    for (i, ident) in idents.enumerate() {
        if ident.location.span.contains(loc) {
            let count = (i + 1).min(path.len());
            let prefix: ItemPath = path.iter().take(count).cloned().collect();
            return Some((prefix, ident.location.span));
        }
    }
    None
}

/// Find a type hit (argument or return type) under the cursor in a function
/// signature — a named ident or a pointer/array/unknown shell.
pub(crate) fn fn_signature_type_ref<'a>(f: &'a Function, loc: &Location) -> Option<TypeHit<'a>> {
    for arg in &f.arguments {
        if let Argument::Named { type_, .. } = arg
            && let Some(found) = type_hit_at(type_, loc)
        {
            return Some(found);
        }
    }
    if let Some(ret) = &f.return_type
        && let Some(found) = type_hit_at(ret, loc)
    {
        return Some(found);
    }
    None
}

pub(crate) fn find_type_ref_in_definition<'a>(
    definition: &'a ItemDefinition,
    loc: &Location,
) -> Option<TypeHit<'a>> {
    use pyxis::grammar::ItemDefinitionInner;

    match &definition.inner {
        ItemDefinitionInner::Type(td) => {
            for statement in td.statements() {
                match &statement.field {
                    TypeField::Field(_, _, type_) => {
                        if let Some(found) = type_hit_at(type_, loc) {
                            return Some(found);
                        }
                    }
                    TypeField::Vftable(fns) => {
                        for sig in fns {
                            for arg in &sig.arguments {
                                if let Argument::Named { type_, .. } = arg
                                    && let Some(found) = type_hit_at(type_, loc)
                                {
                                    return Some(found);
                                }
                            }
                            if let Some(ret) = &sig.return_type
                                && let Some(found) = type_hit_at(ret, loc)
                            {
                                return Some(found);
                            }
                        }
                    }
                    TypeField::Item(_) => {
                        // Nested item type references are handled at the top level
                    }
                }
            }
        }
        ItemDefinitionInner::Enum(e) => return type_hit_at(&e.type_, loc),
        ItemDefinitionInner::Bitflags(b) => return type_hit_at(&b.type_, loc),
        ItemDefinitionInner::TypeAlias(ta) => return type_hit_at(&ta.target, loc),
    }

    None
}

/// Resolve the import-path segment under the cursor within a `use` tree.
///
/// Returns the segment's full path (prefix-joined for grouped imports) and the
/// segment's span. Handles both the imported leaves and the shared prefix of a
/// braced group, resolving the specific `::`-separated segment the cursor is on
/// — so each ident of `types::shared_ptr::{SharedPtr, WeakPtr}` navigates
/// independently. The returned path is already truncated to the segment, so
/// callers must NOT run `segment_at` on it again.
pub(crate) fn use_tree_reference(
    tree: &UseTree,
    loc: &Location,
    tokens: &[Token],
) -> Option<(ItemPath, Span)> {
    match tree {
        UseTree::Path { path, location } => {
            if !location.span.contains(loc) {
                return None;
            }
            segment_at(path, &location.span, loc, tokens)
        }
        UseTree::Group {
            prefix,
            items,
            location,
        } => {
            // NB: a group's span starts at `{`, so the shared prefix that
            // precedes it is *outside* this span — don't early-return on it.
            // A leaf import — prepend the group's prefix to the matched segment.
            for item in items {
                if let Some((sub, span)) = use_tree_reference(item, loc, tokens) {
                    let full: ItemPath = prefix.iter().chain(sub.iter()).cloned().collect();
                    return Some((full, span));
                }
            }
            // The shared prefix (`a::b` in `a::b::{C, D}`). The group's span
            // starts at `{`, so the prefix tokens precede it — they're the
            // contiguous run of Ident/:: tokens immediately before the brace.
            if !prefix.is_empty() {
                let brace = location.span.start;
                if let Some(brace_idx) = tokens.iter().position(|t| t.location.span.start >= brace)
                {
                    let mut start_idx = brace_idx;
                    while start_idx > 0
                        && matches!(
                            tokens[start_idx - 1].kind,
                            TokenKind::Ident(_) | TokenKind::ColonColon
                        )
                    {
                        start_idx -= 1;
                    }
                    if start_idx < brace_idx {
                        let prefix_span = Span::new(
                            tokens[start_idx].location.span.start,
                            tokens[brace_idx - 1].location.span.end,
                        );
                        if prefix_span.contains(loc) {
                            return segment_at(prefix, &prefix_span, loc, tokens);
                        }
                    }
                }
            }
            None
        }
    }
}

/// Recursively locate the cursor within a grammar `Type`. Returns whether the
/// cursor is on a named type identifier (the leaf to resolve — checking the
/// narrower generic args first) or on a pointer/array/unknown *shell* (the
/// pointer/array/unknown wrapping, rather than its inner pointee/element). This
/// single walk drives both type-reference resolution and shell-shape hovers at
/// every type position.
pub(crate) fn type_hit_at<'a>(type_: &'a Type, loc: &Location) -> Option<TypeHit<'a>> {
    if !type_.location().span.contains(loc) {
        return None;
    }
    match type_ {
        Type::Ident {
            path, generic_args, ..
        } => {
            for arg in generic_args {
                if let Some(found) = type_hit_at(arg, loc) {
                    return Some(found);
                }
            }
            Some(TypeHit::Ident(path, type_.location().span))
        }
        Type::ConstPointer { pointee, .. } | Type::MutPointer { pointee, .. } => {
            if pointee.location().span.contains(loc) {
                type_hit_at(pointee, loc)
            } else {
                Some(TypeHit::Shell(type_))
            }
        }
        Type::Array { element, .. } => {
            if element.location().span.contains(loc) {
                type_hit_at(element, loc)
            } else {
                Some(TypeHit::Shell(type_))
            }
        }
        Type::Unknown { .. } => Some(TypeHit::Shell(type_)),
    }
}

/// Resolve a type path (from the grammar AST) to a full `ItemPath` using the
/// declaration registry's scope resolution (handles use-statements, relative
/// paths, and absolute paths). This is the Salsa-computed resolution — we're
/// just calling it, not reimplementing it.
pub(crate) fn resolve_type_path(
    path: &ItemPath,
    scope: &[ItemPath],
    decl_registry: &DeclarationRegistry,
) -> Option<ItemPath> {
    use pyxis::semantic::declaration_registry::NameResolution;

    // For multi-segment paths (e.g. `types::math::Vector2`), the path is
    // likely absolute — check it directly against the registry first.
    if path.len() > 1 && decl_registry.contains(path) {
        return Some(path.clone());
    }

    // Single-segment or unresolved multi-segment: resolve the leaf name
    // through the scope (handles use-statements and relative resolution).
    let name = path.last()?.as_str();
    match decl_registry.resolve_name(scope, name) {
        NameResolution::Found(p)
        | NameResolution::FoundPredefined(p)
        | NameResolution::FoundExtern(p) => Some(p),
        NameResolution::NotFound => None,
    }
}

/// Find the span of the identifier `name` at or after `from` in the token
/// stream.
///
/// Grammar `Ident`s don't carry their own span, so name spans (type names,
/// field names, function names) are recovered from the compiler's cached token
/// stream: the first token at/after the declaration's start point whose text
/// matches `name`. Keyword spellings (e.g. `vftable` → `TokenKind::Vftable`)
/// lex as their own token rather than an `Ident`, so they're matched via
/// [`TokenKind::keyword_str`]. Working off real tokens (rather
/// than a text scan) means occurrences inside comments or `r#"…"#` splices —
/// which lex as comment/string tokens, not `Ident`s — are correctly ignored.
pub(crate) fn name_token_span(tokens: &[Token], from: &Location, name: &str) -> Option<Span> {
    if name.is_empty() {
        return None;
    }
    tokens
        .iter()
        .find(|t| {
            t.location.span.start >= *from
                && match &t.kind {
                    TokenKind::Ident(s) => s == name,
                    kind => kind.keyword_str() == Some(name),
                }
        })
        .map(|t| t.location.span)
}

/// Find the span of a `for <path>` clause (in a backend prologue/epilogue)
/// whose path matches `path` and whose span contains `loc`. The `for_type`
/// clause has no recorded span, so we recover it from the token stream: the
/// `for` keyword (which lexes as `Ident("for")`) within the backend block,
/// followed by the run of `Ident`/`::` tokens that make up the path.
pub(crate) fn find_for_path_span(
    tokens: &[Token],
    block_span: &Span,
    path: &ItemPath,
    loc: &Location,
) -> Option<Span> {
    for (i, t) in tokens.iter().enumerate() {
        if !block_span.contains(&t.location.span.start) {
            continue;
        }
        if !matches!(&t.kind, TokenKind::Ident(s) if s == "for") {
            continue;
        }
        // The path is the run of Ident/:: tokens immediately following `for`.
        let run_start = i + 1;
        let mut run_end = run_start;
        while run_end < tokens.len()
            && matches!(
                tokens[run_end].kind,
                TokenKind::Ident(_) | TokenKind::ColonColon
            )
        {
            run_end += 1;
        }
        let run = &tokens[run_start..run_end];
        let (Some(first), Some(last)) = (run.first(), run.last()) else {
            continue;
        };
        let span = Span::new(first.location.span.start, last.location.span.end);
        if span.contains(loc) && run_matches_path(run, path) {
            return Some(span);
        }
    }
    None
}

/// Whether the `Ident` tokens of `run` (ignoring the interleaved `::`) name
/// exactly the segments of `path`.
pub(crate) fn run_matches_path(run: &[Token], path: &ItemPath) -> bool {
    let idents: Vec<&str> = run
        .iter()
        .filter_map(|t| match &t.kind {
            TokenKind::Ident(s) => Some(s.as_str()),
            _ => None,
        })
        .collect();
    idents.len() == path.len()
        && idents
            .iter()
            .zip(path.iter())
            .all(|(seg, expected)| *seg == expected.as_str())
}
