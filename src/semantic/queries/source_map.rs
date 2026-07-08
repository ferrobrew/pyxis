//! Per-file source map: the leaf-segment span of every type reference in a file
//! paired with the `ItemPath` it resolves to. A single, incrementally-cached
//! source of truth for "what type is referenced at this position", consumed by
//! the LSP to find references / drive rename.

use std::sync::Arc;

use crate::{
    grammar::{
        Argument, Function, ImplItem, ItemDefinition, ItemDefinitionInner, ItemPath, ModuleItem,
        Type, TypeField,
    },
    semantic::name_index::NameIndex,
    span::{HasLocation, Location, Span},
    tokenizer::{Token, TokenKind},
};

use super::{
    super::{
        db::Db,
        inputs::{SourceFile, SourceSet},
        ir::FileTypeReferences,
    },
    index::name_index,
    leaf::{parse_file, tokenize_file},
};

/// Per-file source map: the leaf-segment span of every type reference in a file
/// (in type positions *and* `use` trees) paired with the `ItemPath` it resolves
/// to — a single, incrementally-cached source of truth for "what type is
/// referenced at this position". Depends on this file's `parse_file`/
/// `tokenize_file` and `name_index` (whose output backdates), so it recomputes
/// only for the edited file. The LSP uses it to find references / drive rename
/// without re-walking and re-resolving every file per request.
#[salsa::tracked]
pub fn file_type_references<'db>(
    db: &'db dyn Db,
    source: SourceFile,
    sources: SourceSet<'db>,
    pointer_size: usize,
) -> FileTypeReferences<'db> {
    let index = name_index(db, sources, pointer_size).index(db);
    let parsed = parse_file(db, source);
    let module = parsed.module(db);
    let path_str = source.path(db);
    let module_path = ItemPath::from_path(std::path::Path::new(path_str.as_str()));
    let scope = index
        .module_scope(&module_path)
        .map(<[ItemPath]>::to_vec)
        .unwrap_or_default();
    let tokens: &[Token] = tokenize_file(db, source).tokens(db);

    // Every place a type can be referenced — mirrors the LSP's find_reference_at
    // so this is a faithful, precomputed source of truth.
    let mut references = Vec::new();
    for item in &module.items {
        match item {
            // Definitions: field types, vftable signatures, bases, aliases.
            ModuleItem::Definition { definition } => {
                collect_type_ref_spans(definition, &scope, index, tokens, &mut references)
            }
            // `use`-tree leaves.
            ModuleItem::Use { tree, .. } => {
                for (leaf_path, location) in tree.flatten_with_locations() {
                    if let Some(resolved) = index.resolve_path(&scope, &leaf_path)
                        && let Some(leaf) =
                            last_ident_span(tokens, location.span.start, location.span.end)
                    {
                        references.push((leaf, resolved));
                    }
                }
            }
            // `impl <Type>` target name + each method signature's types.
            ModuleItem::Impl { impl_block } => {
                let name = impl_block.name.as_str();
                if let Some(resolved) = index.resolve_path(&scope, &ItemPath::from(name))
                    && let Some(span) =
                        first_ident_span(tokens, impl_block.location.span.start, name)
                {
                    references.push((span, resolved));
                }
                for impl_item in &impl_block.items {
                    if let ImplItem::Function(f) = impl_item {
                        fn_sig_type_spans(f, &scope, index, tokens, &mut references);
                    }
                }
            }
            // Free-function signature types.
            ModuleItem::Function { function } => {
                fn_sig_type_spans(function, &scope, index, tokens, &mut references)
            }
            // Splice statements: the `for <Type>` attribution clause. (Any
            // cfg-gated `use` is a plain `ModuleItem::Use`, handled above.)
            ModuleItem::Splice { splice } => {
                if let Some(for_type) = &splice.for_type
                    && let Some(resolved) = index.resolve_path(&scope, for_type)
                    && let Some(span) = for_type_span(tokens, &splice.location.span, for_type)
                {
                    references.push((span, resolved));
                }
            }
            _ => {}
        }
    }
    FileTypeReferences::new(db, Arc::new(references))
}

/// Collect `(leaf-segment span, resolved path)` for every named type reference
/// in a definition — fields, vftable signatures, bases/aliases, recursing
/// through pointers, arrays, and generic arguments. The span is the *leaf*
/// identifier (`Foo` in `a::b::Foo`, `Map` in `Map<Foo>`) so rename rewrites
/// exactly that token. Powers the per-file source map.
fn collect_type_ref_spans(
    definition: &ItemDefinition,
    scope: &[ItemPath],
    index: &NameIndex,
    tokens: &[Token],
    out: &mut Vec<(Span, ItemPath)>,
) {
    match &definition.inner {
        ItemDefinitionInner::Type(td) => {
            for statement in td.statements() {
                match &statement.field {
                    TypeField::Field(_, _, type_) => {
                        type_ref_spans(type_, scope, index, tokens, out)
                    }
                    TypeField::Vftable(functions) => {
                        for function in functions {
                            for argument in &function.arguments {
                                if let Argument::Named { type_, .. } = argument {
                                    type_ref_spans(type_, scope, index, tokens, out);
                                }
                            }
                            if let Some(return_type) = &function.return_type {
                                type_ref_spans(return_type, scope, index, tokens, out);
                            }
                        }
                    }
                    TypeField::Item(inner_def) => {
                        // Collect type reference spans from nested item definitions
                        collect_type_ref_spans(inner_def, scope, index, tokens, out);
                    }
                }
            }
        }
        ItemDefinitionInner::Enum(e) => type_ref_spans(&e.type_, scope, index, tokens, out),
        ItemDefinitionInner::Bitflags(b) => type_ref_spans(&b.type_, scope, index, tokens, out),
        ItemDefinitionInner::TypeAlias(ta) => type_ref_spans(&ta.target, scope, index, tokens, out),
        ItemDefinitionInner::Constant(cd) => type_ref_spans(&cd.type_, scope, index, tokens, out),
        ItemDefinitionInner::ExternValue(ev) => {
            type_ref_spans(&ev.type_, scope, index, tokens, out)
        }
    }
}

/// Collect `(leaf span, resolved path)` for the argument and return types of a
/// function signature (impl method or free function).
fn fn_sig_type_spans(
    function: &Function,
    scope: &[ItemPath],
    index: &NameIndex,
    tokens: &[Token],
    out: &mut Vec<(Span, ItemPath)>,
) {
    for argument in &function.arguments {
        if let Argument::Named { type_, .. } = argument {
            type_ref_spans(type_, scope, index, tokens, out);
        }
    }
    if let Some(return_type) = &function.return_type {
        type_ref_spans(return_type, scope, index, tokens, out);
    }
}

/// The leaf-segment span of a `for <path>` clause (backend prologue/epilogue)
/// inside `block_span`. Finds the `for` keyword (which lexes as `Ident("for")`),
/// the run of `Ident`/`::` after it, and returns the last identifier's span when
/// its leaf matches `for_type`.
fn for_type_span(tokens: &[Token], block_span: &Span, for_type: &ItemPath) -> Option<Span> {
    let leaf = for_type.last()?.as_str();
    let mut i = 0;
    while i < tokens.len() {
        if block_span.contains(&tokens[i].location.span.start)
            && matches!(&tokens[i].kind, TokenKind::Ident(s) if s == "for")
        {
            let mut j = i + 1;
            let mut last_ident: Option<&Token> = None;
            while j < tokens.len() {
                match &tokens[j].kind {
                    TokenKind::Ident(_) => {
                        last_ident = Some(&tokens[j]);
                        j += 1;
                    }
                    TokenKind::ColonColon => j += 1,
                    _ => break,
                }
            }
            if let Some(last) = last_ident
                && matches!(&last.kind, TokenKind::Ident(s) if s == leaf)
            {
                return Some(last.location.span);
            }
            i = j;
        } else {
            i += 1;
        }
    }
    None
}

/// The span of the first identifier token at or after `from` whose text is
/// `name` — used to locate an `impl <Type>` target name.
fn first_ident_span(tokens: &[Token], from: Location, name: &str) -> Option<Span> {
    tokens
        .iter()
        .find(|t| {
            t.location.span.start >= from && matches!(&t.kind, TokenKind::Ident(s) if s == name)
        })
        .map(|t| t.location.span)
}

fn type_ref_spans(
    type_: &Type,
    scope: &[ItemPath],
    index: &NameIndex,
    tokens: &[Token],
    out: &mut Vec<(Span, ItemPath)>,
) {
    match type_ {
        Type::Ident {
            path, generic_args, ..
        } => {
            let span = type_.location().span;
            // The outer name ends where the generic args begin, so a generic
            // argument's leaf isn't mistaken for the outer type's.
            let outer_end = generic_args
                .first()
                .map(|a| a.location().span.start)
                .unwrap_or(span.end);
            if let Some(resolved) = index.resolve_path(scope, path)
                && let Some(leaf) = last_ident_span(tokens, span.start, outer_end)
            {
                out.push((leaf, resolved));
            }
            for arg in generic_args {
                type_ref_spans(arg, scope, index, tokens, out);
            }
        }
        Type::ConstPointer { pointee, .. } | Type::MutPointer { pointee, .. } => {
            type_ref_spans(pointee, scope, index, tokens, out)
        }
        Type::Array { element, .. } => type_ref_spans(element, scope, index, tokens, out),
        Type::Unknown { .. } => {}
    }
}

/// The span of the last identifier token in `[start, end)` — the leaf segment of
/// a (possibly qualified) path.
fn last_ident_span(tokens: &[Token], start: Location, end: Location) -> Option<Span> {
    tokens
        .iter()
        .rfind(|t| {
            matches!(t.kind, TokenKind::Ident(_))
                && t.location.span.start >= start
                && t.location.span.start < end
        })
        .map(|t| t.location.span)
}
