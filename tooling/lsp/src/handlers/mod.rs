//! LSP request handlers: hover, definition, completion, symbols, formatting, code lens, inlay hints, rename.

pub(crate) use std::collections::HashMap;

pub(crate) use lsp_server::{Request, Response};
pub(crate) use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeLens, CompletionItem,
    CompletionItemKind, CompletionParams, DocumentLink, DocumentLinkParams, DocumentSymbol,
    DocumentSymbolParams, DocumentSymbolResponse, FoldingRange, FoldingRangeParams, Hover,
    HoverContents, InlayHint, InlayHintLabel, MarkupContent, MarkupKind, Position, Range,
    SymbolInformation, SymbolKind, TextDocumentPositionParams, TextEdit, TypeHierarchyItem,
    TypeHierarchyPrepareParams, TypeHierarchySubtypesParams, TypeHierarchySupertypesParams, Uri,
    WorkspaceEdit, WorkspaceSymbolParams,
};

pub(crate) use pyxis::{
    grammar::{
        Attribute, Attributes, Function, ItemDefinition, ItemPath, Module, ModuleItem, Type,
        TypeField, UseTree, Visibility,
    },
    semantic,
    semantic::{declaration_registry::DeclarationRegistry, type_registry::TypeRegistry},
    span::{FileId, HasLocation, ItemLocation, Location, Span},
    tokenizer::{Token, TokenKind},
};

pub(crate) use crate::{
    span::{lsp_position_to_pyxis_location, pyxis_span_to_lsp_range},
    state::ServerState,
};

pub(crate) use pyxis::semantic::resolve_item;

mod code_action;
mod completion;
mod doc_links;
mod hover_format;
mod navigation;
mod outline;
mod references;
mod symbols;

pub(crate) use hover_format::*;
pub(crate) use references::*;
pub(crate) use symbols::*;

/// Bundled per-file analysis state shared by most request handlers: the parsed
/// module, cached token stream, name-resolution scope, and the Salsa-computed
/// type/declaration registries — plus the `source_set`/`analysis` handles for
/// callers that run further queries (`resolve_item`, doc-link resolution).
///
/// Built once by [`ServerState::analysis_ctx`]; `None` when the document isn't
/// tracked. The lifetime ties every borrowed field to the `&self` it came from.
struct AnalysisCtx<'a> {
    content: &'a str,
    pointer_size: usize,
    scope: Vec<ItemPath>,
    type_registry: &'a TypeRegistry,
    decl_registry: &'a DeclarationRegistry,
    source_set: semantic::SourceSet<'a>,
    analysis: semantic::SemanticAnalysis<'a>,
    module: Module,
    tokens_arc: Option<std::sync::Arc<Vec<Token>>>,
}

impl ServerState {
    /// Assemble the shared [`AnalysisCtx`] for a document. `None` if the document
    /// isn't tracked (no content / no parsed module — equivalent conditions).
    fn analysis_ctx(&self, uri: &Uri) -> Option<AnalysisCtx<'_>> {
        let content = self.get_content(uri)?;
        let module = self.get_parsed_module(uri)?;
        let pointer_size = self.pointer_size_for(uri);
        let scope = self.scope_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, self.sources_for(uri));
        let analysis = semantic::analyze(&self.db, pointer_size, source_set);
        let type_registry = analysis.type_registry(&self.db);
        let decl_set = semantic::collect_declarations(&self.db, source_set, pointer_size);
        let decl_registry = decl_set.registry(&self.db);
        let tokens_arc = self.tokens_for(uri);
        Some(AnalysisCtx {
            content,
            pointer_size,
            scope,
            type_registry,
            decl_registry,
            source_set,
            analysis,
            module,
            tokens_arc,
        })
    }

    /// The project's declaration registry for a file (the decl-only slice of
    /// [`ServerState::analysis_ctx`], for callers that don't need a full analysis).
    fn decl_registry_for(&self, uri: &Uri) -> &DeclarationRegistry {
        let pointer_size = self.pointer_size_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, self.sources_for(uri));
        let decl_set = semantic::collect_declarations(&self.db, source_set, pointer_size);
        decl_set.registry(&self.db)
    }

    /// The fully-qualified path of a definition named `name` declared in `uri`'s
    /// module (module-qualified when the file has a module path, bare otherwise).
    fn definition_path(&self, uri: &Uri, name: &str) -> ItemPath {
        match self.module_path_for(uri) {
            Some(mp) => mp.join(name.into()),
            None => ItemPath::from(name),
        }
    }

    /// Project type/decl registries for a file (shared analysis setup).
    fn registries_for(
        &self,
        uri: &Uri,
    ) -> (
        &std::sync::Arc<TypeRegistry>,
        &std::sync::Arc<DeclarationRegistry>,
    ) {
        let pointer_size = self.pointer_size_for(uri);
        let source_set = semantic::SourceSet::new(&self.db, self.sources_for(uri));
        let analysis = semantic::analyze(&self.db, pointer_size, source_set);
        let decl_set = semantic::collect_declarations(&self.db, source_set, pointer_size);
        (
            analysis.type_registry(&self.db),
            decl_set.registry(&self.db),
        )
    }

    /// Build the name-resolution scope for a document: its own module path plus
    /// every `use`-imported path. This mirrors `DeclarationRegistry::register_module`
    /// — `resolve_name` matches imported names by their full paths' last segment,
    /// so a single-segment type reference like `EventHandler` only resolves when
    /// the corresponding `use game::event_handler::EventHandler;` path is present
    /// in the scope.
    fn scope_for(&self, uri: &Uri) -> Vec<ItemPath> {
        let Some(module_path) = self.module_path_for(uri) else {
            return vec![];
        };
        let mut scope = vec![module_path];
        if let Some(module) = self.get_parsed_module(uri) {
            for item in &module.items {
                if let ModuleItem::Use { tree, .. } = item {
                    scope.extend(tree.flatten());
                }
            }
        }
        scope
    }

    /// Resolve an item path to its grammar definition by locating it in its
    /// module's source file. Deliberately does NOT depend on the type registry
    /// for *finding* the definition — types with semantic errors (e.g. a
    /// mid-edit `#[size]` mismatch, common in RE work) drop out of the registry,
    /// but their declaration is still in the file, so hover/go-to-def keep
    /// working. The registry is only consulted for size/alignment, which is
    /// legitimately absent when the type doesn't resolve.
    fn resolved_definition(
        &self,
        resolved_path: &ItemPath,
        type_registry: &TypeRegistry,
        from_uri: &Uri,
    ) -> Option<ResolvedDefinition> {
        let module_path = resolved_path.parent()?;
        let uri = self.module_uri(&module_path, from_uri)?;
        let tokens_arc = self.tokens_for(&uri)?;
        let module = self.get_parsed_module(&uri)?;
        let target_name = resolved_path.last()?.as_str();
        let def = module
            .definitions()
            .find(|d| d.name.as_str() == target_name)
            .cloned()?;
        let name_span = name_token_span(
            &tokens_arc,
            &def.declaration_location.span.start,
            target_name,
        )
        .unwrap_or(def.location.span);
        let size_align = type_registry
            .get(resolved_path, &ItemLocation::internal())
            .ok()
            .and_then(|i| i.resolved())
            .map(|r| (r.size, r.alignment));
        Some(ResolvedDefinition {
            def,
            uri,
            name_span,
            size_align,
        })
    }

    /// Resolved value of an enum variant / bitflags flag named `variant_name`
    /// within `definition` (auto-incremented values are only known post-resolve).
    fn variant_value(
        &self,
        uri: &Uri,
        definition: &ItemDefinition,
        variant_name: &str,
        source_set: semantic::SourceSet,
        pointer_size: usize,
    ) -> Option<i128> {
        let path = self.definition_path(uri, definition.name.as_str());
        let resolved = resolve_item(&self.db, source_set, pointer_size, path);
        match &resolved.item(&self.db).resolved()?.inner {
            pyxis::semantic::types::ItemDefinitionInner::Enum(e) => e
                .variants
                .iter()
                .find(|v| v.name == variant_name)
                .map(|v| v.value as i128),
            pyxis::semantic::types::ItemDefinitionInner::Bitflags(b) => b
                .flags
                .iter()
                .find(|f| f.name == variant_name)
                .map(|f| f.value as i128),
            _ => None,
        }
    }

    /// Hover markdown for a resolved item path: a user-defined type located in
    /// its source file, or a predefined/extern builtin.
    fn type_hover_text(
        &self,
        item_path: &ItemPath,
        type_registry: &TypeRegistry,
        decl_registry: &DeclarationRegistry,
        from_uri: &Uri,
    ) -> Option<String> {
        self.resolved_definition(item_path, type_registry, from_uri)
            .map(|rd| match rd.size_align {
                Some((size, alignment)) => format_type_hover_with_size(&rd.def, size, alignment),
                None => format_type_hover(&rd.def),
            })
            .or_else(|| builtin_hover(item_path, decl_registry))
    }

    /// Find the document URI whose module path equals `module_path`, scoped to
    /// the same project as `from_uri`. Different projects can have files at the
    /// same relative path (e.g. `game_objects/physics_game_object.pyxis` in both
    /// JustCause2 and MadMax), hence the same module path — so we must resolve
    /// within the requesting file's project rather than matching globally.
    fn module_uri(&self, module_path: &ItemPath, from_uri: &Uri) -> Option<Uri> {
        let from_root = self
            .documents
            .get(from_uri)
            .and_then(|d| d.project_root.clone());
        self.documents
            .keys()
            .find(|uri| {
                self.documents
                    .get(*uri)
                    .and_then(|d| d.project_root.clone())
                    == from_root
                    && self.module_path_for(uri).as_ref() == Some(module_path)
            })
            .cloned()
    }
}

/// A type/item definition located in its source file.
struct ResolvedDefinition {
    def: ItemDefinition,
    /// URI of the file the definition lives in.
    uri: Uri,
    /// Span of the definition's name (the go-to-definition target).
    name_span: Span,
    /// Size/alignment, if the type resolved cleanly.
    size_align: Option<(usize, usize)>,
}

fn error_response(id: lsp_server::RequestId, e: serde_json::Error) -> Response {
    Response {
        id,
        result: None,
        error: Some(lsp_server::ResponseError {
            // JSON-RPC InvalidParams — these only fire on params that fail to
            // deserialize. (0 is not a valid JSON-RPC error code.)
            code: lsp_server::ErrorCode::InvalidParams as i32,
            message: e.to_string(),
            data: None,
        }),
    }
}
