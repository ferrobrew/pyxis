use std::collections::BTreeMap;

use crate::semantic::SemanticOutput;

use super::{
    convert::{DocCx, convert_cfg, convert_function},
    schema::{
        JsonFunction, JsonModule, JsonReexport, JsonSplice, JsonSpliceKind, convert_location,
    },
};

fn convert_splice(splice: &crate::semantic::types::Splice) -> JsonSplice {
    use crate::grammar::SpliceKind;
    JsonSplice {
        kind: match splice.kind {
            SpliceKind::Prologue => JsonSpliceKind::Prologue,
            SpliceKind::Epilogue => JsonSpliceKind::Epilogue,
        },
        cfg: splice.cfg.as_ref().map(convert_cfg),
        definition: splice.definition,
        for_type: splice.for_type.as_ref().map(|p| p.to_string()),
        text: splice.text.clone(),
    }
}

/// Build the module hierarchy from a flat list of modules
pub(super) fn build_module_hierarchy(
    semantic_state: &SemanticOutput,
) -> BTreeMap<String, JsonModule> {
    let mut root_modules: BTreeMap<String, JsonModule> = BTreeMap::new();

    for (module_path, module) in semantic_state.modules() {
        let segments: Vec<String> = module_path
            .to_string()
            .split("::")
            .map(|s| s.to_string())
            .collect();

        let cx = DocCx {
            links: semantic_state.module_doc_links(module_path),
        };

        // Items, externs, and functions are all emitted regardless of
        // any `#[cfg(...)]` predicate: consumers read the predicate off
        // each item/function and decide for themselves how to render.
        // Nested items (whose parent is a type, not a module) are excluded
        // from the top-level items list — they are reachable via their
        // parent type's `nested_items` field.
        let items: Vec<String> = module
            .definitions(semantic_state.type_registry())
            .filter(|item| {
                item.path
                    .parent()
                    .is_some_and(|parent| &parent == module_path)
            })
            .map(|item| item.path.to_string())
            .collect();
        let functions: Vec<JsonFunction> = module
            .functions()
            .iter()
            .map(|f| convert_function(f, &cx))
            .collect();

        // Explicit `pub use` re-exports, each canonicalized (through any
        // re-export chain) to the defining item's path so the viewer links to
        // its page. A target that doesn't resolve to a known item is skipped.
        let type_registry = semantic_state.type_registry();
        let reexports: Vec<JsonReexport> = module
            .reexports()
            .into_iter()
            .filter_map(|(name, target)| {
                let canonical = type_registry.canonicalize(&target);
                type_registry.contains(&canonical).then(|| JsonReexport {
                    name,
                    path: canonical.to_string(),
                })
            })
            .collect();

        // Every splice is emitted with its cfg intact (no per-backend
        // filtering): the docs describe all backends, so the viewer reads
        // the cfg predicate off each splice to decide how to render it.
        let splices: Vec<JsonSplice> = module.splices.iter().map(convert_splice).collect();

        let (doc, doc_links) = cx.convert_module_doc(module.doc());
        let json_module = JsonModule {
            doc,
            doc_links,
            items,
            reexports,
            submodules: BTreeMap::new(),
            functions,
            splices,
            source: convert_location(module.location()),
        };

        // Insert into hierarchy
        if segments.is_empty() || (segments.len() == 1 && segments[0].is_empty()) {
            // Root module
            root_modules.insert("root".to_string(), json_module);
        } else if segments.len() == 1 {
            // Top-level module (e.g., "clock", "game")
            root_modules.insert(segments[0].clone(), json_module);
        } else {
            // Nested module - navigate to the right place in the hierarchy
            let root_name = segments[0].clone();
            let mut current = root_modules
                .entry(root_name.clone())
                .or_insert_with(|| JsonModule {
                    doc: None,
                    doc_links: vec![],
                    items: vec![],
                    reexports: vec![],
                    submodules: BTreeMap::new(),
                    functions: vec![],
                    splices: vec![],
                    source: None,
                });

            for (i, segment) in segments.iter().enumerate().skip(1) {
                if i == segments.len() - 1 {
                    // Last segment, insert the module here
                    current
                        .submodules
                        .insert(segment.clone(), json_module.clone());
                    break;
                } else {
                    // Navigate deeper
                    current = current
                        .submodules
                        .entry(segment.clone())
                        .or_insert_with(|| JsonModule {
                            doc: None,
                            doc_links: vec![],
                            items: vec![],
                            reexports: vec![],
                            submodules: BTreeMap::new(),
                            functions: vec![],
                            splices: vec![],
                            source: None,
                        });
                }
            }
        }
    }

    root_modules
}
