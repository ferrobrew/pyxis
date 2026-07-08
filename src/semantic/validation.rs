//! Semantic validation passes — pure functions that check the resolved
//! type registry and modules for errors.

use std::collections::BTreeMap;

use crate::{
    Backend,
    grammar::{self, ItemPath},
    semantic::{
        Module, TypeRegistry,
        error::{Result, SemanticError},
        types::Visibility,
    },
    span::ItemLocation,
};

/// Validate that all `use` statements reference existing, accessible items.
/// Covers every module-level `use` tree, including cfg-gated ones (a gated
/// `use` is still in scope for resolution regardless of backend).
pub fn validate_uses(
    type_registry: &TypeRegistry,
    modules: &BTreeMap<ItemPath, Module>,
) -> Result<()> {
    for module in modules.values() {
        let mut trees: Vec<&grammar::UseTree> = Vec::new();
        for use_item in module.uses() {
            if let grammar::ModuleItem::Use { tree, .. } = use_item {
                trees.push(tree);
            }
        }

        for tree in trees {
            for (path, location) in tree.flatten_with_locations() {
                if type_registry.contains(&path) {
                    // A `use` of an existing type is fine unless it's private and
                    // the importing module can't see it.
                    if let Ok(item_def) = type_registry.get(&path, &ItemLocation::internal())
                        && item_def.visibility == Visibility::Private
                        && let Some(item_module) = path.parent()
                        && !can_access_private(&module.path, &item_module)
                    {
                        return Err(SemanticError::PrivateItemAccess {
                            item_path: path,
                            from_module: module.path.clone(),
                            location,
                        });
                    }
                    continue;
                }

                if modules.contains_key(&path) {
                    continue;
                }

                // Not a known type or module — the import doesn't resolve.
                return Err(SemanticError::UseItemNotFound { path, location });
            }
        }
    }
    Ok(())
}

/// Reject a `prologue definition` / `epilogue definition` splice whose cfg
/// doesn't resolve **cpp-only**. The `definition` modifier means "splice
/// into the `.cpp` source file"; only cpp has a distinct source file (rust
/// and json emit a single output per module). An ungated `definition`, or
/// one gated so it's active for a non-cpp backend, is almost certainly a
/// typo or copy-paste error, so we require the cfg to select cpp and
/// nothing else.
pub fn validate_splice_definitions(modules: &BTreeMap<ItemPath, Module>) -> Result<()> {
    for module in modules.values() {
        for splice in &module.splices {
            if !splice.definition {
                continue;
            }
            let cpp_only = splice.active_for(Backend::Cpp)
                && Backend::ALL
                    .iter()
                    .all(|&b| b == Backend::Cpp || !splice.active_for(b));
            if !cpp_only {
                return Err(SemanticError::SpliceDefinitionNotCppOnly {
                    location: splice.location,
                });
            }
        }
    }
    Ok(())
}

/// Resolve and validate `prologue for <Type>` / `epilogue for <Type>`
/// attribution targets. Each target must resolve to a type defined in the
/// same module as the splice. On success the splice's `for_type` is
/// replaced with the resolved absolute item path. Cross-module attribution
/// is rejected.
pub fn validate_splice_for_targets(
    type_registry: &TypeRegistry,
    modules: &mut BTreeMap<ItemPath, Module>,
) -> Result<()> {
    for module in modules.values_mut() {
        let module_path = module.path.clone();
        for splice in &mut module.splices {
            let Some(raw) = splice.for_type.take() else {
                continue;
            };
            let relative = join_item_path(&module_path, &raw);
            let resolved = if type_registry.contains(&relative) {
                relative
            } else if type_registry.contains(&raw) {
                raw.clone()
            } else {
                return Err(SemanticError::BackendForTargetNotFound {
                    target: raw,
                    module: module_path,
                    location: splice.location,
                });
            };
            if !module.definition_paths.contains(&resolved) {
                let defined_in = resolved.parent().unwrap_or_else(ItemPath::empty);
                return Err(SemanticError::BackendForTargetCrossModule {
                    target: raw,
                    module: module_path,
                    defined_in,
                    location: splice.location,
                });
            }
            splice.for_type = Some(resolved);
        }
    }
    Ok(())
}

/// Check if a module can access a private item in another module.
/// Private items are visible to the same module and child modules (descendants).
fn can_access_private(from_module: &ItemPath, item_module: &ItemPath) -> bool {
    if from_module == item_module {
        return true;
    }
    from_module.starts_with(item_module)
}

/// Concatenate two item paths: `prefix::suffix`.
fn join_item_path(prefix: &ItemPath, suffix: &ItemPath) -> ItemPath {
    let mut out = prefix.clone();
    for seg in suffix.iter() {
        out.push(seg.clone());
    }
    out
}
