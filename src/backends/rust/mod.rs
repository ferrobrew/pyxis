use std::{
    collections::{BTreeSet, HashMap},
    fmt::Write as _,
    path::Path,
};

use crate::{
    backends::{BackendError, Result},
    grammar::ItemPath,
    semantic::{Module, SemanticOutput, types::ItemDefinitionInner},
};

mod doc_links;
mod helpers;
mod items;
mod values;

use doc_links::DocLinkCx;
use items::build_item;
use values::build_function;

/// A freestanding reimplementation of the subset of `bitflags::bitflags!` that
/// Pyxis needs, so generated crates don't have to depend on the `bitflags`
/// crate. Emitted once into the crate root (see [`BITFLAGS_MACRO`]) whenever the
/// crate contains any `bitflags` definition, and invoked as `__bitflags!` from
/// every module that defines bitflags.
///
/// The generated type is a `#[repr(transparent)]` newtype over the underlying
/// integer, always `Copy + Clone`, with the usual bitflags API (`contains`,
/// `insert`, `|`, `&`, `^`, `!`, `from_bits`, ...). It mirrors the ergonomics of
/// the `bitflags` crate closely enough for typical consumption without pulling
/// in an external dependency.
const BITFLAGS_MACRO: &str = include_str!("bitflags_impl.rs");

/// Whether any (Rust-cfg-included) bitflags definition exists anywhere in the
/// crate. Used to decide whether to emit [`BITFLAGS_MACRO`] into the root
/// module.
fn crate_uses_bitflags(
    semantic_state: &SemanticOutput,
    cfg_ctx: &crate::parser::cfg::CfgContext,
) -> bool {
    let cfg_pass = |cfg: &Option<crate::parser::cfg::CfgPredicate>| match cfg {
        Some(p) => p.evaluate(cfg_ctx),
        None => true,
    };
    let type_registry = semantic_state.type_registry();
    semantic_state.modules().values().any(|module| {
        module
            .definitions(type_registry)
            .filter(|d| cfg_pass(&d.cfg))
            .any(|d| {
                d.resolved()
                    .is_some_and(|r| matches!(r.inner, ItemDefinitionInner::Bitflags(_)))
            })
    })
}

pub fn write_module(
    out_dir: &Path,
    key: &ItemPath,
    semantic_state: &SemanticOutput,
    module: &Module,
    options: &crate::BuildOptions,
) -> Result<()> {
    const FORMAT_OUTPUT: bool = true;

    // Direct child modules (sorted by name), used both to wire up `pub mod`
    // / `pub use` declarations and to decide this module's file layout.
    let mut children: Vec<(&str, &ItemPath, &Module)> = semantic_state
        .modules()
        .iter()
        .filter(|(p, _)| p.parent().as_ref() == Some(key))
        .filter_map(|(p, m)| p.last().map(|s| (s.as_str(), p, m)))
        .collect();
    children.sort_by_key(|(name, _, _)| *name);
    let has_children = !children.is_empty();

    // Output path:
    // - root module (empty key)  -> <out_dir>/lib.rs
    // - module with children     -> <out_dir>/<segments>/mod.rs
    // - leaf module              -> <out_dir>/<segments>.rs
    let mut path = out_dir.to_path_buf();
    for segment in key.iter() {
        path.push(segment.as_str());
    }
    if key.is_empty() {
        path.push(options.rust_root_file_name.as_deref().unwrap_or("lib.rs"));
    } else if has_children {
        path.push("mod.rs");
    } else {
        path.set_extension("rs");
    }

    let directory_path = path.parent().map(|p| p.to_path_buf()).unwrap_or_default();
    std::fs::create_dir_all(&directory_path).map_err(|e| BackendError::Io {
        error: e,
        context: format!("Failed to create directory {}", directory_path.display()),
    })?;

    let mut raw_output = String::new();

    let cfg_ctx = crate::parser::cfg::CfgContext {
        backend: crate::Backend::Rust,
    };

    // Lint `allow`s cascade to descendant modules, so they only need to live
    // on the root file (lib.rs / the mounted-subtree root); emitting them on
    // the root also overrides a stricter host crate (the innermost level
    // wins), keeping the whole generated subtree quiet.
    //
    // `rustdoc::redundant_explicit_links`: every resolved doc link's
    // destination is rewritten to the absolute path of its semantic target
    // (see `DocLinkCx`), uniformly. For links rustdoc could have resolved from
    // the label alone this is "redundant" — deciding when that holds would
    // mean re-implementing rustdoc's own resolution, so the lint is allowed
    // instead.
    if key.is_empty() {
        writeln!(
            raw_output,
            "#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals, clippy::missing_safety_doc, clippy::unnecessary_cast, clippy::module_inception, rustdoc::redundant_explicit_links)]"
        )?;
    }
    // Disable rustfmt on generated files to prevent the prettyplease-formatted code being reformatted
    // by a stray project-wide `cargo fmt` invocation. (Per-file: rustfmt runs
    // per-file, so unlike lint levels this can't live only on the root.)
    // <https://stackoverflow.com/questions/59247458/is-there-a-stable-way-to-tell-rustfmt-to-skip-an-entire-file#comment138279076_75910283>
    writeln!(raw_output, "#![cfg_attr(any(), rustfmt::skip)]")?;
    // Collect all module paths for flattening nested item names.
    let module_paths: BTreeSet<ItemPath> = semantic_state.modules().keys().cloned().collect();

    // Doc-link rewriting context: every resolved link's destination is
    // rendered from its semantic target as an absolute crate path, so rustdoc
    // resolves it without any doc-driven imports or aliases.
    let prefix = options.rust_module_prefix.as_ref();
    let doc_cx = DocLinkCx {
        links: semantic_state.module_doc_links(key),
        type_registry: semantic_state.type_registry(),
        module_paths: &module_paths,
        module_path: key,
        prefix,
        root: match prefix {
            Some(prefix) => format!("crate::{prefix}"),
            None => "crate".to_string(),
        },
    };

    writeln!(raw_output, "{}", doc_cx.module_doc(module.doc()))?;

    // Emit the freestanding `__bitflags!` macro definition exactly once, on the
    // crate root, when the crate contains any bitflags. It is
    // `#[macro_export]`-ed, so it lands at the crate root regardless of any
    // module prefix and is callable from every module as `crate::__bitflags!`.
    // This lets generated crates drop their `bitflags` dependency. It must
    // follow the inner attributes (`#![...]`) above, since those can only
    // precede items in a module body.
    if key.is_empty() && crate_uses_bitflags(semantic_state, &cfg_ctx) {
        raw_output.push_str(BITFLAGS_MACRO);
    }

    use crate::grammar::SpliceKind;
    // Rust has no separate source file, so `definition` splices never apply
    // (validation guarantees they're cpp-only); filter them out defensively.
    let rust_splice_text = |kind: SpliceKind| {
        module
            .splices_for(crate::Backend::Rust)
            .filter(move |s| s.kind == kind && !s.definition)
            .map(|s| s.text.as_str())
            .collect::<Vec<_>>()
            .join("\n")
    };
    let prologues = rust_splice_text(SpliceKind::Prologue);
    let epilogues = rust_splice_text(SpliceKind::Epilogue);

    writeln!(raw_output, "{prologues}")?;

    // Wire up child modules. Every folder that contains `.pyxis` files has a
    // module (see `synthesize_ancestor_modules`), so this produces a complete
    // module tree without any hand-written `mod.rs`/`lib.rs`. Items are reached
    // by their canonical path; a module that wants to re-expose a child's item
    // under its own path does so with an explicit `pub use`.
    for (child, _child_path, _child_module) in &children {
        writeln!(raw_output, "pub mod {child};")?;
    }

    // Emit explicit `pub use` re-exports. Each re-export is canonicalized (past
    // any re-export chain) to the defining item and rendered as an absolute
    // `pub use crate::…;`, so consumers of the generated crate can reach the
    // item through this module too — mirroring the pyxis-level re-export.
    let type_registry = semantic_state.type_registry();
    for (_name, target) in module.reexports() {
        let canonical = type_registry.canonicalize(&target);
        if !type_registry.contains(&canonical) {
            continue;
        }
        writeln!(
            raw_output,
            "pub use {};",
            doc_cx.absolute_item_path(&canonical)
        )?;
    }

    let cfg_pass = |cfg: &Option<crate::parser::cfg::CfgPredicate>| match cfg {
        Some(p) => p.evaluate(&cfg_ctx),
        None => true,
    };
    // Extern types with a `#[rust_name = "..."]` binding get a `pub use`
    // alias to the real Rust type (keyed by extern name).
    let extern_rust_names: HashMap<String, String> = module
        .extern_rust_names()
        .map(|(name, path)| (name.to_string(), path.to_string()))
        .collect();
    let mut definitions = module
        .definitions(semantic_state.type_registry())
        .filter(|d| cfg_pass(&d.cfg))
        .collect::<Vec<_>>();
    definitions.sort_by_key(|d| &d.path);
    for definition in definitions {
        // Skip nested constants and extern values — they're emitted inside their
        // parent's `impl` block by build_type/build_enum/build_bitflags.
        use ItemDefinitionInner as IDI;
        if definition
            .resolved()
            .is_some_and(|r| matches!(r.inner, IDI::Constant(_) | IDI::ExternValue(_)))
        {
            if let Some(parent_path) = definition.path.parent() {
                if semantic_state.type_registry().contains(&parent_path) {
                    continue;
                }
            }
        }
        writeln!(
            raw_output,
            "{}",
            build_item(
                semantic_state.type_registry(),
                definition,
                &cfg_ctx,
                options,
                &extern_rust_names,
                &module_paths,
                &doc_cx,
            )?
        )?;
    }

    // Generate freestanding functions
    let freestanding_functions = module
        .functions()
        .iter()
        .filter(|f| !f.is_internal())
        .filter(|f| cfg_pass(&f.cfg))
        .map(|f| build_function(f, options, false, &module_paths, &doc_cx))
        .collect::<Result<Vec<_>>>()?;
    for func in freestanding_functions {
        writeln!(raw_output, "{func}")?;
    }

    writeln!(raw_output, "{epilogues}")?;

    let mut error = None;
    let output = if FORMAT_OUTPUT {
        // You may think that this is inefficient. It probably is.
        // It's still probably faster than running `rustfmt`.
        match syn::parse_file(&raw_output) {
            Ok(parsed_file) => prettyplease::unparse(&parsed_file),
            Err(err) => {
                let lc = err.span().start();
                error = Some(format!(
                    concat!(
                        "Could not parse generated Rust code to pretty-print. The code has been emitted as-is.\n",
                        "This may be due to a bug in Pyxis or an issue with one of your backend definitions.\n",
                        "\n",
                        "Error: {}\n",
                        "  --> {}:{}:{}\n",
                        "   | {}\n",
                        "   | {}"
                    ),
                    err,
                    path.display(),
                    lc.line,
                    lc.column,
                    raw_output.lines().nth(lc.line - 1).unwrap(),
                    format!("{}^", " ".repeat(lc.column))
                ));
                raw_output
            }
        }
    } else {
        raw_output
    };

    std::fs::write(&path, &output).map_err(|e| BackendError::Io {
        error: e,
        context: format!("Failed to write Rust output to {}", path.display()),
    })?;

    if let Some(error) = error {
        return Err(BackendError::Formatting(error));
    }

    Ok(())
}
