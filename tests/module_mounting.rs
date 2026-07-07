//! Integration test for mounting a generated Rust tree as a submodule:
//! `rust_module_prefix` (so refs become `crate::<prefix>::...`), a custom
//! root file name (`mod.rs` instead of `lib.rs`), and explicit `pub use`
//! re-exports rewritten through the prefix.

use std::path::{Path, PathBuf};

use pyxis::{Backend, BuildOptions, grammar::ItemPath, source_store::FileStore};

/// Create a fresh scratch directory for this test, removing any leftovers
/// from a previous run.
fn scratch_dir(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("pyxis_test_{name}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn write(path: &Path, contents: &str) {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).unwrap();
    }
    std::fs::write(path, contents).unwrap();
}

#[test]
fn mounts_generated_tree_as_a_prefixed_submodule() {
    let root = scratch_dir("module_mounting");
    let in_dir = root.join("in");
    let out_dir = root.join("out");

    write(
        &in_dir.join("pyxis.toml"),
        r#"
[project]
name = "mount-test"
pointer_size = 8
"#,
    );
    write(
        &in_dir.join("foo.pyxis"),
        r#"
pub type Foo {
    pub value: u32,
}
"#,
    );
    // References `foo::Foo`, producing a `crate::`-relative path that the
    // prefix should rewrite to `crate::prefixed::foo::Foo`.
    write(
        &in_dir.join("bar.pyxis"),
        r#"
use foo::Foo;

pub type Bar {
    pub foo: *mut Foo,
}
"#,
    );
    // Explicitly re-exports `foo::Foo` as `baz::Foo`; the emitted `pub use`
    // must be rewritten through the prefix too.
    write(
        &in_dir.join("baz.pyxis"),
        r#"
pub use foo::Foo;

pub type Baz {
    pub value: u32,
}
"#,
    );

    let options = BuildOptions {
        rust_module_prefix: Some(ItemPath::from("prefixed")),
        rust_root_file_name: Some("mod.rs".to_string()),
        ..Default::default()
    };

    let mut file_store = FileStore::new();
    pyxis::build_with_store_and_options(&in_dir, &out_dir, Backend::Rust, &mut file_store, options)
        .expect("build failed");

    // Root module is emitted as `mod.rs` (not `lib.rs`).
    assert!(out_dir.join("mod.rs").exists(), "expected mod.rs root file");
    assert!(
        !out_dir.join("lib.rs").exists(),
        "lib.rs should not be emitted when the root file name is mod.rs"
    );

    // Root wires up children with `pub mod`, without any glob re-export.
    let root_rs = std::fs::read_to_string(out_dir.join("mod.rs")).unwrap();
    assert!(root_rs.contains("pub mod foo;"), "{root_rs}");
    assert!(root_rs.contains("pub mod bar;"), "{root_rs}");
    assert!(root_rs.contains("pub mod baz;"), "{root_rs}");
    assert!(
        !root_rs.contains("pub use bar::*;"),
        "glob re-exports should no longer be emitted:\n{root_rs}"
    );

    // Cross-module references are rewritten through the prefix.
    let bar_rs = std::fs::read_to_string(out_dir.join("bar.rs")).unwrap();
    assert!(
        bar_rs.contains("crate::prefixed::foo::Foo"),
        "expected prefixed reference, got:\n{bar_rs}"
    );

    // An explicit `pub use` re-export is emitted, and its path is rewritten
    // through the prefix like any other cross-module reference.
    let baz_rs = std::fs::read_to_string(out_dir.join("baz.rs")).unwrap();
    assert!(
        baz_rs.contains("pub use crate::prefixed::foo::Foo;"),
        "expected prefixed re-export, got:\n{baz_rs}"
    );

    let _ = std::fs::remove_dir_all(&root);
}
