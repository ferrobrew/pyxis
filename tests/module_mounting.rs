//! Integration test for mounting a generated Rust tree as a submodule:
//! `rust_module_prefix` (so refs become `crate::<prefix>::...`), a custom
//! root file name (`mod.rs` instead of `lib.rs`), and explicit `pub use`
//! re-exports rewritten through the prefix.
//!
//! Output is generated through a `MemoryWriter`, so this test never touches
//! the filesystem: `out` is a synthetic base that only feeds path
//! computation inside the backends.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::unreachable
)]
use std::path::Path;

use pyxis::{
    Backend, BuildOptions, grammar::ItemPath, output::MemoryWriter, source_store::FileStore,
};

#[test]
fn mounts_generated_tree_as_a_prefixed_submodule() {
    let mut file_store = FileStore::new();
    let mut writer = MemoryWriter::default();

    let project = pyxis::config::Project {
        name: "mount-test".to_string(),
        pointer_size: 8,
    };
    let options = BuildOptions {
        rust_module_prefix: Some(ItemPath::from("prefixed")),
        rust_root_file_name: Some("mod.rs".to_string()),
        ..Default::default()
    };

    pyxis::build_sources_into(
        vec![
            // References `foo::Foo`, producing a `crate::`-relative path that
            // the prefix should rewrite to `crate::prefixed::foo::Foo`.
            (
                "foo.pyxis".to_string(),
                r#"
pub type Foo {
    pub value: u32,
}
"#
                .to_string(),
            ),
            (
                "bar.pyxis".to_string(),
                r#"
use foo::Foo;

pub type Bar {
    pub foo: *mut Foo,
}
"#
                .to_string(),
            ),
            // Explicitly re-exports `foo::Foo` as `baz::Foo`; the emitted
            // `pub use` must be rewritten through the prefix too.
            (
                "baz.pyxis".to_string(),
                r#"
pub use foo::Foo;

pub type Baz {
    pub value: u32,
}
"#
                .to_string(),
            ),
        ],
        &project,
        Path::new("out"),
        Backend::Rust,
        &mut file_store,
        options,
        &mut writer,
    )
    .expect("build failed");

    // Root module is emitted as `mod.rs` (not `lib.rs`).
    let root_rs = writer
        .file(Path::new("out/mod.rs"))
        .expect("expected mod.rs root file");
    assert!(
        writer.file(Path::new("out/lib.rs")).is_none(),
        "lib.rs should not be emitted when the root file name is mod.rs"
    );

    // Root wires up children with `pub mod`, without any glob re-export.
    assert!(root_rs.contains("pub mod foo;"), "{root_rs}");
    assert!(root_rs.contains("pub mod bar;"), "{root_rs}");
    assert!(root_rs.contains("pub mod baz;"), "{root_rs}");
    assert!(
        !root_rs.contains("pub use bar::*;"),
        "glob re-exports should no longer be emitted:\n{root_rs}"
    );

    // Cross-module references are rewritten through the prefix.
    let bar_rs = writer
        .file(Path::new("out/bar.rs"))
        .expect("expected generated bar.rs");
    assert!(
        bar_rs.contains("crate::prefixed::foo::Foo"),
        "expected prefixed reference, got:\n{bar_rs}"
    );

    // An explicit `pub use` re-export is emitted, and its path is rewritten
    // through the prefix like any other cross-module reference.
    let baz_rs = writer
        .file(Path::new("out/baz.rs"))
        .expect("expected generated baz.rs");
    assert!(
        baz_rs.contains("pub use crate::prefixed::foo::Foo;"),
        "expected prefixed re-export, got:\n{baz_rs}"
    );
}
