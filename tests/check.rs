//! Integration tests for the `check` function — validates that the
//! semantic analysis pipeline reports errors correctly without
//! generating any output.
//!
//! All tests drive the pipeline through the in-memory [`pyxis::check_sources`]
//! entry point (no real filesystem), matching the contributing guidelines'
//! "tests must be deterministic: no real filesystem" rule.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::unreachable
)]

use std::path::Path;

use pyxis::source_store::FileStore;

/// The project config for a valid test project (pointer size 8).
fn pointer_size() -> usize {
    8
}

/// The `pyxis.toml` contents for a valid test project.
const VALID_PROJECT_TOML: &str = r#"
[project]
name = "test-project"
pointer_size = 8
"#;

/// Build a `(filename, content)` source list for a project with the given
/// `.pyxis` files.
fn sources(files: &[(&str, &str)]) -> Vec<(String, String)> {
    files
        .iter()
        .map(|(name, content)| (name.to_string(), content.to_string()))
        .collect()
}

#[test]
fn check_succeeds_on_valid_project() {
    let files = vec![
        ("pyxis.toml", VALID_PROJECT_TOML),
        (
            "foo.pyxis",
            r#"
pub type Foo {
    pub value: u32,
}
"#,
        ),
    ];

    let mut file_store = FileStore::new();
    let result = pyxis::check_sources(sources(&files), pointer_size(), &mut file_store);
    assert!(result.is_ok(), "expected Ok(()), got {result:?}");
}

#[test]
fn check_reports_semantic_error() {
    // `Undefined` is not a known type — this should produce a semantic error.
    let files = vec![
        ("pyxis.toml", VALID_PROJECT_TOML),
        (
            "bad.pyxis",
            r#"
pub type Bad {
    pub field: Undefined,
}
"#,
        ),
    ];

    let mut file_store = FileStore::new();
    let result = pyxis::check_sources(sources(&files), pointer_size(), &mut file_store);
    let errors = result.expect_err("expected semantic errors");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e, pyxis::BuildError::Semantic(_))),
        "expected at least one BuildError::Semantic, got {errors:?}"
    );
}

#[test]
fn check_reports_parse_error() {
    // Unterminated type body — a syntax error.
    let files = vec![
        ("pyxis.toml", VALID_PROJECT_TOML),
        (
            "bad.pyxis",
            r#"
pub type Foo {
"#,
        ),
    ];

    let mut file_store = FileStore::new();
    let result = pyxis::check_sources(sources(&files), pointer_size(), &mut file_store);
    let errors = result.expect_err("expected parse errors");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e, pyxis::BuildError::Parser(_))),
        "expected at least one BuildError::Parser, got {errors:?}"
    );
}

#[test]
fn check_reports_multiple_errors() {
    // Two files with different semantic errors (undefined type references).
    let files = vec![
        ("pyxis.toml", VALID_PROJECT_TOML),
        (
            "a.pyxis",
            r#"
pub type A {
    pub field: NonexistentA,
}
"#,
        ),
        (
            "b.pyxis",
            r#"
pub type B {
    pub field: NonexistentB,
}
"#,
        ),
    ];

    let mut file_store = FileStore::new();
    let result = pyxis::check_sources(sources(&files), pointer_size(), &mut file_store);
    let errors = result.expect_err("expected multiple errors");
    assert!(
        errors.len() >= 2,
        "expected at least 2 errors, got {}: {errors:?}",
        errors.len()
    );
}

#[test]
fn check_validates_against_codegen_corpus() {
    // Resolve the corpus path relative to the crate root at test time rather
    // than the process CWD, so the test is hermetic.
    let in_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("codegen_tests/input");

    let mut file_store = FileStore::new();
    let result = pyxis::check(&in_dir, &mut file_store);
    assert!(
        result.is_ok(),
        "expected check to succeed on codegen test corpus, got: {result:?}"
    );
}

#[test]
fn check_sources_end_to_end_no_filesystem() {
    // A smoke assertion that the in-memory path runs the full pipeline
    // (config-less: sources carry their own content) without touching the
    // filesystem and without producing output.
    let files = vec![
        ("pyxis.toml", VALID_PROJECT_TOML),
        (
            "foo.pyxis",
            r#"
pub type Foo {
    pub value: u32,
}
"#,
        ),
    ];

    let mut file_store = FileStore::new();
    let result = pyxis::check_sources(sources(&files), pointer_size(), &mut file_store);
    assert!(result.is_ok(), "expected Ok(()), got {result:?}");
}
