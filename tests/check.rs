//! Integration tests for the `check` function — validates that the
//! semantic analysis pipeline reports errors correctly without
//! generating any output.

use std::path::{Path, PathBuf};

use pyxis::source_store::FileStore;

/// Create a fresh scratch directory for this test, removing any leftovers
/// from a previous run.
fn scratch_dir(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("pyxis_test_check_{name}"));
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

fn write_valid_project(in_dir: &Path) {
    write(
        &in_dir.join("pyxis.toml"),
        r#"
[project]
name = "test-project"
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
}

/// Recursively collect all relative file paths in a directory.
fn snapshot_dir(dir: &Path) -> Vec<String> {
    let mut entries = Vec::new();
    collect_dir(dir, dir, &mut entries);
    entries.sort();
    entries
}

fn collect_dir(root: &Path, dir: &Path, entries: &mut Vec<String>) {
    if !dir.is_dir() {
        return;
    }
    for entry in std::fs::read_dir(dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        let relative = path.strip_prefix(root).unwrap();
        if path.is_dir() {
            collect_dir(root, &path, entries);
        } else {
            entries.push(relative.display().to_string());
        }
    }
}

#[test]
fn check_succeeds_on_valid_project() {
    let root = scratch_dir("succeeds");
    let in_dir = root.join("in");
    write_valid_project(&in_dir);

    let mut file_store = FileStore::new();
    let result = pyxis::check(&in_dir, &mut file_store);
    assert!(result.is_ok(), "expected Ok(()), got {result:?}");

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn check_reports_semantic_error() {
    let root = scratch_dir("semantic_error");
    let in_dir = root.join("in");
    write(
        &in_dir.join("pyxis.toml"),
        r#"
[project]
name = "test-project"
pointer_size = 8
"#,
    );
    // `Undefined` is not a known type — this should produce a semantic error.
    write(
        &in_dir.join("bad.pyxis"),
        r#"
pub type Bad {
    pub field: Undefined,
}
"#,
    );

    let mut file_store = FileStore::new();
    let result = pyxis::check(&in_dir, &mut file_store);
    let errors = result.expect_err("expected semantic errors");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e, pyxis::BuildError::Semantic(_))),
        "expected at least one BuildError::Semantic, got {errors:?}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn check_reports_parse_error() {
    let root = scratch_dir("parse_error");
    let in_dir = root.join("in");
    write(
        &in_dir.join("pyxis.toml"),
        r#"
[project]
name = "test-project"
pointer_size = 8
"#,
    );
    // Unterminated type body — a syntax error.
    write(
        &in_dir.join("bad.pyxis"),
        r#"
pub type Foo {
"#,
    );

    let mut file_store = FileStore::new();
    let result = pyxis::check(&in_dir, &mut file_store);
    let errors = result.expect_err("expected parse errors");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e, pyxis::BuildError::Parser(_))),
        "expected at least one BuildError::Parser, got {errors:?}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn check_reports_multiple_errors() {
    let root = scratch_dir("multiple_errors");
    let in_dir = root.join("in");
    write(
        &in_dir.join("pyxis.toml"),
        r#"
[project]
name = "test-project"
pointer_size = 8
"#,
    );
    // Two files with different semantic errors (undefined type references).
    write(
        &in_dir.join("a.pyxis"),
        r#"
pub type A {
    pub field: NonexistentA,
}
"#,
    );
    write(
        &in_dir.join("b.pyxis"),
        r#"
pub type B {
    pub field: NonexistentB,
}
"#,
    );

    let mut file_store = FileStore::new();
    let result = pyxis::check(&in_dir, &mut file_store);
    let errors = result.expect_err("expected multiple errors");
    assert!(
        errors.len() >= 2,
        "expected at least 2 errors, got {}: {errors:?}",
        errors.len()
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn check_validates_against_codegen_corpus() {
    let in_dir = Path::new("codegen_tests/input");

    let mut file_store = FileStore::new();
    let result = pyxis::check(in_dir, &mut file_store);
    assert!(
        result.is_ok(),
        "expected check to succeed on codegen test corpus, got: {result:?}"
    );
}

#[test]
fn check_does_not_create_output_files() {
    let root = scratch_dir("no_output");
    let in_dir = root.join("in");
    write_valid_project(&in_dir);

    let before = snapshot_dir(&root);

    let mut file_store = FileStore::new();
    let result = pyxis::check(&in_dir, &mut file_store);
    assert!(result.is_ok(), "expected Ok(()), got {result:?}");

    let after = snapshot_dir(&root);
    assert_eq!(
        before, after,
        "directory contents changed after check (no output should be created)"
    );

    let _ = std::fs::remove_dir_all(&root);
}
