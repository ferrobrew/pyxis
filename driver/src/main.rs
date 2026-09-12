use std::path::PathBuf;

use clap::{Parser, Subcommand, ValueEnum};

#[derive(Parser)]
#[clap(name = "pyxis", about = "Pyxis code generation tool")]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Build the project using the specified backend
    Build {
        /// The directory containing the Pyxis source files
        in_dir: PathBuf,
        /// The backend to use
        #[clap(short, long)]
        backend: Backend,
        /// The directory to write the generated files to
        #[clap(default_value = "out")]
        out_dir: PathBuf,
        /// (Rust) File name for the root module (default `lib.rs`; use
        /// `mod.rs` when mounting the tree as a submodule).
        #[clap(long)]
        rust_root_file_name: Option<String>,
        /// (Rust) Module path prepended to emitted `crate::`-relative
        /// references, e.g. `jc2` to mount the tree at `crate::jc2`.
        #[clap(long)]
        rust_module_prefix: Option<String>,
    },
    /// Dump AST with span information for a Pyxis file
    AstDump {
        /// The Pyxis file to parse and dump
        file: PathBuf,
        /// Pretty print the AST instead of debug format
        #[clap(short, long)]
        pretty: bool,
    },
    /// Generate TypeScript type definitions from JSON backend types
    GenTypes {
        /// Output file path for TypeScript definitions
        #[clap(default_value = "types/json.ts")]
        output: PathBuf,
    },
    /// Check project for errors without generating output
    Check {
        /// The directory containing the Pyxis source files
        in_dir: PathBuf,
    },
    /// Format all .pyxis files recursively in the current directory
    Fmt {
        /// Check if files are formatted without modifying them
        #[clap(long)]
        check: bool,
    },
}

// The driver depends on `pyxis` with the `json` feature on, so all
// three backends are always available here regardless of upstream
// feature selection.
#[derive(Copy, Clone, ValueEnum)]
enum Backend {
    Rust,
    Json,
    Cpp,
}

impl From<Backend> for pyxis::Backend {
    fn from(backend: Backend) -> Self {
        match backend {
            Backend::Rust => pyxis::Backend::Rust,
            Backend::Json => pyxis::Backend::Json,
            Backend::Cpp => pyxis::Backend::Cpp,
        }
    }
}

/// Build [`pyxis::BuildOptions`] from the CLI's Rust-specific flags. Pure and
/// unit-testable: the smoke tests exercise the option mapping and the
/// in-memory build path without a command-line harness.
fn build_options(
    rust_root_file_name: Option<String>,
    rust_module_prefix: Option<String>,
) -> pyxis::BuildOptions {
    pyxis::BuildOptions {
        rust_root_file_name,
        rust_module_prefix: rust_module_prefix
            .as_deref()
            .map(pyxis::grammar::ItemPath::from),
        ..Default::default()
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    match args.command {
        Command::Build {
            in_dir,
            backend,
            out_dir,
            rust_root_file_name,
            rust_module_prefix,
        } => {
            std::fs::create_dir_all(&out_dir)?;
            let mut file_store = pyxis::source_store::FileStore::new();
            let options = build_options(rust_root_file_name, rust_module_prefix);
            let result = pyxis::build_with_store_and_options(
                &in_dir,
                &out_dir,
                backend.into(),
                &mut file_store,
                options,
            );
            if let Err(err) = result {
                // Format errors with ariadne using the file store
                let formatted = err.format_with_ariadne(&file_store);
                eprintln!("{formatted}");
                std::process::exit(1);
            }
            Ok(())
        }
        Command::Check { in_dir } => {
            let mut file_store = pyxis::source_store::FileStore::new();
            match pyxis::check(&in_dir, &mut file_store) {
                Ok(()) => {
                    println!("No errors found.");
                    Ok(())
                }
                Err(errors) => {
                    for err in &errors {
                        let formatted = err.format_with_ariadne(&file_store);
                        eprintln!("{formatted}");
                    }
                    eprintln!("\n{} error(s) found.", errors.len());
                    std::process::exit(1);
                }
            }
        }
        Command::AstDump { file, pretty } => {
            let content = std::fs::read_to_string(&file)?;
            let filename = file.display().to_string();
            let mut file_store = pyxis::source_store::FileStore::new();
            let file_id = file_store.register_in_memory(filename, content.clone());
            let module = pyxis::parser::parse_str_with_file_id(&content, file_id)?;

            if pretty {
                // Use pretty printer
                println!("{}", pyxis::pretty_print::pretty_print(&module));
            } else {
                // Dump full AST with spans
                println!("{:#?}", module);
            }

            Ok(())
        }
        Command::GenTypes { output } => {
            println!("Exporting TypeScript definitions to {output:?}");

            specta_typescript::Typescript::default()
                .bigint(specta_typescript::BigIntExportBehavior::Number)
                .export_to(output, &pyxis::backends::json::export_types())?;

            Ok(())
        }
        Command::Fmt { check } => {
            let cwd = std::env::current_dir()?;
            let pyxis_files = find_pyxis_files(&cwd)?;

            if pyxis_files.is_empty() {
                println!("No .pyxis files found in {}", cwd.display());
                return Ok(());
            }

            let mut formatted_count = 0;
            let mut error_count = 0;
            let mut needs_formatting = Vec::new();

            for file in &pyxis_files {
                match format_file(file, check) {
                    Ok(was_formatted) => {
                        if was_formatted {
                            formatted_count += 1;
                            if check {
                                needs_formatting.push(file);
                            } else {
                                println!("Formatted: {}", file.display());
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!("Error formatting {}: {}", file.display(), e);
                        error_count += 1;
                    }
                }
            }

            if check {
                if !needs_formatting.is_empty() {
                    println!("\nThe following files need formatting:");
                    for file in needs_formatting {
                        println!("  {}", file.display());
                    }
                    println!("\nRun 'pyxis fmt' to format these files.");
                    std::process::exit(1);
                } else {
                    println!("All {} file(s) are properly formatted.", pyxis_files.len());
                }
            } else {
                println!(
                    "\nFormatted {} file(s), {} error(s)",
                    formatted_count, error_count
                );
            }

            Ok(())
        }
    }
}

/// Recursively find all .pyxis files in the given directory
fn find_pyxis_files(dir: &PathBuf) -> Result<Vec<PathBuf>, Box<dyn std::error::Error>> {
    let mut files = Vec::new();
    find_pyxis_files_recursive(dir, &mut files)?;
    files.sort();
    Ok(files)
}

fn find_pyxis_files_recursive(
    dir: &PathBuf,
    files: &mut Vec<PathBuf>,
) -> Result<(), Box<dyn std::error::Error>> {
    if dir.is_dir() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                find_pyxis_files_recursive(&path, files)?;
            } else if path.extension().and_then(|s| s.to_str()) == Some("pyxis") {
                files.push(path);
            }
        }
    }
    Ok(())
}

/// Format a single file. Returns true if the file was modified (or needs formatting in check mode)
fn format_file(file: &PathBuf, check: bool) -> Result<bool, Box<dyn std::error::Error>> {
    let content = std::fs::read_to_string(file)?;
    let filename = file.display().to_string();
    let mut file_store = pyxis::source_store::FileStore::new();
    let file_id = file_store.register_in_memory(filename, content.clone());
    let module = pyxis::parser::parse_str_with_file_id(&content, file_id)?;
    let formatted = pyxis::pretty_print::pretty_print(&module);

    // Ensure the formatted output ends with a newline
    let formatted_with_newline = if formatted.ends_with('\n') {
        formatted
    } else {
        format!("{}\n", formatted)
    };

    if content != formatted_with_newline {
        if !check {
            std::fs::write(file, &formatted_with_newline)?;
        }
        Ok(true)
    } else {
        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    // The workspace restriction lints target production code; test code is
    // explicitly exempt (see CONTRIBUTING.md).
    #![expect(clippy::expect_used)]

    use pyxis::source_store::FileStore;

    use super::*;

    fn project() -> pyxis::config::Project {
        pyxis::config::Project {
            name: "smoke".to_string(),
            pointer_size: 8,
        }
    }

    #[test]
    fn backend_mapping_is_exhaustive() {
        // Every CLI backend maps to the corresponding compiler backend.
        assert!(matches!(
            pyxis::Backend::from(Backend::Rust),
            pyxis::Backend::Rust
        ));
        assert!(matches!(
            pyxis::Backend::from(Backend::Json),
            pyxis::Backend::Json
        ));
        assert!(matches!(
            pyxis::Backend::from(Backend::Cpp),
            pyxis::Backend::Cpp
        ));
    }

    #[test]
    fn build_options_mapping() {
        // Defaults carry through.
        let defaults = build_options(None, None);
        assert_eq!(defaults.rust_root_file_name, None);
        assert_eq!(defaults.rust_module_prefix, None);

        // A root file name and a module prefix are mapped through.
        let opted = build_options(Some("mod.rs".to_string()), Some("prefixed".to_string()));
        assert_eq!(opted.rust_root_file_name.as_deref(), Some("mod.rs"));
        assert_eq!(
            opted.rust_module_prefix.map(|p| p.to_string()),
            Some("prefixed".to_string())
        );
    }

    #[test]
    fn build_smoke_rust_in_memory() {
        // The driver's build decision (Rust backend, prefixed submodule mount)
        // succeeds against in-memory sources via `build_sources_into` — the
        // same pipeline the driver invokes, minus the CLI/filesystem shell.
        // Output lands in a `MemoryWriter`; the synthetic `out` base only
        // feeds path computation and never touches disk.
        let sources = vec![
            (
                "foo.pyxis".to_string(),
                "pub type Foo {\n    pub value: u32,\n}\n".to_string(),
            ),
            (
                "bar.pyxis".to_string(),
                "use foo::Foo;\n\npub type Bar {\n    pub foo: *mut Foo,\n}\n".to_string(),
            ),
        ];
        let mut file_store = FileStore::new();
        let mut writer = pyxis::output::MemoryWriter::default();
        let result = pyxis::build_sources_into(
            sources,
            &project(),
            std::path::Path::new("out"),
            pyxis::Backend::Rust,
            &mut file_store,
            build_options(Some("mod.rs".to_string()), Some("prefixed".to_string())),
            &mut writer,
        );
        assert!(
            result.is_ok(),
            "expected in-memory Rust build to succeed, got {result:?}"
        );
        // The root is `mod.rs` (per `rust_root_file_name`) and wires up child
        // modules; leaf types land in per-module files.
        let root = writer
            .file(std::path::Path::new("out/mod.rs"))
            .expect("expected a generated out/mod.rs");
        assert!(root.contains("pub mod foo;"), "{root}");
        let foo_rs = writer
            .file(std::path::Path::new("out/foo.rs"))
            .expect("expected a generated out/foo.rs");
        assert!(foo_rs.contains("pub struct Foo"), "{foo_rs}");
    }

    #[test]
    fn build_smoke_json_in_memory() {
        // The JSON backend build path also runs against in-memory sources.
        let sources = vec![(
            "foo.pyxis".to_string(),
            "pub type Foo {\n    pub value: u32,\n}\n".to_string(),
        )];
        let mut file_store = FileStore::new();
        let mut writer = pyxis::output::MemoryWriter::default();
        let result = pyxis::build_sources_into(
            sources,
            &project(),
            std::path::Path::new("out"),
            pyxis::Backend::Json,
            &mut file_store,
            pyxis::BuildOptions::default(),
            &mut writer,
        );
        assert!(
            result.is_ok(),
            "expected in-memory JSON build to succeed, got {result:?}"
        );
        let output_json = writer
            .file(std::path::Path::new("out/output.json"))
            .expect("expected a generated out/output.json");
        assert!(output_json.starts_with('{'), "{output_json}");
        assert!(output_json.contains("\"foo::Foo\""), "{output_json}");
    }

    #[test]
    fn build_smoke_cpp_in_memory() {
        // The C++ backend build path runs against in-memory sources too
        // (the driver compiles pyxis with the `cpp` feature).
        let sources = vec![(
            "foo.pyxis".to_string(),
            "pub type Foo {\n    pub value: u32,\n}\n".to_string(),
        )];
        let mut file_store = FileStore::new();
        let mut writer = pyxis::output::MemoryWriter::default();
        let result = pyxis::build_sources_into(
            sources,
            &project(),
            std::path::Path::new("out"),
            pyxis::Backend::Cpp,
            &mut file_store,
            pyxis::BuildOptions::default(),
            &mut writer,
        );
        assert!(
            result.is_ok(),
            "expected in-memory C++ build to succeed, got {result:?}"
        );
        // The module header, shared runtime header, and CMake glue are all
        // emitted through the writer.
        let foo_hpp = writer
            .file(std::path::Path::new("out/include/foo.hpp"))
            .expect("expected a generated out/include/foo.hpp");
        assert!(foo_hpp.contains("struct Foo"), "{foo_hpp}");
        assert!(
            writer
                .file(std::path::Path::new("out/include/pyxis_runtime.hpp"))
                .is_some(),
            "expected a generated pyxis_runtime.hpp"
        );
        assert!(
            writer
                .file(std::path::Path::new("out/CMakeLists.txt"))
                .is_some(),
            "expected a generated CMakeLists.txt"
        );
    }

    #[test]
    fn check_smoke_in_memory() {
        // The check decision path (used by `pyxis check`) runs the analysis
        // pipeline against in-memory sources and reports no errors.
        let sources = vec![(
            "foo.pyxis".to_string(),
            "pub type Foo {\n    pub value: u32,\n}\n".to_string(),
        )];
        let mut file_store = FileStore::new();
        let result = pyxis::check_sources(sources, 8, &mut file_store);
        assert!(
            result.is_ok(),
            "expected in-memory check to succeed, got {result:?}"
        );
    }
}
