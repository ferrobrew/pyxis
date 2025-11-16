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
    /// Format all .pyxis files recursively in the current directory
    Fmt {
        /// Check if files are formatted without modifying them
        #[clap(long)]
        check: bool,
    },
}

#[derive(Copy, Clone, ValueEnum)]
enum Backend {
    Rust,
    Json,
}

impl From<Backend> for pyxis::Backend {
    fn from(backend: Backend) -> Self {
        match backend {
            Backend::Rust => pyxis::Backend::Rust,
            Backend::Json => pyxis::Backend::Json,
        }
    }
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    match args.command {
        Command::Build {
            in_dir,
            backend,
            out_dir,
        } => {
            std::fs::create_dir_all(&out_dir)?;
            pyxis::build(&in_dir, &out_dir, backend.into())
        }
        Command::AstDump { file, pretty } => {
            let content = std::fs::read_to_string(&file)?;
            let filename = file.display().to_string();
            let module = pyxis::parser::parse_str_with_filename(&content, &filename)?;

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
fn find_pyxis_files(dir: &PathBuf) -> anyhow::Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    find_pyxis_files_recursive(dir, &mut files)?;
    files.sort();
    Ok(files)
}

fn find_pyxis_files_recursive(dir: &PathBuf, files: &mut Vec<PathBuf>) -> anyhow::Result<()> {
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
fn format_file(file: &PathBuf, check: bool) -> anyhow::Result<bool> {
    let content = std::fs::read_to_string(file)?;
    let filename = file.display().to_string();
    let module = pyxis::parser::parse_str_with_filename(&content, &filename)?;
    let formatted = pyxis::pretty_print::pretty_print(&module);

    if content != formatted {
        if !check {
            std::fs::write(file, formatted)?;
        }
        Ok(true)
    } else {
        Ok(false)
    }
}
