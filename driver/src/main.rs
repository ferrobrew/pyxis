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
    }
}
