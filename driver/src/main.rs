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
    /// Build Pyxis source files
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
}

#[derive(Copy, Clone, ValueEnum)]
enum Backend {
    Rust,
}
impl From<Backend> for pyxis::Backend {
    fn from(backend: Backend) -> Self {
        match backend {
            Backend::Rust => pyxis::Backend::Rust,
        }
    }
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    match args.command {
        Command::Build { in_dir, backend, out_dir } => {
            std::fs::create_dir_all(&out_dir)?;
            pyxis::build(&in_dir, &out_dir, backend.into())
        }
        Command::AstDump { file, pretty } => {
            let content = std::fs::read_to_string(&file)?;
            let module = pyxis::parser::parse_str(&content)?;

            if pretty {
                // Use pretty printer
                println!("{}", pyxis::pretty_print::pretty_print(&module));
            } else {
                // Dump full AST with spans
                println!("{:#?}", module);
            }

            Ok(())
        }
    }
}
