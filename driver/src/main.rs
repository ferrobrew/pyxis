use std::path::PathBuf;

use clap::{Parser, ValueEnum};

#[derive(Parser)]
struct Args {
    /// The directory containing the Pyxis source files
    in_dir: PathBuf,
    /// The backend to use
    #[clap(short, long)]
    backend: Backend,
    #[clap(default_value = "out")]
    /// The directory to write the generated Rust files to
    out_dir: PathBuf,
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

    std::fs::create_dir_all(&args.out_dir)?;
    pyxis::build(&args.in_dir, &args.out_dir, args.backend.into())
}
