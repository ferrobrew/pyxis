//! Very rudimentary CLI application to run Pyxis with the Rust backend
//! to test what the output will look like without having to look at it
//! in situ

use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
struct Args {
    /// The directory containing the Pyxis source files
    in_dir: Option<PathBuf>,
    #[clap(requires = "in_dir")]
    /// The directory to write the generated Rust files to
    out_dir: Option<PathBuf>,
    #[clap(long, default_value = "4")]
    /// The size of a pointer in bytes
    pointer_size: usize,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let in_dir = args
        .in_dir
        .unwrap_or_else(|| PathBuf::from("codegen_tests/input"));
    let out_dir = args
        .out_dir
        .unwrap_or_else(|| PathBuf::from("codegen_tests/output"));

    std::fs::create_dir_all(&out_dir)?;
    pyxis::build(&in_dir, &out_dir, args.pointer_size)
}
