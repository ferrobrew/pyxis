use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
struct Args {
    /// The directory containing the Pyxis source files
    in_dir: PathBuf,
    #[clap(default_value = "out")]
    /// The directory to write the generated Rust files to
    out_dir: PathBuf,
    #[clap(long, default_value = "4")]
    /// The size of a pointer in bytes
    pointer_size: usize,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    std::fs::create_dir_all(&args.out_dir)?;
    pyxis::build(&args.in_dir, &args.out_dir, args.pointer_size)
}
