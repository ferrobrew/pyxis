//! Very rudimentary CLI application to run Pyxis with the Rust backend
//! to test what the output will look like without having to look at it
//! in situ

use std::path::PathBuf;

use anyhow::Context;

fn main() -> anyhow::Result<()> {
    let mut args = std::env::args();
    args.next(); // skip the executable name
    let in_dir: PathBuf = args.next().context("No input directory provided")?.into();
    let out_dir: PathBuf = args.next().unwrap_or_else(|| String::from("out")).into();

    let _ = std::fs::remove_dir_all(&out_dir);

    // assume hardcoded pointer size for now
    let pointer_size = 4;
    pyxis::build(&in_dir, &out_dir, pointer_size)
}
