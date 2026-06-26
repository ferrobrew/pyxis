//! Pyxis Language Server entry point.

fn main() {
    if let Err(e) = pyxis_lsp::main_loop::run() {
        eprintln!("pyxis-lsp error: {e}");
        std::process::exit(1);
    }
}
