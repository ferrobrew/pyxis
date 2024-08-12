use std::path::Path;

fn main() -> anyhow::Result<()> {
    let root = Path::new("codegen_tests");
    pyxis::build(&root.join("input"), &root.join("output"), 8)
}
