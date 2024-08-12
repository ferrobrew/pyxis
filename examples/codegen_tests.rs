use std::path::Path;

fn main() -> anyhow::Result<()> {
    pyxis::build(
        &Path::new("codegen_tests/input"),
        &Path::new("codegen_tests/output"),
        8,
    )
}
