use std::path::Path;

fn main() -> anyhow::Result<()> {
    let root = Path::new("codegen_tests");
    let output_dir = root.join("output");
    pyxis::build(&root.join("input"), &output_dir, 8)?;

    let module_decls = std::fs::read_dir(&output_dir)?
        .filter_map(|entry| Some(entry.ok()?.path()))
        .filter(|path| path.extension().and_then(|p| p.to_str()) == Some("rs"))
        .filter_map(|path| path.file_stem().map(|s| s.to_string_lossy().to_string()))
        .filter(|name| name != "lib")
        .map(|name| format!("pub mod {name};"))
        .collect::<Vec<_>>();
    std::fs::write(output_dir.join("lib.rs"), module_decls.join("\n"))?;

    let status = std::process::Command::new("cargo")
        .arg("clippy")
        .current_dir(&output_dir)
        .status()?;
    if !status.success() {
        anyhow::bail!("cargo clippy failed");
    }

    Ok(())
}
