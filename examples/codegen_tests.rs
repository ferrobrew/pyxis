use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let root = Path::new("codegen_tests");
    let output_dir = root.join("output");
    let json_output_dir = root.join("json_output");

    // Create a shared source store for error reporting
    let mut store = pyxis::source_store::FilesystemSourceStore::new();

    // Generate Rust backend output
    if let Err(err) = pyxis::build(&root.join("input"), &output_dir, pyxis::Backend::Rust) {
        // Format errors with ariadne before displaying
        let formatted = err.format_with_ariadne(&mut store);
        eprintln!("{formatted}");
        std::process::exit(1);
    }

    let mut module_decls = std::fs::read_dir(&output_dir)?
        .filter_map(|entry| Some(entry.ok()?.path()))
        .filter(|path| path.extension().and_then(|p| p.to_str()) == Some("rs"))
        .filter_map(|path| path.file_stem().map(|s| s.to_string_lossy().to_string()))
        .filter(|name| name != "lib")
        .map(|name| format!("pub mod {name};"))
        .collect::<Vec<_>>();
    module_decls.sort();
    std::fs::write(output_dir.join("lib.rs"), module_decls.join("\n") + "\n")?;

    let status = std::process::Command::new("cargo")
        .arg("clippy")
        .current_dir(&output_dir)
        .status()?;
    if !status.success() {
        return Err("cargo clippy failed".into());
    }

    // Generate JSON backend output
    std::fs::create_dir_all(&json_output_dir)?;
    if let Err(err) = pyxis::build(&root.join("input"), &json_output_dir, pyxis::Backend::Json) {
        // Format errors with ariadne before displaying
        let formatted = err.format_with_ariadne(&mut store);
        eprintln!("{formatted}");
        std::process::exit(1);
    }

    // Verify JSON output exists and is valid
    let json_output_file = json_output_dir.join("output.json");
    if !json_output_file.exists() {
        return Err("JSON output file was not created".into());
    }

    // Parse JSON to verify it's valid
    let json_content = std::fs::read_to_string(&json_output_file)?;
    serde_json::from_str::<pyxis::backends::json::JsonDocumentation>(&json_content)?;

    Ok(())
}
