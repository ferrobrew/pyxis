use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let root = Path::new("codegen_tests");
    let input_dir = root.join("input");
    let output_root = root.join("output");
    let rust_output = output_root.join("rust");
    let json_output = output_root.join("json");
    let cpp_output = output_root.join("cpp");

    // -- Rust backend ---------------------------------------------------
    std::fs::create_dir_all(&rust_output)?;
    let mut file_store = pyxis::source_store::FileStore::new();
    if let Err(err) = pyxis::build_with_store(
        &input_dir,
        &rust_output,
        pyxis::Backend::Rust,
        &mut file_store,
    ) {
        eprintln!("{}", err.format_with_ariadne(&file_store));
        std::process::exit(1);
    }
    let mut module_decls = std::fs::read_dir(&rust_output)?
        .filter_map(|entry| Some(entry.ok()?.path()))
        .filter(|path| path.extension().and_then(|p| p.to_str()) == Some("rs"))
        .filter_map(|path| path.file_stem().map(|s| s.to_string_lossy().to_string()))
        .filter(|name| name != "lib")
        .map(|name| format!("pub mod {name};"))
        .collect::<Vec<_>>();
    module_decls.sort();
    std::fs::write(rust_output.join("lib.rs"), module_decls.join("\n") + "\n")?;
    let status = std::process::Command::new("cargo")
        .arg("clippy")
        .current_dir(&rust_output)
        .status()?;
    if !status.success() {
        return Err("cargo clippy failed".into());
    }

    // -- JSON backend ---------------------------------------------------
    std::fs::create_dir_all(&json_output)?;
    let mut file_store = pyxis::source_store::FileStore::new();
    if let Err(err) = pyxis::build_with_store(
        &input_dir,
        &json_output,
        pyxis::Backend::Json,
        &mut file_store,
    ) {
        eprintln!("{}", err.format_with_ariadne(&file_store));
        std::process::exit(1);
    }
    let json_output_file = json_output.join("output.json");
    if !json_output_file.exists() {
        return Err("JSON output file was not created".into());
    }
    let json_content = std::fs::read_to_string(&json_output_file)?;
    serde_json::from_str::<pyxis::backends::json::JsonDocumentation>(&json_content)?;

    // -- C++ backend ----------------------------------------------------
    std::fs::create_dir_all(&cpp_output)?;
    let mut file_store = pyxis::source_store::FileStore::new();
    if let Err(err) = pyxis::build_with_store(
        &input_dir,
        &cpp_output,
        pyxis::Backend::Cpp,
        &mut file_store,
    ) {
        eprintln!("{}", err.format_with_ariadne(&file_store));
        std::process::exit(1);
    }
    // Configure + build the emitted CMake project against the host's vanilla
    // C++ compiler. The codegen_tests corpus uses pointer_size = 8, so the
    // layout `static_assert`s hold on x86-64 hosts without needing xwin or
    // an MSVC toolchain.
    let cpp_build_dir = root.join("target").join("cpp_build");
    std::fs::create_dir_all(&cpp_build_dir)?;
    let configure = std::process::Command::new("cmake")
        .args(["-S"])
        .arg(&cpp_output)
        .args(["-B"])
        .arg(&cpp_build_dir)
        .arg("-DCMAKE_BUILD_TYPE=Release")
        .status()?;
    if !configure.success() {
        return Err("cmake configure failed for C++ codegen output".into());
    }
    let build = std::process::Command::new("cmake")
        .args(["--build"])
        .arg(&cpp_build_dir)
        .arg("--parallel")
        .status()?;
    if !build.success() {
        return Err("cmake build failed for C++ codegen output".into());
    }

    Ok(())
}
