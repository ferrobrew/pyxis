use std::fs;
use std::path::PathBuf;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Get output path from command line or use default
    let args: Vec<String> = std::env::args().collect();
    let output_path = if args.len() > 1 {
        PathBuf::from(&args[1])
    } else {
        PathBuf::from("pyxis-types.ts")
    };

    println!("Generating TypeScript definitions to {:?}", output_path);

    // Export all the JSON types
    let typescript = specta_typescript::export::<pyxis::backends::json::JsonDocumentation>(&Default::default())?;

    // Write to file
    fs::write(&output_path, typescript)?;

    println!("✓ TypeScript definitions generated successfully!");
    println!("  File: {:?}", output_path);

    Ok(())
}
