use std::{env, io::Write, path::PathBuf, process::Command};

use super::semantic_analysis::{Region, SemanticState, TypeState};

use itertools::Itertools;
use quote::quote;

pub fn build() -> anyhow::Result<()> {
    let pointer_size = env::var("CARGO_CFG_TARGET_POINTER_WIDTH")?.parse::<usize>()? / 8;
    let mut semantic_state = SemanticState::new(pointer_size);

    println!("cargo:rerun-if-changed=types");
    for path in glob::glob("types/**/*.rstl")?.filter_map(Result::ok) {
        semantic_state.add_file(&path)?;
    }
    semantic_state.build()?;

    const FORMAT_OUTPUT: bool = true;
    for (key, group) in semantic_state
        .type_registry()
        .resolved()
        .iter()
        .group_by(|t| t.parent())
        .into_iter()
        .filter_map(|(key, group)| key.map(|k| (k, group)))
    {
        let path = std::iter::once(env::var("OUT_DIR")?)
            .chain(key.iter().map(|s| s.as_str().to_string()))
            .collect::<PathBuf>()
            .with_extension("rs");

        let directory_path = path.parent().map(|p| p.to_path_buf()).unwrap_or_default();
        std::fs::create_dir_all(&directory_path)?;

        let mut file = std::fs::File::create(&path)?;
        for item_path in group {
            write_type(&semantic_state, item_path, &mut file)?;
        }

        if FORMAT_OUTPUT {
            Command::new("rustfmt").args([&path]).output()?;
        }
    }

    Ok(())
}

fn write_type(
    semantic_state: &SemanticState,
    item_path: &super::grammar::ItemPath,
    file: &mut std::fs::File,
) -> Result<(), anyhow::Error> {
    let type_definition = semantic_state.type_registry().get(item_path).unwrap();
    if let (Some(name), TypeState::Resolved { size, regions }) =
        (item_path.last(), &type_definition.state)
    {
        let name_ident = quote::format_ident!("{}", name.as_str());
        let fields = regions
            .iter()
            .enumerate()
            .map(|(i, r)| {
                Ok(match r {
                    Region::Field(field, type_ref) => {
                        let field_ident = quote::format_ident!("{}", field);
                        let syn_type: syn::Type = syn::parse_str(&type_ref.to_string())?;
                        quote! {
                            pub #field_ident: #syn_type
                        }
                    }
                    Region::Padding(size) => {
                        let field_ident = quote::format_ident!("padding_{}", i);
                        quote! {
                            #field_ident: [u8; #size]
                        }
                    }
                })
            })
            .collect::<syn::Result<Vec<_>>>()?;

        let body = quote! {
            pub struct #name_ident {
                #(#fields),*
            }
        };

        writeln!(file, "{}", body)?;
    };
    Ok(())
}
