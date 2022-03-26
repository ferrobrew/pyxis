use std::{env, io::Write, path::PathBuf, process::Command};

use super::{
    grammar::ItemPath,
    semantic_analysis::{MetadataValue, Region, SemanticState, TypeState, TypeStateResolved},
};

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

    for (key, group) in semantic_state
        .type_registry()
        .resolved()
        .iter()
        .sorted()
        .group_by(|t| t.parent())
        .into_iter()
        .filter_map(|(key, group)| key.map(|k| (k, group)))
    {
        write_module(key, group, &semantic_state)?;
    }

    Ok(())
}

fn write_module<'a>(
    key: ItemPath,
    types: impl Iterator<Item = &'a ItemPath>,
    semantic_state: &SemanticState,
) -> Result<(), anyhow::Error> {
    const FORMAT_OUTPUT: bool = true;

    let path = std::iter::once(env::var("OUT_DIR")?)
        .chain(key.iter().map(|s| s.as_str().to_string()))
        .collect::<PathBuf>()
        .with_extension("rs");
    let directory_path = path.parent().map(|p| p.to_path_buf()).unwrap_or_default();
    std::fs::create_dir_all(&directory_path)?;

    let mut file = std::fs::File::create(&path)?;
    for item_path in types {
        write_type(semantic_state, item_path, &mut file)?;
    }

    if FORMAT_OUTPUT {
        Command::new("rustfmt").args([&path]).output()?;
    }

    Ok(())
}

fn write_type(
    semantic_state: &SemanticState,
    item_path: &ItemPath,
    file: &mut std::fs::File,
) -> Result<(), anyhow::Error> {
    let type_definition = semantic_state.type_registry().get(item_path).unwrap();
    if let (
        Some(name),
        TypeState::Resolved(TypeStateResolved {
            size,
            regions,
            metadata,
        }),
    ) = (item_path.last(), &type_definition.state)
    {
        use super::semantic_analysis::TypeRef;

        fn fully_qualified_type_ref_impl(
            out: &mut String,
            type_ref: &TypeRef,
        ) -> Result<(), std::fmt::Error> {
            use std::fmt::Write;
            match type_ref {
                TypeRef::Raw(path) => {
                    // todo: re-evaluate this hack
                    if path.len() > 1 {
                        write!(out, "crate::")?;
                    }
                    write!(out, "{}", path)
                }
                TypeRef::ConstPointer(tr) => {
                    write!(out, "*const ")?;
                    fully_qualified_type_ref_impl(out, tr.as_ref())
                }
                TypeRef::MutPointer(tr) => {
                    write!(out, "*mut ")?;
                    fully_qualified_type_ref_impl(out, tr.as_ref())
                }
                TypeRef::Array(tr, size) => {
                    write!(out, "[")?;
                    fully_qualified_type_ref_impl(out, tr.as_ref())?;
                    write!(out, "; {}]", size)
                }
            }
        }

        fn fully_qualified_type_ref(type_ref: &TypeRef) -> Result<String, std::fmt::Error> {
            let mut out = String::new();
            fully_qualified_type_ref_impl(&mut out, type_ref)?;
            Ok(out)
        }

        let fields = regions
            .iter()
            .enumerate()
            .map(|(i, r)| {
                Ok(match r {
                    Region::Field(field, type_ref) => {
                        let field_ident = quote::format_ident!("{}", field);
                        let syn_type: syn::Type =
                            syn::parse_str(&fully_qualified_type_ref(type_ref)?)?;
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
            .collect::<anyhow::Result<Vec<_>>>()?;

        let name_ident = quote::format_ident!("{}", name.as_str());
        let size_check_ident = quote::format_ident!("_{}_size_check", name.as_str());

        let singleton_impl = metadata
            .iter()
            .find(|(k, _)| k.as_str() == "singleton")
            .map(|(_, address)| match address {
                MetadataValue::Integer(ref address) => {
                    let address = *address as usize;
                    quote! {
                        impl #name_ident {
                            pub fn get() -> &'static mut Self {
                                unsafe {
                                    &mut **::std::mem::transmute::<usize, *mut *mut Self>(#address)
                                }
                            }
                        }
                    }
                }
            });

        let body = quote! {
            #[repr(C)]
            pub struct #name_ident {
                #(#fields),*
            }
            #[allow(non_snake_case)]
            fn #size_check_ident() {
                unsafe {
                    ::std::mem::transmute::<_, #name_ident>([0u8; #size]);
                }
                unreachable!()
            }
            #singleton_impl
        };

        writeln!(file, "{}", body)?;
    };
    Ok(())
}
