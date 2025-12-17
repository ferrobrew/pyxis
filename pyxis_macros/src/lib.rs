use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{Data, DeriveInput, Fields, Ident, parse_macro_input, spanned::Spanned};

/// Derive macro for the HasLocation trait.
///
/// For structs, returns a reference to the `location` field.
/// For enums, matches each variant and returns its `location` field.
///
/// # Example
/// ```ignore
/// #[derive(HasLocation)]
/// pub struct Function {
///     pub name: String,
///     pub location: ItemLocation,
/// }
///
/// #[derive(HasLocation)]
/// pub enum Argument {
///     Named { ident: String, location: ItemLocation },
///     ConstSelf { location: ItemLocation },
/// }
/// ```
#[proc_macro_derive(HasLocation)]
pub fn derive_has_location(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let body = match &input.data {
        Data::Struct(data) => {
            // For structs, just return &self.location
            match &data.fields {
                Fields::Named(fields) => {
                    let has_location = fields
                        .named
                        .iter()
                        .any(|f| f.ident.as_ref().is_some_and(|i| i == "location"));
                    if !has_location {
                        return syn::Error::new(
                            input.span(),
                            "HasLocation derive requires a field named `location`",
                        )
                        .to_compile_error()
                        .into();
                    }
                    quote! { &self.location }
                }
                _ => {
                    return syn::Error::new(
                        input.span(),
                        "HasLocation can only be derived for structs with named fields",
                    )
                    .to_compile_error()
                    .into();
                }
            }
        }
        Data::Enum(data) => {
            // For enums, match each variant and return its location field
            let mut errors = Vec::new();
            let arms: Vec<_> = data
                .variants
                .iter()
                .filter_map(|variant| {
                    let variant_name = &variant.ident;
                    match &variant.fields {
                        Fields::Named(fields) => {
                            let has_location = fields
                                .named
                                .iter()
                                .any(|f| f.ident.as_ref().is_some_and(|i| i == "location"));
                            if has_location {
                                Some(quote! {
                                    #name::#variant_name { location, .. } => location
                                })
                            } else if let Some(first_field) = fields.named.first() {
                                // Variant delegates to an inner type that implements HasLocation
                                let field_name = first_field.ident.as_ref().unwrap();
                                Some(quote! {
                                    #name::#variant_name { #field_name, .. } => #field_name.location()
                                })
                            } else {
                                errors.push(syn::Error::new(
                                    variant.span(),
                                    format!(
                                        "variant `{}` has no fields; cannot derive HasLocation",
                                        variant_name
                                    ),
                                ));
                                None
                            }
                        }
                        Fields::Unnamed(fields) => {
                            // For tuple variants like Variant(InnerType), delegate to inner
                            if fields.unnamed.len() == 1 {
                                Some(quote! {
                                    #name::#variant_name(inner) => inner.location()
                                })
                            } else {
                                errors.push(syn::Error::new(
                                    variant.span(),
                                    format!(
                                        "variant `{}` must have exactly one field to derive HasLocation",
                                        variant_name
                                    ),
                                ));
                                None
                            }
                        }
                        Fields::Unit => {
                            errors.push(syn::Error::new(
                                variant.span(),
                                format!(
                                    "unit variant `{}` cannot derive HasLocation",
                                    variant_name
                                ),
                            ));
                            None
                        }
                    }
                })
                .collect();

            if let Some(first_error) = errors.into_iter().reduce(|mut acc, e| {
                acc.combine(e);
                acc
            }) {
                return first_error.to_compile_error().into();
            }

            quote! {
                match self {
                    #(#arms),*
                }
            }
        }
        Data::Union(_) => {
            return syn::Error::new(input.span(), "HasLocation cannot be derived for unions")
                .to_compile_error()
                .into();
        }
    };

    let expanded = quote! {
        impl #impl_generics crate::span::HasLocation for #name #ty_generics #where_clause {
            fn location(&self) -> &crate::span::ItemLocation {
                #body
            }
        }
    };

    TokenStream::from(expanded)
}

/// Derive macro for the StripLocations trait (test-only).
///
/// For structs, reconstructs the struct with all fields calling `.strip_locations()`
/// except the `location` field which becomes `ItemLocation::test()`.
///
/// For enums, matches each variant and reconstructs similarly.
///
/// # Attributes
/// - `#[strip_locations(copy)]` - For Copy types, just returns `*self`
/// - `#[strip_locations(skip)]` - Skip this field entirely (set to default or empty Vec)
///
/// # Example
/// ```ignore
/// #[derive(StripLocations)]
/// pub struct Function {
///     pub name: String,
///     pub location: ItemLocation,
///     #[strip_locations(skip)]
///     pub inline_comments: Vec<String>,
/// }
///
/// #[derive(StripLocations)]
/// #[strip_locations(copy)]
/// pub enum Visibility {
///     Public,
///     Private,
/// }
/// ```
#[proc_macro_derive(StripLocations, attributes(strip_locations))]
pub fn derive_strip_locations(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    // Check for top-level #[strip_locations(copy)] attribute
    let is_copy = input.attrs.iter().any(|attr| {
        if attr.path().is_ident("strip_locations") {
            attr.parse_args::<Ident>()
                .is_ok_and(|ident| ident == "copy")
        } else {
            false
        }
    });

    if is_copy {
        let expanded = quote! {
            #[cfg(test)]
            impl #impl_generics crate::span::StripLocations for #name #ty_generics #where_clause {
                fn strip_locations(&self) -> Self {
                    *self
                }
            }
        };
        return TokenStream::from(expanded);
    }

    let body = match &input.data {
        Data::Struct(data) => generate_struct_strip_locations(name, &data.fields),
        Data::Enum(data) => {
            let arms = data.variants.iter().map(|variant| {
                let variant_name = &variant.ident;
                generate_enum_variant_strip_locations(name, variant_name, &variant.fields)
            });
            quote! {
                match self {
                    #(#arms),*
                }
            }
        }
        Data::Union(_) => {
            return syn::Error::new(input.span(), "StripLocations cannot be derived for unions")
                .to_compile_error()
                .into();
        }
    };

    let expanded = quote! {
        #[cfg(test)]
        impl #impl_generics crate::span::StripLocations for #name #ty_generics #where_clause {
            fn strip_locations(&self) -> Self {
                #body
            }
        }
    };

    TokenStream::from(expanded)
}

fn generate_struct_strip_locations(name: &Ident, fields: &Fields) -> proc_macro2::TokenStream {
    match fields {
        Fields::Named(fields) => {
            let field_assignments: Vec<_> = fields
                .named
                .iter()
                .map(|field| {
                    let field_name = field.ident.as_ref().unwrap();
                    let field_attr = get_strip_locations_attr(field);

                    if field_name == "location" {
                        quote! { #field_name: crate::span::ItemLocation::test() }
                    } else if field_attr == Some(FieldAttr::Skip) {
                        // Skip fields get their default value
                        quote! { #field_name: Default::default() }
                    } else {
                        quote! { #field_name: self.#field_name.strip_locations() }
                    }
                })
                .collect();

            quote! {
                #name {
                    #(#field_assignments),*
                }
            }
        }
        Fields::Unnamed(fields) => {
            let field_assignments: Vec<_> = fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, _)| {
                    let index = syn::Index::from(i);
                    quote! { self.#index.strip_locations() }
                })
                .collect();

            quote! {
                #name(#(#field_assignments),*)
            }
        }
        Fields::Unit => {
            quote! { #name }
        }
    }
}

fn generate_enum_variant_strip_locations(
    enum_name: &Ident,
    variant_name: &Ident,
    fields: &Fields,
) -> proc_macro2::TokenStream {
    match fields {
        Fields::Named(fields) => {
            let field_names: Vec<_> = fields
                .named
                .iter()
                .map(|f| f.ident.as_ref().unwrap())
                .collect();

            let field_assignments: Vec<_> = fields
                .named
                .iter()
                .map(|field| {
                    let field_name = field.ident.as_ref().unwrap();

                    if field_name == "location" {
                        quote! { #field_name: crate::span::ItemLocation::test() }
                    } else {
                        quote! { #field_name: #field_name.strip_locations() }
                    }
                })
                .collect();

            // Generate pattern with all field names
            let pattern_fields: Vec<_> = field_names
                .iter()
                .map(|name| {
                    if *name == "location" {
                        quote! { #name: _ }
                    } else {
                        quote! { #name }
                    }
                })
                .collect();

            quote! {
                #enum_name::#variant_name { #(#pattern_fields),* } => #enum_name::#variant_name {
                    #(#field_assignments),*
                }
            }
        }
        Fields::Unnamed(fields) => {
            let binding_names: Vec<_> = (0..fields.unnamed.len())
                .map(|i| format_ident!("f{}", i))
                .collect();

            let field_assignments: Vec<_> = binding_names
                .iter()
                .map(|name| quote! { #name.strip_locations() })
                .collect();

            quote! {
                #enum_name::#variant_name(#(#binding_names),*) => #enum_name::#variant_name(#(#field_assignments),*)
            }
        }
        Fields::Unit => {
            quote! {
                #enum_name::#variant_name => #enum_name::#variant_name
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FieldAttr {
    Skip,
}

fn get_strip_locations_attr(field: &syn::Field) -> Option<FieldAttr> {
    for attr in &field.attrs {
        if attr.path().is_ident("strip_locations")
            && let Ok(ident) = attr.parse_args::<Ident>()
            && ident == "skip"
        {
            return Some(FieldAttr::Skip);
        }
    }
    None
}
