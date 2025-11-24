mod args;
mod file_utils;
mod method;
mod route;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use std::path::Path;
use syn::LitStr;
use syn::parse::{Parse, ParseStream};

use crate::file_utils::{collect_files, file_to_segments, get_function_list};
use crate::method::Method;
use crate::route::extract_route_info;

/// route attribute macro
#[proc_macro_attribute]
pub fn route(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}

struct AutoRouterInput {
    folder: Option<LitStr>,
}

impl Parse for AutoRouterInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            return Ok(AutoRouterInput { folder: None });
        }

        let folder = input.parse::<LitStr>()?;
        Ok(AutoRouterInput {
            folder: Some(folder),
        })
    }
}

#[proc_macro]
pub fn vespera(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as AutoRouterInput);

    let folder_name = input
        .folder
        .map(|f| f.value())
        .unwrap_or_else(|| "routes".to_string());

    let folder_path = find_folder_path(&folder_name);

    if !folder_path.exists() {
        return syn::Error::new(
            Span::call_site(),
            format!("Folder not found: {}", folder_name),
        )
        .to_compile_error()
        .into();
    }

    generate_router_code(&folder_path, &folder_name)
}

fn find_folder_path(folder_name: &str) -> std::path::PathBuf {
    let path = format!("src/{}", folder_name);
    let path = Path::new(&path);
    if path.exists() && path.is_dir() {
        return path.to_path_buf();
    }

    Path::new(folder_name).to_path_buf()
}

fn generate_router_code(folder_path: &Path, folder_name: &str) -> TokenStream {
    let mut router_nests = Vec::new();
    for file in collect_files(folder_path) {
        if let Ok(file_stem) = file.strip_prefix(folder_path) {
            let segments = file_to_segments(file_stem, folder_path);

            let mut p: syn::punctuated::Punctuated<syn::PathSegment, syn::Token![::]> =
                syn::punctuated::Punctuated::new();
            p.extend(segments.iter().map(|s| syn::PathSegment {
                ident: syn::Ident::new(s, Span::call_site()),
                arguments: syn::PathArguments::None,
            }));
            let (route_path, mod_name) = (
                format!("/{}", segments.join("/")),
                if let Some(folder_name_ident) = if !folder_name.is_empty() {
                    Some(syn::Ident::new(folder_name, Span::call_site()))
                } else {
                    None
                } {
                    quote! { crate::#folder_name_ident::#p }
                } else {
                    quote! { crate::#p }
                },
            );

            // collect all fn names and their HTTP methods and paths from the file
            let fn_list = get_function_list(&file, &route_path).unwrap();

            for (fn_name, method, custom_path) in fn_list {
                // Use custom path if provided, otherwise use default route_path
                let final_path = custom_path.unwrap_or_else(|| route_path.clone());

                // Use full path for axum routing methods
                let method_path: proc_macro2::TokenStream = Method::try_from(method.as_str())
                    .unwrap()
                    .try_into()
                    .unwrap();
                router_nests.push(quote!(
                    .route(#final_path, #method_path(#mod_name::#fn_name))
                ));
            }
        }
    }

    let expanded = quote! {
        axum::Router::new()
            #( #router_nests )*
    };

    expanded.into()
}
