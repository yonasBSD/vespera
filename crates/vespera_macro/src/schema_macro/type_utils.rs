//! Type utility functions for schema macro
//!
//! Provides helper functions for type analysis and manipulation.

use proc_macro2::TokenStream;
use quote::quote;
use syn::Type;

/// Extract type name from a Type
pub fn extract_type_name(ty: &Type) -> Result<String, syn::Error> {
    match ty {
        Type::Path(type_path) => {
            // Get the last segment (handles paths like crate::User)
            let segment = type_path.path.segments.last().ok_or_else(|| {
                syn::Error::new_spanned(ty, "expected a type path with at least one segment")
            })?;
            Ok(segment.ident.to_string())
        }
        _ => Err(syn::Error::new_spanned(
            ty,
            "expected a type path (e.g., `User` or `crate::User`)",
        )),
    }
}

/// Check if a type is a qualified path (has multiple segments like crate::models::User)
pub fn is_qualified_path(ty: &Type) -> bool {
    match ty {
        Type::Path(type_path) => type_path.path.segments.len() > 1,
        _ => false,
    }
}

/// Check if a type is Option<T>
pub fn is_option_type(ty: &Type) -> bool {
    match ty {
        Type::Path(type_path) => type_path
            .path
            .segments
            .first()
            .map(|s| s.ident == "Option")
            .unwrap_or(false),
        _ => false,
    }
}

/// Check if a type is a SeaORM relation type (HasOne, HasMany, BelongsTo)
pub fn is_seaorm_relation_type(ty: &Type) -> bool {
    match ty {
        Type::Path(type_path) => {
            if let Some(segment) = type_path.path.segments.last() {
                let ident = segment.ident.to_string();
                matches!(ident.as_str(), "HasOne" | "HasMany" | "BelongsTo")
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Check if a struct is a SeaORM Model (has #[sea_orm::model] or #[sea_orm(table_name = ...)] attribute)
pub fn is_seaorm_model(struct_item: &syn::ItemStruct) -> bool {
    for attr in &struct_item.attrs {
        // Check for #[sea_orm::model] or #[sea_orm(...)]
        let path = attr.path();
        if path.is_ident("sea_orm") {
            return true;
        }
        // Check for path like sea_orm::model
        let segments: Vec<_> = path.segments.iter().map(|s| s.ident.to_string()).collect();
        if segments.first().is_some_and(|s| s == "sea_orm") {
            return true;
        }
    }
    false
}

/// Check if a type name is a primitive or well-known type that doesn't need path resolution.
pub fn is_primitive_or_known_type(name: &str) -> bool {
    matches!(
        name,
        // Rust primitives
        "bool"
            | "char"
            | "str"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "isize"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
            | "f32"
            | "f64"
            // Common std types
            | "String"
            | "Vec"
            | "Option"
            | "Result"
            | "Box"
            | "Rc"
            | "Arc"
            | "HashMap"
            | "HashSet"
            | "BTreeMap"
            | "BTreeSet"
            // Chrono types
            | "DateTime"
            | "NaiveDateTime"
            | "NaiveDate"
            | "NaiveTime"
            | "Utc"
            | "Local"
            | "FixedOffset"
            // SeaORM types (will be converted separately)
            | "DateTimeWithTimeZone"
            | "DateTimeUtc"
            | "DateTimeLocal"
            // UUID
            | "Uuid"
            // Serde JSON
            | "Value"
    )
}

/// Resolve a simple type to an absolute path using the source module path.
///
/// For example, if source_module_path is ["crate", "models", "memo"] and
/// the type is `MemoStatus`, it returns `crate::models::memo::MemoStatus`.
///
/// If the type is already qualified (has `::`) or is a primitive/known type,
/// returns the original type unchanged.
pub fn resolve_type_to_absolute_path(ty: &Type, source_module_path: &[String]) -> TokenStream {
    let type_path = match ty {
        Type::Path(tp) => tp,
        _ => return quote! { #ty },
    };

    // If path has multiple segments (already qualified like `crate::foo::Bar`), return as-is
    if type_path.path.segments.len() > 1 {
        return quote! { #ty };
    }

    // Get the single segment
    let segment = match type_path.path.segments.first() {
        Some(s) => s,
        None => return quote! { #ty },
    };

    let ident_str = segment.ident.to_string();

    // If it's a primitive or known type, return as-is
    if is_primitive_or_known_type(&ident_str) {
        return quote! { #ty };
    }

    // If no source module path, return as-is
    if source_module_path.is_empty() {
        return quote! { #ty };
    }

    // Build absolute path: source_module_path + type_name
    let path_idents: Vec<syn::Ident> = source_module_path
        .iter()
        .map(|s| syn::Ident::new(s, proc_macro2::Span::call_site()))
        .collect();
    let type_ident = &segment.ident;
    let args = &segment.arguments;

    quote! { #(#path_idents)::* :: #type_ident #args }
}

/// Extract the module path from a type (excluding the type name itself).
/// e.g., `crate::models::memo::Model` -> ["crate", "models", "memo"]
pub fn extract_module_path(ty: &Type) -> Vec<String> {
    match ty {
        Type::Path(type_path) => {
            let segments: Vec<String> = type_path
                .path
                .segments
                .iter()
                .map(|s| s.ident.to_string())
                .collect();
            // Return all but the last segment (which is the type name)
            if segments.len() > 1 {
                segments[..segments.len() - 1].to_vec()
            } else {
                vec![]
            }
        }
        _ => vec![],
    }
}

/// Capitalize the first letter of a string.
pub fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
    }
}
