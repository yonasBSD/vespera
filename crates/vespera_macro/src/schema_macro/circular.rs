//! Circular reference detection and handling
//!
//! Provides functions to detect and handle circular references between
//! SeaORM models when generating schema types.

use proc_macro2::TokenStream;
use quote::quote;

use super::seaorm::extract_belongs_to_from_field;
use super::type_utils::{capitalize_first, is_option_type, is_seaorm_relation_type};
use crate::parser::extract_skip;

/// Detect circular reference fields in a related schema.
///
/// When generating `MemoSchema.user`, we need to check if `UserSchema` has any fields
/// that reference back to `MemoSchema` via BelongsTo/HasOne (FK-based relations).
///
/// HasMany relations are NOT considered circular because they are excluded by default
/// from generated schemas.
///
/// Returns a list of field names that would create circular references.
pub fn detect_circular_fields(
    _source_schema_name: &str,
    source_module_path: &[String],
    related_schema_def: &str,
) -> Vec<String> {
    let mut circular_fields = Vec::new();

    // Parse the related schema definition
    let Ok(parsed) = syn::parse_str::<syn::ItemStruct>(related_schema_def) else {
        return circular_fields;
    };

    // Get the source module name (e.g., "memo" from ["crate", "models", "memo"])
    let source_module = source_module_path.last().map(|s| s.as_str()).unwrap_or("");

    if let syn::Fields::Named(fields_named) = &parsed.fields {
        for field in &fields_named.named {
            let Some(field_ident) = &field.ident else {
                continue;
            };
            let field_name = field_ident.to_string();

            // Check if this field's type references the source schema
            let field_ty = &field.ty;
            let ty_str = quote!(#field_ty).to_string();

            // Normalize whitespace: quote!() produces "foo :: bar" instead of "foo::bar"
            // Remove all whitespace to make pattern matching reliable
            let ty_str_normalized = ty_str.replace(' ', "");

            // SKIP HasMany relations - they are excluded by default from schemas,
            // so they don't create actual circular references in the output
            if ty_str_normalized.contains("HasMany<") {
                continue;
            }

            // Check for BelongsTo/HasOne patterns that reference the source:
            // - HasOne<memo::Entity>
            // - BelongsTo<memo::Entity>
            // - Box<memo::Schema> (already converted)
            // - Option<Box<memo::Schema>>
            let is_circular = (ty_str_normalized.contains("HasOne<")
                || ty_str_normalized.contains("BelongsTo<")
                || ty_str_normalized.contains("Box<"))
                && (ty_str_normalized.contains(&format!("{}::Schema", source_module))
                    || ty_str_normalized.contains(&format!("{}::Entity", source_module))
                    || ty_str_normalized
                        .contains(&format!("{}Schema", capitalize_first(source_module))));

            if is_circular {
                circular_fields.push(field_name);
            }
        }
    }

    circular_fields
}

/// Check if a Model has any BelongsTo or HasOne relations (FK-based relations).
///
/// This is used to determine if the target schema has `from_model()` method
/// (async, with DB) or simple `From<Model>` impl (sync, no DB).
///
/// - Schemas with FK relations -> have `from_model()`, need async call
/// - Schemas without FK relations -> have `From<Model>`, can use sync conversion
pub fn has_fk_relations(model_def: &str) -> bool {
    let Ok(parsed) = syn::parse_str::<syn::ItemStruct>(model_def) else {
        return false;
    };

    if let syn::Fields::Named(fields_named) = &parsed.fields {
        for field in &fields_named.named {
            let field_ty = &field.ty;
            let ty_str = quote!(#field_ty).to_string().replace(' ', "");

            // Check for BelongsTo or HasOne patterns
            if ty_str.contains("HasOne<") || ty_str.contains("BelongsTo<") {
                return true;
            }
        }
    }

    false
}

/// Check if a circular relation field in the related schema is required (Box<T>) or optional (Option<Box<T>>).
///
/// Returns true if the circular relation is required and needs a parent stub.
pub fn is_circular_relation_required(related_model_def: &str, circular_field_name: &str) -> bool {
    let Ok(parsed) = syn::parse_str::<syn::ItemStruct>(related_model_def) else {
        return false;
    };

    if let syn::Fields::Named(fields_named) = &parsed.fields {
        for field in &fields_named.named {
            let Some(field_ident) = &field.ident else {
                continue;
            };
            if *field_ident != circular_field_name {
                continue;
            }

            // Check if this is a HasOne/BelongsTo with required FK
            let ty_str = quote!(#field.ty).to_string().replace(' ', "");
            if ty_str.contains("HasOne<") || ty_str.contains("BelongsTo<") {
                // Check FK field optionality
                let fk_field = extract_belongs_to_from_field(&field.attrs);
                if let Some(fk) = fk_field {
                    // Find FK field and check if it's Option
                    for f in &fields_named.named {
                        if f.ident.as_ref().map(|i| i.to_string()) == Some(fk.clone()) {
                            return !is_option_type(&f.ty);
                        }
                    }
                }
            }
        }
    }
    false
}

/// Generate a default value for a SeaORM relation field in inline construction.
///
/// - `HasMany<T>` -> `vec![]`
/// - `HasOne<T>`/`BelongsTo<T>` with optional FK -> `None`
/// - `HasOne<T>`/`BelongsTo<T>` with required FK -> needs parent stub (handled separately)
pub fn generate_default_for_relation_field(
    ty: &syn::Type,
    field_ident: &syn::Ident,
    field_attrs: &[syn::Attribute],
    all_fields: &syn::FieldsNamed,
) -> TokenStream {
    let ty_str = quote!(#ty).to_string().replace(' ', "");

    // Check the SeaORM relation type
    if ty_str.contains("HasMany<") {
        // HasMany -> Vec<Schema> -> empty vec
        quote! { #field_ident: vec![] }
    } else if ty_str.contains("HasOne<") || ty_str.contains("BelongsTo<") {
        // Check FK field optionality
        let fk_field = extract_belongs_to_from_field(field_attrs);
        let is_optional = fk_field
            .as_ref()
            .map(|fk| {
                all_fields.named.iter().any(|f| {
                    f.ident.as_ref().map(|i| i.to_string()) == Some(fk.clone())
                        && is_option_type(&f.ty)
                })
            })
            .unwrap_or(true);

        if is_optional {
            // Option<Box<Schema>> -> None
            quote! { #field_ident: None }
        } else {
            // Box<Schema> (required) -> use __parent_stub__
            // This variable will be defined by the caller when needed
            quote! { #field_ident: Box::new(__parent_stub__.clone()) }
        }
    } else {
        // Unknown relation type - try Default::default()
        quote! { #field_ident: Default::default() }
    }
}

/// Generate inline struct construction for a related schema, excluding circular fields.
///
/// Instead of `<user::Schema as From<_>>::from(r)`, generates:
/// ```ignore
/// user::Schema {
///     id: r.id,
///     name: r.name,
///     memos: vec![], // circular field - use default
/// }
/// ```
pub fn generate_inline_struct_construction(
    schema_path: &TokenStream,
    related_schema_def: &str,
    circular_fields: &[String],
    var_name: &str,
) -> TokenStream {
    // Parse the related schema definition
    let Ok(parsed) = syn::parse_str::<syn::ItemStruct>(related_schema_def) else {
        // Fallback to From::from if parsing fails
        let var_ident = syn::Ident::new(var_name, proc_macro2::Span::call_site());
        return quote! { <#schema_path as From<_>>::from(#var_ident) };
    };

    let var_ident = syn::Ident::new(var_name, proc_macro2::Span::call_site());

    // Get the named fields for FK checking
    let fields_named = match &parsed.fields {
        syn::Fields::Named(f) => f,
        _ => {
            return quote! { <#schema_path as From<_>>::from(#var_ident) };
        }
    };

    let field_assignments: Vec<TokenStream> = fields_named
        .named
        .iter()
        .filter_map(|field| {
            let field_ident = field.ident.as_ref()?;
            let field_name = field_ident.to_string();

            // Skip fields marked with serde(skip)
            if extract_skip(&field.attrs) {
                return None;
            }

            if circular_fields.contains(&field_name) || is_seaorm_relation_type(&field.ty) {
                // Circular field or relation field - generate appropriate default
                // based on the SeaORM relation type
                Some(generate_default_for_relation_field(
                    &field.ty,
                    field_ident,
                    &field.attrs,
                    fields_named,
                ))
            } else {
                // Regular field - copy from model
                Some(quote! { #field_ident: #var_ident.#field_ident })
            }
        })
        .collect();

    quote! {
        #schema_path {
            #(#field_assignments),*
        }
    }
}

/// Generate inline type construction for from_model.
///
/// When we have an inline type (e.g., `MemoResponseRel_User`), this function generates
/// the construction code that only includes the fields present in the inline type.
///
/// ```ignore
/// MemoResponseRel_User {
///     id: r.id,
///     name: r.name,
///     email: r.email,
///     // memos field is NOT included - it was excluded from inline type
/// }
/// ```
pub fn generate_inline_type_construction(
    inline_type_name: &syn::Ident,
    included_fields: &[String],
    related_model_def: &str,
    var_name: &str,
) -> TokenStream {
    // Parse the related model definition
    let Ok(parsed) = syn::parse_str::<syn::ItemStruct>(related_model_def) else {
        // Fallback to Default if parsing fails
        return quote! { Default::default() };
    };

    let var_ident = syn::Ident::new(var_name, proc_macro2::Span::call_site());

    // Get the named fields
    let fields_named = match &parsed.fields {
        syn::Fields::Named(f) => f,
        _ => {
            return quote! { Default::default() };
        }
    };

    let field_assignments: Vec<TokenStream> = fields_named
        .named
        .iter()
        .filter_map(|field| {
            let field_ident = field.ident.as_ref()?;
            let field_name = field_ident.to_string();

            // Skip fields marked with serde(skip)
            if extract_skip(&field.attrs) {
                return None;
            }

            // Skip relation fields (they are not in the inline type)
            if is_seaorm_relation_type(&field.ty) {
                return None;
            }

            // Only include fields that are in the inline type's field list
            if included_fields.contains(&field_name) {
                // Regular field - copy from model
                Some(quote! { #field_ident: #var_ident.#field_ident })
            } else {
                // This field was excluded (circular reference or otherwise)
                None
            }
        })
        .collect();

    quote! {
        #inline_type_name {
            #(#field_assignments),*
        }
    }
}
