//! Code generation utilities for schema macros
//!
//! Provides functions to convert schema structures to TokenStream for code generation.

use std::collections::HashSet;

use proc_macro2::TokenStream;
use quote::quote;

use super::type_utils::is_option_type;
use crate::metadata::StructMetadata;
use crate::parser::{
    extract_default, extract_field_rename, extract_rename_all, extract_skip,
    extract_skip_serializing_if, parse_type_to_schema_ref, rename_field, strip_raw_prefix,
};
use vespera_core::schema::{Schema, SchemaRef, SchemaType};

/// Generate Schema construction code with field filtering
pub fn generate_filtered_schema(
    struct_item: &syn::ItemStruct,
    omit_set: &HashSet<String>,
    pick_set: &HashSet<String>,
    schema_storage: &[StructMetadata],
) -> Result<TokenStream, syn::Error> {
    let rename_all = extract_rename_all(&struct_item.attrs);

    // Build known_schemas and struct_definitions for type resolution
    let known_schemas: std::collections::HashMap<String, String> = schema_storage
        .iter()
        .map(|s| (s.name.clone(), s.definition.clone()))
        .collect();
    let struct_definitions = known_schemas.clone();

    let mut property_tokens = Vec::new();
    let mut required_fields = Vec::new();

    if let syn::Fields::Named(fields_named) = &struct_item.fields {
        for field in &fields_named.named {
            // Skip if serde(skip)
            if extract_skip(&field.attrs) {
                continue;
            }

            let rust_field_name = field
                .ident
                .as_ref()
                .map(|i| strip_raw_prefix(&i.to_string()).to_string())
                .unwrap_or_else(|| "unknown".to_string());

            // Apply rename
            let field_name = if let Some(renamed) = extract_field_rename(&field.attrs) {
                renamed
            } else {
                rename_field(&rust_field_name, rename_all.as_deref())
            };

            // Apply omit filter (check both rust name and json name)
            if !omit_set.is_empty()
                && (omit_set.contains(&rust_field_name) || omit_set.contains(&field_name))
            {
                continue;
            }

            // Apply pick filter (check both rust name and json name)
            if !pick_set.is_empty()
                && !pick_set.contains(&rust_field_name)
                && !pick_set.contains(&field_name)
            {
                continue;
            }

            let field_type = &field.ty;

            // Generate schema for field type
            let schema_ref =
                parse_type_to_schema_ref(field_type, &known_schemas, &struct_definitions);
            let schema_ref_tokens = schema_ref_to_tokens(&schema_ref);

            property_tokens.push(quote! {
                properties.insert(#field_name.to_string(), #schema_ref_tokens);
            });

            // Check if field is required (not Option, no default, no skip_serializing_if)
            let has_default = extract_default(&field.attrs).is_some();
            let has_skip_serializing_if = extract_skip_serializing_if(&field.attrs);
            let is_optional = is_option_type(field_type);

            if !is_optional && !has_default && !has_skip_serializing_if {
                required_fields.push(field_name.clone());
            }
        }
    }

    let required_tokens = if required_fields.is_empty() {
        quote! { None }
    } else {
        let required_strs: Vec<&str> = required_fields.iter().map(|s| s.as_str()).collect();
        quote! { Some(vec![#(#required_strs.to_string()),*]) }
    };

    Ok(quote! {
        {
            let mut properties = std::collections::BTreeMap::new();
            #(#property_tokens)*
            vespera::schema::Schema {
                schema_type: Some(vespera::schema::SchemaType::Object),
                properties: if properties.is_empty() { None } else { Some(properties) },
                required: #required_tokens,
                ..vespera::schema::Schema::new(vespera::schema::SchemaType::Object)
            }
        }
    })
}

/// Convert SchemaRef to TokenStream for code generation
pub fn schema_ref_to_tokens(schema_ref: &SchemaRef) -> TokenStream {
    match schema_ref {
        SchemaRef::Ref(reference) => {
            let ref_path = &reference.ref_path;
            quote! {
                vespera::schema::SchemaRef::Ref(vespera::schema::Reference::new(#ref_path.to_string()))
            }
        }
        SchemaRef::Inline(schema) => {
            let schema_tokens = schema_to_tokens(schema);
            quote! {
                vespera::schema::SchemaRef::Inline(Box::new(#schema_tokens))
            }
        }
    }
}

/// Convert Schema to TokenStream for code generation
pub fn schema_to_tokens(schema: &Schema) -> TokenStream {
    let schema_type_tokens = match &schema.schema_type {
        Some(SchemaType::String) => quote! { Some(vespera::schema::SchemaType::String) },
        Some(SchemaType::Number) => quote! { Some(vespera::schema::SchemaType::Number) },
        Some(SchemaType::Integer) => quote! { Some(vespera::schema::SchemaType::Integer) },
        Some(SchemaType::Boolean) => quote! { Some(vespera::schema::SchemaType::Boolean) },
        Some(SchemaType::Array) => quote! { Some(vespera::schema::SchemaType::Array) },
        Some(SchemaType::Object) => quote! { Some(vespera::schema::SchemaType::Object) },
        Some(SchemaType::Null) => quote! { Some(vespera::schema::SchemaType::Null) },
        None => quote! { None },
    };

    let format_tokens = match &schema.format {
        Some(f) => quote! { Some(#f.to_string()) },
        None => quote! { None },
    };

    let nullable_tokens = match schema.nullable {
        Some(true) => quote! { Some(true) },
        Some(false) => quote! { Some(false) },
        None => quote! { None },
    };

    let ref_path_tokens = match &schema.ref_path {
        Some(rp) => quote! { Some(#rp.to_string()) },
        None => quote! { None },
    };

    let items_tokens = match &schema.items {
        Some(items) => {
            let inner = schema_ref_to_tokens(items);
            quote! { Some(Box::new(#inner)) }
        }
        None => quote! { None },
    };

    let properties_tokens = match &schema.properties {
        Some(props) => {
            let entries: Vec<_> = props
                .iter()
                .map(|(k, v)| {
                    let v_tokens = schema_ref_to_tokens(v);
                    quote! { (#k.to_string(), #v_tokens) }
                })
                .collect();
            quote! {
                Some({
                    let mut map = std::collections::BTreeMap::new();
                    #(map.insert(#entries.0, #entries.1);)*
                    map
                })
            }
        }
        None => quote! { None },
    };

    let required_tokens = match &schema.required {
        Some(req) => {
            let req_strs: Vec<_> = req.iter().map(|s| s.as_str()).collect();
            quote! { Some(vec![#(#req_strs.to_string()),*]) }
        }
        None => quote! { None },
    };

    quote! {
        vespera::schema::Schema {
            ref_path: #ref_path_tokens,
            schema_type: #schema_type_tokens,
            format: #format_tokens,
            nullable: #nullable_tokens,
            items: #items_tokens,
            properties: #properties_tokens,
            required: #required_tokens,
            ..vespera::schema::Schema::new(vespera::schema::SchemaType::Object)
        }
    }
}
