//! Input parsing for schema macros
//!
//! Defines input structures for `schema!` and `schema_type!` macros.

use syn::punctuated::Punctuated;
use syn::{bracketed, parenthesized, parse::Parse, parse::ParseStream, Ident, LitStr, Token, Type};

/// Input for the schema! macro
///
/// Supports:
/// - `schema!(Type)` - Full schema
/// - `schema!(Type, omit = ["field1", "field2"])` - Schema with fields omitted
/// - `schema!(Type, pick = ["field1", "field2"])` - Schema with only specified fields (future)
pub struct SchemaInput {
    /// The type to generate schema for
    pub ty: Type,
    /// Fields to omit from the schema
    pub omit: Option<Vec<String>>,
    /// Fields to pick (include only these fields)
    pub pick: Option<Vec<String>>,
}

impl Parse for SchemaInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse the type
        let ty: Type = input.parse()?;

        let mut omit = None;
        let mut pick = None;

        // Parse optional parameters
        while input.peek(Token![,]) {
            input.parse::<Token![,]>()?;

            if input.is_empty() {
                break;
            }

            let ident: Ident = input.parse()?;
            let ident_str = ident.to_string();

            match ident_str.as_str() {
                "omit" => {
                    input.parse::<Token![=]>()?;
                    let content;
                    let _ = bracketed!(content in input);
                    let fields: Punctuated<LitStr, Token![,]> =
                        content.parse_terminated(|input| input.parse::<LitStr>(), Token![,])?;
                    omit = Some(fields.into_iter().map(|s| s.value()).collect());
                }
                "pick" => {
                    input.parse::<Token![=]>()?;
                    let content;
                    let _ = bracketed!(content in input);
                    let fields: Punctuated<LitStr, Token![,]> =
                        content.parse_terminated(|input| input.parse::<LitStr>(), Token![,])?;
                    pick = Some(fields.into_iter().map(|s| s.value()).collect());
                }
                _ => {
                    return Err(syn::Error::new(
                        ident.span(),
                        format!(
                            "unknown parameter: `{}`. Expected `omit` or `pick`",
                            ident_str
                        ),
                    ));
                }
            }
        }

        // Validate: can't use both omit and pick
        if omit.is_some() && pick.is_some() {
            return Err(syn::Error::new(
                input.span(),
                "cannot use both `omit` and `pick` in the same schema! invocation",
            ));
        }

        Ok(SchemaInput { ty, omit, pick })
    }
}

/// Input for the schema_type! macro
///
/// Syntax: `schema_type!(NewTypeName from SourceType, pick = ["field1", "field2"])`
/// Or:     `schema_type!(NewTypeName from SourceType, omit = ["field1", "field2"])`
/// Or:     `schema_type!(NewTypeName from SourceType, rename = [("old", "new")])`
/// Or:     `schema_type!(NewTypeName from SourceType, add = [("field": Type)])`
/// Or:     `schema_type!(NewTypeName from SourceType, ignore)` - skip Schema derive
/// Or:     `schema_type!(NewTypeName from SourceType, name = "CustomName")` - custom OpenAPI name
/// Or:     `schema_type!(NewTypeName from SourceType, rename_all = "camelCase")` - serde rename_all
pub struct SchemaTypeInput {
    /// The new type name to generate
    pub new_type: Ident,
    /// The source type to derive from
    pub source_type: Type,
    /// Fields to omit from the new type
    pub omit: Option<Vec<String>>,
    /// Fields to pick (include only these fields)
    pub pick: Option<Vec<String>>,
    /// Field renames: (source_field_name, new_field_name)
    pub rename: Option<Vec<(String, String)>>,
    /// New fields to add: (field_name, field_type)
    pub add: Option<Vec<(String, Type)>>,
    /// Whether to derive Clone (default: true)
    pub derive_clone: bool,
    /// Fields to wrap in `Option<T>` for partial updates.
    ///
    /// - `partial` (bare) = all fields become `Option<T>`
    /// - `partial = ["field1", "field2"]` = only listed fields become `Option<T>`
    /// - Fields already `Option<T>` are left unchanged.
    pub partial: Option<PartialMode>,
    /// Whether to skip deriving the Schema trait (default: false)
    /// Use `ignore` keyword to set this to true.
    pub ignore_schema: bool,
    /// Custom OpenAPI schema name (overrides Rust struct name)
    /// Use `name = "CustomName"` to set this.
    pub schema_name: Option<String>,
    /// Serde rename_all strategy (e.g., "camelCase", "snake_case", "PascalCase")
    /// If not specified, defaults to "camelCase" when source has no rename_all
    pub rename_all: Option<String>,
}

/// Mode for the `partial` keyword in schema_type!
#[derive(Clone, Debug)]
pub enum PartialMode {
    /// All fields become Option<T>
    All,
    /// Only listed fields become Option<T>
    Fields(Vec<String>),
}

/// Helper struct to parse an add field: ("field_name": Type)
struct AddField {
    name: String,
    ty: Type,
}

impl Parse for AddField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);
        let name: LitStr = content.parse()?;
        content.parse::<Token![:]>()?;
        let ty: Type = content.parse()?;
        Ok(AddField {
            name: name.value(),
            ty,
        })
    }
}

/// Helper struct to parse a rename pair: ("old_name", "new_name")
struct RenamePair {
    from: String,
    to: String,
}

impl Parse for RenamePair {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);
        let from: LitStr = content.parse()?;
        content.parse::<Token![,]>()?;
        let to: LitStr = content.parse()?;
        Ok(RenamePair {
            from: from.value(),
            to: to.value(),
        })
    }
}

impl Parse for SchemaTypeInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse new type name
        let new_type: Ident = input.parse()?;

        // Parse "from" keyword
        let from_ident: Ident = input.parse()?;
        if from_ident != "from" {
            return Err(syn::Error::new(
                from_ident.span(),
                format!("expected `from`, found `{}`", from_ident),
            ));
        }

        // Parse source type
        let source_type: Type = input.parse()?;

        let mut omit = None;
        let mut pick = None;
        let mut rename = None;
        let mut add = None;
        let mut derive_clone = true;
        let mut partial = None;
        let mut ignore_schema = false;
        let mut schema_name = None;
        let mut rename_all = None;

        // Parse optional parameters
        while input.peek(Token![,]) {
            input.parse::<Token![,]>()?;

            if input.is_empty() {
                break;
            }

            let ident: Ident = input.parse()?;
            let ident_str = ident.to_string();

            match ident_str.as_str() {
                "omit" => {
                    input.parse::<Token![=]>()?;
                    let content;
                    let _ = bracketed!(content in input);
                    let fields: Punctuated<LitStr, Token![,]> =
                        content.parse_terminated(|input| input.parse::<LitStr>(), Token![,])?;
                    omit = Some(fields.into_iter().map(|s| s.value()).collect());
                }
                "pick" => {
                    input.parse::<Token![=]>()?;
                    let content;
                    let _ = bracketed!(content in input);
                    let fields: Punctuated<LitStr, Token![,]> =
                        content.parse_terminated(|input| input.parse::<LitStr>(), Token![,])?;
                    pick = Some(fields.into_iter().map(|s| s.value()).collect());
                }
                "rename" => {
                    input.parse::<Token![=]>()?;
                    let content;
                    let _ = bracketed!(content in input);
                    let pairs: Punctuated<RenamePair, Token![,]> =
                        content.parse_terminated(RenamePair::parse, Token![,])?;
                    rename = Some(pairs.into_iter().map(|p| (p.from, p.to)).collect());
                }
                "add" => {
                    input.parse::<Token![=]>()?;
                    let content;
                    let _ = bracketed!(content in input);
                    let fields: Punctuated<AddField, Token![,]> =
                        content.parse_terminated(AddField::parse, Token![,])?;
                    add = Some(fields.into_iter().map(|f| (f.name, f.ty)).collect());
                }
                "clone" => {
                    input.parse::<Token![=]>()?;
                    let value: syn::LitBool = input.parse()?;
                    derive_clone = value.value();
                }
                "partial" => {
                    if input.peek(Token![=]) {
                        // partial = ["field1", "field2"]
                        input.parse::<Token![=]>()?;
                        let content;
                        let _ = bracketed!(content in input);
                        let fields: Punctuated<LitStr, Token![,]> =
                            content.parse_terminated(|input| input.parse::<LitStr>(), Token![,])?;
                        partial = Some(PartialMode::Fields(
                            fields.into_iter().map(|s| s.value()).collect(),
                        ));
                    } else {
                        // bare `partial` - all fields
                        partial = Some(PartialMode::All);
                    }
                }
                "ignore" => {
                    // bare `ignore` - skip Schema derive
                    ignore_schema = true;
                }
                "name" => {
                    // name = "CustomSchemaName" - custom OpenAPI schema name
                    input.parse::<Token![=]>()?;
                    let name_lit: LitStr = input.parse()?;
                    schema_name = Some(name_lit.value());
                }
                "rename_all" => {
                    // rename_all = "camelCase" - serde rename_all strategy
                    // Validation is delegated to serde at compile time
                    input.parse::<Token![=]>()?;
                    let rename_all_lit: LitStr = input.parse()?;
                    rename_all = Some(rename_all_lit.value());
                }
                _ => {
                    return Err(syn::Error::new(
                        ident.span(),
                        format!(
                            "unknown parameter: `{}`. Expected `omit`, `pick`, `rename`, `add`, `clone`, `partial`, `ignore`, `name`, or `rename_all`",
                            ident_str
                        ),
                    ));
                }
            }
        }

        // Validate: can't use both omit and pick
        if omit.is_some() && pick.is_some() {
            return Err(syn::Error::new(
                input.span(),
                "cannot use both `omit` and `pick` in the same schema_type! invocation",
            ));
        }

        Ok(SchemaTypeInput {
            new_type,
            source_type,
            omit,
            pick,
            rename,
            add,
            derive_clone,
            partial,
            ignore_schema,
            schema_name,
            rename_all,
        })
    }
}
