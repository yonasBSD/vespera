//! Parser module for analyzing function signatures and converting to OpenAPI structures

use std::collections::{BTreeMap, HashMap};
use syn::{Fields, FnArg, Pat, PatType, ReturnType, Type};
use vespera_core::{
    route::{MediaType, Operation, Parameter, ParameterLocation, RequestBody, Response},
    schema::{Reference, Schema, SchemaRef, SchemaType},
};

/// Extract path parameters from a path string
pub fn extract_path_parameters(path: &str) -> Vec<String> {
    let mut params = Vec::new();
    let segments: Vec<&str> = path.split('/').collect();

    for segment in segments {
        if segment.starts_with('{') && segment.ends_with('}') {
            let param = segment.trim_start_matches('{').trim_end_matches('}');
            params.push(param.to_string());
        } else if segment.starts_with(':') {
            let param = segment.trim_start_matches(':');
            params.push(param.to_string());
        }
    }

    params
}

/// Analyze function parameter and convert to OpenAPI Parameter
pub fn parse_function_parameter(
    arg: &FnArg,
    path_params: &[String],
    known_schemas: &HashMap<String, String>,
) -> Option<Parameter> {
    match arg {
        FnArg::Receiver(_) => None,
        FnArg::Typed(PatType { pat, ty, .. }) => {
            // Extract parameter name from pattern
            let param_name = match pat.as_ref() {
                Pat::Ident(ident) => ident.ident.to_string(),
                Pat::TupleStruct(tuple_struct) => {
                    // Handle Path(id) pattern
                    if tuple_struct.elems.len() == 1 {
                        match &tuple_struct.elems[0] {
                            Pat::Ident(ident) => ident.ident.to_string(),
                            _ => return None,
                        }
                    } else {
                        return None;
                    }
                }
                _ => return None,
            };

            // Check for common Axum extractors first (before checking path_params)
            if let Type::Path(type_path) = ty.as_ref() {
                let path = &type_path.path;
                if !path.segments.is_empty() {
                    let segment = &path.segments[0];
                    let ident_str = segment.ident.to_string();

                    match ident_str.as_str() {
                        "Path" => {
                            // Path<T> extractor - use path parameter name from route if available
                            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments
                                && let Some(syn::GenericArgument::Type(inner_ty)) =
                                    args.args.first()
                            {
                                // If there's exactly one path parameter, use its name
                                let name = if path_params.len() == 1 {
                                    path_params[0].clone()
                                } else {
                                    // Otherwise use the parameter name from the pattern
                                    param_name
                                };
                                return Some(Parameter {
                                    name,
                                    r#in: ParameterLocation::Path,
                                    description: None,
                                    required: Some(true),
                                    schema: Some(parse_type_to_schema_ref(inner_ty, known_schemas)),
                                    example: None,
                                });
                            }
                        }
                        "Query" => {
                            // Query<T> extractor
                            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments
                                && let Some(syn::GenericArgument::Type(inner_ty)) =
                                    args.args.first()
                            {
                                return Some(Parameter {
                                    name: param_name.clone(),
                                    r#in: ParameterLocation::Query,
                                    description: None,
                                    required: Some(true),
                                    schema: Some(parse_type_to_schema_ref(inner_ty, known_schemas)),
                                    example: None,
                                });
                            }
                        }
                        "Header" => {
                            // Header<T> extractor
                            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments
                                && let Some(syn::GenericArgument::Type(inner_ty)) =
                                    args.args.first()
                            {
                                return Some(Parameter {
                                    name: param_name.clone(),
                                    r#in: ParameterLocation::Header,
                                    description: None,
                                    required: Some(true),
                                    schema: Some(parse_type_to_schema_ref(inner_ty, known_schemas)),
                                    example: None,
                                });
                            }
                        }
                        "Json" => {
                            // Json<T> extractor - this will be handled as RequestBody
                            return None;
                        }
                        _ => {}
                    }
                }
            }

            // Check if it's a path parameter (by name match) - for non-extractor cases
            if path_params.contains(&param_name) {
                return Some(Parameter {
                    name: param_name.clone(),
                    r#in: ParameterLocation::Path,
                    description: None,
                    required: Some(true),
                    schema: Some(parse_type_to_schema_ref(ty, known_schemas)),
                    example: None,
                });
            }

            // Check if it's a primitive type (direct parameter)
            if is_primitive_type(ty.as_ref()) {
                return Some(Parameter {
                    name: param_name.clone(),
                    r#in: ParameterLocation::Query,
                    description: None,
                    required: Some(true),
                    schema: Some(parse_type_to_schema_ref(ty, known_schemas)),
                    example: None,
                });
            }

            None
        }
    }
}

/// Check if a type is a primitive type
fn is_primitive_type(ty: &Type) -> bool {
    match ty {
        Type::Path(type_path) => {
            let path = &type_path.path;
            if path.segments.len() == 1 {
                let ident = path.segments[0].ident.to_string();
                matches!(
                    ident.as_str(),
                    "i8" | "i16"
                        | "i32"
                        | "i64"
                        | "u8"
                        | "u16"
                        | "u32"
                        | "u64"
                        | "f32"
                        | "f64"
                        | "bool"
                        | "String"
                        | "str"
                )
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Parse struct definition to OpenAPI Schema
pub fn parse_struct_to_schema(
    struct_item: &syn::ItemStruct,
    known_schemas: &HashMap<String, String>,
) -> Schema {
    let mut properties = BTreeMap::new();
    let mut required = Vec::new();

    match &struct_item.fields {
        Fields::Named(fields_named) => {
            for field in &fields_named.named {
                let field_name = field
                    .ident
                    .as_ref()
                    .map(|i| i.to_string())
                    .unwrap_or_else(|| "unknown".to_string());

                let field_type = &field.ty;
                let schema_ref = parse_type_to_schema_ref(field_type, known_schemas);

                properties.insert(field_name.clone(), schema_ref);

                // Check if field is Option<T>
                let is_optional = matches!(
                    field_type,
                    Type::Path(type_path)
                        if type_path
                            .path
                            .segments
                            .first()
                            .map(|s| s.ident == "Option")
                            .unwrap_or(false)
                );

                if !is_optional {
                    required.push(field_name);
                }
            }
        }
        Fields::Unnamed(_) => {
            // Tuple structs are not supported for now
        }
        Fields::Unit => {
            // Unit structs have no fields
        }
    }

    Schema {
        schema_type: Some(SchemaType::Object),
        properties: if properties.is_empty() {
            None
        } else {
            Some(properties)
        },
        required: if required.is_empty() {
            None
        } else {
            Some(required)
        },
        ..Schema::object()
    }
}

/// Parse Rust type to OpenAPI SchemaRef
pub fn parse_type_to_schema_ref(ty: &Type, known_schemas: &HashMap<String, String>) -> SchemaRef {
    match ty {
        Type::Path(type_path) => {
            let path = &type_path.path;
            if path.segments.is_empty() {
                return SchemaRef::Inline(Box::new(Schema::new(SchemaType::Object)));
            }

            let segment = &path.segments[0];
            let ident_str = segment.ident.to_string();

            // Handle generic types
            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                match ident_str.as_str() {
                    "Vec" | "Option" => {
                        if let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first() {
                            let inner_schema = parse_type_to_schema_ref(inner_ty, known_schemas);
                            if ident_str == "Vec" {
                                return SchemaRef::Inline(Box::new(Schema::array(inner_schema)));
                            } else {
                                // Option<T> -> nullable schema
                                if let SchemaRef::Inline(mut schema) = inner_schema {
                                    schema.nullable = Some(true);
                                    return SchemaRef::Inline(schema);
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            // Handle primitive types
            match ident_str.as_str() {
                "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" => {
                    SchemaRef::Inline(Box::new(Schema::integer()))
                }
                "f32" | "f64" => SchemaRef::Inline(Box::new(Schema::number())),
                "bool" => SchemaRef::Inline(Box::new(Schema::boolean())),
                "String" | "str" => SchemaRef::Inline(Box::new(Schema::string())),
                // Standard library types that should not be referenced
                "HashMap" | "BTreeMap" | "Vec" | "Option" | "Result" | "Json" | "Path"
                | "Query" | "Header" => {
                    // These are not schema types, return object schema
                    SchemaRef::Inline(Box::new(Schema::new(SchemaType::Object)))
                }
                _ => {
                    // Check if this is a known schema (struct with Schema derive)
                    if known_schemas.contains_key(&ident_str) {
                        SchemaRef::Ref(Reference::schema(&ident_str))
                    } else {
                        // For unknown custom types, return object schema instead of reference
                        // This prevents creating invalid references to non-existent schemas
                        SchemaRef::Inline(Box::new(Schema::new(SchemaType::Object)))
                    }
                }
            }
        }
        Type::Reference(type_ref) => {
            // Handle &T, &mut T, etc.
            parse_type_to_schema_ref(&type_ref.elem, known_schemas)
        }
        _ => SchemaRef::Inline(Box::new(Schema::new(SchemaType::Object))),
    }
}

/// Analyze function signature and extract RequestBody
pub fn parse_request_body(
    arg: &FnArg,
    known_schemas: &HashMap<String, String>,
) -> Option<RequestBody> {
    match arg {
        FnArg::Receiver(_) => None,
        FnArg::Typed(PatType { ty, .. }) => {
            if let Type::Path(type_path) = ty.as_ref() {
                let path = &type_path.path;
                if path.segments.is_empty() {
                    return None;
                }

                let segment = &path.segments[0];
                let ident_str = segment.ident.to_string();

                if ident_str == "Json"
                    && let syn::PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    let schema = parse_type_to_schema_ref(inner_ty, known_schemas);
                    let mut content = BTreeMap::new();
                    content.insert(
                        "application/json".to_string(),
                        MediaType {
                            schema: Some(schema),
                            example: None,
                            examples: None,
                        },
                    );
                    return Some(RequestBody {
                        description: None,
                        required: Some(true),
                        content,
                    });
                }
            }
            None
        }
    }
}

/// Analyze return type and convert to Response
pub fn parse_return_type(
    return_type: &ReturnType,
    known_schemas: &HashMap<String, String>,
) -> Response {
    let schema = match return_type {
        ReturnType::Default => None,
        ReturnType::Type(_, ty) => Some(parse_type_to_schema_ref(ty, known_schemas)),
    };

    let mut content = BTreeMap::new();
    if let Some(schema) = schema {
        content.insert(
            "application/json".to_string(),
            MediaType {
                schema: Some(schema),
                example: None,
                examples: None,
            },
        );
    }

    Response {
        description: "Successful response".to_string(),
        headers: None,
        content: if content.is_empty() {
            None
        } else {
            Some(content)
        },
    }
}

/// Build Operation from function signature
pub fn build_operation_from_function(
    sig: &syn::Signature,
    path: &str,
    known_schemas: &HashMap<String, String>,
) -> Operation {
    let path_params = extract_path_parameters(path);
    let mut parameters = Vec::new();
    let mut request_body = None;

    // Parse function parameters
    for input in &sig.inputs {
        // Check if it's a request body (Json<T>)
        if let Some(body) = parse_request_body(input, known_schemas) {
            request_body = Some(body);
        } else if let Some(param) = parse_function_parameter(input, &path_params, known_schemas) {
            parameters.push(param);
        }
    }

    // Parse return type
    let response = parse_return_type(&sig.output, known_schemas);
    let mut responses = BTreeMap::new();
    responses.insert("200".to_string(), response);

    Operation {
        operation_id: Some(sig.ident.to_string()),
        tags: None,
        summary: None,
        description: None,
        parameters: if parameters.is_empty() {
            None
        } else {
            Some(parameters)
        },
        request_body,
        responses,
        security: None,
    }
}
