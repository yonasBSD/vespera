//! Tests for schema macro module

use super::codegen::{schema_ref_to_tokens, schema_to_tokens};
use super::input::{PartialMode, SchemaInput, SchemaTypeInput};
use super::type_utils::{
    extract_type_name, is_option_type, is_qualified_path, is_seaorm_model, is_seaorm_relation_type,
};
use super::{generate_schema_code, generate_schema_type_code};
use crate::metadata::StructMetadata;

#[test]
fn test_parse_schema_input_simple() {
    let tokens = quote::quote!(User);
    let input: SchemaInput = syn::parse2(tokens).unwrap();
    assert!(input.omit.is_none());
    assert!(input.pick.is_none());
}

#[test]
fn test_parse_schema_input_with_omit() {
    let tokens = quote::quote!(User, omit = ["password", "secret"]);
    let input: SchemaInput = syn::parse2(tokens).unwrap();
    let omit = input.omit.unwrap();
    assert_eq!(omit, vec!["password", "secret"]);
}

#[test]
fn test_parse_schema_input_with_pick() {
    let tokens = quote::quote!(User, pick = ["id", "name"]);
    let input: SchemaInput = syn::parse2(tokens).unwrap();
    let pick = input.pick.unwrap();
    assert_eq!(pick, vec!["id", "name"]);
}

#[test]
fn test_parse_schema_input_omit_and_pick_error() {
    let tokens = quote::quote!(User, omit = ["a"], pick = ["b"]);
    let result: syn::Result<SchemaInput> = syn::parse2(tokens);
    assert!(result.is_err());
}

// schema_type! tests

#[test]
fn test_parse_schema_type_input_simple() {
    let tokens = quote::quote!(CreateUser from User);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.new_type.to_string(), "CreateUser");
    assert!(input.omit.is_none());
    assert!(input.pick.is_none());
    assert!(input.rename.is_none());
    assert!(input.derive_clone);
}

#[test]
fn test_parse_schema_type_input_with_pick() {
    let tokens = quote::quote!(CreateUser from User, pick = ["name", "email"]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.new_type.to_string(), "CreateUser");
    let pick = input.pick.unwrap();
    assert_eq!(pick, vec!["name", "email"]);
}

#[test]
fn test_parse_schema_type_input_with_rename() {
    let tokens =
        quote::quote!(UserDTO from User, rename = [("id", "user_id"), ("name", "full_name")]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.new_type.to_string(), "UserDTO");
    let rename = input.rename.unwrap();
    assert_eq!(rename.len(), 2);
    assert_eq!(rename[0], ("id".to_string(), "user_id".to_string()));
    assert_eq!(rename[1], ("name".to_string(), "full_name".to_string()));
}

#[test]
fn test_parse_schema_type_input_with_single_rename() {
    let tokens = quote::quote!(UserDTO from User, rename = [("id", "user_id")]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let rename = input.rename.unwrap();
    assert_eq!(rename.len(), 1);
    assert_eq!(rename[0], ("id".to_string(), "user_id".to_string()));
}

#[test]
fn test_parse_schema_type_input_with_pick_and_rename() {
    let tokens =
        quote::quote!(UserDTO from User, pick = ["id", "name"], rename = [("id", "user_id")]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.pick.unwrap(), vec!["id", "name"]);
    assert_eq!(
        input.rename.unwrap(),
        vec![("id".to_string(), "user_id".to_string())]
    );
}

#[test]
fn test_parse_schema_type_input_with_omit_and_rename() {
    let tokens =
        quote::quote!(UserPublic from User, omit = ["password"], rename = [("id", "user_id")]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.omit.unwrap(), vec!["password"]);
    assert_eq!(
        input.rename.unwrap(),
        vec![("id".to_string(), "user_id".to_string())]
    );
}

#[test]
fn test_parse_schema_type_input_with_clone_false() {
    let tokens = quote::quote!(NonCloneUser from User, clone = false);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert!(!input.derive_clone);
}

#[test]
fn test_parse_schema_type_input_unknown_param_error() {
    let tokens = quote::quote!(UserDTO from User, unknown = ["a"]);
    let result: syn::Result<SchemaTypeInput> = syn::parse2(tokens);
    assert!(result.is_err());
    // Note: Can't use unwrap_err() because SchemaTypeInput doesn't impl Debug (contains syn::Type)
    match result {
        Err(e) => assert!(e.to_string().contains("unknown parameter")),
        Ok(_) => panic!("Expected error"),
    }
}

// Tests for `add` parameter

#[test]
fn test_parse_schema_type_input_with_add_single() {
    let tokens = quote::quote!(UserWithTimestamp from User, add = [("created_at": String)]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.new_type.to_string(), "UserWithTimestamp");
    let add = input.add.unwrap();
    assert_eq!(add.len(), 1);
    assert_eq!(add[0].0, "created_at");
}

#[test]
fn test_parse_schema_type_input_with_add_multiple() {
    let tokens = quote::quote!(UserWithMeta from User, add = [("created_at": String), ("updated_at": Option<String>)]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let add = input.add.unwrap();
    assert_eq!(add.len(), 2);
    assert_eq!(add[0].0, "created_at");
    assert_eq!(add[1].0, "updated_at");
}

#[test]
fn test_parse_schema_type_input_with_pick_and_add() {
    let tokens = quote::quote!(CreateUserWithMeta from User, pick = ["name", "email"], add = [("request_id": String)]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.pick.unwrap(), vec!["name", "email"]);
    let add = input.add.unwrap();
    assert_eq!(add.len(), 1);
    assert_eq!(add[0].0, "request_id");
}

#[test]
fn test_parse_schema_type_input_with_omit_and_add() {
    let tokens = quote::quote!(UserPublicWithMeta from User, omit = ["password"], add = [("display_name": String)]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.omit.unwrap(), vec!["password"]);
    let add = input.add.unwrap();
    assert_eq!(add.len(), 1);
    assert_eq!(add[0].0, "display_name");
}

#[test]
fn test_parse_schema_type_input_with_add_complex_type() {
    let tokens = quote::quote!(UserWithVec from User, add = [("tags": Vec<String>)]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let add = input.add.unwrap();
    assert_eq!(add.len(), 1);
    assert_eq!(add[0].0, "tags");
}

// Tests for `partial` parameter

#[test]
fn test_parse_schema_type_input_with_partial_all() {
    let tokens = quote::quote!(UpdateUser from User, partial);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert!(matches!(input.partial, Some(PartialMode::All)));
}

#[test]
fn test_parse_schema_type_input_with_partial_fields() {
    let tokens = quote::quote!(UpdateUser from User, partial = ["name", "email"]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    match input.partial {
        Some(PartialMode::Fields(fields)) => {
            assert_eq!(fields, vec!["name", "email"]);
        }
        _ => panic!("Expected PartialMode::Fields"),
    }
}

#[test]
fn test_parse_schema_type_input_with_pick_and_partial() {
    let tokens = quote::quote!(UpdateUser from User, pick = ["name", "email"], partial);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.pick.unwrap(), vec!["name", "email"]);
    assert!(matches!(input.partial, Some(PartialMode::All)));
}

#[test]
fn test_parse_schema_type_input_with_pick_and_partial_fields() {
    let tokens = quote::quote!(UpdateUser from User, pick = ["name", "email"], partial = ["name"]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.pick.unwrap(), vec!["name", "email"]);
    match input.partial {
        Some(PartialMode::Fields(fields)) => {
            assert_eq!(fields, vec!["name"]);
        }
        _ => panic!("Expected PartialMode::Fields"),
    }
}

#[test]
fn test_generate_schema_type_code_with_partial_all() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String, pub bio: Option<String> }",
    )];

    let tokens = quote::quote!(UpdateUser from User, partial);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_type_code(&input, &storage);

    assert!(result.is_ok());
    let (tokens, _metadata) = result.unwrap();
    let output = tokens.to_string();
    // id and name should be wrapped in Option, bio already Option stays unchanged
    assert!(output.contains("Option < i32 >"));
    assert!(output.contains("Option < String >"));
}

#[test]
fn test_generate_schema_type_code_with_partial_fields() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String, pub email: String }",
    )];

    let tokens = quote::quote!(UpdateUser from User, partial = ["name"]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_type_code(&input, &storage);

    assert!(result.is_ok());
    let (tokens, _metadata) = result.unwrap();
    let output = tokens.to_string();
    // name should be Option<String>, but id and email should remain unwrapped
    assert!(output.contains("UpdateUser"));
}

#[test]
fn test_generate_schema_type_code_partial_nonexistent_field() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String }",
    )];

    let tokens = quote::quote!(UpdateUser from User, partial = ["nonexistent"]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_type_code(&input, &storage);

    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("does not exist"));
    assert!(err.contains("nonexistent"));
}

#[test]
fn test_generate_schema_type_code_partial_from_impl_wraps_some() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String }",
    )];

    let tokens = quote::quote!(UpdateUser from User, partial);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_type_code(&input, &storage);

    assert!(result.is_ok());
    let (tokens, _metadata) = result.unwrap();
    let output = tokens.to_string();
    // From impl should wrap values in Some()
    assert!(output.contains("Some (source . id)"));
    assert!(output.contains("Some (source . name)"));
}

// =========================================================================
// Tests for generate_schema_code() - success paths
// =========================================================================

fn create_test_struct_metadata(name: &str, definition: &str) -> StructMetadata {
    StructMetadata::new(name.to_string(), definition.to_string())
}

#[test]
fn test_generate_schema_code_simple_struct() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String }",
    )];

    let tokens = quote::quote!(User);
    let input: SchemaInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_code(&input, &storage);

    assert!(result.is_ok());
    let output = result.unwrap().to_string();
    assert!(output.contains("properties"));
    assert!(output.contains("Schema"));
}

#[test]
fn test_generate_schema_code_with_omit() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String, pub password: String }",
    )];

    let tokens = quote::quote!(User, omit = ["password"]);
    let input: SchemaInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_code(&input, &storage);

    assert!(result.is_ok());
    let output = result.unwrap().to_string();
    // Should have id and name but not password in properties
    assert!(output.contains("properties"));
}

#[test]
fn test_generate_schema_code_with_pick() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String, pub email: String }",
    )];

    let tokens = quote::quote!(User, pick = ["id", "name"]);
    let input: SchemaInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_code(&input, &storage);

    assert!(result.is_ok());
    let output = result.unwrap().to_string();
    assert!(output.contains("properties"));
}

// =========================================================================
// Tests for generate_schema_code() - error paths
// =========================================================================

#[test]
fn test_generate_schema_code_type_not_found() {
    let storage: Vec<StructMetadata> = vec![];

    let tokens = quote::quote!(NonExistent);
    let input: SchemaInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_code(&input, &storage);

    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("not found"));
}

#[test]
fn test_generate_schema_code_malformed_definition() {
    let storage = vec![create_test_struct_metadata(
        "BadStruct",
        "this is not valid rust code {{{",
    )];

    let tokens = quote::quote!(BadStruct);
    let input: SchemaInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_code(&input, &storage);

    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("failed to parse"));
}

// =========================================================================
// Tests for schema_ref_to_tokens()
// =========================================================================

#[test]
fn test_schema_ref_to_tokens_ref_variant() {
    use vespera_core::schema::{Reference, SchemaRef};

    let schema_ref = SchemaRef::Ref(Reference::new("#/components/schemas/User".to_string()));
    let tokens = schema_ref_to_tokens(&schema_ref);
    let output = tokens.to_string();

    assert!(output.contains("SchemaRef :: Ref"));
    assert!(output.contains("Reference :: new"));
}

#[test]
fn test_schema_ref_to_tokens_inline_variant() {
    use vespera_core::schema::{Schema, SchemaRef, SchemaType};

    let schema = Schema::new(SchemaType::String);
    let schema_ref = SchemaRef::Inline(Box::new(schema));
    let tokens = schema_ref_to_tokens(&schema_ref);
    let output = tokens.to_string();

    assert!(output.contains("SchemaRef :: Inline"));
    assert!(output.contains("Box :: new"));
}

// =========================================================================
// Tests for schema_to_tokens()
// =========================================================================

#[test]
fn test_schema_to_tokens_string_type() {
    use vespera_core::schema::{Schema, SchemaType};

    let schema = Schema::new(SchemaType::String);
    let tokens = schema_to_tokens(&schema);
    let output = tokens.to_string();

    assert!(output.contains("SchemaType :: String"));
}

#[test]
fn test_schema_to_tokens_integer_type() {
    use vespera_core::schema::{Schema, SchemaType};

    let schema = Schema::new(SchemaType::Integer);
    let tokens = schema_to_tokens(&schema);
    let output = tokens.to_string();

    assert!(output.contains("SchemaType :: Integer"));
}

#[test]
fn test_schema_to_tokens_number_type() {
    use vespera_core::schema::{Schema, SchemaType};

    let schema = Schema::new(SchemaType::Number);
    let tokens = schema_to_tokens(&schema);
    let output = tokens.to_string();

    assert!(output.contains("SchemaType :: Number"));
}

#[test]
fn test_schema_to_tokens_boolean_type() {
    use vespera_core::schema::{Schema, SchemaType};

    let schema = Schema::new(SchemaType::Boolean);
    let tokens = schema_to_tokens(&schema);
    let output = tokens.to_string();

    assert!(output.contains("SchemaType :: Boolean"));
}

#[test]
fn test_schema_to_tokens_array_type() {
    use vespera_core::schema::{Schema, SchemaType};

    let schema = Schema::new(SchemaType::Array);
    let tokens = schema_to_tokens(&schema);
    let output = tokens.to_string();

    assert!(output.contains("SchemaType :: Array"));
}

#[test]
fn test_schema_to_tokens_object_type() {
    use vespera_core::schema::{Schema, SchemaType};

    let schema = Schema::new(SchemaType::Object);
    let tokens = schema_to_tokens(&schema);
    let output = tokens.to_string();

    assert!(output.contains("SchemaType :: Object"));
}

#[test]
fn test_schema_to_tokens_null_type() {
    use vespera_core::schema::{Schema, SchemaType};

    let schema = Schema::new(SchemaType::Null);
    let tokens = schema_to_tokens(&schema);
    let output = tokens.to_string();

    assert!(output.contains("SchemaType :: Null"));
}

#[test]
fn test_schema_to_tokens_with_format() {
    use vespera_core::schema::{Schema, SchemaType};

    let mut schema = Schema::new(SchemaType::String);
    schema.format = Some("date-time".to_string());
    let tokens = schema_to_tokens(&schema);
    let output = tokens.to_string();

    assert!(output.contains("date-time"));
}

#[test]
fn test_schema_to_tokens_with_nullable() {
    use vespera_core::schema::{Schema, SchemaType};

    let mut schema = Schema::new(SchemaType::String);
    schema.nullable = Some(true);
    let tokens = schema_to_tokens(&schema);
    let output = tokens.to_string();

    assert!(output.contains("Some (true)"));
}

#[test]
fn test_schema_to_tokens_with_ref_path() {
    use vespera_core::schema::{Schema, SchemaType};

    let mut schema = Schema::new(SchemaType::Object);
    schema.ref_path = Some("#/components/schemas/User".to_string());
    let tokens = schema_to_tokens(&schema);
    let output = tokens.to_string();

    assert!(output.contains("#/components/schemas/User"));
}

#[test]
fn test_schema_to_tokens_with_items() {
    use vespera_core::schema::{Schema, SchemaRef, SchemaType};

    let mut schema = Schema::new(SchemaType::Array);
    let item_schema = Schema::new(SchemaType::String);
    schema.items = Some(Box::new(SchemaRef::Inline(Box::new(item_schema))));
    let tokens = schema_to_tokens(&schema);
    let output = tokens.to_string();

    assert!(output.contains("items"));
    assert!(output.contains("Some (Box :: new"));
}

#[test]
fn test_schema_to_tokens_with_properties() {
    use std::collections::BTreeMap;
    use vespera_core::schema::{Schema, SchemaRef, SchemaType};

    let mut schema = Schema::new(SchemaType::Object);
    let mut props = BTreeMap::new();
    props.insert(
        "name".to_string(),
        SchemaRef::Inline(Box::new(Schema::new(SchemaType::String))),
    );
    schema.properties = Some(props);
    let tokens = schema_to_tokens(&schema);
    let output = tokens.to_string();

    assert!(output.contains("properties"));
    assert!(output.contains("name"));
}

#[test]
fn test_schema_to_tokens_with_required() {
    use vespera_core::schema::{Schema, SchemaType};

    let mut schema = Schema::new(SchemaType::Object);
    schema.required = Some(vec!["id".to_string(), "name".to_string()]);
    let tokens = schema_to_tokens(&schema);
    let output = tokens.to_string();

    assert!(output.contains("required"));
    assert!(output.contains("id"));
    assert!(output.contains("name"));
}

// =========================================================================
// Tests for generate_schema_type_code() - validation errors
// =========================================================================

#[test]
fn test_generate_schema_type_code_pick_nonexistent_field() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String }",
    )];

    let tokens = quote::quote!(NewUser from User, pick = ["nonexistent"]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_type_code(&input, &storage);

    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("does not exist"));
    assert!(err.contains("nonexistent"));
}

#[test]
fn test_generate_schema_type_code_omit_nonexistent_field() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String }",
    )];

    let tokens = quote::quote!(NewUser from User, omit = ["nonexistent"]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_type_code(&input, &storage);

    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("does not exist"));
    assert!(err.contains("nonexistent"));
}

#[test]
fn test_generate_schema_type_code_rename_nonexistent_field() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String }",
    )];

    let tokens = quote::quote!(NewUser from User, rename = [("nonexistent", "new_name")]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_type_code(&input, &storage);

    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("does not exist"));
    assert!(err.contains("nonexistent"));
}

#[test]
fn test_generate_schema_type_code_type_not_found() {
    let storage: Vec<StructMetadata> = vec![];

    let tokens = quote::quote!(NewUser from NonExistent);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_type_code(&input, &storage);

    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("not found"));
}

#[test]
fn test_generate_schema_type_code_success() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String }",
    )];

    let tokens = quote::quote!(CreateUser from User, pick = ["name"]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_type_code(&input, &storage);

    assert!(result.is_ok());
    let (tokens, _metadata) = result.unwrap();
    let output = tokens.to_string();
    assert!(output.contains("CreateUser"));
    assert!(output.contains("name"));
}

#[test]
fn test_generate_schema_type_code_with_omit() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String, pub password: String }",
    )];

    let tokens = quote::quote!(SafeUser from User, omit = ["password"]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_type_code(&input, &storage);

    assert!(result.is_ok());
    let (tokens, _metadata) = result.unwrap();
    let output = tokens.to_string();
    assert!(output.contains("SafeUser"));
    // Should not contain password
    assert!(!output.contains("password"));
}

#[test]
fn test_generate_schema_type_code_with_add() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String }",
    )];

    let tokens = quote::quote!(UserWithExtra from User, add = [("extra": String)]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_type_code(&input, &storage);

    assert!(result.is_ok());
    let (tokens, _metadata) = result.unwrap();
    let output = tokens.to_string();
    assert!(output.contains("UserWithExtra"));
    assert!(output.contains("extra"));
}

#[test]
fn test_generate_schema_type_code_generates_from_impl() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String }",
    )];

    // Without add parameter, should generate From impl
    let tokens = quote::quote!(UserResponse from User, pick = ["id", "name"]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_type_code(&input, &storage);

    assert!(result.is_ok());
    let (tokens, _metadata) = result.unwrap();
    let output = tokens.to_string();
    assert!(output.contains("impl From"));
    assert!(output.contains("for UserResponse"));
}

#[test]
fn test_generate_schema_type_code_no_from_impl_with_add() {
    let storage = vec![create_test_struct_metadata(
        "User",
        "pub struct User { pub id: i32, pub name: String }",
    )];

    // With add parameter, should NOT generate From impl
    let tokens = quote::quote!(UserWithExtra from User, add = [("extra": String)]);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    let result = generate_schema_type_code(&input, &storage);

    assert!(result.is_ok());
    let (tokens, _metadata) = result.unwrap();
    let output = tokens.to_string();
    // Should NOT contain From impl when add is used
    assert!(!output.contains("impl From"));
}

// =========================================================================
// Tests for is_option_type()
// =========================================================================

#[test]
fn test_is_option_type_true() {
    let ty: syn::Type = syn::parse_str("Option<String>").unwrap();
    assert!(is_option_type(&ty));
}

#[test]
fn test_is_option_type_false() {
    let ty: syn::Type = syn::parse_str("String").unwrap();
    assert!(!is_option_type(&ty));
}

#[test]
fn test_is_option_type_vec_false() {
    let ty: syn::Type = syn::parse_str("Vec<String>").unwrap();
    assert!(!is_option_type(&ty));
}

// =========================================================================
// Tests for extract_type_name()
// =========================================================================

#[test]
fn test_extract_type_name_simple() {
    let ty: syn::Type = syn::parse_str("User").unwrap();
    let name = extract_type_name(&ty).unwrap();
    assert_eq!(name, "User");
}

#[test]
fn test_extract_type_name_with_path() {
    let ty: syn::Type = syn::parse_str("crate::models::User").unwrap();
    let name = extract_type_name(&ty).unwrap();
    assert_eq!(name, "User");
}

#[test]
fn test_extract_type_name_non_path_error() {
    // Reference type is not a Type::Path
    let ty: syn::Type = syn::parse_str("&str").unwrap();
    let result = extract_type_name(&ty);
    assert!(result.is_err());
}

// =========================================================================
// Tests for rename_all parsing
// =========================================================================

#[test]
fn test_parse_schema_type_input_with_rename_all() {
    let tokens = quote::quote!(NewType from User, rename_all = "snake_case");
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.rename_all.as_deref(), Some("snake_case"));
}

#[test]
fn test_parse_schema_type_input_rename_all_with_other_params() {
    // rename_all should work alongside other parameters
    let tokens = quote::quote!(NewType from User, pick = ["id", "name"], rename_all = "snake_case");
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.pick.unwrap(), vec!["id", "name"]);
    assert_eq!(input.rename_all.as_deref(), Some("snake_case"));
}

// =========================================================================
// Tests for helper functions
// =========================================================================

#[test]
fn test_is_qualified_path_simple() {
    let ty: syn::Type = syn::parse_str("User").unwrap();
    assert!(!is_qualified_path(&ty));
}

#[test]
fn test_is_qualified_path_crate_path() {
    let ty: syn::Type = syn::parse_str("crate::models::User").unwrap();
    assert!(is_qualified_path(&ty));
}

#[test]
fn test_is_qualified_path_non_path_type() {
    let ty: syn::Type = syn::parse_str("&str").unwrap();
    assert!(!is_qualified_path(&ty));
}

#[test]
fn test_is_seaorm_relation_type_has_one() {
    let ty: syn::Type = syn::parse_str("HasOne<User>").unwrap();
    assert!(is_seaorm_relation_type(&ty));
}

#[test]
fn test_is_seaorm_relation_type_has_many() {
    let ty: syn::Type = syn::parse_str("HasMany<Post>").unwrap();
    assert!(is_seaorm_relation_type(&ty));
}

#[test]
fn test_is_seaorm_relation_type_belongs_to() {
    let ty: syn::Type = syn::parse_str("BelongsTo<User>").unwrap();
    assert!(is_seaorm_relation_type(&ty));
}

#[test]
fn test_is_seaorm_relation_type_regular_type() {
    let ty: syn::Type = syn::parse_str("String").unwrap();
    assert!(!is_seaorm_relation_type(&ty));
}

#[test]
fn test_is_seaorm_relation_type_non_path() {
    let ty: syn::Type = syn::parse_str("&str").unwrap();
    assert!(!is_seaorm_relation_type(&ty));
}

#[test]
fn test_is_seaorm_model_with_sea_orm_attr() {
    let struct_item: syn::ItemStruct = syn::parse_str(
        r#"
        #[sea_orm(table_name = "users")]
        struct Model {
            id: i32,
        }
    "#,
    )
    .unwrap();
    assert!(is_seaorm_model(&struct_item));
}

#[test]
fn test_is_seaorm_model_with_qualified_attr() {
    let struct_item: syn::ItemStruct = syn::parse_str(
        r#"
        #[sea_orm::model]
        struct Model {
            id: i32,
        }
    "#,
    )
    .unwrap();
    assert!(is_seaorm_model(&struct_item));
}

#[test]
fn test_is_seaorm_model_regular_struct() {
    let struct_item: syn::ItemStruct = syn::parse_str(
        r#"
        #[derive(Debug)]
        struct User {
            id: i32,
        }
    "#,
    )
    .unwrap();
    assert!(!is_seaorm_model(&struct_item));
}

#[test]
fn test_parse_schema_input_trailing_comma() {
    // Test that trailing comma is handled
    let tokens = quote::quote!(User, omit = ["password"],);
    let input: SchemaInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.omit.unwrap(), vec!["password"]);
}

#[test]
fn test_parse_schema_input_unknown_param() {
    let tokens = quote::quote!(User, unknown = ["a"]);
    let result: syn::Result<SchemaInput> = syn::parse2(tokens);
    assert!(result.is_err());
    if let Err(e) = result {
        assert!(e.to_string().contains("unknown parameter"));
    }
}

#[test]
fn test_parse_schema_type_input_with_ignore() {
    let tokens = quote::quote!(NewType from User, ignore);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert!(input.ignore_schema);
}

#[test]
fn test_parse_schema_type_input_with_name() {
    let tokens = quote::quote!(NewType from User, name = "CustomName");
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.schema_name.as_deref(), Some("CustomName"));
}

#[test]
fn test_parse_schema_type_input_with_name_and_ignore() {
    let tokens = quote::quote!(NewType from User, name = "CustomName", ignore);
    let input: SchemaTypeInput = syn::parse2(tokens).unwrap();
    assert_eq!(input.schema_name.as_deref(), Some("CustomName"));
    assert!(input.ignore_schema);
}

// Test doc comment preservation in schema_type
#[test]
fn test_generate_schema_type_code_preserves_struct_doc() {
    let input = SchemaTypeInput {
        new_type: syn::Ident::new("NewUser", proc_macro2::Span::call_site()),
        source_type: syn::parse_str("User").unwrap(),
        omit: None,
        pick: None,
        rename: None,
        add: None,
        derive_clone: true,
        partial: None,
        schema_name: None,
        ignore_schema: false,
        rename_all: None,
    };
    // Create a struct with doc comments
    let struct_def = StructMetadata {
        name: "User".to_string(),
        definition: r#"
            /// User struct documentation
            pub struct User {
                /// The user ID
                pub id: i32,
                /// The user name
                pub name: String,
            }
        "#
        .to_string(),
        include_in_openapi: true,
    };
    let result = generate_schema_type_code(&input, &[struct_def]);
    assert!(result.is_ok());
    let (tokens, _) = result.unwrap();
    let tokens_str = tokens.to_string();
    // Should contain doc comments
    assert!(tokens_str.contains("User struct documentation") || tokens_str.contains("doc"));
}
