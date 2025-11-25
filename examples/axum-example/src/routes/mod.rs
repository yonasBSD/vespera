use std::collections::HashMap;

use serde::Deserialize;
use vespera::{
    Schema,
    axum::{Json, extract::Query},
};

pub mod health;
pub mod path;
pub mod users;

/// Health check endpoint
#[vespera::route(get)]
pub async fn root_endpoint() -> &'static str {
    "root endpoint"
}

#[vespera::route(get, path = "/hello")]
pub async fn mod_file_endpoint() -> &'static str {
    "mod file endpoint"
}

#[vespera::route(get, path = "/map-query")]
pub async fn mod_file_with_map_query(
    Query(_query): Query<HashMap<String, String>>,
) -> &'static str {
    "mod file endpoint"
}

#[derive(Deserialize, Schema)]
pub struct StructQuery {
    pub name: String,
    pub age: u32,
}

#[vespera::route(get, path = "/struct-query")]
pub async fn mod_file_with_struct_query(Query(query): Query<StructQuery>) -> String {
    format!("name: {}, age: {}", query.name, query.age)
}

#[derive(Deserialize, Schema)]
pub struct StructBody {
    pub name: String,
    pub age: u32,
}

#[vespera::route(post, path = "/struct-body")]
pub async fn mod_file_with_struct_body(Json(body): Json<StructBody>) -> String {
    format!("name: {}, age: {}", body.name, body.age)
}

#[derive(Deserialize, Schema)]
pub struct StructBodyWithOptional {
    pub name: Option<String>,
    pub age: Option<u32>,
}

#[vespera::route(post, path = "/struct-body-with-optional")]
pub async fn mod_file_with_struct_body_with_optional(
    Json(body): Json<StructBodyWithOptional>,
) -> String {
    format!("name: {:?}, age: {:?}", body.name, body.age)
}
