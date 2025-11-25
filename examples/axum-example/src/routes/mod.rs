pub mod health;
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
