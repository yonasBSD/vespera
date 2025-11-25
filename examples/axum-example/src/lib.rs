mod routes;

use vespera::{axum, vespera};

/// Create the application router for testing
pub fn create_app() -> axum::Router {
    vespera!()
}
