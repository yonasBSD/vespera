#![allow(unsafe_code)]

//! Vespera JNI integration demo.
//!
//! | Mode | Entry | Transport |
//! |------|-------|-----------|
//! | Server | `main.rs` → `axum::serve` | TCP :3000 |
//! | JNI | `jni_app!` → Java calls `VesperaBridge.dispatch()` | In-process |

mod routes;

use vespera::{axum, vespera};

/// Build the application router.
pub fn create_app() -> axum::Router {
    vespera!(
        title = "Document Validation API",
        version = "0.1.0"
    )
}

// Register this app for JNI dispatch — one line, no boilerplate.
vespera::jni_app!(create_app);

// ── Tests ────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use vespera::inprocess::{dispatch, RequestEnvelope};

    use super::*;

    fn req(method: &str, path: &str, body: &str) -> RequestEnvelope {
        RequestEnvelope {
            method: method.into(),
            path: path.into(),
            query: String::new(),
            headers: HashMap::new(),
            body: body.into(),
        }
    }

    fn parse(json: &str) -> serde_json::Value {
        serde_json::from_str(json).unwrap()
    }

    #[tokio::test]
    async fn health_returns_200() {
        let json = dispatch(create_app(), &req("GET", "/health", "")).await;
        let v = parse(&json);
        assert_eq!(v["status"], 200);
        assert!(v["body"].as_str().unwrap().contains("ok"));
        assert!(v["metadata"]["version"].is_string());
    }

    #[tokio::test]
    async fn response_includes_content_type_header() {
        let json = dispatch(create_app(), &req("GET", "/health", "")).await;
        let v = parse(&json);
        assert!(v["headers"]["content-type"].is_string());
    }

    #[tokio::test]
    async fn validate_valid_document() {
        let body = serde_json::json!({
            "documentType": "regulation",
            "title": "Test Policy",
            "content": "This regulation establishes the framework for handling \
                        personal data within the organisation and outlines compliance.",
            "author": "Kim Minjun",
            "department": "Legal",
            "classification": "internal",
            "effectiveDate": "2025-01-01"
        });
        let json = dispatch(create_app(), &req("POST", "/documents/validate", &body.to_string())).await;
        let v = parse(&json);
        assert_eq!(v["status"], 200);
        let inner: serde_json::Value = serde_json::from_str(v["body"].as_str().unwrap()).unwrap();
        assert_eq!(inner["valid"], true);
    }

    #[tokio::test]
    async fn validate_invalid_document() {
        let body = serde_json::json!({
            "documentType": "invalid_type",
            "title": "",
            "content": "",
            "author": "",
            "department": "",
            "classification": "bogus",
            "effectiveDate": "not-a-date"
        });
        let json = dispatch(create_app(), &req("POST", "/documents/validate", &body.to_string())).await;
        let v = parse(&json);
        assert_eq!(v["status"], 200);
        let inner: serde_json::Value = serde_json::from_str(v["body"].as_str().unwrap()).unwrap();
        assert_eq!(inner["valid"], false);
    }

    #[tokio::test]
    async fn not_found_returns_404() {
        let json = dispatch(create_app(), &req("GET", "/nope", "")).await;
        let v = parse(&json);
        assert_eq!(v["status"], 404);
    }

    #[tokio::test]
    async fn request_headers_forwarded() {
        let envelope = RequestEnvelope {
            method: "GET".into(),
            path: "/health".into(),
            query: String::new(),
            headers: HashMap::from([
                ("cookie".into(), "session=abc123".into()),
                ("x-custom".into(), "test-value".into()),
            ]),
            body: String::new(),
        };
        let json = dispatch(create_app(), &envelope).await;
        assert_eq!(parse(&json)["status"], 200);
    }

    #[tokio::test]
    async fn query_string_forwarded() {
        let envelope = RequestEnvelope {
            method: "GET".into(),
            path: "/health".into(),
            query: "foo=bar&baz=1".into(),
            headers: HashMap::new(),
            body: String::new(),
        };
        let json = dispatch(create_app(), &envelope).await;
        assert_eq!(parse(&json)["status"], 200);
    }
}
