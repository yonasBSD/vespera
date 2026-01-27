//! Metadata collection and storage for routes and schemas

use serde::{Deserialize, Serialize};

/// Route metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteMetadata {
    /// HTTP method
    pub method: String,
    /// Route path
    pub path: String,
    /// Function name
    pub function_name: String,
    /// Module path
    pub module_path: String,
    /// File path
    pub file_path: String,
    /// Function signature (as string for serialization)
    pub signature: String,
    /// Additional error status codes from error_status attribute
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error_status: Option<Vec<u16>>,
    /// Tags for OpenAPI grouping
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tags: Option<Vec<String>>,
    /// Description for OpenAPI (from route attribute or doc comment)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}

/// Struct metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructMetadata {
    /// Struct name
    pub name: String,
    /// Struct definition (as string for serialization)
    pub definition: String,
    /// Whether to include in OpenAPI spec (components/schemas)
    /// - true: from #[derive(Schema)] - appears in openapi.json (DEFAULT)
    /// - false: from cross-file lookup - only for schema_type! source, NOT in openapi.json
    #[serde(default = "default_include_in_openapi")]
    pub include_in_openapi: bool,
}

fn default_include_in_openapi() -> bool {
    true
}

impl Default for StructMetadata {
    fn default() -> Self {
        Self {
            name: String::new(),
            definition: String::new(),
            include_in_openapi: true, // Default to true (appears in OpenAPI)
        }
    }
}

impl StructMetadata {
    /// Create a new StructMetadata with include_in_openapi defaulting to true
    pub fn new(name: String, definition: String) -> Self {
        Self {
            name,
            definition,
            include_in_openapi: true,
        }
    }

    /// Create a new StructMetadata for model types (not included in OpenAPI)
    pub fn new_model(name: String, definition: String) -> Self {
        Self {
            name,
            definition,
            include_in_openapi: false,
        }
    }
}

/// Collected metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollectedMetadata {
    /// Routes
    pub routes: Vec<RouteMetadata>,
    /// Structs
    pub structs: Vec<StructMetadata>,
}

impl CollectedMetadata {
    pub fn new() -> Self {
        Self {
            routes: Vec::new(),
            structs: Vec::new(),
        }
    }
}
