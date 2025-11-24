use proc_macro2::TokenStream;
use quote::quote;

pub enum Method {
    Get,
    Post,
    Put,
    Patch,
    Delete,
    Head,
    Options,
}

impl TryFrom<&str> for Method {
    type Error = String;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value.to_lowercase().as_str() {
            "get" => Ok(Method::Get),
            "post" => Ok(Method::Post),
            "put" => Ok(Method::Put),
            "patch" => Ok(Method::Patch),
            "delete" => Ok(Method::Delete),
            "head" => Ok(Method::Head),
            "options" => Ok(Method::Options),
            _ => return Err(format!("Invalid method: {}", value)),
        }
    }
}

impl TryFrom<Method> for TokenStream {
    type Error = String;
    fn try_from(value: Method) -> Result<Self, Self::Error> {
        match value {
            Method::Get => Ok(quote! { axum::routing::get }),
            Method::Post => Ok(quote! { axum::routing::post }),
            Method::Put => Ok(quote! { axum::routing::put }),
            Method::Patch => Ok(quote! { axum::routing::patch }),
            Method::Delete => Ok(quote! { axum::routing::delete }),
            Method::Head => Ok(quote! { axum::routing::head }),
            Method::Options => Ok(quote! { axum::routing::options }),
        }
    }
}
