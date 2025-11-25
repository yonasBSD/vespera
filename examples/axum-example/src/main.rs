use axum_example::create_app;

#[tokio::main]
async fn main() {
    let app = create_app();

    let addr = std::net::SocketAddr::from(([0, 0, 0, 0], 3000));
    let listener = tokio::net::TcpListener::bind(addr).await.unwrap();

    println!("🚀 Server running on http://localhost:3000");
    println!("📝 Available routes:");
    println!("   GET  /health");
    println!("   GET  /users");
    println!("   GET  /users/:id");
    println!("   POST /users");

    axum::serve(listener, app).await.unwrap();
}
