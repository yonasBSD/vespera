# rust-jni-demo

Vespera app that runs in two modes from the same codebase:

| Mode | Transport | How to start |
|------|-----------|--------------|
| **Standalone** | TCP :3000 | `cargo run -p rust-jni-demo` |
| **JNI** | In-process (no network) | Java loads the cdylib |

Both modes use the same `create_app()` → same routes, same logic.

## Prerequisites

- Rust 1.85+
- Java 17+ (for JNI mode)

## Mode A: Standalone Rust Server

```bash
cargo run -p rust-jni-demo
```

```
Server running on http://localhost:3000
  GET  /health
  POST /documents/validate
```

### Test with curl

```bash
# Health check
curl http://localhost:3000/health

# Validate a document
curl -X POST http://localhost:3000/documents/validate \
  -H 'Content-Type: application/json' \
  -d '{
    "documentType": "regulation",
    "title": "Data Protection Policy",
    "content": "This regulation establishes the framework for handling personal data within the organisation.",
    "author": "Kim Minjun",
    "department": "Information Security",
    "classification": "internal",
    "effectiveDate": "2025-01-01"
  }'
```

## Mode B: Java + JNI

Java calls Rust in-process — no HTTP between them.

```
Client ── HTTP ──> Spring Boot ── JNI ──> Rust (axum router)
```

### Step 1: Build the Rust shared library

```bash
cargo build -p rust-jni-demo --release
```

Output:
- Linux: `target/release/librust_jni_demo.so`
- macOS: `target/release/librust_jni_demo.dylib`
- Windows: `target/release/rust_jni_demo.dll`

### Step 2: Build the vespera-bridge JAR

```bash
cd libs/vespera-bridge
./gradlew jar
```

### Step 3: Build and run the Spring Boot app

The native library is **bundled inside the JAR** — single-file deployment.

```bash
cd examples/rust-jni-demo/java
./gradlew :demo-app:bootJar

# Single file, no -Djava.library.path needed:
java -jar demo-app/build/libs/demo-app-0.1.0.jar

# Windows PowerShell (quotes required)
java "-Djava.library.path=..\..\..\target\release" -jar demo-app\build\libs\demo-app-0.1.0.jar
```

Spring starts on `http://localhost:8080` and proxies every request to Rust:

```bash
curl -X POST http://localhost:8080/documents/validate \
  -H 'Content-Type: application/json' \
  -d '{"documentType":"regulation","title":"Test","content":"Some content here for validation purposes.","author":"Kim","department":"Legal","classification":"internal","effectiveDate":"2025-01-01"}'
```

## Run tests

```bash
cargo test -p rust-jni-demo
```

## Project structure

```
examples/rust-jni-demo/
├── Cargo.toml
├── src/
│   ├── lib.rs              # create_app() + vespera::jni_app!(create_app)
│   ├── main.rs             # Mode A: axum::serve on :3000
│   └── routes/
│       ├── documents.rs    # POST /documents/validate
│       └── health.rs       # GET /health
└── java/
    └── demo-app/           # Mode B: Spring Boot proxy (16 lines)
        └── src/.../DemoApplication.java

libs/
└── vespera-bridge/         # Reusable JAR (com.devfive.vespera.bridge)
    └── src/.../VesperaBridge.java
    └── src/.../VesperaProxyController.java
```

## How it works

### Rust side

```rust
// lib.rs — the entire JNI integration:
pub fn create_app() -> axum::Router {
    vespera!(title = "Document Validation API", version = "0.1.0")
}

vespera::jni_app!(create_app);
```

### Java side

```java
// DemoApplication.java — the entire Spring app:
@SpringBootApplication
@ComponentScan(basePackages = {"kr.go.demo", "com.devfive.vespera.bridge"})
public class DemoApplication {
    public static void main(String[] args) {
        VesperaBridge.init("rust_jni_demo");
        SpringApplication.run(DemoApplication.class, args);
    }
}
```

### What happens

1. `vespera::jni_app!` generates `JNI_OnLoad` which registers the router factory
2. `vespera::jni` exports a fixed JNI symbol matching `com.devfive.vespera.bridge.VesperaBridge`
3. `VesperaBridge.init()` loads the native library (from JAR or system path)
4. `VesperaProxyController` (in vespera-bridge JAR) catches all HTTP requests and dispatches to Rust via `VesperaBridge.dispatch(json)`
5. Rust routes through `router.oneshot()` — no TCP between Java and Rust

### Maven/Gradle dependency

```kotlin
// build.gradle.kts
repositories {
    maven { url = uri("https://maven.pkg.github.com/dev-five-git/vespera") }
}
dependencies {
    implementation("com.devfive.vespera:vespera-bridge:0.1.0")
}
```
