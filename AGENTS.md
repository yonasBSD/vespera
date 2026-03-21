# VESPERA PROJECT KNOWLEDGE BASE

**Generated:** 2026-03-19
**Branch:** main

## OVERVIEW

Vespera is a fully automated OpenAPI 3.1 engine for Axum - delivers FastAPI-like DX to Rust. Zero-config route discovery via compile-time macro scanning.

Also provides in-process dispatch (`inprocess` feature) and JNI integration (`jni` feature) for embedding Rust axum apps inside Java/Spring applications without HTTP overhead.

## STRUCTURE

```
vespera/
├── crates/
│   ├── vespera/              # Public API - re-exports everything
│   │   ├── src/lib.rs        # Core re-exports + conditional modules
│   │   ├── src/inprocess.rs  # In-process dispatch (feature: inprocess)
│   │   └── src/jni.rs        # JNI integration (feature: jni)
│   ├── vespera_core/         # OpenAPI types, route/schema abstractions
│   └── vespera_macro/        # Proc-macros (main logic lives here)
├── libs/
│   └── vespera-bridge/       # Java library (com.devfive.vespera.bridge)
│       ├── VesperaBridge.java          # JNI native loader + dispatch
│       └── VesperaProxyController.java # Auto-configured Spring proxy
├── examples/
│   ├── axum-example/         # Standard axum server demo
│   └── rust-jni-demo/        # JNI + standalone server demo
│       ├── src/              # Rust: routes, create_app(), JNI_OnLoad
│       └── java/demo-app/    # Java: Spring Boot proxy (16 lines)
```

## WHERE TO LOOK

| Task | Location | Notes |
|------|----------|-------|
| Add new macro feature | `crates/vespera_macro/src/` | Main macro in `lib.rs` |
| Modify OpenAPI output | `crates/vespera_macro/src/openapi_generator.rs` | JSON generation |
| Add route parser feature | `crates/vespera_macro/src/parser/` | Type extraction logic |
| Change schema generation | `crates/vespera_macro/src/parser/schema.rs` | Rust→JSON Schema |
| Modify route attribute | `crates/vespera_macro/src/args.rs` | `#[route]` parsing |
| Modify schema_type! macro | `crates/vespera_macro/src/schema_macro.rs` | Type derivation & SeaORM support |
| Add core types | `crates/vespera_core/src/` | OpenAPI spec types |
| Test new features | `examples/axum-example/` | Add route, run example |
| In-process dispatch | `crates/vespera/src/inprocess.rs` | RequestEnvelope → Router → ResponseEnvelope |
| JNI integration | `crates/vespera/src/jni.rs` | JNI_OnLoad macro, fixed JNI symbol export |
| Java bridge library | `libs/vespera-bridge/` | com.devfive.vespera.bridge package |
| JNI demo (Rust) | `examples/rust-jni-demo/src/` | Routes + vespera::jni_app! |
| JNI demo (Java) | `examples/rust-jni-demo/java/` | Spring Boot proxy app |

## KEY COMPONENTS

| File | Lines | Role |
|------|-------|------|
| `vespera_macro/src/lib.rs` | ~1044 | `vespera!`, `#[route]`, `#[derive(Schema)]` |
| `vespera_macro/src/schema_macro.rs` | ~3000 | `schema_type!` macro, SeaORM relation handling |
| `vespera_macro/src/parser/schema.rs` | ~1527 | Rust struct → JSON Schema conversion |
| `vespera_macro/src/parser/parameters.rs` | ~845 | Extract path/query params from handlers |
| `vespera_macro/src/openapi_generator.rs` | ~808 | OpenAPI doc assembly |
| `vespera_macro/src/collector.rs` | ~707 | Filesystem route scanning |
| `vespera/src/inprocess.rs` | ~150 | In-process HTTP dispatch via oneshot |
| `vespera/src/jni.rs` | ~155 | JNI exports + `jni_app!` macro |

## FEATURES (crates/vespera)

```toml
[features]
default = [...]           # axum-extra features
inprocess = [http, http-body-util, tower, serde]   # In-process dispatch
jni = [inprocess, jni-crate, tokio]                 # JNI = inprocess + JVM glue
```

- `inprocess`: Transport-agnostic `dispatch(Router, &RequestEnvelope) -> String`
- `jni`: Adds `jni_app!` macro + `JNI_OnLoad` + fixed JNI symbol export
- JNI symbol: `Java_com_devfive_vespera_bridge_VesperaBridge_dispatch`

## JNI ARCHITECTURE

```
Java (Spring Boot)          Rust (cdylib)           vespera framework
─────────────────          ──────────────          ─────────────────
VesperaBridge.init()   →   JNI_OnLoad             register_app(create_app)
    ↓                          ↓
VesperaBridge.dispatch() → JNI symbol             → inprocess::dispatch()
    ↓                          ↓                        ↓
VesperaProxyController     catch_unwind            router.oneshot(request)
    ↓                          ↓                        ↓
ResponseEntity             JSON envelope           axum handlers
```

### Rust side (example app — 2 lines of JNI code):
```rust
pub fn create_app() -> axum::Router { vespera!(...) }
vespera::jni_app!(create_app);
```

### Java side (user app — 1 meaningful line):
```java
VesperaBridge.init("rust_jni_demo");
SpringApplication.run(DemoApplication.class, args);
```

## SCHEMA_TYPE! MACRO

Generate request/response types from existing structs with powerful transformations.

### Key Features
- **Same-file Model reference**: `schema_type!(Schema from Model, name = "UserSchema")`
- **Cross-file reference**: `schema_type!(Response from crate::models::user::Model, omit = ["password"])`
- **SeaORM integration**: Automatic conversion of `HasOne`, `BelongsTo`, `HasMany` relations
- **Chrono conversion**: `DateTimeWithTimeZone` → `vespera::chrono::DateTime<FixedOffset>`
- **Circular reference handling**: Automatic detection and inline field generation

### Parameters
| Parameter | Description |
|-----------|-------------|
| `pick` | Include only specified fields |
| `omit` | Exclude specified fields |
| `rename` | Rename fields: `[("old", "new")]` |
| `add` | Add new fields (disables auto `From`) |
| `clone` | Control Clone derive (default: true) |
| `partial` | Make fields optional for PATCH |
| `name` | Custom OpenAPI schema name |
| `rename_all` | Serde rename strategy |
| `ignore` | Skip Schema derive |

## CONVENTIONS

- **Rust 2024 edition** across all crates
- **Workspace dependencies**: Internal crates use `{ workspace = true }`
- **Test frameworks**: `rstest` for unit tests, `insta` for snapshots
- **No `build.rs`**: All code gen via proc-macros at compile time
- **Java package**: `com.devfive.vespera.bridge` (fixed for JNI symbol stability)
- **Java build**: Gradle (Kotlin DSL), published to GitHub Packages

## ANTI-PATTERNS (THIS PROJECT)

- **NEVER** add `build.rs` - macro handles compile-time generation
- **NEVER** manually register routes - `vespera!` macro discovers them
- **NEVER** write OpenAPI JSON by hand - generated from code
- **NEVER** write JNI boilerplate in examples - use `vespera::jni_app!` macro
- **NEVER** parse domain JSON in Java - Spring is a proxy, Rust owns business logic
- Route functions **MUST** be `pub async fn`

## COMMANDS

```bash
# Development
cargo build                    # Build all crates
cargo test --workspace         # Run all tests
cargo test -p vespera_macro    # Test macros only
cargo test -p rust-jni-demo    # Test JNI demo

# Run axum example
cd examples/axum-example
cargo run                      # Starts server on :3000

# Run JNI demo (standalone Rust server)
cargo run -p rust-jni-demo     # Starts server on :3000

# Run JNI demo (Java + Rust)
cd libs/vespera-bridge && ./gradlew jar
cargo build -p rust-jni-demo --release
cd examples/rust-jni-demo/java && ./gradlew :demo-app:bootJar
java -jar demo-app/build/libs/demo-app-0.1.0.jar

# Check generated OpenAPI
cat examples/axum-example/openapi.json
```

## NOTES

- Macro performs **filesystem I/O at compile time** - may affect IDE performance
- OpenAPI files are **regenerated on every build** when `openapi = "..."` specified
- `CARGO_MANIFEST_DIR` env var used to locate `src/routes/` folder
- Generic types in schemas require `#[derive(Schema)]` on all type params
- JNI native library can be bundled inside the fat JAR for single-file deployment
- `VesperaBridge.init()` auto-extracts bundled native lib to temp, falls back to system path
