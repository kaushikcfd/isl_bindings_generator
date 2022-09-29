# isl_bindings_generator

- Provides a binary that reads the C header files in the relative path `isl/` and emits Rust-bindings for the same in the path `src/bindings/`.
- Generates the bindings for [isl_rs](https://crates.io/crates/isl_rs).
- Put into a separate crate to prevent `isl_rs` from having expensive dependencies.
- License: MIT
