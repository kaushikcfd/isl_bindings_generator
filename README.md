# isl_bindings_generator

- Provides a binary that reads the C header files in the relative path `isl/` and emits Rust-bindings for the same in the path `src/bindings/`.
- Generates the bindings for [isl_rs](https://crates.io/crates/isl_rs).
- Put into a separate crate to prevent `isl_rs` from having expensive dependencies.
- Why not use `bindgen`?
  - Simpler, less dependencies
  - Fine-grained control to match ISL's ownership semantics of `__isl_keep`, `__isl_take`, etc.
- To generate the bindings:
```
git clone https://github.com/kaushikcfd/isl_bindings_generator
cd isl_bindings_generator
git clone https://repo.or.cz/w/isl.git
cargo run
# `src/bindings` will be populated with the generated .rs files.
```
  - Note: `isl_bindings_generator` depends on `clang-rs` to parse the `isl` header files for generating the bindings. `clang-rs` itself needs access to `libclang` that can be installed by `sudo apt install llvm-dev clang`.

- License: MIT
* Problems yet to be solved:
  - [ ] Extracting tokens from functions defined using macros. For eg. `isl_(id|aff|...)_list_from_(id|aff|...)`. Currently `clang`'s tokenizer fails for such entities.
  - [ ] Supporting functions that take in a callback. For eg. `isl_for_each_point`.
  - [ ] Supporting more types like `qpolynomial`, `pwqpolynomial`, `isl_schedule`.
  - [ ] Fixing the docs build at `docs.rs`.
  - [ ] Supporting functions that take at least parameters with `__isl_give`.
  - [ ] Implementing standard traits `Clone`, `Eq`, `Display`, `Debug` for the core types.
