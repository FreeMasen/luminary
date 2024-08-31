# Contributing

To start, thank you for your interest in contributing to Luminary! This document will outline
some things that should help you get started.

## Project Structure

This project is split into 4 components currently

- [Runtime Library](./crates/runtime-lib/) This is where the runtime logic for the final output
  resides.
- [Runtime Macros](./crates/runtime-macros/) This is where helper macros are defined
  - The only macro currently is `std_tvalue_export` which defines the link name for the function it
    decorates. There are 2 features in the runtime library that control what this is exporting, the
    `runtime` feature will actually export the function while the `names` feature will export the
    name as a `cost &'static str`
- [Compiler Library](./src/lib.rs) This is the primary entrypoint for compiling lua to llvm-ir.
- [Compiler Binary](./src/main.rs) This is the comand line application that can be used to compile
  code.

## Running Tests

### Runtime

By default this crate provides nothing, and the tests will require that the `runtime` feature is
enabled. This means running these tests should look like this:
`cargo test -p luminary-runtime --feature runtime`.

### Compiler Lib/Compiler Binary

These two tests will require a linkable LLVM version 16 library. The location should be provided in
the environment variable `LLVM_SYS_160_PREFIX`. As is the requirement of `llvm-sys` this directory
must also include the `llvm-config` in the `bin` directory, this isn't provided by default by
packages managers so a custom download would be required see
[llvm-build](https://github.com/FreeMasen/llvm-build), there should be a compatible package there.
