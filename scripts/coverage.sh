#!/bin/bash

set -e

cargo install grcov --version 0.8.4

rustup component add llvm-tools-preview
export RUSTFLAGS="-Zinstrument-coverage -Copt-level=0 -Clink-dead-code"

# Clean to remove cached build artifacts
cargo clean

# Build the binary for the compiler tests
cargo build

LLVM_PROFILE_FILE="target/coverage/jswt-%p-%m.profraw" cargo test
grcov ./**/target/coverage --binary-path ./target/debug/ -s . -t lcov --branch --ignore-not-existing --ignore "/*" -o lcov.info
grcov ./**/target/coverage --binary-path ./target/debug/ -s . -t html --branch --ignore-not-existing --ignore "/*" -o ./coverage

set +e