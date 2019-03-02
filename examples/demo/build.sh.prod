#!/usr/bin/env bash
cargo build --target wasm32-unknown-unknown --release
mkdir -p ./pkg
wasm-bindgen target/wasm32-unknown-unknown/release/demo.wasm --no-modules --out-dir ./pkg --out-name package
