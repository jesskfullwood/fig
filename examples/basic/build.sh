#!/usr/bin/env bash
cargo build --target wasm32-unknown-unknown
mkdir -p ./pkg
wasm-bindgen target/wasm32-unknown-unknown/debug/basic.wasm --no-modules --out-dir ./pkg --out-name package
