#!/usr/bin/env bash
cargo build --target wasm32-unknown-unknown --release
mkdir -p server/static/dist
wasm-bindgen target/wasm32-unknown-unknown/release/demo.wasm --no-modules --out-dir ./server/static/dist --out-name demo
