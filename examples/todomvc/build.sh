#!/usr/bin/env bash

set -e

cargo build --target wasm32-unknown-unknown --release
rm -r dist
mkdir -p ./dist
cp index.html app.css dist

wasm-bindgen target/wasm32-unknown-unknown/release/todomvc.wasm \
             --no-typescript \
             --no-modules \
             --out-dir ./dist \
             --out-name app

../serve.py dist
