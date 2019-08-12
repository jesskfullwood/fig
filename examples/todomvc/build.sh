#!/usr/bin/env bash

set -e

cargo build --target wasm32-unknown-unknown --release
mkdir -p ./dist

wasm-bindgen target/wasm32-unknown-unknown/release/todomvc.wasm \
             --target no-modules \
             --no-typescript \
             --out-dir ./dist \
             --out-name app

../serve.py dist
