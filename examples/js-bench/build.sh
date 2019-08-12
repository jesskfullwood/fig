set -e

#!/usr/bin/env bash
cargo build --target wasm32-unknown-unknown --release
mkdir -p ./dist
wasm-bindgen target/wasm32-unknown-unknown/release/dbmon.wasm --no-modules --out-dir ./dist --out-name dbmon
cp index.html dist/

python3 ../serve.py dist
