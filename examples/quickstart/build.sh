set -e

#!/usr/bin/env bash
cargo build --target wasm32-unknown-unknown --release
mkdir -p ./dist
wasm-bindgen target/wasm32-unknown-unknown/release/quickstart.wasm --no-modules --out-dir ./dist --out-name quickstart
cp index.html dist/

cd dist && python3 -m http.server
