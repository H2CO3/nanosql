#!/bin/sh

set -eu

cargo clean
cargo test --workspace --all-features
