#!/usr/bin/env bash

set -e
cd $(dirname "$0")

echo "Running unit tests"
zig test --main-pkg-path ../ ../lib/imp.zig -lc