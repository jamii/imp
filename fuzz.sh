#!/usr/bin/env bash

set -e

time zig test test/fuzz.zig --main-pkg-path ./ --release-fast -lc --test-filter fuzz
