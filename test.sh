#!/usr/bin/env bash

set -x

time zig test test/fuzz.zig --main-pkg-path ./ --release-fast -lc
