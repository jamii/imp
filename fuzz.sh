#!/usr/bin/env bash

set -e

zig test test/fuzz.zig --main-pkg-path ./ --release-safe -lc --test-filter fuzz --test-cmd rr --test-cmd record --test-cmd-bin $@
