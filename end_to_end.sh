#!/usr/bin/env bash

set -e

zig run ./test/end_to_end.zig --main-pkg-path ./ -lc $@
