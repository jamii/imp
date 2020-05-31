#!/usr/bin/env bash

set -e

zig run ./bin/run.zig --main-pkg-path ./ -lc -- $@
