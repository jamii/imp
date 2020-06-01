#!/usr/bin/env bash

set -e

ARGS=$@
if [ -z $ARGS ] ; then
    ARGS=$(ls test/*.test)
fi

zig run ./test/end_to_end.zig --main-pkg-path ./ -lc -- $ARGS
