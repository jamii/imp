#!/usr/bin/env bash

set -e
# because of weird caching behavior in zig run, we run this in the directory above
cd $(dirname "$0")/..

ARGS=$@
if [ -z $ARGS ] ; then
    ARGS=$(ls ./test/*.test)
fi

echo Running end_to_end $ARGS
zig run ./test/end_to_end.zig --main-pkg-path ./ -lc -- $ARGS