#!/usr/bin/env bash

set -e
cd $(dirname "$0")

ARGS=$@
if [ -z $ARGS ] ; then
    ARGS=$(ls ./*.test)
fi

echo Running end_to_end $ARGS
zig run ./end_to_end.zig --main-pkg-path ../ -lc -- $ARGS