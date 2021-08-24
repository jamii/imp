#!/usr/bin/env bash

set -e
cd $(dirname "$0")

test/unit.sh
test/end_to_end.sh