#!/usr/bin/env bash

set -e 
cd $(dirname "$0")

test/end_to_end.sh
test/unit.sh