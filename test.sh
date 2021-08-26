#!/usr/bin/env bash

set -e
cd $(dirname "$0")

test/unit.sh
test/end_to_end.sh

echo 'Checking that ./run.sh compiles'
echo '1+1' | ./run.sh - > /dev/null