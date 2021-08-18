#!/usr/bin/env bash

set -e
cd $(dirname "$0")

for file in $(rg --files ../lib | grep -F '.zig')
do 
  echo Running unit $file
  zig test --main-pkg-path ../ $file
done