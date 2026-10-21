#!/bin/bash
# Copy release/tonto into the run folder (the current directory) as
# tonto.grid, and record its commit and a hash of any uncommitted change to
# foofiles/, so every stored row names the binary that made it.
T=$(cd "$(dirname "$0")/../.." && pwd)
cp "$T/release/tonto" ./tonto.grid
( cd "$T" && echo "$(git rev-parse --short HEAD)+$(git diff -- foofiles | shasum | cut -c1-8)" ) > tonto.grid.id
cat tonto.grid.id
