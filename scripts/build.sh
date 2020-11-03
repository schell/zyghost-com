#!/bin/sh -eu

ROOT="$(git rev-parse --show-toplevel)"
. $ROOT/scripts/common.sh

if [ -z ${GITHUB_REF+blah} ]; then
    GITHUB_REF="$(git rev-parse --abbrev-ref HEAD)"
fi

BRANCH=$(basename $GITHUB_REF)

echo "Building site from the '$BRANCH' branch..."
cargo run
