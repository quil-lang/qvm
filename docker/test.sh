#!/bin/bash
set -ex
###############################################################################
# Script for running the dockerized qvm tests
###############################################################################

# Trailing "/." is special docker cp syntax. See link for details.
# https://docs.docker.com/engine/reference/commandline/cp/#extended-description
INPUT="/src/qvm/coverage-report/html/."
OUTPUT="../coverage-report/html/"
CONTAINER=qvm-tests
IMAGE=qvm-tests

mkdir -p "$OUTPUT"
docker run --ipc=host --name "$CONTAINER" "$IMAGE"
docker cp "$CONTAINER":"$INPUT" "$OUTPUT"
