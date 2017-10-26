#!/bin/bash
###############################################################################
# Bamboo script for cleaning up Docker resources after a build
#
# Steps:
# 1) Stop and remove any existing qvm-tests container
# 2) Remove all dangling Docker images and volumes from the worker
###############################################################################
set -ex

docker rm -f qvm-tests || true
docker system prune -af || true
