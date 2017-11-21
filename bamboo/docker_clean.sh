#!/bin/bash
set -ex
###############################################################################
# Bamboo script for cleaning up Docker resources after a build
#
# Steps:
# 1) Remove Docker containers and images as defined in the docker Makefile
# 2) Remove all dangling Docker images and volumes from the worker
###############################################################################
make -C ../docker clean
docker system prune -af || true
