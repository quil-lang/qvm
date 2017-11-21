#!/bin/bash
set -ex
###############################################################################
# Bamboo script for building the Docker image(s) defined in the docker Makefile
###############################################################################
make -C ../docker
