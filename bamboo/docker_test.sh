#!/bin/bash
set -ex
###############################################################################
# Bamboo script for running the dockerized tests defined in the docker Makefile
###############################################################################
make -C ../docker test
