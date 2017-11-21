#!/bin/bash
set -ex
###############################################################################
# Script for running the dockerized qvm tests
###############################################################################
docker run --ipc=host --name qvm-tests qvm-tests
