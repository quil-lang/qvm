#!/bin/bash
###############################################################################
# Script for cleaning up qvm-tests-related Docker resources
###############################################################################
set -ex

docker rm $(docker ps -a -q --filter=ancestor=docker.lab.rigetti.com/qcs/qvm-tests) || true

docker rmi docker.lab.rigetti.com/qcs/qvm-tests || true
