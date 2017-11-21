#!/bin/bash
set -ex
###############################################################################
# Script for cleaning up qvm-tests-related Docker resources
###############################################################################
docker rm -f qvm-tests || true
docker rm $(docker ps -a -q --filter=ancestor=docker.lab.rigetti.com/qcs/qvm-tests) || true
docker rm $(docker ps -a -q --filter=ancestor=qvm-tests) || true
docker rmi docker.lab.rigetti.com/qcs/qvm-tests || true
docker rmi qvm-tests || true
