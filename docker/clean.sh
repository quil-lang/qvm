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

docker rm -f qvm || true
docker rm $(docker ps -a -q --filter=ancestor=docker.lab.rigetti.com/qcs/qvm) || true
docker rm $(docker ps -a -q --filter=ancestor=qvm) || true
docker rmi docker.lab.rigetti.com/qcs/qvm || true
docker rmi qvm || true

docker rm -f qvm-base || true
docker rm $(docker ps -a -q --filter=ancestor=docker.lab.rigetti.com/qcs/qvm-base) || true
docker rm $(docker ps -a -q --filter=ancestor=qvm-base) || true
docker rmi docker.lab.rigetti.com/qcs/qvm-base || true
docker rmi qvm-base || true
