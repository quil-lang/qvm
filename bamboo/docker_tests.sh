#!/bin/bash
###############################################################################
# Bamboo script for running QVM tests in the qvm-tests Docker container
#
# Steps:
# 1) Log in to our internal Docker Harbor
# 2) Build the qvm-tests Docker image
# 3) Run the qvm-tests Docker image to execute the QVM tests
# 4) Push the qvm-tests Docker image to the Docker Harbor
###############################################################################
set -ex

docker login -u bamboo -p Bamboo1@ docker.lab.rigetti.com
make -C ../docker qvm-tests
docker run --ipc=host --name qvm-tests docker.lab.rigetti.com/qcs/qvm-tests
docker push docker.lab.rigetti.com/qcs/qvm-tests
