#!/bin/bash

set -e

make cleanall

QVM_WORKSPACE=30720 make

docker build -t qvm-prod -f Dockerfile_prod .

docker tag qvm-prod rigetticomputing/qvm-prod
docker push rigetticomputing/qvm-prod
