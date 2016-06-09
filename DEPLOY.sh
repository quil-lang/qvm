#!/bin/bash

make docker
docker save -o docker_qvm.tar qvm
scp -i ../../keys/qvm_demo.pem docker_qvm.tar ubuntu@ec2-54-183-88-123.us-west-1.compute.amazonaws.com:~/
ssh -i ../../keys/qvm_demo.pem ubuntu@ec2-54-183-88-123.us-west-1.compute.amazonaws.com << EOF
  docker kill qvm
  docker rm qvm
  docker rmi qvm
  docker load -i docker_qvm.tar 
  docker run --name qvm -p 5000:5000 -d qvm
EOF
